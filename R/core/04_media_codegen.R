# R/core/04_media_codegen.R
# Code generation for media VOFs (configured VOF + its Spend/Activity twin +
# backcasting) and the matching metadata rows. The app code is tidyverse; the
# GENERATED code must run on AWB's R 3.6 using base R only.

# ------------------------------------------------------------------------------
# Validation
# ------------------------------------------------------------------------------
validate_components <- function(components, mff_vars = NULL) {
  if (length(components) == 0) return("Add at least one Component Row.")
  imap(components, function(comp, i) {
    s <- comp$start_period; e <- comp$end_period
    c(
      if (comp$analytical_var == "") sprintf("Row %d: select an Analytical Variable.", i)
      else if (!is.null(mff_vars) && !comp$analytical_var %in% mff_vars)
        sprintf("Row %d: '%s' is not an MFF variable of the Analytical.", i, comp$analytical_var),
      if (comp$variable_name == "") sprintf("Row %d: VariableName is empty.", i),
      if (!is.na(s) && !is.na(e) && s > e) sprintf("Row %d: Start Period is after End Period.", i)
    )
  }) %>% unlist() %>% as.character()
}

# ------------------------------------------------------------------------------
# Filters and feed lines
# ------------------------------------------------------------------------------
# Row filter of a component, e.g.
#   Data$Period >= as.Date('2024-01-01') & !(tolower(Data$Geography) %in% tolower(c('Atlanta')))
build_filter_expr <- function(start_period, end_period, cs) {
  period_filters <- c(
    if (is_valid_value(start_period)) sprintf("Data$%s >= as.Date(%s)", PERIOD_COL, r_str(format_date_iso(start_period))),
    if (is_valid_value(end_period))   sprintf("Data$%s <= as.Date(%s)", PERIOD_COL, r_str(format_date_iso(end_period)))
  )
  cs_filters <- MFF_DIMS %>% keep(~ !is.null(cs[[.x]])) %>% map_chr(~ cs_filter_expr(.x, cs[[.x]]))
  str_c(c(period_filters, cs_filters), collapse = " & ")
}

component_comment <- function(comp) {
  sprintf("# VariableName: %s; Complement: %s; Effect: %s", comp$variable_name, comp$complement, comp$effect)
}

build_init_line <- function(vof_name) sprintf("Data[, %s] <- 0", r_str(vof_name))

# Data[<filter>, 'VOF'] <- rowSums(Data[<filter>, c('VOF', 'MFF var')], na.rm = TRUE) # labels
build_feed_line <- function(vof_name, comp) {
  f <- build_filter_expr(comp$start_period, comp$end_period, comp$cs)
  sprintf("Data[%s, %s] <- rowSums(Data[%s, %s], na.rm = TRUE) %s",
          f, r_str(vof_name), f, r_str_vec(c(vof_name, comp$analytical_var)), component_comment(comp))
}

# ------------------------------------------------------------------------------
# Twins (Activity <-> Spend)
# ------------------------------------------------------------------------------
# MFF variables that differ from `analytical_var` only in the metric word,
# for metrics of `target_type` ("Spend" or "Activity"), in METRICS order.
find_twin_candidates <- function(analytical_var, target_type, mff_vars, long_dims) {
  empty <- tibble(var = character(0), Metric = character(0))
  m <- detect_metric(analytical_var, long_dims)
  if (is.null(m) || length(mff_vars) == 0) return(empty)
  parsed <- parse_mff_names(analytical_var, long_dims)
  is_mff <- isTRUE(parsed$is_mff[1])
  base <- if (is_mff) parsed$base[1] else analytical_var
  dim_values <- if (is_mff) unlist(parsed[1, long_dims]) else character(0)

  metric_terms() %>%
    filter(Type == target_type) %>%
    mutate(candidate = map_chr(term, ~ build_mff_name(str_replace(base, word_regex(m$term), .x), dim_values)),
           idx = match(str_to_lower(candidate), str_to_lower(mff_vars))) %>%
    filter(!is.na(idx)) %>%
    transmute(var = mff_vars[idx], Metric) %>%
    distinct(var, .keep_all = TRUE)
}

# Backwards-compatible helper: first spend twin of an activity variable.
find_spend_pair <- function(activity_var, mff_vars, long_dims) {
  cand <- find_twin_candidates(activity_var, "Spend", mff_vars, long_dims)
  if (nrow(cand) == 0) NULL else cand$var[1]
}

# Preferred activity metric for a channel name (CHANNEL_METRIC_RULES).
channel_preferred_metric <- function(variable_name) {
  hit <- CHANNEL_METRIC_RULES %>% filter(map_lgl(Pattern, ~ str_detect(variable_name, regex(str_c("\\b(", .x, ")\\b"), ignore_case = TRUE))))
  if (nrow(hit) == 0) NULL else hit$Metric[1]
}

# Candidates of every component. Returns a list (one element per component)
# with `candidates` (tibble var/Metric) and `preselected` (var or NA).
twin_options <- function(components, target_type, mff_vars, long_dims) {
  map(components, function(comp) {
    cand <- find_twin_candidates(comp$analytical_var, target_type, mff_vars, long_dims)
    pre <- NA_character_
    if (nrow(cand) > 0) {
      pref <- if (target_type == "Activity") channel_preferred_metric(comp$variable_name) else NULL
      pre <- if (!is.null(pref) && pref %in% cand$Metric) cand$var[cand$Metric == pref][1] else cand$var[1]
    }
    list(analytical_var = comp$analytical_var, variable_name = comp$variable_name,
         candidates = cand, preselected = pre)
  })
}

# ------------------------------------------------------------------------------
# Backcasting (VOF level)
# ------------------------------------------------------------------------------
resolve_backcasting <- function(backcasting) {
  bc <- backcasting %||% list()
  if (!isTRUE(bc$enabled)) return(list(enabled = FALSE, errors = character(0)))
  start <- if (is_valid_value(bc$start)) parse_dates_flexible(bc$start)[1] else NA
  end   <- if (is_valid_value(bc$end))   parse_dates_flexible(bc$end)[1]   else NA
  errors <- c(
    if (is.na(start) || is.na(end)) "Backcasting is enabled but the calibration window is incomplete.",
    if (!is.na(start) && !is.na(end) && start > end) "Backcasting calibration start is after its end."
  )
  list(enabled = length(errors) == 0, start = start, end = end, errors = errors %||% character(0))
}

# Imputes the zeros before the calibration window with the mean of the non-zero
# values inside the window, by cross-sectional entity. Guarded so it never
# fails in AWB when the window has no data. (Generated code: base R only.)
build_backcasting_code <- function(vof_name, cs_dims, start_date, end_date) {
  v <- r_col(vof_name)
  header <- c(
    "# --- BACKCASTING IMPUTATION ---",
    sprintf("lower_bound <- as.Date(%s)", r_str(format_date_iso(start_date))),
    sprintf("upper_bound <- as.Date(%s)", r_str(format_date_iso(end_date))),
    "",
    "# 1. Calibration window: non-zero, non-missing values within range",
    sprintf("in_range <- Data$%s >= lower_bound & Data$%s <= upper_bound & %s != 0", PERIOD_COL, PERIOD_COL, v),
    "in_range[is.na(in_range)] <- FALSE",
    "",
    "if (any(in_range)) {"
  )
  if (length(cs_dims) > 0) {
    keys <- function(df) if (length(cs_dims) > 1) sprintf("paste(%s, sep = '|')", str_c(df, "$", cs_dims, collapse = ", ")) else str_c(df, "$", cs_dims)
    means <- c(
      "  # 2. Mean by cross-sectional entity",
      sprintf("  means <- aggregate(x = %s[in_range], by = list(%s), FUN = mean)", v,
              str_c(sprintf("%s = Data$%s[in_range]", cs_dims, cs_dims), collapse = ", ")),
      "  # 3. Map each row to the mean of its entity",
      sprintf("  mapped_means <- means$x[match(%s, %s)]", keys("Data"), keys("means"))
    )
    cleanup <- "  rm(means, mapped_means, to_impute)"
  } else {
    means <- c("  # 2. Global mean", sprintf("  mapped_means <- rep(mean(%s[in_range]), nrow(Data))", v))
    cleanup <- "  rm(mapped_means, to_impute)"
  }
  c(header, means,
    "  # 4. Impute zeros before the calibration window",
    sprintf("  to_impute <- Data$%s < lower_bound & %s == 0 & !is.na(mapped_means)", PERIOD_COL, v),
    "  to_impute[is.na(to_impute)] <- FALSE",
    sprintf("  Data[to_impute, %s] <- mapped_means[to_impute]", r_str(vof_name)),
    cleanup,
    "}",
    "rm(lower_bound, upper_bound, in_range)"
  ) %>% str_c(collapse = "\n")
}

# ------------------------------------------------------------------------------
# Metadata rows
# ------------------------------------------------------------------------------
build_media_metadata <- function(vof_name, components, cs_dims, metric,
                                 backcasting = list(enabled = FALSE), cs_labels = list()) {
  bc_on <- isTRUE(backcasting$enabled)
  map(components, function(comp) {
    row <- tibble(
      Type = METADATA_TYPES[["media"]],
      MainModelVariableName  = vof_name,
      AnalyticalVariableName = comp$analytical_var,
      VofVariableName = comp$variable_name,
      VofComplement   = comp$complement,
      Effect          = comp$effect,
      MinPeriod       = format_date_iso(comp$start_period),
      MaxPeriod       = format_date_iso(comp$end_period),
      BackcastingStart = if (bc_on) format_date_iso(backcasting$start) else "",
      BackcastingEnd   = if (bc_on) format_date_iso(backcasting$end) else ""
    )
    for (d in cs_dims) {
      meta <- cs_filter_to_meta(comp$cs[[d]])
      row[[d]] <- meta$values
      row[[str_c(d, "Filter")]] <- meta$filter
      row[[str_c(d, "Group")]] <- if (!is.null(comp$cs[[d]])) cs_labels[[d]] %||% "" else ""
    }
    row %>% mutate(Metric = metric)
  }) %>% list_rbind()
}

# ------------------------------------------------------------------------------
# Main entry point
# ------------------------------------------------------------------------------
# Builds a media VOF and (optionally) its twin from a list of components.
#   backcasting:     list(enabled, start, end), applied to both VOFs.
#   metric_override: METRICS$Metric used when a component's metric is unknown.
#   twin_choices:    named list (component index -> MFF variable) chosen by the
#                    user when an activity twin has several candidates.
#   context:         list(analytical, history, cs_sep) used for name flags
#                    ("topN", groups) and their uniqueness in the project.
# Returns names, code, metadata, warnings, errors, or `needs_choice` (twin
# options to show to the user) when a choice is required.
generate_media_vof <- function(components, cs_dims, mff_vars, long_dims = longitudinal_dims(cs_dims),
                               generate_twin = TRUE, backcasting = NULL, metric_override = NULL,
                               twin_choices = NULL, context = list()) {
  bc <- resolve_backcasting(backcasting)
  errors <- c(validate_components(components, mff_vars), bc$errors)
  if (length(errors) > 0) return(list(errors = errors))

  metric <- resolve_vof_metric(components, long_dims, metric_override)
  if (length(metric$errors) > 0) return(list(errors = metric$errors))
  label_errors <- validate_metric_not_in_labels(components, metric)
  if (length(label_errors) > 0) return(list(errors = label_errors))

  # --- Twin -----------------------------------------------------------------
  warnings <- character(0)
  twin <- NULL
  if (isTRUE(generate_twin)) {
    target <- if (metric$type == "Spend") "Activity" else "Spend"
    opts <- twin_options(components, target, mff_vars, long_dims)
    chosen <- imap(opts, function(o, i) {
      if (nrow(o$candidates) == 0) return(NA_character_)
      if (nrow(o$candidates) == 1 || target == "Spend") return(o$candidates$var[1])
      twin_choices[[as.character(i)]] %||% NA_character_
    })
    pending <- keep(seq_along(opts), ~ nrow(opts[[.x]]$candidates) > 1 && target == "Activity" && is.na(chosen[[.x]]))
    if (length(pending) > 0) return(list(needs_choice = opts[pending] %>% set_names(pending), errors = character(0)))

    paired <- !map_lgl(chosen, is.na)
    unpaired <- map_chr(components[!paired], "analytical_var")
    if (any(paired)) {
      twin_components <- map2(components[paired], chosen[paired], ~ modifyList(.x, list(analytical_var = .y)))
      twin_metric <- resolve_vof_metric(twin_components, long_dims)
      if (length(twin_metric$errors) > 0)
        return(list(errors = str_c("Twin VOF: ", twin_metric$errors)))
      twin <- list(components = twin_components, metric = twin_metric)
      if (length(unpaired) > 0)
        warnings <- c(warnings, sprintf("No %s twin found for: %s. The twin VOF does not include them.",
                                        str_to_lower(target), str_c(unique(unpaired), collapse = "; ")))
    } else {
      warnings <- c(warnings, sprintf("No %s twin was found for any component; only the configured VOF was created.",
                                      str_to_lower(target)))
    }
  }

  # --- Name flags -------------------------------------------------------------
  analytical <- context$analytical
  registry <- if (!is.null(analytical)) cs_flag_registry(context$history, cs_dims, analytical) else NULL
  roots <- unique(c(map_chr(components, "analytical_var"), if (!is.null(twin)) map_chr(twin$components, "analytical_var")))
  cs_labels <- list()
  for (d in active_cs_dims(components)) {
    customs <- components %>% map_chr(~ .x$cs_labels[[d]] %||% "") %>% keep(nzchar) %>% unique()
    f <- common_cs_filter(components, d)
    if (is.null(f)) {
      if (length(customs) > 0) warnings <- c(warnings, sprintf("%s group name '%s' not used: the rows filter %s differently.", d, customs[1], d))
      next
    }
    entities <- if (!is.null(analytical)) cs_dim_values(analytical, d) else unlist(map(f$levels, "values"))
    if (length(customs) > 1)
      return(list(errors = sprintf("The rows give different %s group names (%s): use one name.", d, str_c(customs, collapse = ", "))))
    if (length(customs) == 1) {
      lab <- cs_custom_flag(d, f, entities, customs, registry)
      if (nzchar(lab$error)) return(list(errors = lab$error))
      cs_labels[[d]] <- lab$label
      if (nzchar(lab$note)) warnings <- c(warnings, lab$note)
      next
    }
    totals <- if (!is.null(analytical)) cs_root_totals(analytical, d, map_chr(components, "analytical_var")) else NULL
    lab <- cs_flag_label(d, f, entities, totals, registry, sep = context$cs_sep)
    cs_labels[[d]] <- lab$label
    if (nzchar(lab$note)) warnings <- c(warnings, lab$note)
  }

  base_name <- build_media_vof_base_name(components, cs_labels, backcasted = bc$enabled)
  title <- build_media_vof_title(components, NULL, backcasted = bc$enabled)

  vofs <- list(list(components = components, metric = metric))
  if (!is.null(twin)) vofs <- c(vofs, list(twin))
  vofs <- vofs[order(map_chr(vofs, ~ .x$metric$type) == "Spend")]  # activity first, spend second

  blocks <- map(vofs, function(v) {
    name <- vof_full_name(base_name, v$metric$suffix)
    lines <- c(sprintf("#### %s ####", str_c(title, " - ", v$metric$metric)), build_init_line(name),
               map_chr(v$components, ~ build_feed_line(name, .x)),
               if (bc$enabled) c("", build_backcasting_code(name, cs_dims, bc$start, bc$end)))
    list(name = name, type = v$metric$type,
         code = str_c(lines, collapse = "\n"),
         metadata = build_media_metadata(name, v$components, cs_dims, v$metric$metric, bc, cs_labels))
  })
  configured <- vof_full_name(base_name, metric$suffix)

  list(name = configured,
       twin_name = if (!is.null(twin)) vof_full_name(base_name, twin$metric$suffix),
       activity_name = keep(blocks, ~ .x$type == "Activity") %>% map_chr("name") %>% first(default = NA_character_),
       spend_name = keep(blocks, ~ .x$type == "Spend") %>% map_chr("name") %>% first(default = NA_character_),
       base_name = base_name, title = title, cs_labels = cs_labels,
       code = str_c(str_c(map_chr(blocks, "code"), collapse = "\n\n"), "\n"),
       metadata = map(blocks, "metadata") %>% list_rbind(),
       warnings = warnings, errors = character(0))
}
