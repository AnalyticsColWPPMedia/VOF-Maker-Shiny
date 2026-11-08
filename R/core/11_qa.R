# R/core/11_qa.R
# QA of the VOFs in the metadata: computes them on a copy of the Analytical
# (running the same base-R code AWB will run), compares them with the values
# already in the Analytical (built by AWB) and describes how each VOF is built.
# The QA unit is a VOF "group": a VOF and its twin (same name without the
# "---<metric>" suffix).

# Runs every VOF block on a copy of the Analytical. Each block runs on its own
# (an error in one does not stop the others), in history order so dependent
# VOFs (e.g. the weight variable on the KPI log) find their inputs.
# Returns list(data, awb, blocks) where `awb` holds the original Analytical
# values of VOFs that already existed, and `blocks` gains `error` and `group`.
qa_compute <- function(analytical, metadata, cs_dims) {
  blocks <- vof_code_blocks(metadata, cs_dims)
  data <- as.data.frame(analytical, check.names = FALSE)
  awb <- blocks$name %>% keep(~ .x %in% names(data)) %>% set_names() %>% map(~ data[[.x]])
  env <- new.env(parent = as.environment("package:stats"))  # default R packages only, as in AWB
  env$Data <- data
  blocks$error <- map_chr(blocks$code, function(code) {
    if (!nzchar(code)) return("Code not available (imported from a metadata file without code).")
    tryCatch({ eval(parse(text = code), envir = env); "" }, error = function(e) conditionMessage(e))
  })
  blocks$group <- if_else(blocks$type == METADATA_TYPES[["media"]], vof_base_name(blocks$name), blocks$name)
  list(data = env$Data, awb = awb, blocks = blocks, cs_dims = cs_dims)
}

# Groups (VOF + twin) in history order.
qa_groups <- function(qa) unique(qa$blocks$group)

qa_group_blocks <- function(qa, group) {
  qa$blocks %>% filter(.data$group == !!group) %>%
    arrange(map_chr(metric, ~ if (.x == "Spend") "2" else "1"))  # activity first, spend second
}

# Entity key (all CS dimensions combined) for every row.
qa_entity <- function(data, cs_dims) {
  if (length(cs_dims) == 0) return(rep("Total", nrow(data)))
  do.call(paste, c(map(cs_dims, ~ as.character(data[[.x]])), sep = " | "))
}

# Rows each component lets into the VOF (its period + CS filter).
qa_component_mask <- function(data, comp) {
  expr <- build_filter_expr(comp$start_period, comp$end_period, comp$cs)
  if (!nzchar(expr)) return(rep(TRUE, nrow(data)))
  m <- eval(parse(text = expr), envir = list(Data = data), enclos = baseenv())
  m[is.na(m)] <- FALSE
  m
}

qa_component_contribution <- function(data, comp) {
  if (!comp$analytical_var %in% names(data)) return(rep(NA_real_, nrow(data)))
  ifelse(qa_component_mask(data, comp), as.numeric(data[[comp$analytical_var]]), 0)
}

# ------------------------------------------------------------------------------
# What kind of VOF is it?
# ------------------------------------------------------------------------------
# A media VOF can sum several variables, split a variable by dates, filter by
# cross-sectional entities, or any combination.
qa_vof_kind <- function(components) {
  if (length(components) == 0) return(list(sum = FALSE, date = FALSE, cs = FALSE, labels = character(0)))
  sum_vars <- n_distinct(map_chr(components, "analytical_var")) > 1
  date <- some(components, ~ is_valid_value(.x$start_period) || is_valid_value(.x$end_period))
  cs <- some(components, ~ length(.x$cs) > 0)
  list(sum = sum_vars, date = date, cs = cs,
       labels = c(if (sum_vars) "Sum of variables", if (date) "Date filter", if (cs) "Cross-sectional filter"))
}

# Period cuts of the components (dates where some component starts or ends).
qa_period_cuts <- function(components) {
  starts <- map_chr(components, ~ format_date_iso(.x$start_period)) %>% keep(nzchar)
  ends <- map_chr(components, ~ format_date_iso(.x$end_period)) %>% keep(nzchar)
  bind_rows(tibble(date = unique(starts), edge = "start"), tibble(date = unique(ends), edge = "end")) %>%
    mutate(date = as_date(date))
}

# Human description of a component's filters.
describe_cs_filter <- function(dim, f) {
  mode <- if (identical(f$mode, "exclude")) "excludes" else "keeps"
  levels <- map_chr(f$levels, function(l) {
    vals <- if (l$match == "Exact") l$values else str_c("'", l$values, "'")
    shown <- str_c(head(vals, 4), collapse = ", ")
    if (length(vals) > 4) shown <- str_c(shown, " (+", length(vals) - 4, ")")
    if (l$match == "Exact") shown else str_c(str_to_lower(l$match), " ", shown)
  })
  str_c(dim, " ", mode, " ", str_c(levels, collapse = " and "))
}

describe_period <- function(comp) {
  s <- format_date_iso(comp$start_period); e <- format_date_iso(comp$end_period)
  if (s == "" && e == "") "All periods" else if (e == "") str_c("From ", s) else if (s == "") str_c("Until ", e) else str_c(s, " to ", e)
}

# ------------------------------------------------------------------------------
# Chart series
# ------------------------------------------------------------------------------
# Long table (Period, series, kind, value) for one VOF, summed over the
# selected entities: the VOF, the AWB values when it already existed, and one
# series per root with what that root contributes to the VOF (after filters).
qa_series <- function(qa, vof, entities = NULL) {
  data <- qa$data
  block <- qa$blocks %>% filter(name == vof)
  if (nrow(block) == 0 || !vof %in% names(data)) return(NULL)
  ent <- qa_entity(data, qa$cs_dims)
  rows <- if (length(entities) > 0) ent %in% entities else rep(TRUE, nrow(data))
  period <- data[[PERIOD_COL]]
  by_period <- function(v) {
    tibble(Period = period[rows], value = as.numeric(v)[rows]) %>%
      group_by(Period) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
  }
  out <- list(by_period(data[[vof]]) %>% mutate(series = vof, kind = "VOF"))
  if (!is.null(qa$awb[[vof]])) out <- c(out, list(by_period(qa$awb[[vof]]) %>% mutate(series = "AWB (Analytical)", kind = "AWB")))

  comps <- block$components[[1]]
  if (length(comps) > 0) {
    roots <- unique(map_chr(comps, "analytical_var")) %>% keep(~ .x %in% names(data))
    for (r in roots) {
      contrib <- comps %>% keep(~ .x$analytical_var == r) %>% map(~ qa_component_contribution(data, .x)) %>% reduce(`+`)
      out <- c(out, list(by_period(contrib) %>% mutate(series = r, kind = "Root")))
    }
  } else {
    for (r in block$roots[[1]]) if (r %in% names(data))
      out <- c(out, list(by_period(data[[r]]) %>% mutate(series = r, kind = "Root")))
  }
  list_rbind(out)
}

# ------------------------------------------------------------------------------
# How the VOF is built
# ------------------------------------------------------------------------------
check_row <- function(check, ok, result, detail = "") tibble(Check = check, OK = ok, Result = result, Detail = detail)

# Components of a media VOF with their filters and contribution.
qa_components_table <- function(qa, vof) {
  block <- qa$blocks %>% filter(name == vof)
  comps <- block$components[[1]]
  if (length(comps) == 0) return(NULL)
  data <- qa$data
  imap(comps, function(comp, i) {
    contrib <- qa_component_contribution(data, comp)
    tibble(Row = i, `Root variable` = comp$analytical_var,
           Period = describe_period(comp),
           `Cross-sectional filter` = if (length(comp$cs) == 0) "None" else str_c(imap_chr(comp$cs, ~ describe_cs_filter(.y, .x)), collapse = "; "),
           Contribution = sum(contrib, na.rm = TRUE))
  }) %>% list_rbind() %>%
    mutate(`Share of VOF` = if (sum(Contribution) != 0) Contribution / sum(Contribution) else NA_real_)
}

# Checks of how one VOF is built: sum of its parts, date cuts, CS coverage.
qa_build_checks <- function(qa, vof) {
  data <- qa$data
  block <- qa$blocks %>% filter(name == vof)
  if (nrow(block) == 0) return(NULL)
  if (nzchar(block$error)) return(check_row("Code runs", FALSE, "Error", block$error))
  v <- as.numeric(data[[vof]])
  comps <- block$components[[1]]
  out <- list(check_row("Code runs", TRUE, "OK"),
              check_row("No missing values", !anyNA(v), if (anyNA(v)) str_c(sum(is.na(v)), " NA values") else "OK"))
  if (!is.null(qa$awb[[vof]])) {
    diff <- max(abs(v - as.numeric(qa$awb[[vof]])), na.rm = TRUE)
    out <- c(out, list(check_row("Same values as the Analytical (AWB)", diff <= 1e-6,
                                 if (diff <= 1e-6) "Identical" else str_c("Max difference ", signif(diff, 4)),
                                 "The VOF already exists in the Analytical; the app recomputed it with the metadata.")))
  }
  if (length(comps) == 0) return(list_rbind(out))

  kind <- qa_vof_kind(comps)
  bc <- block$code %>% str_detect(fixed("BACKCASTING IMPUTATION"))
  parts <- comps %>% map(~ qa_component_contribution(data, .x)) %>% reduce(`+`)
  gap <- abs(v - parts)
  if (bc) {
    out <- c(out, list(check_row("VOF = sum of its components", TRUE, "Backcasted",
                                 sprintf("Differs from the sum in %d rows: the zeros before the calibration window were imputed.", sum(gap > 1e-6, na.rm = TRUE)))))
  } else {
    out <- c(out, list(check_row("VOF = sum of its components", all(gap <= 1e-6, na.rm = TRUE),
                                 if (all(gap <= 1e-6, na.rm = TRUE)) "OK" else sprintf("Differs in %d rows", sum(gap > 1e-6, na.rm = TRUE)),
                                 sprintf("%d component(s), %d root variable(s).", length(comps), n_distinct(map_chr(comps, "analytical_var"))))))
  }

  period <- data[[PERIOD_COL]]
  nz <- !is.na(v) & v != 0
  window <- if (any(nz)) sprintf("Data from %s to %s", min(period[nz]), max(period[nz])) else "No data"
  if (kind$date) {
    cuts <- qa_period_cuts(comps)
    covered <- comps %>% map(~ {
      s <- .x$start_period; e <- .x$end_period
      (is.na(s) | period >= s) & (is.na(e) | period <= e)
    }) %>% reduce(`|`)
    outside <- sum(nz & !covered)
    out <- c(out, list(check_row("Date filter respected", outside == 0 || bc,
                                 if (outside == 0) "OK" else if (bc) "Backcasted history" else sprintf("%d rows with data outside the periods", outside),
                                 str_c("Cuts: ", str_c(sprintf("%s %s", if_else(cuts$edge == "start", "starts", "ends"), cuts$date), collapse = ", "), ". ", window, "."))))
  } else {
    out <- c(out, list(check_row("Date filter", TRUE, "None", str_c(window, "."))))
  }

  if (kind$cs) {
    ent <- qa_entity(data, qa$cs_dims)
    e <- qa_entity_coverage(qa, vof)
    out <- c(out, list(check_row("Cross-sectional filter respected", sum(e$Status == "Data outside the filter") == 0,
                                 sprintf("%d of %d entities with data", sum(e$Total != 0), nrow(e)),
                                 sprintf("%d kept by the filters, %d excluded; %d kept but without data.",
                                         sum(e$`In filter` == "Yes"), sum(e$`In filter` == "No"),
                                         sum(e$`In filter` == "Yes" & e$Total == 0)))))
  }
  sisters <- qa_sister_check(qa, vof)
  if (!is.null(sisters)) out <- c(out, list(sisters))
  list_rbind(out)
}

# Entity coverage of a media VOF: whether its filters keep each entity and
# whether the VOF has data there.
qa_entity_coverage <- function(qa, vof) {
  data <- qa$data
  block <- qa$blocks %>% filter(name == vof)
  comps <- block$components[[1]]
  ent <- qa_entity(data, qa$cs_dims)
  kept <- if (length(comps) > 0) comps %>% map(~ qa_component_mask(data, .x)) %>% reduce(`|`) else rep(TRUE, nrow(data))
  tibble(Entity = ent, value = as.numeric(data[[vof]]), kept = kept) %>%
    group_by(Entity) %>%
    summarise(`In filter` = if (any(kept)) "Yes" else "No", Total = sum(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(Status = case_when(`In filter` == "Yes" & Total != 0 ~ "With data",
                              `In filter` == "Yes" ~ "Kept, no data",
                              Total != 0 ~ "Data outside the filter",
                              TRUE ~ "Excluded by the filter"))
}

# Entity table of a group: coverage + total of each VOF of the group.
qa_group_entities <- function(qa, group) {
  blocks <- qa_group_blocks(qa, group) %>% filter(error == "", name %in% names(qa$data))
  if (nrow(blocks) == 0) return(NULL)
  ent <- qa_entity(qa$data, qa$cs_dims)
  base <- qa_entity_coverage(qa, blocks$name[1]) %>% select(Entity, `In filter`)
  for (i in seq_len(nrow(blocks))) {
    tot <- tibble(Entity = ent, v = as.numeric(qa$data[[blocks$name[i]]])) %>%
      group_by(Entity) %>% summarise(!!str_c(blocks$metric[i], " total") := sum(v, na.rm = TRUE), .groups = "drop")
    base <- left_join(base, tot, by = "Entity")
  }
  totals <- base %>% select(ends_with(" total"))
  base %>%
    mutate(`Periods with data` = map_int(Entity, function(e) {
             v <- as.numeric(qa$data[[blocks$name[1]]])[ent == e]
             sum(v != 0, na.rm = TRUE)
           }),
           Status = case_when(`In filter` == "Yes" & rowSums(totals != 0) > 0 ~ "With data",
                              `In filter` == "Yes" ~ "Kept, no data",
                              rowSums(totals != 0) > 0 ~ "Data outside the filter",
                              TRUE ~ "Excluded by the filter"), .after = `In filter`) %>%
    arrange(desc(`In filter` == "Yes"), desc(.data[[names(totals)[1]]]))
}

# ------------------------------------------------------------------------------
# Twins
# ------------------------------------------------------------------------------
# Root name with its metric word replaced by a placeholder, so the roots of
# twins compare equal ("OOH Spend_X" and "OOH Impressions_X" -> "OOH {metric}_X").
root_signature <- function(var, long_dims) {
  m <- detect_metric(var, long_dims)
  if (is.null(m)) var else str_replace(var, word_regex(m$term), "{metric}")
}

# The twin must have the same components (labels, periods, filters) with roots
# that differ only in the metric.
qa_twin_check <- function(qa, group, long_dims) {
  blocks <- qa_group_blocks(qa, group) %>% filter(type == METADATA_TYPES[["media"]])
  if (nrow(blocks) < 2) {
    return(check_row("Twin VOF", TRUE, "No twin", if (nrow(blocks) == 1) "This VOF has no activity/spend twin in the history." else ""))
  }
  sig <- function(comps) {
    set_names(map_chr(comps, function(c) str_c(root_signature(c$analytical_var, long_dims), c$variable_name, c$complement, c$effect,
                                               format_date_iso(c$start_period), format_date_iso(c$end_period),
                                               str_c(map_chr(c$cs, cs_filter_key), collapse = "#"), sep = "|")),
              map_chr(comps, "analytical_var"))
  }
  a <- sig(blocks$components[[1]]); b <- sig(blocks$components[[2]])
  only_a <- names(a)[!a %in% b]; only_b <- names(b)[!b %in% a]
  same <- length(only_a) == 0 && length(only_b) == 0
  detail <- if (same) sprintf("%s and %s have the same %d component(s); only the metric changes.", blocks$metric[1], blocks$metric[2], length(a))
            else str_c(c(if (length(only_a) > 0) sprintf("Only in %s: %s.", blocks$metric[1], str_c(only_a, collapse = "; ")),
                         if (length(only_b) > 0) sprintf("Only in %s: %s.", blocks$metric[2], str_c(only_b, collapse = "; "))),
                       collapse = " ")
  check_row("Twin built consistently", same, if (same) "OK" else "Components differ", detail)
}

# ------------------------------------------------------------------------------
# Overview of every VOF
# ------------------------------------------------------------------------------
# Status: Error (code fails or is not available) > Differs from AWB (the VOF
# exists in the Analytical with other values) > All zero (a media VOF with no
# data at all) > Check (missing or negative values) > OK.
qa_checks <- function(qa) {
  data <- qa$data
  qa$blocks %>%
    mutate(
      values = map(name, ~ if (.x %in% names(data)) suppressWarnings(as.numeric(data[[.x]])) else NULL),
      Total = map_dbl(values, ~ if (is.null(.x)) NA_real_ else sum(.x, na.rm = TRUE)),
      `Non-zero rows` = map_int(values, ~ if (is.null(.x)) NA_integer_ else sum(.x != 0, na.rm = TRUE)),
      nas = map_int(values, ~ if (is.null(.x)) NA_integer_ else sum(is.na(.x))),
      neg = map_int(values, ~ if (is.null(.x)) NA_integer_ else sum(.x < 0, na.rm = TRUE)),
      `Max diff vs AWB` = map2_dbl(name, values, function(n, v) {
        awb <- qa$awb[[n]]
        if (is.null(awb) || is.null(v) || !is.numeric(awb)) NA_real_ else max(abs(v - awb), na.rm = TRUE)
      }),
      Status = pmap_chr(list(error, values, Total, nas, neg, `Max diff vs AWB`, type),
        function(err, vals, tot, nas, neg, diff, type) {
          if (nzchar(err) || is.null(vals)) return("Error")
          if (!is.na(diff) && diff > 1e-6) return("Differs from AWB")
          if (type == METADATA_TYPES[["media"]] && tot == 0) return("All zero")
          if (nas > 0 || neg > 0) return("Check")
          "OK"
        }),
      Detail = pmap_chr(list(error, nas, neg, `Max diff vs AWB`, Total, type), function(err, nas, neg, diff, tot, type) {
        str_c(c(if (nzchar(err)) err,
                if (!is.na(nas) && nas > 0) str_c(nas, " missing values"),
                if (!is.na(neg) && neg > 0) str_c(neg, " negative values"),
                if (!is.na(diff) && diff > 1e-6) sprintf("differs from the Analytical by up to %s", signif(diff, 4)),
                if (!is.na(diff) && diff <= 1e-6) "identical to the Analytical",
                if (!is.na(tot) && tot == 0 && type == METADATA_TYPES[["media"]]) "the VOF is 0 everywhere"), collapse = "; ")
      })
    ) %>%
    transmute(VOF = name, Group = group, Type = type, Metric = metric, Status, Total, `Non-zero rows`, Detail)
}

# ------------------------------------------------------------------------------
# Sister VOFs (TimeBreaks / CS Groups)
# ------------------------------------------------------------------------------
# Name without the period and CS tags (and the Backcasted prefix): sisters
# share it. "OLV --p -20240630 --g All Others" -> "OLV".
sister_key <- function(base_name) {
  cs_tags <- str_c("(", str_c(str_replace_all(unname(CS_NAME_TAGS), "-", "\\\\-"), collapse = "|"), ")")
  base_name %>%
    str_remove(str_c("^", BACKCASTED_LABEL, " ")) %>%
    str_remove_all("\\s--p(\\s+[0-9]*-[0-9]*)?(?=\\s|$)") %>%
    str_remove_all(str_c("\\s", cs_tags, "(\\s.*?)?(?=\\s--|$)")) %>%
    str_squish()
}

# The sisters of a VOF (same name except the period / CS tags, same metric)
# must add up to their roots without the split: no gaps and no overlaps.
qa_sister_check <- function(qa, vof) {
  block <- qa$blocks %>% filter(name == vof)
  if (nrow(block) == 0 || block$type != METADATA_TYPES[["media"]] || length(block$components[[1]]) == 0) return(NULL)
  key <- sister_key(block$group)
  sis <- qa$blocks %>%
    filter(type == METADATA_TYPES[["media"]], metric == block$metric, !nzchar(error)) %>%
    filter(map_chr(group, sister_key) == key)
  if (nrow(sis) < 2) return(NULL)
  roots <- map(sis$components, ~ sort(unique(map_chr(.x, "analytical_var"))))
  if (n_distinct(map_chr(roots, str_c, collapse = "|")) > 1)
    return(check_row("Sister VOFs add up to the roots", FALSE, "Different components",
                     sprintf("%d VOFs share the name %s but do not have the same root variables: they are not sisters of one split.", nrow(sis), key)))
  first_comp <- map(sis$components, 1)
  period_varies <- n_distinct(map_chr(first_comp, ~ str_c(format_date_iso(.x$start_period), format_date_iso(.x$end_period)))) > 1
  cs_varies <- MFF_DIMS %>% keep(function(d) n_distinct(map_chr(first_comp, ~ if (is.null(.x$cs[[d]])) "" else cs_filter_key(.x$cs[[d]]))) > 1)
  # Without the split, the period is the whole window the sisters cover
  # (open unless every sister is closed on that side: start / end of the TimeBreaks).
  starts <- map(first_comp, "start_period") %>% do.call(c, .)
  ends <- map(first_comp, "end_period") %>% do.call(c, .)
  outer_start <- if (anyNA(starts)) NA else min(starts)
  outer_end <- if (anyNA(ends)) NA else max(ends)
  unsplit <- map(block$components[[1]], function(comp) {
    if (period_varies) { comp$start_period <- outer_start; comp$end_period <- outer_end }
    for (d in cs_varies) comp$cs[[d]] <- NULL
    comp
  })
  data <- qa$data
  reference <- unsplit %>% map(~ qa_component_contribution(data, .x)) %>% reduce(`+`)
  total <- sis$name %>% map(~ as.numeric(data[[.x]])) %>% reduce(`+`)
  split_by <- c(if (period_varies) "period", cs_varies) %>% str_c(collapse = " and ")
  backcasted <- any(str_detect(sis$code, fixed("BACKCASTING IMPUTATION")))
  gap <- abs(total - reference)
  ok <- all(gap <= 1e-6, na.rm = TRUE)
  share <- if (sum(reference, na.rm = TRUE) != 0) 100 * sum(total, na.rm = TRUE) / sum(reference, na.rm = TRUE) else NA_real_
  result <- if (ok) "OK" else if (backcasted) "Backcasted" else sprintf("%.1f%% of the roots", share)
  detail <- sprintf("%d sister VOFs split by %s (%s). %s", nrow(sis), split_by, str_c(sis$name, collapse = "; "),
                    if (ok) "Their sum equals the roots without the split: no gaps, no overlaps."
                    else if (backcasted) "A backcasted sister imputes history, so the sum is above the roots before the calibration window."
                    else sprintf("Their sum differs from the roots without the split in %d rows: some sister is missing (not generated) or they overlap.", sum(gap > 1e-6, na.rm = TRUE)))
  check_row("Sister VOFs add up to the roots", ok || backcasted, result, detail)
}
