# R/core/06_external_codegen.R
# External Sources tab: aligns an external series to the Analytical periods and
# generates the base-R code that maps it into Data.
# (Frequency conversion options for coarser data are planned for Phase 3.)

# Typical spacing (in days) of a vector of dates.
median_step <- function(dates) {
  d <- dates %>% discard(is.na) %>% unique() %>% sort()
  if (length(d) < 2) return(NA_real_)
  as.numeric(stats::median(diff(d)))
}

# Maps every external date to an Analytical period.
# * External data finer than the Analytical (e.g. daily vs weekly): each date
#   goes to the period that contains it, [period_k, period_k+1). This uses the
#   Analytical's own periods, so the project's week-start day is respected.
# * Same or coarser frequency: each date goes to the nearest period (handles
#   weekly series whose weeks start on another weekday).
# Dates outside the Analytical range (by more than one period) are dropped.
map_to_periods <- function(ext_dates, ana_periods) {
  ana_periods <- ana_periods %>% discard(is.na) %>% unique() %>% sort()
  ana_step <- median_step(ana_periods) %|NA|% 7
  ext_step <- median_step(ext_dates)
  mode <- if (!is.na(ext_step) && ext_step < ana_step) "contain" else "nearest"
  ana_num <- as.numeric(ana_periods)

  mapped_num <- map_dbl(as.numeric(ext_dates), function(d) {
    if (is.na(d)) return(NA_real_)
    if (mode == "contain") {
      i <- findInterval(d, ana_num)
      if (i == 0 || d >= max(ana_num) + ana_step) NA_real_ else ana_num[i]
    } else {
      i <- which.min(abs(ana_num - d))
      if (abs(ana_num[i] - d) <= ana_step) ana_num[i] else NA_real_
    }
  })
  list(mapped = as_date(mapped_num), mode = mode, ext_step = ext_step, ana_step = ana_step)
}

`%|NA|%` <- function(a, b) if (is.na(a)) b else a

# Diagnosis shown in the UI before generating code: list(ok, needs_agg,
# messages = list of list(level, text), missing_entities).
diagnose_external <- function(df_ext, period_col, value_col, ana_df, entity_col = "", target_dim = "") {
  msgs <- list()
  add <- function(level, text) msgs[[length(msgs) + 1]] <<- list(level = level, text = text)
  out <- list(ok = FALSE, needs_agg = FALSE, messages = NULL, missing_entities = character(0))

  ext_dates <- parse_dates_flexible(df_ext[[period_col]])
  if (all(is.na(ext_dates))) {
    add("danger", "Could not parse dates in the selected Period Column. Use a standard format (e.g. YYYY-MM-DD).")
    out$messages <- msgs
    return(out)
  }
  n_bad <- sum(is.na(ext_dates))
  if (n_bad > 0) add("warning", sprintf("%d rows have unreadable dates and will be ignored.", n_bad))

  raw_values <- df_ext[[value_col]]
  values <- suppressWarnings(as.numeric(as.character(raw_values)))
  n_nonnum <- sum(is.na(values) & !is.na(raw_values) & str_trim(as.character(raw_values)) != "")
  if (n_nonnum > 0) add("warning", sprintf("%d values are not numeric and will be ignored.", n_nonnum))

  ana_periods <- analytical_periods(ana_df)
  m <- map_to_periods(ext_dates, ana_periods)
  exact_pct <- round(100 * mean(discard(ext_dates, is.na) %in% ana_periods), 1)
  add("info", sprintf("Date alignment: %s%% of external dates match an Analytical period exactly.", exact_pct))
  if (m$mode == "contain") {
    add("info", sprintf("External data is more granular (~%s days) than the Analytical (~%s days): each date is assigned to the period that contains it.",
                        m$ext_step, m$ana_step))
  } else if (exact_pct < 100) {
    add("warning", "Dates are not aligned with the Analytical periods: each date is assigned to the nearest period.")
  }
  if (!is.na(m$ext_step) && m$ext_step > m$ana_step * 1.5)
    add("warning", "External data is less granular than the Analytical (e.g. monthly vs weekly): only the nearest period of each date gets a value, the rest will be 0.")
  n_out <- sum(!is.na(ext_dates) & is.na(m$mapped))
  if (n_out > 0) add("warning", sprintf("%d rows fall outside the Analytical date range and will be ignored.", n_out))

  keys <- tibble(period = m$mapped,
                 entity = if (entity_col != "") str_trim(as.character(df_ext[[entity_col]])) else "") %>%
    filter(!is.na(period))
  out$needs_agg <- anyDuplicated(keys) > 0
  if (out$needs_agg) add("warning", "Several external rows fall in the same period: choose how to aggregate them.")

  cs_ok <- TRUE
  if (entity_col != "") {
    if (target_dim == "") {
      cs_ok <- FALSE
      add("warning", "Select a Target Analytical Entity for mapping.")
    } else {
      ext_vals <- df_ext[[entity_col]] %>% as.character() %>% str_trim() %>% clean_values() %>% unique()
      ana_vals <- ana_df[[target_dim]] %>% as.character() %>% str_trim() %>% clean_values() %>% unique()
      missing <- setdiff(ext_vals, ana_vals)
      if (length(missing) > 0) {
        cs_ok <- FALSE
        out$missing_entities <- missing
        add("danger", sprintf("Cross-Sectional Mismatch: these entities exist in '%s' but not in '%s'. Code generation is blocked until this is fixed.",
                              entity_col, target_dim))
      } else {
        add("success", "Cross-Sectional entities mapped correctly.")
        no_data <- setdiff(ana_vals, ext_vals)
        if (length(no_data) > 0)
          add("warning", sprintf("%d %s values have no external data and will get 0: %s",
                                 length(no_data), target_dim, str_c(head(no_data, 10), collapse = ", ")))
      }
    }
  }
  out$ok <- cs_ok
  out$messages <- msgs
  out
}

# Aligned + aggregated table (Period, [Entity], Value).
aggregate_external <- function(df_ext, period_col, value_col, ana_periods, entity_col = "", agg = "sum") {
  has_cs <- entity_col != ""
  fun <- if (identical(agg, "mean")) mean else sum
  tibble(Period = map_to_periods(parse_dates_flexible(df_ext[[period_col]]), ana_periods)$mapped,
         Entity = if (has_cs) str_trim(as.character(df_ext[[entity_col]])) else NA_character_,
         Value  = suppressWarnings(as.numeric(as.character(df_ext[[value_col]])))) %>%
    filter(!is.na(Period), !is.na(Value), !has_cs | (!is.na(Entity) & Entity != "")) %>%
    group_by(across(c(if (has_cs) "Entity", "Period"))) %>%
    summarise(Value = fun(Value), .groups = "drop") %>%
    select(Period, any_of("Entity"), Value)
}

one_line <- function(x) str_replace_all(convert_to_char(x), "[\r\n]+", " ")

# Generated code (base R).
build_external_code <- function(var_name, agg_df, target_dim = "", source = "", notes = "", why = "") {
  has_cs <- target_dim != "" && "Entity" %in% names(agg_df)
  num <- if_else(is.na(agg_df$Value), "NA", as.character(signif(agg_df$Value, 12)))
  df_lines <- c(
    "tmp_ext <- data.frame(",
    sprintf("  Period = as.Date(%s),", r_str_vec(format(agg_df$Period, "%Y-%m-%d"))),
    if (has_cs) sprintf("  Entity = %s,", r_str_vec(agg_df$Entity)),
    sprintf("  Value = c(%s),", str_c(num, collapse = ", ")),
    "  stringsAsFactors = FALSE",
    ")"
  )
  map_lines <- if (has_cs) c(
    "# 2. Map values with composite keys (Period + entity)",
    sprintf("key_data <- paste(Data$%s, Data$%s, sep = '|')", PERIOD_COL, target_dim),
    "key_ext <- paste(tmp_ext$Period, tmp_ext$Entity, sep = '|')",
    sprintf("%s <- tmp_ext$Value[match(key_data, key_ext)]", r_col(var_name)),
    "rm(key_data, key_ext)"
  ) else c(
    "# 2. Map values by Period",
    sprintf("%s <- tmp_ext$Value[match(Data$%s, tmp_ext$Period)]", r_col(var_name), PERIOD_COL)
  )
  c(sprintf("#### External Variable: %s ####", one_line(var_name)),
    sprintf("# Source: %s", one_line(source)),
    sprintf("# Notes: %s", one_line(notes)),
    sprintf("# Why: %s", one_line(why)),
    "",
    "# 1. External data (aligned to the Analytical periods)",
    df_lines,
    "",
    map_lines,
    "",
    "# 3. Periods/entities without external data get 0",
    sprintf("%s[is.na(%s)] <- 0", r_col(var_name), r_col(var_name)),
    "rm(tmp_ext)",
    "") %>% str_c(collapse = "\n")
}

generate_external_vof <- function(var_name, df_ext, period_col, value_col, ana_df,
                                  entity_col = "", target_dim = "", agg = "sum",
                                  source = "", notes = "", why = "", cs_dims = character(0)) {
  var_name <- str_squish(convert_to_char(var_name))   # no extra spaces in VOF names
  errors <- c(if (var_name == "") "Variable Name is empty.",
              if (!is_valid_value(period_col) || !is_valid_value(value_col)) "Select the Period and Value columns.")
  if (length(errors) > 0) return(list(errors = errors))

  warnings <- if (var_name %in% names(ana_df))
    sprintf("'%s' already exists in the Analytical; the VOF will overwrite it.", var_name) else character(0)

  agg_df <- aggregate_external(df_ext, period_col, value_col, analytical_periods(ana_df), entity_col, agg)
  if (nrow(agg_df) == 0) return(list(errors = "No external rows could be aligned to the Analytical periods."))

  code <- build_external_code(var_name, agg_df, if (entity_col != "") target_dim else "", source, notes, why)
  meta <- tibble(Type = METADATA_TYPES[["external"]], MainModelVariableName = var_name,
                 AnalyticalVariableName = "", Metric = "External", !!METADATA_CODE_COL := code)
  for (d in cs_dims) meta[[d]] <- ""
  list(code = code, metadata = meta, warnings = warnings, errors = character(0))
}
