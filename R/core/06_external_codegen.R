# R/core/06_external_codegen.R
# External Sources tab: aligns an external series to the Analytical periods and
# generates the base-R code that maps it into Data.
# Finer data (e.g. daily) is aggregated into the Analytical periods (sum / mean);
# coarser data (e.g. monthly) is expanded to them (replicate / divide equally /
# divide by days), as the analyst chooses.

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

# ------------------------------------------------------------------------------
# Coarser external data (monthly, quarterly, ...) -> Analytical periods
# ------------------------------------------------------------------------------
EXPAND_METHODS <- c(replicate = "replicate", divide = "divide", days = "days")

is_coarser <- function(ext_step, ana_step) !is.na(ext_step) && !is.na(ana_step) && ext_step > ana_step * 1.5

# Calendar span covered by each date of a coarse series: the whole month /
# quarter / year when the spacing says so (whatever day of it the date is), or
# [date, date + spacing) otherwise. Returns list(start, end, unit, step).
external_intervals <- function(dates) {
  step <- median_step(dates)
  unit <- if (is.na(step)) NA_character_ else if (step >= 27 && step <= 32) "month"
          else if (step >= 85 && step <= 95) "quarter" else if (step >= 360 && step <= 370) "year" else NA_character_
  if (!is.na(unit)) {
    start <- floor_date(dates, unit)
    end <- start %m+% months(c(month = 1, quarter = 3, year = 12)[[unit]])
  } else {
    start <- dates; end <- dates + (step %|NA|% 1)
  }
  list(start = start, end = end, unit = unit, step = step)
}

frequency_label <- function(unit, step) {
  if (!is.na(unit)) return(c(month = "monthly", quarter = "quarterly", year = "yearly")[[unit]])
  sprintf("every ~%s days", round(step))
}
span_label <- function(unit) if (is.na(unit)) "span" else unit

# Expands coarse rows (Date, Entity, Value) to the Analytical periods:
# - replicate: every period starting in the span gets the value (levels,
#   indexes, rates, prices);
# - divide: the value is divided equally among the periods starting in the
#   span (totals: the sum of the span is kept);
# - days: each period gets the share of the value of the days it has in the
#   span (totals; a week across two months takes part of each).
# Periods are counted on the Analytical calendar extended over the whole span,
# so a month only partly inside the Analytical is divided like any other.
expand_external <- function(rows, ana_periods, method) {
  ana_periods <- sort(unique(discard(ana_periods, is.na)))
  step <- median_step(ana_periods) %|NA|% 7
  iv <- external_intervals(rows$Date)
  spans <- rows %>% mutate(start = iv$start, end = iv$end)
  lo <- min(spans$start, ana_periods[1]); hi <- max(spans$end, max(ana_periods) + step)
  back <- ceiling(as.numeric(ana_periods[1] - lo) / step)
  grid <- seq(ana_periods[1] - back * step, hi, by = step)   # the Analytical calendar, extended
  out <- spans %>% pmap(function(Date, Entity, Value, start, end, ...) {
    if (method == "days") {
      ov <- pmax(0, pmin(as.numeric(grid + step), as.numeric(end)) - pmax(as.numeric(grid), as.numeric(start)))
      keep <- ov > 0
      tibble(Period = grid[keep], Entity = Entity, Value = Value * ov[keep] / as.numeric(end - start))
    } else {
      inside <- grid[grid >= start & grid < end]
      if (length(inside) == 0) return(NULL)
      tibble(Period = inside, Entity = Entity, Value = if (method == "divide") Value / length(inside) else Value)
    }
  }) %>% list_rbind()
  if (is.null(out) || nrow(out) == 0) return(tibble(Period = as.Date(character(0)), Entity = character(0), Value = numeric(0)))
  out %>% filter(Period %in% ana_periods) %>%
    group_by(Entity, Period) %>% summarise(Value = sum(Value), .groups = "drop")
}

# Consecutive periods as readable ranges: "2023-01-02 to 2023-03-27 (13 weeks)".
period_ranges <- function(dates, step, unit = "periods") {
  dates <- sort(unique(discard(as.Date(dates), is.na)))
  if (length(dates) == 0) return(character(0))
  run <- cumsum(c(TRUE, diff(as.numeric(dates)) > step * 1.5))
  map_chr(split(dates, run), function(d) {
    if (length(d) == 1) format(d) else sprintf("%s to %s (%d %s)", min(d), max(d), length(d), unit)
  }) %>% unname()
}

# Diagnosis shown in the UI before generating code: list(ok, needs_agg,
# messages = list of list(level, text, items), missing_entities). `items`
# lists what the message is about (weeks, months, entities).
diagnose_external <- function(df_ext, period_col, value_col, ana_df, entity_col = "", target_dim = "") {
  msgs <- list()
  add <- function(level, text, items = NULL) msgs[[length(msgs) + 1]] <<- list(level = level, text = text, items = items)
  out <- list(ok = FALSE, needs_agg = FALSE, coarse = FALSE, needs_expand = FALSE, expand_labels = NULL,
              messages = NULL, missing_entities = character(0))

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
  out$coarse <- is_coarser(m$ext_step, m$ana_step)
  if (out$coarse) {
    # Coarser than the Analytical: each value is expanded to the periods of its span.
    iv <- external_intervals(ext_dates[!is.na(ext_dates)])
    freq <- frequency_label(iv$unit, iv$step)
    unit_ana <- if (isTRUE(abs(m$ana_step - 7) < 0.5)) "weeks" else "periods"
    out$needs_expand <- TRUE
    out$expand_labels <- list(freq = freq, span = span_label(iv$unit), unit = unit_ana)
    add("info", sprintf("External data is %s and the Analytical is %s (~%s days): choose below how to expand each value to the %s of its %s.",
                        freq, if (unit_ana == "weeks") "weekly" else "finer", m$ana_step, unit_ana, span_label(iv$unit)))
    last_p <- max(ana_periods) + m$ana_step
    out_rows <- iv$end <= min(ana_periods) | iv$start >= last_p
    span_fmt <- function(d) if (identical(iv$unit, "month")) format(d, "%Y-%m") else format(d)
    if (any(out_rows)) add("warning", sprintf("%d %s values fall outside the Analytical date range and will be ignored:", sum(out_rows), freq),
                           items = span_fmt(sort(unique(iv$start[out_rows]))))
    covered <- map_lgl(ana_periods, function(p) any(iv$start < p + m$ana_step & iv$end > p))
    if (any(!covered)) add("warning", sprintf("%d Analytical %s have no external data and will get 0:", sum(!covered), unit_ana),
                           items = period_ranges(ana_periods[!covered], m$ana_step, unit_ana))
  }
  exact_pct <- round(100 * mean(discard(ext_dates, is.na) %in% ana_periods), 1)
  if (!out$coarse) add("info", sprintf("Date alignment: %s%% of external dates match an Analytical period exactly.", exact_pct))
  if (out$coarse) {
    # (messages above)
  } else if (m$mode == "contain") {
    add("info", sprintf("External data is more granular (~%s days) than the Analytical (~%s days): each date is assigned to the period that contains it.",
                        m$ext_step, m$ana_step))
  } else if (exact_pct < 100) {
    add("warning", "Dates are not aligned with the Analytical periods: each date is assigned to the nearest period.")
  }
  outside <- !is.na(ext_dates) & is.na(m$mapped)
  if (!out$coarse && any(outside))
    add("warning", sprintf("%d rows fall outside the Analytical date range and will be ignored:", sum(outside)),
        items = period_ranges(ext_dates[outside], m$ext_step %|NA|% m$ana_step, "dates"))
  if (!out$coarse) {
    no_data <- setdiff(as.character(ana_periods), as.character(m$mapped[!is.na(m$mapped)]))
    if (length(no_data) > 0)
      add("warning", sprintf("%d Analytical periods have no external data and will get 0:", length(no_data)),
          items = period_ranges(as.Date(no_data), m$ana_step, "periods"))
  }

  keys <- tibble(period = if (out$coarse) external_intervals(ext_dates)$start else m$mapped,
                 entity = if (entity_col != "") str_trim(as.character(df_ext[[entity_col]])) else "") %>%
    filter(!is.na(period))
  out$needs_agg <- anyDuplicated(keys) > 0
  if (out$needs_agg) add("warning", sprintf("Several external rows fall in the same %s: choose how to aggregate them.",
                                            if (out$coarse) out$expand_labels$span else "period"))

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
          add("warning", sprintf("%d %s values have no external data and will get 0:", length(no_data), target_dim),
              items = sort(no_data))
      }
    }
  }
  out$ok <- cs_ok
  out$messages <- msgs
  out
}

# Aligned + aggregated table (Period, [Entity], Value). Coarser data needs an
# `expand` method (EXPAND_METHODS); rows of the same span are aggregated first.
aggregate_external <- function(df_ext, period_col, value_col, ana_periods, entity_col = "", agg = "sum", expand = NULL) {
  has_cs <- entity_col != ""
  fun <- if (identical(agg, "mean")) mean else sum
  dates <- parse_dates_flexible(df_ext[[period_col]])
  ana_step <- median_step(ana_periods) %|NA|% 7
  if (is_coarser(median_step(dates), ana_step)) {
    if (!is_valid_value(expand) || !expand %in% EXPAND_METHODS) stop("Choose how to expand the coarser external data.")
    rows <- tibble(Date = dates,
                   Entity = if (has_cs) str_trim(as.character(df_ext[[entity_col]])) else NA_character_,
                   Value = suppressWarnings(as.numeric(as.character(df_ext[[value_col]])))) %>%
      filter(!is.na(Date), !is.na(Value), !has_cs | (!is.na(Entity) & Entity != ""))
    if (nrow(rows) == 0) return(tibble(Period = as.Date(character(0)), Value = numeric(0)))
    rows <- rows %>% mutate(Date = external_intervals(Date)$start) %>%
      group_by(Entity, Date) %>% summarise(Value = fun(Value), .groups = "drop")
    return(expand_external(rows, ana_periods, expand) %>% select(Period, any_of(if (has_cs) "Entity"), Value))
  }
  tibble(Period = map_to_periods(dates, ana_periods)$mapped,
         Entity = if (has_cs) str_trim(as.character(df_ext[[entity_col]])) else NA_character_,
         Value  = suppressWarnings(as.numeric(as.character(df_ext[[value_col]])))) %>%
    filter(!is.na(Period), !is.na(Value), !has_cs | (!is.na(Entity) & Entity != "")) %>%
    group_by(across(c(if (has_cs) "Entity", "Period"))) %>%
    summarise(Value = fun(Value), .groups = "drop") %>%
    select(Period, any_of("Entity"), Value)
}

one_line <- function(x) str_replace_all(convert_to_char(x), "[\r\n]+", " ")

# Generated code (base R).
build_external_code <- function(var_name, agg_df, target_dim = "", source = "", notes = "", why = "", how = "") {
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
  # Same shape as the media VOFs: start at 0, fill the periods (and entities)
  # that have external data, remove the temporary table. Rows without a match
  # keep their 0 (a plain match() would write NA on them).
  key <- if (has_cs) sprintf("paste(Data$%s, Data$%s, sep = '|')", PERIOD_COL, target_dim) else sprintf("Data$%s", PERIOD_COL)
  key_ext <- if (has_cs) "paste(tmp_ext$Period, tmp_ext$Entity, sep = '|')" else "tmp_ext$Period"
  map_lines <- c(
    if (has_cs) "# 2. Start at 0 and fill with the external data (key: Period + entity)" else "# 2. Start at 0 and fill with the external data (key: Period)",
    sprintf("%s <- 0", r_col(var_name)),
    sprintf("idx <- match(%s, %s)", key, key_ext),
    sprintf("%s[!is.na(idx)] <- tmp_ext$Value[idx[!is.na(idx)]]", r_col(var_name)),
    "rm(tmp_ext, idx)"
  )
  c(sprintf("#### External Variable: %s ####", one_line(var_name)),
    sprintf("# Source: %s", one_line(source)),
    sprintf("# Notes: %s", one_line(notes)),
    sprintf("# Why: %s", one_line(why)),
    if (nzchar(how)) sprintf("# Frequency: %s", one_line(how)),
    "",
    "# 1. External data (aligned to the Analytical periods)",
    df_lines,
    "",
    map_lines,
    "") %>% str_c(collapse = "\n")
}

generate_external_vof <- function(var_name, df_ext, period_col, value_col, ana_df,
                                  entity_col = "", target_dim = "", agg = "sum",
                                  source = "", notes = "", why = "", cs_dims = character(0), expand = NULL) {
  var_name <- str_squish(convert_to_char(var_name))   # no extra spaces in VOF names
  errors <- c(if (var_name == "") "Variable Name is empty.",
              if (!is_valid_value(period_col) || !is_valid_value(value_col)) "Select the Period and Value columns.")
  if (length(errors) > 0) return(list(errors = errors))

  warnings <- if (var_name %in% names(ana_df))
    sprintf("'%s' already exists in the Analytical; the VOF will overwrite it.", var_name) else character(0)

  ana_periods <- analytical_periods(ana_df)
  agg_df <- tryCatch(aggregate_external(df_ext, period_col, value_col, ana_periods, entity_col, agg, expand),
                     error = function(e) conditionMessage(e))
  if (is.character(agg_df)) return(list(errors = agg_df))
  if (nrow(agg_df) == 0) return(list(errors = "No external rows could be aligned to the Analytical periods."))

  dates <- parse_dates_flexible(df_ext[[period_col]])
  how <- if (is_coarser(median_step(dates), median_step(ana_periods) %|NA|% 7)) {
    iv <- external_intervals(dates[!is.na(dates)])
    sprintf("%s data, %s", frequency_label(iv$unit, iv$step),
            c(replicate = str_c("value replicated in every period of its ", span_label(iv$unit)),
              divide = str_c("value divided equally among the periods of its ", span_label(iv$unit)),
              days = str_c("value divided by the days of each period in its ", span_label(iv$unit)))[[expand]])
  } else ""
  code <- build_external_code(var_name, agg_df, if (entity_col != "") target_dim else "", source, notes, why, how)
  meta <- tibble(Type = METADATA_TYPES[["external"]], MainModelVariableName = var_name,
                 AnalyticalVariableName = "", Metric = "External", !!METADATA_CODE_COL := code)
  for (d in cs_dims) meta[[d]] <- ""
  list(code = code, metadata = meta, warnings = warnings, errors = character(0))
}

# ------------------------------------------------------------------------------
# Preview: the new variable as it will be in the Analytical
# ------------------------------------------------------------------------------
# Every Analytical period (0 where there is no external data), for the whole
# Analytical (no entity) or for one entity. Only the first and last `months`
# months are kept. Returns tibble(Part, Period, Value).
external_preview <- function(agg_df, ana_periods, entity = NULL, months = 3) {
  ana_periods <- sort(unique(discard(ana_periods, is.na)))
  rows <- if (!is.null(entity) && "Entity" %in% names(agg_df)) agg_df %>% filter(Entity == entity) else agg_df
  grid <- tibble(Period = ana_periods) %>%
    left_join(rows %>% group_by(Period) %>% summarise(Value = sum(Value), .groups = "drop"), by = "Period") %>%
    mutate(Value = replace_na(Value, 0))
  first_end <- ana_periods[1] %m+% months(months)
  last_start <- max(ana_periods) %m-% months(months)
  grid %>%
    mutate(Part = case_when(Period < first_end ~ sprintf("First %d months", months),
                            Period > last_start ~ sprintf("Last %d months", months),
                            TRUE ~ NA_character_)) %>%
    filter(!is.na(Part)) %>%
    select(Part, Period, Value)
}
