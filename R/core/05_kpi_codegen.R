# R/core/05_kpi_codegen.R
# KPI tab: log-transformed dependent variable, business cycles and the weight
# variable. App code is tidyverse; the generated code is base R (AWB, R 3.6).

# "<VariableName> <longitudinal values that vary across MFF variables> Log"
# (naming convention: transformations are suffixes). Longitudinal positions
# that are constant (e.g. always "Total") are dropped.
build_kpi_log_name <- function(kpi_var, mff_vars, cs_dims) {
  long_dims <- longitudinal_dims(cs_dims)
  parsed_kpi <- parse_mff_names(kpi_var, long_dims)
  if (!isTRUE(parsed_kpi$is_mff[1]) || length(long_dims) == 0)
    return(str_c(to_title_case(kpi_var), " Log"))

  parsed_all <- parse_mff_names(mff_vars, long_dims) %>% filter(is_mff)
  usable <- keep(long_dims, ~ n_distinct(parsed_all[[.x]]) > 1)
  vals <- map_chr(usable, ~ parsed_kpi[[.x]][1]) %>% discard(is.na)
  str_c(c(to_title_case(parsed_kpi$base[1]), map_chr(vals, to_title_case), "Log"), collapse = " ")
}

build_kpi_log_code <- function(kpi_var, log_name) {
  init <- sprintf("%s <- 0", r_col(log_name))
  feed <- sprintf("%s <- ifelse(%s <= 0, 0, log(%s))", r_col(log_name), r_col(kpi_var), r_col(kpi_var))
  list(init = init, feed = feed, code = str_c(c("#### DepVar ####", init, feed), collapse = "\n"))
}

# ------------------------------------------------------------------------------
# Business cycles
# ------------------------------------------------------------------------------
# A cycle table has the columns Label, Start, End (one row per cycle).
cycle_label <- function(label, year) str_c(label, str_sub(as.character(year), 3, 4))

# Positive-KPI periods: cycles are anchored on the last date with data.
kpi_periods <- function(periods, kpi_values) {
  periods[!is.na(kpi_values) & kpi_values > 0 & !is.na(periods)] %>% unique() %>% sort()
}

# 1. Default: trailing cycles of WEEKS_PER_CYCLE weeks counted back from the
#    last period where the KPI is positive. Each cycle covers the periods in
#    (end - 364 days, end], so a weekly series gets exactly 52 periods per cycle
#    whatever weekday the project's weeks start on. Only complete cycles are
#    built (periods before them are left out of scope); `n_cycles` caps them.
build_trailing_cycles <- function(periods, kpi_values, label = "FY", n_cycles = Inf) {
  valid <- kpi_periods(periods, kpi_values)
  if (length(valid) == 0) return(NULL)
  span <- WEEKS_PER_CYCLE * 7
  step <- if (length(valid) > 1) as.numeric(stats::median(diff(valid))) else 7
  last_year <- year(max(valid))
  cycles <- list()
  cycle_end <- max(valid)
  while (length(cycles) < n_cycles) {
    if (min(valid) > cycle_end - span + step) break  # not a complete cycle
    in_cycle <- valid[valid > cycle_end - span & valid <= cycle_end]
    cycles[[length(cycles) + 1]] <- tibble(Label = cycle_label(label, last_year - length(cycles)),
                                           Start = min(in_cycle), End = cycle_end)
    earlier <- valid[valid < min(in_cycle)]
    if (length(earlier) == 0) break
    cycle_end <- max(earlier)
  }
  if (length(cycles) == 0) return(NULL)
  cycles %>% list_rbind() %>% arrange(Start)
}

# Calendar-year table (Year_Cal, Include, Start, End). Used by the calendar
# fallback and to pre-fill the custom table.
default_cycle_table <- function(periods) {
  tibble(Period = periods) %>%
    filter(!is.na(Period)) %>%
    distinct() %>%
    group_by(Year_Cal = year(Period)) %>%
    summarise(Start = min(Period), End = max(Period), .groups = "drop") %>%
    mutate(Include = TRUE, .after = Year_Cal)
}

# 2. Fallback: calendar years (the first/last year may be incomplete).
build_calendar_cycles <- function(periods, label = "FY") {
  default_cycle_table(periods) %>% transmute(Label = cycle_label(label, Year_Cal), Start, End)
}

# 3. Custom: editable table pre-filled with the calendar years.
custom_cycles <- function(cycle_table, label = "FY") {
  if (is.null(cycle_table) || nrow(cycle_table) == 0) return(NULL)
  out <- cycle_table %>%
    filter(as.logical(Include) %in% TRUE) %>%
    transmute(Label = cycle_label(label, Year_Cal),
              Start = parse_dates_flexible(Start), End = parse_dates_flexible(End))
  if (nrow(out) == 0) NULL else out
}

# Number of Analytical periods inside each cycle (for the UI preview).
cycle_period_counts <- function(cycles, periods) {
  periods <- unique(periods[!is.na(periods)])
  cycles %>% mutate(Periods = map2_int(Start, End, ~ sum(periods >= .x & periods <= .y)))
}

validate_cycles <- function(cycles) {
  if (is.null(cycles) || nrow(cycles) == 0) return(list(errors = "No business cycles are defined.", warnings = character(0)))
  errors <- c(
    if (any(is.na(cycles$Start) | is.na(cycles$End))) "Some business cycles have missing dates.",
    if (any(cycles$Start > cycles$End, na.rm = TRUE)) "Some business cycles start after they end."
  )
  ord <- arrange(cycles, Start)
  overlap <- nrow(ord) > 1 && any(ord$Start[-1] <= ord$End[-nrow(ord)], na.rm = TRUE)
  list(errors = errors %||% character(0),
       warnings = if (overlap) "Business cycles overlap; the first matching one wins in the generated code." else character(0))
}

# Nested ifelse that labels each period with its business cycle (base R).
build_cycle_code <- function(cycles, header) {
  pad <- strrep(" ", 25)
  lines <- pmap_chr(list(seq_len(nrow(cycles)), cycles$Start, cycles$End, cycles$Label), function(i, s, e, lab) {
    cond <- sprintf("Data$%s >= as.Date(%s) & Data$%s <= as.Date(%s)",
                    PERIOD_COL, r_str(format_date_iso(s)), PERIOD_COL, r_str(format_date_iso(e)))
    prefix <- if (i == 1) str_c(r_col(BUSINESS_CYCLE_VAR_NAME), " <- ") else pad
    sprintf("%sifelse(%s, %s,", prefix, cond, r_str(lab))
  })
  closing <- str_c(pad, r_str(OUT_OF_SCOPE_LABEL), strrep(")", nrow(cycles)))
  str_c(c(header, lines, closing), collapse = "\n")
}

# Mean of the non-zero target by cross-section x business cycle (base R).
build_weight_code <- function(target_var, cs_dims, cycle_code) {
  groups <- c(if (length(cs_dims) > 0) str_c("Data$", cs_dims), r_col(BUSINESS_CYCLE_VAR_NAME))
  c("#### Weight Variable ####",
    "",
    cycle_code,
    "",
    sprintf("%s <- ave(", r_col(WEIGHT_VAR_NAME)),
    "  # Target Variable (DepVar Transformed)",
    sprintf("  %s,", r_col(target_var)),
    "  # Cross-sectional dimensions + Business Cycle",
    sprintf("  %s,", str_c(groups, collapse = ", ")),
    "  FUN = function(x) {",
    "    valid_x <- x[!is.na(x) & x != 0]",
    "    if (length(valid_x) == 0) return(0)",
    "    mean(valid_x)",
    "  }",
    ")") %>% str_c(collapse = "\n")
}

kpi_metadata_row <- function(type, vof_name, analytical_var, metric, code, cs_dims) {
  row <- tibble(Type = type, MainModelVariableName = vof_name, AnalyticalVariableName = analytical_var,
                Metric = metric, !!METADATA_CODE_COL := code)
  for (d in cs_dims) row[[d]] <- ""
  row
}

# Main entry point. `cycles` is a cycle table (Label, Start, End) or NULL.
generate_kpi_code <- function(kpi_var, mff_vars, cs_dims, gen_log = TRUE, gen_weight = TRUE,
                              cycles = NULL, cycles_header = "# Business Cycle") {
  if (!is_valid_value(kpi_var)) return(list(errors = "Select a KPI variable."))
  if (!gen_log && !gen_weight) return(list(errors = "Select at least one transformation."))

  code <- character(0); meta <- list(); warnings <- character(0)
  log_name <- build_kpi_log_name(kpi_var, mff_vars, cs_dims)

  if (gen_log) {
    lc <- build_kpi_log_code(kpi_var, log_name)
    code <- c(code, lc$code)
    meta <- c(meta, list(kpi_metadata_row(METADATA_TYPES[["kpi_log"]], log_name, kpi_var, "KPI", lc$code, cs_dims)))
  }
  if (gen_weight) {
    v <- validate_cycles(cycles)
    if (length(v$errors) > 0) return(list(errors = v$errors))
    warnings <- c(warnings, v$warnings)
    target <- if (gen_log) log_name else kpi_var
    wc <- build_weight_code(target, cs_dims, build_cycle_code(cycles, cycles_header))
    code <- c(code, wc)
    meta <- c(meta, list(kpi_metadata_row(METADATA_TYPES[["weight"]], WEIGHT_VAR_NAME, target, "Weight", wc, cs_dims)))
  }
  list(code = str_c(str_c(code, collapse = "\n\n"), "\n"), metadata = list_rbind(meta),
       log_name = log_name, warnings = warnings, errors = character(0))
}
