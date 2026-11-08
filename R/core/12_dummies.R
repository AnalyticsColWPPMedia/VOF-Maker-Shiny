# R/core/12_dummies.R
# Dummies tab: holiday calendar, KPI series in levels and the dummy builder
# (exact periods, ramps before and decays after an event, CS scope). Pattern
# detection lives in 13_pattern_detection.R.
# Generated code follows the team's format (base R, AWB R 3.6):
#
#   #### Thanksgiving ####
#
#   Data[,"Holiday Thanksgiving"] = 0
#   Data[which(Data$Period %in% as.Date(c('2023-11-20'))), "Holiday Thanksgiving"] = 1

DUMMY_CATEGORIES <- c("Holiday", "Season", "Business", "Macro", "Dummy")
DUMMY_PROFILES <- c("Linear" = "linear", "Exponential" = "exponential", "Custom weights" = "custom")

# ------------------------------------------------------------------------------
# Holiday calendar
# ------------------------------------------------------------------------------
read_holiday_calendar <- function(path = file.path("data", "holiday_calendar.csv")) {
  if (!file.exists(path)) return(tibble(MarketCode = character(0), Market = character(0), Date = as.Date(character(0)),
                                        Name = character(0), Category = character(0)))
  read_csv(path, col_types = cols(.default = col_character()), show_col_types = FALSE) %>%
    mutate(Date = as_date(Date))
}

# Optional calendar uploaded by the user: Date + Name (+ Category).
read_custom_calendar <- function(path, file_name, market_code = "CUSTOM") {
  df <- read_table_file(path, file_name, as_text = TRUE)
  names(df) <- str_to_title(names(df))
  if (!all(c("Date", "Name") %in% names(df))) stop("The calendar needs the columns Date and Name.")
  tibble(MarketCode = market_code, Market = "Custom", Date = parse_dates_flexible(df$Date),
         Name = clean_event_name(df$Name), Category = if ("Category" %in% names(df)) df$Category else "Custom") %>%
    filter(!is.na(Date), Name != "")
}

# English, ASCII, no apostrophes or special characters (naming convention).
clean_event_name <- function(x) {
  x %>% iconv(to = "ASCII//TRANSLIT", sub = "") %>% str_remove_all("['’]") %>%
    str_replace_all("[^A-Za-z0-9 \\-]", " ") %>% str_squish()
}

market_choices <- function(calendar) {
  m <- calendar %>% distinct(MarketCode, Market) %>% arrange(Market)
  set_names(m$MarketCode, str_c(m$Market, " (", m$MarketCode, ")"))
}

# Analytical period that contains each date (weeks start on the project's own
# weekday, taken from the Analytical). NA outside the data range.
date_to_period <- function(dates, periods) {
  periods <- sort(unique(periods[!is.na(periods)]))
  step <- median_step(periods) %|NA|% 7
  idx <- findInterval(as.numeric(dates), as.numeric(periods))
  ok <- idx > 0 & as.numeric(dates) < as.numeric(max(periods)) + step
  out <- rep(as.Date(NA), length(dates))
  out[ok] <- periods[idx[ok]]
  out
}

# Calendar events mapped to the Analytical periods.
calendar_in_periods <- function(calendar, market, periods) {
  calendar %>%
    filter(MarketCode %in% market) %>%
    mutate(Period = date_to_period(Date, periods)) %>%
    filter(!is.na(Period))
}

# ------------------------------------------------------------------------------
# KPI series (always in levels)
# ------------------------------------------------------------------------------
# Peaks must be found on the KPI in levels. A KPI may already be in logs in the
# MFF/Analytical (not only through the app's transformation), so the scale is
# detected from the name and the values; the analyst confirms it in the UI.
detect_log_scale <- function(name, values) {
  v <- suppressWarnings(as.numeric(values))
  v <- v[is.finite(v)]
  by_name <- str_detect(name, regex("(^|[\\s_])(ln|log)([\\s_]|$)", ignore_case = TRUE))
  by_values <- length(v) > 0 && max(v) < 30 && (any(v < 0) || (max(v) - min(v[v > 0], na.rm = TRUE)) < 15) && max(v) > 0
  reasons <- c(if (by_name) "its name says Log/LN",
               if (by_values) sprintf("its values range from %s to %s, typical of a log scale", signif(min(v), 3), signif(max(v), 3)))
  list(is_log = by_name || by_values, reason = str_c(reasons, collapse = " and "))
}

# KPI by period, for the total of all entities and/or by entity, in levels.
# Log zeros (the app writes 0 when the KPI is <= 0) become missing.
kpi_level_series <- function(data, kpi, cs_dims, is_log = FALSE) {
  v <- suppressWarnings(as.numeric(data[[kpi]]))
  if (is_log) v <- ifelse(v == 0, NA_real_, exp(v))
  tibble(Period = data[[PERIOD_COL]], Entity = qa_entity(data, cs_dims), value = v)
}

total_series <- function(level) {
  level %>% group_by(Period) %>% summarise(value = if (all(is.na(value))) NA_real_ else sum(value, na.rm = TRUE), .groups = "drop") %>%
    arrange(Period)
}

# ------------------------------------------------------------------------------
# Dummy specification, weights and code
# ------------------------------------------------------------------------------
# shape: list(ramp = n, ramp_profile, ramp_param, ramp_weights,
#             decay = m, decay_profile, decay_param, decay_weights)
#   linear:      ramp goes from ramp_param (default 1/(n+1)) up to 1;
#                decay goes from 1 down to decay_param (default 1/(m+1)).
#   exponential: weight = rate^k at k periods from the event (param = rate).
#   custom:      weights typed by the user (ramp: farthest -> nearest;
#                decay: nearest -> farthest).
new_dummy <- function(event, category = "Holiday", dates = as.Date(character(0)), ranges = NULL,
                      shape = list(), cs = list(), note = "") {
  list(event = clean_event_name(event), category = category, dates = sort(unique(as_date(dates))),
       ranges = ranges, shape = modifyList(list(ramp = 0, ramp_profile = "linear", ramp_param = NA, ramp_weights = NULL,
                                                decay = 0, decay_profile = "linear", decay_param = NA, decay_weights = NULL), shape),
       cs = map(cs, as_cs_filter) %>% compact(), note = note)
}

shape_suffix <- function(shape) {
  str_c(c(if (shape$ramp > 0) str_c("Ramp", shape$ramp), if (shape$decay > 0) str_c("Decay", shape$decay)), collapse = " ")
}

dummy_name <- function(d) {
  base <- to_title_case(d$event)
  prefix <- if (str_detect(base, str_c("^", d$category, "\\b"))) "" else str_c(d$category, " ")
  str_squish(str_c(prefix, base, " ", shape_suffix(d$shape)))
}

side_weights <- function(n, profile, param, custom, side) {
  if (n <= 0) return(numeric(0))
  w <- switch(profile,
    linear = {
      edge <- if (is.na(param %||% NA)) 1 / (n + 1) else param
      if (side == "ramp") seq(edge, 1, length.out = n + 1)[seq_len(n)] else seq(1, edge, length.out = n + 1)[-1]
    },
    exponential = {
      rate <- if (is.na(param %||% NA)) 0.5 else param
      k <- if (side == "ramp") n:1 else 1:n
      rate^k
    },
    custom = {
      v <- suppressWarnings(as.numeric(custom))
      v <- v[!is.na(v)]
      if (length(v) < n) stop(sprintf("%s: %d custom weights are needed.", side, n))
      v[seq_len(n)]
    })
  round(w, 4)
}

# Weight of the dummy in every affected period (max when windows overlap).
dummy_weights <- function(d, periods) {
  periods <- sort(unique(periods[!is.na(periods)]))
  ev_periods <- unique(date_to_period(d$dates, periods)) %>% discard(is.na)
  idx <- match(ev_periods, periods)
  ramp <- side_weights(d$shape$ramp, d$shape$ramp_profile, d$shape$ramp_param, d$shape$ramp_weights, "ramp")
  decay <- side_weights(d$shape$decay, d$shape$decay_profile, d$shape$decay_param, d$shape$decay_weights, "decay")
  w <- list()
  for (i in idx) {
    w <- c(w, list(tibble(i = i, weight = 1)))
    if (length(ramp) > 0) w <- c(w, list(tibble(i = i - rev(seq_along(ramp)), weight = ramp)))
    if (length(decay) > 0) w <- c(w, list(tibble(i = i + seq_along(decay), weight = decay)))
  }
  # Multi-period events (start-end ranges): every period inside is 1.
  for (r in d$ranges %||% list()) {
    inside <- which(periods >= date_to_period(r$start, periods) & periods <= (date_to_period(r$end, periods) %|NA|% max(periods)))
    if (length(inside) > 0) w <- c(w, list(tibble(i = inside, weight = 1)))
  }
  if (length(w) == 0) return(tibble(Period = as.Date(character(0)), weight = numeric(0)))
  list_rbind(w) %>% filter(i >= 1, i <= length(periods)) %>%
    group_by(i) %>% summarise(weight = max(weight), .groups = "drop") %>%
    transmute(Period = periods[i], weight) %>% arrange(Period)
}

dq <- function(x) str_c('"', str_replace_all(x, fixed('"'), '\\\\"'), '"')

# Team format: one line per period, `which()` on the Period (and CS filter).
build_dummy_code <- function(d, weights) {
  name <- dummy_name(d)
  cs_expr <- MFF_DIMS %>% keep(~ !is.null(d$cs[[.x]])) %>% map_chr(~ cs_filter_expr(.x, d$cs[[.x]]))
  cond_cs <- if (length(cs_expr) > 0) str_c(" & ", str_c(cs_expr, collapse = " & ")) else ""
  lines <- map2_chr(format(weights$Period, "%Y-%m-%d"), weights$weight, function(p, w) {
    sprintf("Data[which(Data$%s %%in%% as.Date(c('%s'))%s), %s] = %s", PERIOD_COL, p, cond_cs, dq(name), format(w, scientific = FALSE))
  })
  c(sprintf("#### %s ####", str_c(to_title_case(d$event), if (nzchar(shape_suffix(d$shape))) str_c(" ", shape_suffix(d$shape)))),
    "",
    sprintf("Data[,%s] = 0", dq(name)),
    lines) %>% str_c(collapse = "\n")
}

dummy_metadata <- function(d, weights, code, cs_dims) {
  row <- tibble(Type = METADATA_TYPES[["dummy"]], MainModelVariableName = dummy_name(d), AnalyticalVariableName = "",
                VofVariableName = d$category, VofComplement = to_title_case(d$event),
                MinPeriod = if (nrow(weights) > 0) format(min(weights$Period)) else "",
                MaxPeriod = if (nrow(weights) > 0) format(max(weights$Period)) else "",
                Metric = "Dummy", !!METADATA_CODE_COL := code)
  for (dim in cs_dims) {
    meta <- cs_filter_to_meta(d$cs[[dim]])
    row[[dim]] <- meta$values
    row[[str_c(dim, "Filter")]] <- meta$filter
  }
  row
}

generate_dummy <- function(d, periods, cs_dims, existing = character(0)) {
  errors <- c(if (!nzchar(d$event)) "The dummy needs an event name.",
              if (!d$category %in% DUMMY_CATEGORIES) "Unknown category.",
              if (length(d$dates) == 0 && length(d$ranges %||% list()) == 0) "The dummy needs at least one date.")
  if (length(errors) > 0) return(list(errors = errors))
  weights <- tryCatch(dummy_weights(d, periods), error = function(e) e)
  if (inherits(weights, "error")) return(list(errors = conditionMessage(weights)))
  if (nrow(weights) == 0) return(list(errors = sprintf("'%s': no date falls inside the Analytical periods.", dummy_name(d))))
  name <- dummy_name(d)
  code <- build_dummy_code(d, weights)
  list(name = name, code = code, weights = weights, metadata = dummy_metadata(d, weights, code, cs_dims),
       warnings = c(if (name %in% existing) sprintf("'%s' already exists; it will be replaced.", name),
                    if (any(is.na(date_to_period(d$dates, periods))))
                      sprintf("'%s': %d date(s) fall outside the Analytical periods and were ignored.", name, sum(is.na(date_to_period(d$dates, periods))))),
       errors = character(0))
}

# ------------------------------------------------------------------------------
# Events CSV
# ------------------------------------------------------------------------------
EVENTS_TEMPLATE <- tibble(Name = c("Watches and Wonders Event", "Watches and Wonders Event", "Hurricane Milton"),
                          Category = c("Business", "Business", "Macro"),
                          Start = c("2024-04-09", "2025-04-01", "2024-10-07"),
                          End = c("2024-04-15", "2025-04-07", ""),
                          Geography = c("", "", "Tampa; Orlando"))

# One dummy per Name + Category. Rows give single dates (Start) or ranges
# (Start-End). Optional CS columns (e.g. Geography) list entities separated
# by ";" (the dummy keeps those entities).
read_events_file <- function(path, file_name, cs_dims) {
  df <- read_table_file(path, file_name, as_text = TRUE) %>% mutate(across(everything(), ~ replace_na(.x, "")))
  if (!all(c("Name", "Start") %in% names(df))) stop("The events file needs at least the columns Name and Start.")
  if (!"Category" %in% names(df)) df$Category <- "Business"
  if (!"End" %in% names(df)) df$End <- ""
  df %>%
    mutate(Category = if_else(Category %in% DUMMY_CATEGORIES, Category, "Dummy")) %>%
    group_by(Name, Category) %>%
    group_map(function(rows, key) {
      starts <- parse_dates_flexible(rows$Start); ends <- parse_dates_flexible(rows$End)
      single <- starts[is.na(ends) | ends == starts]
      ranges <- map2(starts[!is.na(ends) & ends != starts], ends[!is.na(ends) & ends != starts], ~ list(start = .x, end = .y))
      cs <- cs_dims %>% keep(~ .x %in% names(rows)) %>% set_names() %>%
        map(~ str_split(rows[[.x]], ";") %>% unlist() %>% str_trim() %>% clean_values() %>% unique())
      new_dummy(key$Name, key$Category, dates = single[!is.na(single)], ranges = ranges, cs = cs, note = "Events file")
    })
}
