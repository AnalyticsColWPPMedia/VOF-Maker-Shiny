# R/core/13_pattern_detection.R
# Pattern detection for dummies. Every method works on the deviation of the KPI
# from a robust trend, so the effects read as "+25% over the normal level" and
# are comparable across years and KPIs:
#
#   A. Holidays: event study around every calendar date (aligned on the real
#      date of each year, so moving holidays and 53-week years are handled).
#   B. Yearly seasonality: windows of consecutive weeks of the year with a
#      consistent deviation across years (aligned by date, +-1 week).
#   C. Sub-annual patterns: month turns, first/last week of the month,
#      4-4-5 fiscal quarters and periodicities found by autocorrelation.
#   D. Atypical points: what is still out of range in a single year after
#      removing the yearly profile.
#
# Thresholds are in business terms: minimum lift (% of the normal level) and
# minimum consistency (share of years / occurrences with the same sign).

PATTERN_TYPES <- c(holiday = "Holiday", season = "Seasonality", subannual = "Sub-annual", atypical = "Atypical")
DEV_SCALES <- c("Relative to trend (%)" = "relative", "Additive (% of median level)" = "additive")

week_of_year <- function(d) pmin(as.integer(floor((yday(d) - 1) / 7) + 1), 52L)

# ------------------------------------------------------------------------------
# Deviation from the trend
# ------------------------------------------------------------------------------
# Trend: centred running median (13 periods by default), robust to peaks of a
# few weeks. Relative: KPI / trend - 1 (sales, traffic, subscriptions...).
# Additive: (KPI - trend) / median level, for KPIs near 0 or with sign changes
# (e.g. some brand-health scores).
kpi_deviation <- function(series, scale = "relative", window = 13) {
  s <- series %>% arrange(Period)
  x <- s$value
  filled <- x; filled[is.na(filled)] <- stats::median(x, na.rm = TRUE)
  k <- min(window, length(x) - (1 - length(x) %% 2))
  trend <- if (k >= 3) as.numeric(stats::runmed(filled, k, endrule = "median")) else filled
  if (scale == "relative" && any(trend <= 0, na.rm = TRUE)) scale <- "additive"
  dev <- if (scale == "relative") x / trend - 1 else (x - trend) / stats::median(abs(trend[trend != 0]))
  tibble(Period = s$Period, value = x, trend = trend, dev = dev, year = year(Period), woy = week_of_year(Period)) %>%
    structure(scale = scale)
}

choose_scale <- function(values) if (all(values[!is.na(values)] >= 0)) "relative" else "additive"

consistent <- function(devs, sign_ref, min_lift) {
  devs <- devs[!is.na(devs)]
  if (length(devs) == 0 || sign_ref == 0) return(0)
  mean(sign(devs) == sign_ref & abs(devs) >= min_lift / 2)
}

# ------------------------------------------------------------------------------
# A. Holidays (event study)
# ------------------------------------------------------------------------------
# For every calendar event: deviation at -window..+window periods around the
# event in each year. The anchor is the strongest offset within +-max_offset
# (e.g. the week before Christmas); a small max_offset keeps a holiday from
# borrowing the effect of a big neighbouring season. Ramp/decay are the
# consecutive weeks before/after the anchor that move in the same direction.
holiday_effects <- function(dev, events, min_lift = 0.10, min_consistency = 0.67, window = 4, max_offset = 1) {
  per <- dev$Period
  occ <- events %>% filter(Period %in% per) %>% distinct(Name, Period)
  if (nrow(occ) == 0) return(tibble())
  ks <- -window:window
  occ %>%
    group_by(Name) %>%
    group_map(function(o, key) {
      idx <- match(o$Period, per)
      mat <- map(idx, function(i) dev$dev[ifelse(i + ks >= 1 & i + ks <= length(per), i + ks, NA)]) %>% do.call(rbind, .)
      med <- apply(mat, 2, stats::median, na.rm = TRUE)
      cons <- map_dbl(seq_along(ks), ~ consistent(mat[, .x], sign(med[.x]), min_lift))
      cand <- which(abs(ks) <= max_offset)
      best <- cand[which.max(abs(med[cand]) * cons[cand])]
      lift <- med[best]; s <- sign(lift)
      run_len <- function(steps) {
        n <- 0
        for (j in steps) {
          if (j < 1 || j > length(ks) || is.na(med[j]) || sign(med[j]) != s || abs(med[j]) < min_lift / 2) break
          n <- n + 1
        }
        n
      }
      anchor <- per[idx + ks[best]]
      tibble(Pattern = key$Name, Type = PATTERN_TYPES[["holiday"]], Offset = ks[best], Lift = lift,
             Consistency = cons[best], Years = sum(!is.na(mat[, best])),
             Ramp = run_len(seq(best - 1, 1)), Decay = run_len(seq(best + 1, length(ks))),
             Dates = list(sort(anchor[!is.na(anchor)])), Profile = list(tibble(k = ks, median = med)),
             Matrix = list(mat), EventPeriods = list(o$Period))
    }) %>%
    list_rbind() %>%
    mutate(Passes = abs(Lift) >= min_lift & Consistency >= min_consistency & Years >= 2) %>%
    arrange(desc(Passes), desc(abs(Lift) * Consistency)) %>%
    merge_same_weeks()
}

# Holidays whose anchor weeks overlap (Christmas Eve / Christmas Day) are one
# effect: the strongest is kept and the others are listed in SameWeeks.
merge_same_weeks <- function(hol) {
  if (nrow(hol) == 0) return(hol)
  keep_idx <- integer(0); same <- character(nrow(hol))
  for (i in seq_len(nrow(hol))) {
    owner <- detect(keep_idx, ~ mean(hol$Dates[[i]] %in% hol$Dates[[.x]]) >= 0.5)
    if (is.null(owner)) keep_idx <- c(keep_idx, i)
    else same[owner] <- str_c(c(if (nzchar(same[owner])) same[owner], hol$Pattern[i]), collapse = ", ")
  }
  hol$SameWeeks <- same
  hol[keep_idx, ]
}

# ------------------------------------------------------------------------------
# B. Yearly seasonality (windows)
# ------------------------------------------------------------------------------
# Yearly profile: median deviation of each week of the year across years.
yearly_profile <- function(dev) {
  dev %>% group_by(woy) %>% summarise(profile = stats::median(dev, na.rm = TRUE), .groups = "drop") %>%
    right_join(tibble(woy = 1:52), by = "woy") %>% arrange(woy) %>% mutate(profile = replace_na(profile, 0))
}

season_windows <- function(dev, min_lift = 0.10, min_consistency = 0.67, holidays = tibble()) {
  years <- unique(dev$year)
  if (length(years) < 2) return(tibble())
  by_year <- dev %>% group_by(year, woy) %>% summarise(dev = mean(dev, na.rm = TRUE), .groups = "drop")
  prof <- yearly_profile(dev)
  stats <- prof %>% mutate(
    cons = map2_dbl(woy, profile, function(w, p) {
      if (abs(p) < min_lift) return(0)
      # A year counts if the same week (+-1) moves the same way.
      hits <- map_lgl(years, function(y) {
        v <- by_year$dev[by_year$year == y & abs(by_year$woy - w) <= 1]
        any(sign(v) == sign(p) & abs(v) >= min_lift / 2, na.rm = TRUE)
      })
      n_obs <- n_distinct(by_year$year[by_year$woy == w & !is.na(by_year$dev)])
      if (n_obs < 2) 0 else sum(hits) / length(years)
    }),
    flag = abs(profile) >= min_lift & cons >= min_consistency)
  if (!any(stats$flag)) return(tibble())
  run <- cumsum(c(TRUE, diff(stats$flag) != 0 | diff(sign(stats$profile)) != 0))
  hol_dates <- if (nrow(holidays) > 0) holidays %>% filter(Passes) %>% select(Pattern, Dates) else tibble(Pattern = character(0), Dates = list())
  stats %>% mutate(run = run) %>% filter(flag) %>%
    group_by(run) %>%
    summarise(Start = min(woy), End = max(woy), Lift = mean(profile), Peak = profile[which.max(abs(profile))],
              Consistency = min(cons), .groups = "drop") %>%
    mutate(
      Dates = map2(Start, End, ~ sort(dev$Period[dev$woy >= .x & dev$woy <= .y])),
      Years = map_int(Dates, ~ n_distinct(year(.x))),
      direction = if_else(Lift > 0, "Peak", "Drop"),
      months = map_chr(Dates, function(d) {
        m <- unique(month(d, label = TRUE, abbr = FALSE, locale = "C"))
        m <- as.character(sort(m))
        if (length(m) == 1) m else str_c(m[1], " to ", m[length(m)])
      }),
      Pattern = str_c(months, " ", direction),
      Covers = map_chr(Dates, function(d) {
        h <- hol_dates$Pattern[map_lgl(hol_dates$Dates, ~ mean(.x %in% d) >= 0.5)]
        str_c(h, collapse = ", ")
      }),
      Type = PATTERN_TYPES[["season"]], Passes = TRUE) %>%
    select(Pattern, Type, Start, End, Lift, Peak, Consistency, Years, Dates, Covers, Passes)
}

# ------------------------------------------------------------------------------
# C. Sub-annual patterns
# ------------------------------------------------------------------------------
# Calendar candidates (month turn, first/last full week, 4-4-5 quarters) and
# the strongest periodicity of the autocorrelation; each is a set of periods
# whose deviation (after the yearly profile) is consistently one way.
subannual_patterns <- function(dev, min_lift = 0.10, min_consistency = 0.6, max_lag = 26) {
  if (nrow(dev) < 26) return(tibble())
  prof <- yearly_profile(dev)
  d <- dev %>% left_join(prof, by = "woy") %>% mutate(r = dev - profile, idx = row_number())
  step <- median_step(d$Period) %|NA|% 7
  end <- d$Period + step - 1
  candidates <- list(
    `Month Turn` = month(d$Period) != month(end) | mday(d$Period) == 1,
    `First Full Week of the Month` = mday(d$Period) <= 7 & month(d$Period) == month(end),
    `Last Full Week of the Month` = month(d$Period) == month(end) & month(end + 1) != month(end)
  )
  if (abs(step - 7) <= 1) for (ph in 0:12) candidates[[str_c("Quarter Week ", ph + 1, " of 13")]] <- (d$idx - 1) %% 13 == ph

  # Periodicity from the autocorrelation of the residual (lags 2..max_lag).
  r_filled <- replace_na(d$r, 0)
  ac <- stats::acf(r_filled, lag.max = min(max_lag, nrow(d) - 1), plot = FALSE)$acf[-1]
  lags <- seq_along(ac)
  ok <- lags >= 2 & !is.na(ac) & ac > 2 / sqrt(nrow(d))   # NA when the residual is flat
  if (any(ok)) {
    p <- lags[ok][which.max(ac[ok])]
    phase_means <- map_dbl(0:(p - 1), ~ mean(d$r[(d$idx - 1) %% p == .x], na.rm = TRUE))
    ph <- which.max(abs(phase_means)) - 1
    candidates[[str_c("Every ", p, " Weeks")]] <- (d$idx - 1) %% p == ph
  }

  imap(candidates, function(sel, name) {
    v <- d$r[sel]
    lift <- stats::median(v, na.rm = TRUE)
    tibble(Pattern = name, Type = PATTERN_TYPES[["subannual"]], Lift = lift,
           Consistency = consistent(v, sign(lift), min_lift), Occurrences = sum(sel),
           Dates = list(d$Period[sel]))
  }) %>%
    list_rbind() %>%
    mutate(direction = if_else(Lift > 0, "Peak", "Drop"), Pattern = str_c(Pattern, " ", direction),
           Passes = abs(Lift) >= min_lift & Consistency >= min_consistency) %>%
    # The 13 quarter phases: keep only the strongest one.
    mutate(score = abs(Lift) * Consistency, quarter = str_detect(Pattern, "^Quarter Week")) %>%
    filter(!quarter | row_number() == which(quarter)[which.max(score[quarter])]) %>%
    select(-score, -quarter) %>%
    arrange(desc(Passes), desc(abs(Lift) * Consistency))
}

# ------------------------------------------------------------------------------
# D. Atypical points
# ------------------------------------------------------------------------------
# Single-year points out of range after removing the yearly profile, outside
# the holidays and windows already found. One point per episode.
atypical_points <- function(dev, min_lift = 0.10, z_threshold = 3.5, exclude = as.Date(character(0))) {
  prof <- yearly_profile(dev)
  d <- dev %>% left_join(prof, by = "woy") %>% mutate(r = dev - profile)
  scale <- stats::mad(d$r, na.rm = TRUE)
  if (!is.finite(scale) || scale == 0) return(tibble())
  d <- d %>% mutate(z = (r - stats::median(r, na.rm = TRUE)) / scale,
                    flag = !is.na(r) & abs(z) >= z_threshold & abs(r) >= min_lift & !Period %in% exclude)
  if (!any(d$flag)) return(tibble())
  d %>% mutate(run = cumsum(c(TRUE, diff(flag) != 0 | diff(sign(r)) != 0))) %>%
    filter(flag) %>% group_by(run) %>% slice_max(abs(r), n = 1, with_ties = FALSE) %>% ungroup() %>%
    transmute(Pattern = str_c(if_else(r > 0, "Peak", "Drop"), " ", month(Period, label = TRUE, abbr = TRUE, locale = "C"), " ", year(Period)),
              Type = PATTERN_TYPES[["atypical"]], Lift = r, z, Dates = as.list(Period), Passes = TRUE) %>%
    arrange(desc(abs(z)))
}

# ------------------------------------------------------------------------------
# All patterns of one series
# ------------------------------------------------------------------------------
detect_patterns <- function(series, events, scale = "relative", min_lift = 0.10, min_consistency = 0.67,
                            z_threshold = 3.5, max_offset = 1) {
  dev <- kpi_deviation(series, scale)
  hol <- holiday_effects(dev, events, min_lift, min_consistency, max_offset = max_offset)
  sea <- season_windows(dev, min_lift, min_consistency, hol)
  sub <- subannual_patterns(dev, min_lift, min(min_consistency, 0.6))
  covered <- c(if (nrow(hol) > 0) as_date(unlist(hol$Dates[hol$Passes])), if (nrow(sea) > 0) as_date(unlist(sea$Dates)))
  aty <- atypical_points(dev, min_lift, z_threshold, covered)
  list(dev = dev, scale = attr(dev, "scale"), holidays = hol, seasons = sea, subannual = sub, atypical = aty)
}

# Suggested dummy (naming convention) for a detected pattern.
pattern_dummy <- function(row, cs = list()) {
  type <- row$Type
  if (type == PATTERN_TYPES[["holiday"]])
    return(new_dummy(row$Pattern, "Holiday", dates = row$Dates[[1]],
                     shape = list(ramp = row$Ramp %||% 0, decay = row$Decay %||% 0), cs = cs, note = "Detected (holiday)"))
  if (type == PATTERN_TYPES[["atypical"]])
    return(new_dummy(str_c("Atypical ", row$Pattern), "Business", dates = row$Dates[[1]], cs = cs, note = "Detected (atypical)"))
  new_dummy(row$Pattern, "Season", dates = row$Dates[[1]], cs = cs,
            note = if (type == PATTERN_TYPES[["season"]]) "Detected (seasonality)" else "Detected (sub-annual)")
}

# ------------------------------------------------------------------------------
# Entities
# ------------------------------------------------------------------------------
# Effect of each pattern found on the total in every entity: lift over the
# pattern periods and share of them with the same sign.
entity_pattern_effects <- function(level, patterns, scale = "relative", min_lift = 0.10, min_consistency = 0.67) {
  if (nrow(patterns) == 0) return(tibble())
  level %>% group_split(Entity) %>%
    map(function(e) {
      dev <- kpi_deviation(e %>% select(Period, value), scale)
      prof <- yearly_profile(dev)
      dev <- dev %>% left_join(prof, by = "woy") %>% mutate(r = dev - profile)
      map(seq_len(nrow(patterns)), function(i) {
        p <- patterns[i, ]
        v <- if (p$Type == PATTERN_TYPES[["subannual"]]) dev$r[dev$Period %in% p$Dates[[1]]] else dev$dev[dev$Period %in% p$Dates[[1]]]
        lift <- stats::median(v, na.rm = TRUE)
        tibble(Entity = e$Entity[1], Pattern = p$Pattern, Type = p$Type, TotalLift = p$Lift, Lift = lift,
               Consistency = consistent(v, sign(p$Lift), min_lift),
               Effect = !is.na(lift) && sign(lift) == sign(p$Lift) && abs(lift) >= min_lift &&
                 consistent(v, sign(p$Lift), min_lift) >= min_consistency)
      }) %>% list_rbind()
    }) %>% list_rbind()
}

entity_pattern_summary <- function(effects) {
  if (nrow(effects) == 0) return(tibble())
  effects %>% group_by(Pattern, Type) %>%
    summarise(`Total lift` = first(TotalLift), `Entities with the effect` = sum(Effect), Entities = n(),
              `Median entity lift` = stats::median(Lift, na.rm = TRUE),
              `Without the effect` = str_c(Entity[!Effect], collapse = ", "), .groups = "drop") %>%
    arrange(desc(`Entities with the effect` / Entities))
}

# Heatmap data: deviation by week of the year x year.
seasonality_matrix <- function(dev) {
  dev %>% group_by(year, woy) %>% summarise(dev = mean(dev, na.rm = TRUE), .groups = "drop")
}
