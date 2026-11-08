# R/core/15_dummy_conflicts.R
# Dummies that switch on in the same periods (and entities) are collinear: the
# model cannot tell their effects apart. Each pending dummy is compared with the
# other pending dummies and with the dummies saved in the metadata, on the
# column it produces (periods of the Analytical x entities), so dates on
# different days of the same week collide and different CS scopes do not.
#
# Levels:
#   Blocked  - the event periods coincide in every year both dummies have, or
#              the two columns switch on in exactly the same rows;
#   Warning  - the event periods coincide only in some years, or a ramp/decay
#              of one dummy runs into the other;
#   Info     - a Season window contains the other dummy (usual: the holiday
#              models the peak on top of the season), unless identical.

DUMMY_CONFLICT_LEVELS <- c(blocked = "Blocked", warning = "Warning", info = "Info")

# Columns needed to evaluate dummy code (Period + CS dimensions).
dummy_base_data <- function(analytical, cs_dims) {
  as.data.frame(analytical, check.names = FALSE)[, c(PERIOD_COL, intersect(cs_dims, names(analytical))), drop = FALSE]
}

# Value of a dummy in every row of the Analytical, from its base-R code
# (the same code AWB runs). NULL when the code cannot be evaluated.
dummy_column <- function(code, name, base) {
  if (!is_valid_value(code)) return(NULL)
  env <- new.env(parent = as.environment("package:stats"))
  env$Data <- base
  ok <- tryCatch({ eval(parse(text = code), envir = env); TRUE }, error = function(e) FALSE)
  if (!ok || !name %in% names(env$Data)) return(NULL)
  as.numeric(env$Data[[name]])
}

# Conflict between two dummy columns `a` and `b` (same rows as `periods`).
# Returns NULL or list(level, detail).
dummy_pair_conflict <- function(a, b, periods, season = FALSE) {
  on_a <- !is.na(a) & a != 0; on_b <- !is.na(b) & b != 0
  both <- on_a & on_b
  if (!any(both)) return(NULL)
  fmt_weeks <- function(p) { p <- sort(unique(p)); str_c(str_c(format(head(p, 4)), collapse = ", "), if (length(p) > 4) sprintf(" (+%d)", length(p) - 4) else "") }
  if (identical(on_a, on_b))
    return(list(level = DUMMY_CONFLICT_LEVELS[["blocked"]], detail = "switch on in exactly the same periods and entities"))
  if (season)
    return(list(level = DUMMY_CONFLICT_LEVELS[["info"]], detail = str_c("inside the season window (", fmt_weeks(periods[both]), ")")))
  yr <- year(periods)
  ev_a <- on_a & a >= max(a[on_a]); ev_b <- on_b & b >= max(b[on_b])   # event periods (weight 1), not ramp / decay
  common_years <- intersect(yr[ev_a], yr[ev_b])
  clash_years <- sort(unique(yr[ev_a & ev_b]))
  if (length(clash_years) > 0 && length(common_years) > 0 && all(common_years %in% clash_years))
    return(list(level = DUMMY_CONFLICT_LEVELS[["blocked"]],
                detail = sprintf("event periods coincide every year (%s)", str_c(clash_years, collapse = ", "))))
  if (length(clash_years) > 0)
    return(list(level = DUMMY_CONFLICT_LEVELS[["warning"]],
                detail = sprintf("event periods coincide in %s", str_c(clash_years, collapse = ", "))))
  list(level = DUMMY_CONFLICT_LEVELS[["warning"]], detail = str_c("ramp / decay overlap in ", fmt_weeks(periods[both])))
}

# All conflicts of the pending dummies.
# pending: tibble(name, category, code); saved: tibble(name, category, code)
# (saved dummies with the name of a pending one are being replaced: skipped).
# Returns tibble(Dummy, Other, OtherSource, Level, Detail).
dummy_conflicts <- function(pending, saved, analytical, cs_dims) {
  empty <- tibble(Dummy = character(0), Other = character(0), OtherSource = character(0), Level = character(0), Detail = character(0))
  if (is.null(analytical) || nrow(pending) == 0) return(empty)
  base <- dummy_base_data(analytical, cs_dims)
  periods <- as.Date(base[[PERIOD_COL]])
  saved <- (saved %||% pending[0, ]) %>% filter(!name %in% pending$name)
  all <- bind_rows(pending %>% mutate(source = "pending"), saved %>% mutate(source = "saved")) %>%
    distinct(name, .keep_all = TRUE)
  cols <- map2(all$code, all$name, ~ dummy_column(.x, .y, base))
  out <- list()
  n_p <- sum(all$source == "pending")
  for (i in seq_len(n_p)) for (j in seq_len(nrow(all))) {
    if (j == i || (j <= n_p && j < i) || is.null(cols[[i]]) || is.null(cols[[j]])) next
    season <- any(c(all$category[i], all$category[j]) == "Season")
    cf <- dummy_pair_conflict(cols[[i]], cols[[j]], periods, season)
    if (is.null(cf)) next
    out[[length(out) + 1]] <- tibble(Dummy = all$name[i], Other = all$name[j], OtherSource = all$source[j],
                                     Level = cf$level, Detail = cf$detail)
  }
  if (length(out) == 0) empty else list_rbind(out)
}

# One line per pending dummy for the builder table ("Blocked: with X (...)").
dummy_conflict_summary <- function(conflicts, names) {
  map_chr(names, function(n) {
    rows <- conflicts %>% filter(Dummy == n | (Other == n & OtherSource == "pending"))
    if (nrow(rows) == 0) return("")
    rows %>%
      mutate(other = if_else(Dummy == n, Other, Dummy),
             other = if_else(OtherSource == "saved" & Dummy == n, str_c(other, " (saved)"), other),
             order = match(Level, DUMMY_CONFLICT_LEVELS)) %>%
      arrange(order) %>%
      transmute(txt = sprintf("%s: with %s, %s", Level, other, Detail)) %>%
      pull(txt) %>% str_c(collapse = "; ")
  })
}
