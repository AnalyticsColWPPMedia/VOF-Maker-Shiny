# R/core/14_splits.R
# Sister VOFs: one configuration of the Media tab generates several VOFs that
# share everything except their period (TimeBreaks) and/or the entities of one
# cross-sectional dimension (CS Groups). Every combination is generated, each
# one with its twin.
#
# TimeBreaks: optional start, cut dates and optional end. A cut date is the
# last day of the TimeBreak on its left; the next TimeBreak starts the day
# after. Dates are calendar dates (they do not need to exist in the
# Analytical), so there are no gaps between TimeBreaks:
#   cuts 2024-06-30, 2025-06-30 -> "-20240630", "20240701-20250630", "20250701-"
#
# CS Groups: groups of entities of one dimension (filters built like the CS
# filter of a row), plus "All Others" (the entities in no group) when some are
# left. Names follow the usual flag rules: "--g San", "--g not San", "--g top3",
# "--g not Atlanta etc", "--g group2" when a flag already names another set.
#
# Rows follow the split unless the analyst gives them their own dates or
# filter. A row that does not follow makes sisters impossible: only one VOF is
# generated (the following rows take the first selected part) and the analyst
# is told.

CS_ALL_OTHERS_LABEL <- "All Others"
SPLIT_MAX_PARTS <- 6

new_split <- function(tb = NULL, cs = NULL) list(tb = tb, cs = cs)

# ------------------------------------------------------------------------------
# TimeBreaks
# ------------------------------------------------------------------------------
# tb: list(enabled, start, end, cuts, selected). Returns list(parts, errors):
# parts = tibble(index, start, end, label) with NA for open ends.
time_break_parts <- function(tb) {
  as_d <- function(x) if (is_valid_value(x)) parse_dates_flexible(x)[1] else as.Date(NA)
  cuts <- map(tb$cuts %||% list(), as_d)
  start <- as_d(tb$start); end <- as_d(tb$end)
  errors <- character(0)
  if (length(cuts) == 0 || some(cuts, is.na))
    return(list(parts = NULL, errors = "TimeBreaks: set every cut date."))
  cuts <- do.call(c, cuts)
  if (any(diff(cuts) <= 0)) errors <- c(errors, "TimeBreaks: each cut date must be after the previous one.")
  if (!is.na(start) && cuts[1] < start) errors <- c(errors, "TimeBreaks: the first cut is before the start date.")
  if (!is.na(end) && cuts[length(cuts)] >= end) errors <- c(errors, "TimeBreaks: the last cut must be before the end date.")
  if (length(errors) > 0) return(list(parts = NULL, errors = errors))
  starts <- c(start, cuts + 1)
  ends <- c(cuts, end)
  parts <- tibble(index = seq_along(starts), start = starts, end = ends) %>%
    mutate(label = str_c(map_chr(start, format_date_tag), "-", map_chr(end, format_date_tag)))
  list(parts = parts, errors = character(0))
}

# Periods of the Analytical in every TimeBreak (information for the analyst).
time_break_coverage <- function(parts, periods) {
  periods <- sort(unique(as.Date(periods)))
  parts %>% mutate(
    in_part = map2(start, end, ~ periods[(is.na(.x) | periods >= .x) & (is.na(.y) | periods <= .y)]),
    n = lengths(in_part),
    first = map(in_part, ~ if (length(.x)) min(.x) else as.Date(NA)) %>% do.call(c, .),
    last = map(in_part, ~ if (length(.x)) max(.x) else as.Date(NA)) %>% do.call(c, .)
  ) %>% select(-in_part)
}

time_break_text <- function(parts) {
  if (is.null(parts) || nrow(parts) == 0) return("")
  str_c(sprintf("TimeBreak %d: %s", parts$index, parts$label), collapse = " | ")
}

# ------------------------------------------------------------------------------
# CS Groups
# ------------------------------------------------------------------------------
# cs: list(enabled, dim, groups, selected). Each group is a CS filter built as
# in the Component Rows (Include / Exclude; exact entities or CS Children
# positional patterns) or a plain vector of entities. Returns list(parts,
# errors, notes): parts = tibble(index ("1", ..., "others"), label, filter
# (list), kept (list of entities)). Every group, All Others included, is named
# with the usual flag rules (pattern, "not ...", topX, "<first> etc", groupX).
cs_group_parts <- function(cs, entities) {
  if (!is_valid_value(cs$dim)) return(list(parts = NULL, errors = "CS Groups: choose a dimension."))
  filters <- map(cs$groups %||% list(), as_cs_filter)
  if (length(filters) == 0 || some(filters, is.null))
    return(list(parts = NULL, errors = "CS Groups: select the entities of every group."))
  kept <- map(filters, ~ cs_filter_resolve(.x, entities))
  errors <- character(0)
  empty <- which(lengths(kept) == 0)
  if (length(empty) > 0) errors <- c(errors, sprintf("CS Groups: Group %s keeps no %s.", str_c(empty, collapse = ", "), cs$dim))
  all_kept <- unlist(kept)
  dup <- unique(all_kept[duplicated(str_to_lower(all_kept))])
  if (length(dup) > 0) errors <- c(errors, sprintf("CS Groups: %s in more than one group.", str_c(head(dup, 5), collapse = ", ")))
  if (length(errors) > 0) return(list(parts = NULL, errors = errors))
  # Optional custom names ("--g MyGroup"): cleaned, not reserved, not repeated.
  raw_names <- cs$names %||% list()
  names_clean <- map(seq_along(filters), ~ clean_cs_label(if (length(raw_names) >= .x) raw_names[[.x]] else ""))
  # The All Others name only counts when some entities are left out of the groups.
  residual <- setdiff(clean_values(entities), all_kept)
  others_name <- clean_cs_label(if (length(residual) > 0) cs$others_name %||% "" else "")
  bad <- c(map_chr(names_clean, "error"), others_name$error) %>% keep(nzchar)
  if (length(bad) > 0) return(list(parts = NULL, errors = str_c("CS Groups: ", bad)))
  given <- c(map_chr(names_clean, "name"), others_name$name) %>% keep(nzchar)
  dup_names <- unique(given[duplicated(str_to_lower(given))])
  if (length(dup_names) > 0)
    return(list(parts = NULL, errors = sprintf("CS Groups: the name '%s' is used by more than one group.", str_c(dup_names, collapse = "', '"))))
  groups <- tibble(
    index = as.character(seq_along(filters)), label = str_c("Group ", seq_along(filters)),
    filter = filters, kept = kept, name = map_chr(names_clean, "name")
  )
  # All Others only when some entities are left out of the groups.
  if (length(residual) == 0) {
    return(list(parts = groups, errors = character(0),
                notes = sprintf("Every %s is already in a group: no All Others VOF.", cs$dim)))
  }
  others <- all_others_filter(filters, all_kept)
  parts <- bind_rows(groups, tibble(index = "others", label = CS_ALL_OTHERS_LABEL, filter = list(others$filter),
                                    kept = list(residual), name = others_name$name))
  list(parts = parts, errors = character(0), notes = others$note)
}

# "All Others" = not (Group 1 or Group 2 or ...), written so it stays correct
# when the Analytical gains entities:
# - one group: the same filter with Include / Exclude switched;
# - Include groups with one level of the same match ("Boston - " and "Miami - "
#   as StartsWith): one Exclude level with all the values;
# - otherwise the exact list of the grouped entities, excluded (with a note).
all_others_filter <- function(filters, grouped) {
  if (length(filters) == 1) {
    f <- filters[[1]]
    f$mode <- if (identical(f$mode, "exclude")) "include" else "exclude"
    return(list(filter = f, note = character(0)))
  }
  single <- every(filters, ~ !identical(.x$mode, "exclude") && length(.x$levels) == 1)
  matches <- map_chr(filters, ~ .x$levels[[1]]$match)
  if (single && n_distinct(matches) == 1) {
    vals <- filters %>% map(~ .x$levels[[1]]$values) %>% unlist() %>% unique()
    return(list(filter = new_cs_filter(vals, mode = "exclude", match = matches[1]), note = character(0)))
  }
  list(filter = new_cs_filter(grouped, mode = "exclude"),
       note = "All Others is written as the exact list of grouped entities (the groups use different kinds of patterns).")
}

# One line per group for the Component Rows.
cs_group_text <- function(dim, parts, max_shown = 3) {
  if (is.null(parts) || nrow(parts) == 0) return("")
  if (!"name" %in% names(parts)) parts$name <- ""
  parts$label <- if_else(nzchar(parts$name), str_c(parts$label, " (", parts$name, ")"), parts$label)
  shown <- pmap_chr(list(parts$label, parts$kept, parts$filter), function(lab, kept, f) {
    if (str_starts(lab, CS_ALL_OTHERS_LABEL)) return(sprintf("%s: %d entities", lab, length(kept)))
    if (cs_filter_is_exact(f) && !identical(f$mode, "exclude"))
      return(str_c(lab, ": ", str_c(head(kept, max_shown), collapse = ", "),
                   if (length(kept) > max_shown) sprintf(" +%d", length(kept) - max_shown) else ""))
    sprintf("%s: %s (%d)", lab, str_remove(describe_cs_filter(dim, f), str_c("^", dim, " ")), length(kept))
  })
  str_c("CS Groups (", dim, "): ", str_c(shown, collapse = " | "))
}

# ------------------------------------------------------------------------------
# Expansion into sisters
# ------------------------------------------------------------------------------
# Returns list(siblings, warnings, errors, tb_parts). Each sibling:
# list(components, tb (one-row tibble or NULL), cs (one-row tibble or NULL), label).
expand_split <- function(components, split, entities = character(0)) {
  warnings <- character(0)
  rows_txt <- function(idx) str_c(if (length(idx) > 1) "Rows " else "Row ", str_c(idx, collapse = ", "))

  pick_parts <- function(parts, selected, follow, what, custom_what) {
    sel <- if (is.null(selected)) parts$index else intersect(as.character(parts$index), as.character(selected))
    if (length(sel) == 0) return(list(error = sprintf("%s: select at least one to generate.", what)))
    chosen <- parts %>% filter(as.character(index) %in% sel)
    if (!any(follow)) return(list(parts = NULL, warning = sprintf("No row follows the %s: they are not applied.", what)))
    if (!all(follow)) {
      first <- chosen[1, ]
      custom <- which(!follow)
      return(list(parts = first, warning = sprintf(
        "%s %s %s: no sister VOFs. The other rows use %s.", rows_txt(custom), if (length(custom) > 1) "use" else "uses",
        custom_what, first$label)))
    }
    list(parts = chosen)
  }

  tb_list <- list(NULL); cs_list <- list(NULL); tb_parts <- NULL
  if (isTRUE(split$tb$enabled)) {
    tp <- time_break_parts(split$tb)
    if (length(tp$errors) > 0) return(list(errors = tp$errors))
    tb_parts <- tp$parts %>% mutate(label = str_c("TimeBreak ", index, " (", label, ")"))
    follow <- map_lgl(components, ~ !isFALSE(.x$follow_tb))
    p <- pick_parts(tb_parts, split$tb$selected, follow, "TimeBreaks", "custom dates")
    if (!is.null(p$error)) return(list(errors = p$error))
    warnings <- c(warnings, p$warning)
    if (!is.null(p$parts)) tb_list <- base::split(p$parts, seq_len(nrow(p$parts)))
  }
  if (isTRUE(split$cs$enabled)) {
    cp <- cs_group_parts(split$cs, entities)
    if (length(cp$errors) > 0) return(list(errors = cp$errors))
    warnings <- c(warnings, cp$notes)
    follow <- map_lgl(components, ~ !isFALSE(.x$follow_cs))
    parts <- cp$parts
    p <- pick_parts(parts, split$cs$selected, follow, "CS Groups", "custom CS filters")
    if (!is.null(p$error)) return(list(errors = p$error))
    warnings <- c(warnings, p$warning)
    if (!is.null(p$parts)) cs_list <- base::split(p$parts, seq_len(nrow(p$parts)))
  }

  siblings <- list()
  for (tb in tb_list) for (cs in cs_list) {
    comps <- map(components, function(comp) {
      if (!is.null(tb) && !isFALSE(comp$follow_tb)) { comp$start_period <- tb$start; comp$end_period <- tb$end }
      if (!is.null(cs) && !isFALSE(comp$follow_cs)) {
        comp$cs[[split$cs$dim]] <- cs$filter[[1]]
        comp$cs_labels[[split$cs$dim]] <- if (nzchar(cs$name)) cs$name else NULL
      }
      comp
    })
    label <- str_c(c(if (!is.null(tb)) tb$label, if (!is.null(cs)) cs$label), collapse = " · ")
    siblings[[length(siblings) + 1]] <- list(components = comps, tb = tb, cs = cs, label = label)
  }
  list(siblings = siblings, warnings = warnings, errors = character(0), tb_parts = tb_parts)
}

# ------------------------------------------------------------------------------
# Generation of all the sisters
# ------------------------------------------------------------------------------
# Same arguments as generate_media_vof() plus `split`. Returns the combined
# code / metadata / warnings, `vofs` (tibble Sibling, Type, Name, Configured) or
# `needs_choice` (twin choices, shared by every sister: they have the same rows).
generate_split_vofs <- function(components, cs_dims, mff_vars, long_dims = longitudinal_dims(cs_dims),
                                generate_twin = TRUE, backcasting = NULL, metric_override = NULL,
                                twin_choices = NULL, context = list(), split = NULL,
                                check_backcasting_window = TRUE) {
  cs_dim <- split$cs$dim
  entities <- if (isTRUE(split$cs$enabled) && is_valid_value(cs_dim)) cs_dim_values(context$analytical, cs_dim) else character(0)
  ex <- expand_split(components, split %||% new_split(), entities)
  if (length(ex$errors) > 0) return(list(errors = ex$errors))
  warnings <- ex$warnings

  uses_tb <- some(ex$siblings, ~ !is.null(.x$tb))
  bc_on <- !is.null(backcasting) && isTRUE(backcasting$enabled)
  if (bc_on && uses_tb) {
    first <- ex$tb_parts[1, ]
    if (check_backcasting_window && is_valid_value(backcasting$start) && is_valid_value(backcasting$end)) {
      s <- parse_dates_flexible(backcasting$start)[1]; e <- parse_dates_flexible(backcasting$end)[1]
      if ((!is.na(first$start) && s < first$start) || (!is.na(first$end) && e > first$end))
        return(list(errors = sprintf("Backcasting applies only to the oldest TimeBreak: the calibration window must be inside %s.", first$label)))
    }
    if (!some(ex$siblings, ~ !is.null(.x$tb) && .x$tb$index == 1))
      warnings <- c(warnings, "Backcasting applies only to TimeBreak 1, which is not selected: no VOF is backcasted.")
  }

  history <- context$history
  results <- list()
  for (sib in ex$siblings) {
    ctx <- context
    ctx$history <- history
    bc <- if (uses_tb && !is.null(sib$tb) && sib$tb$index != 1) NULL else backcasting
    res <- generate_media_vof(sib$components, cs_dims, mff_vars, long_dims, generate_twin, bc,
                              metric_override, twin_choices, ctx)
    if (!is.null(res$needs_choice)) return(list(needs_choice = res$needs_choice, errors = character(0)))
    prefix <- if (length(ex$siblings) > 1) str_c(sib$label, ": ") else ""
    if (length(res$errors) > 0) return(list(errors = str_c(prefix, res$errors)))
    history <- if (is.null(history)) res$metadata else upsert_metadata(history, res$metadata)
    res$label <- sib$label
    results[[length(results) + 1]] <- res
    warnings <- c(warnings, str_c(prefix, res$warnings))
  }

  vofs <- map(results, function(r) {
    tibble(Sibling = r$label, Type = c("Activity", "Spend"), Name = c(r$activity_name, r$spend_name)) %>%
      filter(!is.na(Name)) %>% mutate(Configured = Name == r$name)
  }) %>% list_rbind()
  list(vofs = vofs, n_sisters = length(results),
       code = str_c(map_chr(results, "code"), collapse = "\n"),
       metadata = map(results, "metadata") %>% list_rbind(),
       warnings = unique(keep(warnings, nzchar)), errors = character(0))
}

# ------------------------------------------------------------------------------
# Live preview (Media tab)
# ------------------------------------------------------------------------------
# Names every VOF would get with the rows as they are now, without saving.
# Rows without a variable are ignored; the activity twin uses its pre-selected
# candidate when it needs a choice (`twin_pending`). Returns list(status
# ("empty" / "incomplete" / "ok"), vofs (tibble Sibling, Type, Name,
# Configured, InHistory), n_sisters, twin_pending, messages).
preview_media_vof <- function(components, cs_dims, mff_vars, long_dims = longitudinal_dims(cs_dims),
                              generate_twin = TRUE, backcasting = NULL, metric_override = NULL,
                              context = list(), split = NULL) {
  components <- keep(components, ~ is_valid_value(.x$analytical_var))
  if (length(components) == 0) return(list(status = "empty", vofs = NULL, messages = character(0)))
  # The preview must not stop on an incomplete backcasting window.
  if (!is.null(backcasting) && isTRUE(backcasting$enabled) &&
      (!is_valid_value(backcasting$start) || !is_valid_value(backcasting$end)))
    backcasting <- list(enabled = TRUE, start = Sys.Date() - 1, end = Sys.Date())
  run <- function(choices) generate_split_vofs(components, cs_dims, mff_vars, long_dims, generate_twin, backcasting,
                                               metric_override, choices, context, split, check_backcasting_window = FALSE)
  res <- run(NULL)
  twin_pending <- FALSE
  if (!is.null(res$needs_choice)) {
    twin_pending <- TRUE
    res <- run(map(res$needs_choice, "preselected"))
  }
  if (length(res$errors) > 0) return(list(status = "incomplete", vofs = NULL, messages = res$errors))
  vofs <- res$vofs %>% mutate(InHistory = Name %in% (context$history$MainModelVariableName %||% character(0)))
  list(status = "ok", vofs = vofs, n_sisters = res$n_sisters, twin_pending = twin_pending, messages = res$warnings)
}
