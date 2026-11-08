# R/core/09_cs_children.R
# "CS Children": splitting cross-sectional values by a separator token
# ("Bogota - North" -> parts "Bogota", "North") to filter by each part.

# List with the sorted unique values of each part position.
cs_children_parts <- function(values, sep) {
  values <- clean_values(values)
  if (length(values) == 0 || is.null(sep) || !nzchar(sep)) return(list())  # " " is a valid separator
  splits <- str_split(values, fixed(sep))
  n <- min(max(lengths(splits)), MAX_CS_CHILDREN)
  map(seq_len(n), function(p) {
    splits %>% map_chr(~ if (length(.x) >= p) str_trim(.x[p]) else NA_character_) %>%
      clean_values() %>% unique() %>% sort()
  })
}

# Parent values matching the child selections (empty selection = no filter on
# that part). Returns character(0) when no child is selected at all.
cs_children_filter <- function(values, sep, selections) {
  values <- clean_values(values)
  selections <- map(selections, ~ .x %||% character(0))
  if (!some(selections, ~ length(.x) > 0)) return(character(0))
  splits <- str_split(values, fixed(sep))
  keep_value <- map_lgl(splits, function(parts) {
    every(seq_along(selections), function(p) {
      sel <- selections[[p]]
      length(sel) == 0 || (length(parts) >= p && str_trim(parts[p]) %in% sel)
    })
  })
  sort(values[keep_value])
}

# Child selections implied by a set of selected parent values (used when a row
# is restored or inherited). Re-applying them may select a superset of the
# original parent values when these were not a full combination of parts.
cs_children_from_parent <- function(selected, sep) cs_children_parts(selected, sep)
