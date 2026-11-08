# R/core/10_cs_filters.R
# Cross-sectional filters of media components.
#
# A filter (one per CS dimension) is:
#   list(mode = "include" | "exclude",
#        levels = list(list(match = "Exact" | "StartsWith" | "Contains" | "EndsWith",
#                           values = c(...)), ...))
# * Exact filters have one level with the selected entities.
# * CS Children filters have one level per child position with positional
#   patterns that keep the separator: child A -> StartsWith "Boston - ",
#   middle child -> Contains " - North - ", last child -> EndsWith " - Existing".
# Levels are combined with AND, values inside a level with OR, and "exclude"
# negates the whole condition. Matching is case-insensitive (as in AWB code).

new_cs_filter <- function(values = character(0), mode = "include", match = "Exact") {
  list(mode = mode, levels = list(list(match = match, values = clean_values(values))))
}

# Accepts a filter or a plain vector of entities (exact include).
as_cs_filter <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.list(x) && !is.null(x$levels)) {
    x$levels <- x$levels %>%
      map(~ list(match = .x$match %||% "Exact", values = clean_values(.x$values))) %>%
      keep(~ length(.x$values) > 0)
    x$mode <- x$mode %||% "include"
    return(if (length(x$levels) == 0) NULL else x)
  }
  vals <- clean_values(x)
  if (length(vals) == 0) NULL else new_cs_filter(vals)
}

cs_filter_is_exact <- function(f) all(map_chr(f$levels, "match") == "Exact")

# Comparable representation (used to detect "the same filter" across rows).
cs_filter_key <- function(f) {
  if (is.null(f)) return("")
  levels <- map_chr(f$levels, ~ str_c(.x$match, ":", str_c(sort(str_to_lower(.x$values)), collapse = "\u001f")))
  str_c(f$mode, "|", str_c(levels, collapse = "\u001e"))
}

# ------------------------------------------------------------------------------
# Generated code (base R, R 3.6 compatible)
# ------------------------------------------------------------------------------
cs_level_expr <- function(dim, level) {
  col <- str_c("tolower(Data$", dim, ")")
  conds <- switch(level$match,
    Exact      = return(sprintf("%s %%in%% tolower(%s)", col, r_str_vec(level$values))),
    StartsWith = sprintf("startsWith(%s, tolower(%s))", col, r_str(level$values)),
    EndsWith   = sprintf("endsWith(%s, tolower(%s))", col, r_str(level$values)),
    Contains   = sprintf("grepl(tolower(%s), %s, fixed = TRUE)", r_str(level$values), col),
    stop("Unknown CS match type: ", level$match))
  if (length(conds) > 1) str_c("(", str_c(conds, collapse = " | "), ")") else conds
}

cs_filter_expr <- function(dim, f) {
  conds <- map_chr(f$levels, ~ cs_level_expr(dim, .x))
  expr <- str_c(conds, collapse = " & ")
  if (identical(f$mode, "exclude")) str_c("!(", expr, ")") else expr
}

# ------------------------------------------------------------------------------
# Resolution: which entities does a filter keep?
# ------------------------------------------------------------------------------
cs_level_matches <- function(level, entities) {
  e <- str_to_lower(entities)
  v <- str_to_lower(level$values)
  hits <- switch(level$match,
    Exact      = list(e %in% v),
    StartsWith = map(v, ~ startsWith(e, .x)),
    EndsWith   = map(v, ~ endsWith(e, .x)),
    Contains   = map(v, ~ str_detect(e, fixed(.x))))
  reduce(hits, `|`)
}

cs_filter_resolve <- function(f, entities) {
  entities <- clean_values(entities)
  if (is.null(f)) return(entities)
  keep_it <- f$levels %>% map(~ cs_level_matches(.x, entities)) %>% reduce(`&`)
  if (identical(f$mode, "exclude")) keep_it <- !keep_it
  sort(entities[keep_it])
}

# ------------------------------------------------------------------------------
# CS Children -> positional filter
# ------------------------------------------------------------------------------
# selections: list (one element per child position) of selected child values.
# Each selected value becomes a pattern that keeps the separator on the side(s)
# where the entity continues. If the positional patterns cannot reproduce the
# selection exactly (e.g. a value appears both as a middle and as a last part),
# the filter falls back to the exact list of entities.
cs_children_to_filter <- function(entities, sep, selections, mode = "include") {
  entities <- clean_values(entities)
  selections <- map(selections, ~ .x %||% character(0))
  target <- cs_children_filter(entities, sep, selections)
  if (length(target) == 0) return(list(filter = NULL, fallback = FALSE))

  splits <- str_split(entities, fixed(sep)) %>% map(str_trim)
  build_level <- function(p) {
    vals <- selections[[p]]
    if (length(vals) == 0) return(NULL)
    if (p == 1) return(list(match = "StartsWith", values = str_c(vals, sep)))
    n_parts <- map(vals, function(v) unique(map_int(keep(splits, ~ length(.x) >= p && .x[p] == v), length)))
    if (all(map_lgl(n_parts, ~ length(.x) > 0 && all(.x == p))))
      return(list(match = "EndsWith", values = str_c(sep, vals)))
    if (all(map_lgl(n_parts, ~ length(.x) > 0 && all(.x > p))))
      return(list(match = "Contains", values = str_c(sep, vals, sep)))
    NA  # mixed positions: not representable
  }
  levels <- map(seq_along(selections), build_level) %>% compact()
  positional_ok <- length(levels) > 0 && !some(levels, ~ identical(.x, NA))
  if (positional_ok) {
    f <- list(mode = "include", levels = levels)
    positional_ok <- identical(cs_filter_resolve(f, entities), sort(target))
  }
  if (!positional_ok) f <- new_cs_filter(target)
  if (identical(mode, "exclude")) f$mode <- "exclude"
  list(filter = f, fallback = !positional_ok)
}

# Child values behind positional patterns ("Boston - " -> "Boston").
cs_pattern_core <- function(values, sep) {
  if (is.null(sep) || !nzchar(sep)) return(str_trim(values))  # " " is a valid separator
  n <- nchar(sep)
  values <- if_else(startsWith(values, sep), str_sub(values, n + 1), values)
  values <- if_else(endsWith(values, sep), str_sub(values, 1, -n - 1), values)
  str_trim(values)
}

# ------------------------------------------------------------------------------
# Metadata encoding
# ------------------------------------------------------------------------------
# <Dim>: exact values joined by ", " (as before); positional patterns quoted
#        (so leading/trailing separators are visible), levels joined by "; ".
# <Dim>Filter: "<Mode>|<Match>[;<Match>...]", e.g. "Exclude|StartsWith;EndsWith".
cs_filter_to_meta <- function(f) {
  if (is.null(f)) return(list(values = "", filter = ""))
  values <- f$levels %>%
    map_chr(function(l) if (l$match == "Exact") str_c(l$values, collapse = ", ")
                        else str_c(r_str(l$values), collapse = ", ")) %>%
    str_c(collapse = "; ")
  filter <- str_c(names(CS_FILTER_MODES)[CS_FILTER_MODES == f$mode], "|",
                  str_c(map_chr(f$levels, "match"), collapse = ";"))
  list(values = values, filter = filter)
}

cs_filter_from_meta <- function(values, filter) {
  if (!is_valid_value(values)) return(NULL)
  filter <- if (is_valid_value(filter)) filter else "Include|Exact"
  parts <- str_split_fixed(filter, fixed("|"), 2)
  mode <- if (str_to_lower(parts[1]) == "exclude") "exclude" else "include"
  matches <- str_split(parts[2], fixed(";"))[[1]] %>% str_trim()
  raw_levels <- if (length(matches) > 1) str_split(values, fixed("; "))[[1]] else values
  levels <- map2(matches, raw_levels, function(m, v) {
    vals <- if (m == "Exact") str_split(v, fixed(", "))[[1]]
            else str_match_all(v, "'((?:[^'\\\\]|\\\\.)*)'")[[1]][, 2] %>% str_replace_all("\\\\(.)", "\\1")
    list(match = m, values = vals)
  })
  as_cs_filter(list(mode = mode, levels = levels))
}

# ------------------------------------------------------------------------------
# Name flags
# ------------------------------------------------------------------------------
# Registry of the flags already used in the project: one row per (dim, label)
# with the set of entities it stands for. Built from the history metadata.
cs_flag_registry <- function(metadata, cs_dims, analytical) {
  empty <- tibble(dim = character(0), label = character(0), key = character(0))
  if (is.null(metadata) || nrow(metadata) == 0 || is.null(analytical)) return(empty)
  map(cs_dims, function(d) {
    cols <- str_c(d, METADATA_DIM_SUFFIXES)
    if (!all(cols %in% names(metadata))) return(empty)
    entities <- cs_dim_values(analytical, d)
    metadata %>%
      select(values = all_of(cols[1]), filter = all_of(cols[2]), label = all_of(cols[3])) %>%
      filter(label != "") %>%
      distinct() %>%
      mutate(dim = d,
             key = map2_chr(values, filter, ~ cs_set_key(cs_filter_resolve(cs_filter_from_meta(.x, .y), entities)))) %>%
      select(dim, label, key)
  }) %>% list_rbind() %>% distinct()
}

cs_set_key <- function(entities) str_c(sort(str_to_lower(entities)), collapse = "\u001f")

next_group_label <- function(registry, dim) {
  used <- registry %>% filter(dim == !!dim, str_detect(label, str_c("^", CS_FLAG_GROUP_PREFIX, "\\d+$"))) %>% pull(label)
  n <- if (length(used) == 0) 0 else max(as.integer(str_remove(used, CS_FLAG_GROUP_PREFIX)))
  str_c(CS_FLAG_GROUP_PREFIX, n + 1)
}

# Label shown after the dimension tag ("--g <label>").
# 1. A set already registered in the project reuses its label.
# 2. CS Children patterns: the child values ("uis-ews"), "not ..." if excluded.
# 3. Exact sets equal to the N largest entities of the roots: "topN".
# 4. Otherwise "<first value> etc" ("not ..." if excluded); if that label is
#    already used for a different set, the next "groupK" is assigned instead.
# Returns list(label, note) where note explains automatic group assignments.
cs_flag_label <- function(dim, f, entities, root_totals = NULL, registry = NULL, sep = NULL) {
  registry <- registry %||% tibble(dim = character(0), label = character(0), key = character(0))
  resolved <- cs_filter_resolve(f, entities)
  key <- cs_set_key(resolved)
  # A pattern filter (CS Children: starts with / contains / ends with) is always
  # named by its pattern ("uis-ews", "not uis"), even if the same entities were
  # named otherwise before. Exact lists reuse the flag of the same set.
  same_set <- registry %>% filter(dim == !!dim, key == !!key)
  if (cs_filter_is_exact(f) && nrow(same_set) > 0) return(list(label = same_set$label[1], note = ""))

  neg <- if (identical(f$mode, "exclude")) "not " else ""
  if (!cs_filter_is_exact(f)) {
    core <- f$levels %>% map(~ cs_pattern_core(.x$values, sep)) %>% unlist() %>% str_c(collapse = "-")
    label <- str_c(neg, core)
  } else {
    label <- NULL
    if (!is.null(root_totals) && nrow(root_totals) > 0 && length(resolved) < nrow(root_totals)) {
      top <- root_totals %>% arrange(desc(total)) %>% slice_head(n = length(resolved)) %>% pull(entity)
      if (cs_set_key(top) == key) label <- str_c(CS_FLAG_TOP_PREFIX, length(resolved))
    }
    if (is.null(label)) {
      shown <- sort(f$levels[[1]]$values)
      label <- str_c(neg, shown[1], if (length(shown) > 1) " etc" else "")
    }
  }
  clash <- registry %>% filter(dim == !!dim, label == !!label, key != !!key)
  if (nrow(clash) > 0) {
    new_label <- next_group_label(registry, dim)
    return(list(label = new_label,
                note = sprintf("'%s %s' already names a different set of %s values; this filter was named '%s'.",
                               CS_NAME_TAGS[[dim]], label, dim, new_label)))
  }
  list(label = label, note = "")
}

# Total of the root variables by entity (ranking for "topN").
cs_root_totals <- function(analytical, dim, roots) {
  roots <- intersect(roots, names(analytical))
  if (is.null(analytical) || !dim %in% names(analytical) || length(roots) == 0) return(NULL)
  analytical %>%
    transmute(entity = .data[[dim]], total = rowSums(across(all_of(roots)), na.rm = TRUE)) %>%
    group_by(entity) %>%
    summarise(total = sum(total), .groups = "drop")
}

# ------------------------------------------------------------------------------
# UI helpers
# ------------------------------------------------------------------------------
# Child selections that reproduce a positional filter (to show it in the CS
# Children pickers). Returns NULL if the filter cannot be shown that way.
cs_filter_to_children <- function(f, entities, sep) {
  if (is.null(f) || cs_filter_is_exact(f) || is.null(sep) || !nzchar(sep)) return(NULL)
  splits <- str_split(clean_values(entities), fixed(sep)) %>% map(str_trim)
  out <- list()
  for (l in f$levels) {
    cores <- cs_pattern_core(l$values, sep)
    pos <- map_int(cores, function(v) {
      p <- switch(l$match,
        StartsWith = 1L,
        EndsWith = splits %>% keep(~ length(.x) > 1 && .x[length(.x)] == v) %>% map_int(length) %>% unique(),
        Contains = splits %>% map(~ which(.x == v)) %>% unlist() %>% unique() %>% keep(~ .x > 1))
      if (length(p) == 1) p else NA_integer_
    })
    if (any(is.na(pos)) || n_distinct(pos) != 1) return(NULL)
    p <- pos[1]
    # The list may be shorter than p (e.g. only child B is selected).
    previous <- if (length(out) >= p) out[[p]] else NULL
    out[[p]] <- unique(c(previous, cores))
  }
  out
}

# "Write the shortest list": an exact include filter keeping more than half of
# the entities is written as an exclusion of the rest (and vice versa).
shortest_exact_filter <- function(f, entities) {
  if (is.null(f) || !cs_filter_is_exact(f)) return(f)
  kept <- cs_filter_resolve(f, entities)
  dropped <- setdiff(clean_values(entities), kept)
  if (length(kept) == 0) return(f)
  if (length(dropped) == 0) return(NULL)  # every entity: no filter needed
  if (length(dropped) < length(kept)) new_cs_filter(dropped, "exclude") else new_cs_filter(kept, "include")
}

# ------------------------------------------------------------------------------
# Custom CS flags ("--g MyGroup")
# ------------------------------------------------------------------------------
# Cleans a custom flag typed by the analyst: special characters become spaces
# (they could break the VOF name: "---" separates the metric, " --" the tags),
# extra spaces are removed and the case is kept. The automatic forms (topN,
# groupN, "not ...") are reserved. Returns list(name, error).
clean_cs_label <- function(x) {
  x <- convert_to_char(x %||% "")
  name <- x %>% str_remove_all("['\u2019`]") %>% str_replace_all("[^[:alnum:] ]", " ") %>% str_squish()
  error <- if (name == "") "" else if (str_detect(name, regex(str_c("^(", CS_FLAG_TOP_PREFIX, "|group)\\d+$"), ignore_case = TRUE)))
    sprintf("'%s' is reserved for the automatic flags (topN / groupN).", name)
  else if (str_detect(name, regex("^not\\b", ignore_case = TRUE)))
    sprintf("'%s': names starting with 'not' are reserved for exclusions.", name)
  else ""
  list(name = name, error = error)
}

# Flag of a filter with a custom name: blocked when that name already names a
# different set of entities in the metadata. Returns list(label, note, error).
cs_custom_flag <- function(dim, f, entities, custom, registry = NULL) {
  cl <- clean_cs_label(custom)
  if (nzchar(cl$error)) return(list(label = cl$name, note = "", error = str_c(dim, " group name: ", cl$error)))
  registry <- registry %||% tibble(dim = character(0), label = character(0), key = character(0))
  key <- cs_set_key(cs_filter_resolve(f, entities))
  clash <- registry %>% filter(dim == !!dim, str_to_lower(label) == str_to_lower(cl$name), key != !!key)
  if (nrow(clash) > 0)
    return(list(label = cl$name, note = "", error = sprintf("'%s %s' already names a different set of %s entities in the metadata. Choose another name.",
                                                           CS_NAME_TAGS[[dim]], cl$name, dim)))
  other <- registry %>% filter(dim == !!dim, key == !!key, str_to_lower(label) != str_to_lower(cl$name))
  note <- if (nrow(other) > 0) sprintf("These %s entities are also named '%s' in other VOFs.", dim, str_c(unique(other$label), collapse = "', '")) else ""
  list(label = cl$name, note = note, error = "")
}

# Is a flag read from a VOF name custom (typed by an analyst) or automatic?
# Automatic: topN, groupN, or what the rules give for that filter.
is_custom_cs_label <- function(label, dim, f, entities, root_totals = NULL, sep = NULL) {
  if (!is_valid_value(label)) return(FALSE)
  if (str_detect(label, str_c("^(", CS_FLAG_TOP_PREFIX, "|group)\\d+$"))) return(FALSE)
  auto <- cs_flag_label(dim, f, entities, root_totals, NULL, sep)$label
  !identical(str_to_lower(auto), str_to_lower(label))
}

# Flags of every CS dimension written in a VOF name ("--g MyGroup").
name_cs_labels <- function(name, cs_dims) {
  cs_dims %>% set_names() %>%
    map(~ str_match(name, str_c(CS_NAME_TAGS[[.x]], " (.+?)(?= --|\\s*---|$)"))[1, 2] %|NA|% "")
}
