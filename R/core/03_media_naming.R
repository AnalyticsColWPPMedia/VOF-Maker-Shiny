# R/core/03_media_naming.R
# Media components, metrics and the naming rules for media VOFs.
#
# A "component" is one Component Row of the Variable Builder: one MFF variable
# plus its labels and filters. A media VOF is the sum of its components.
#
# Name structure:
#   [Backcasted ]<VariableName and ...>[ - <Complement and ...>][ --Effect][ --p start-end][ --g flag ...]---<Metric>
#   e.g. "Display and OLV - Watches --Direct --g top5---Imps"

# Constructor that normalises every field, so the rest of the code never has to
# deal with NULLs coming from Shiny inputs. `cs` is a named list (by dimension)
# of filters (see 10_cs_filters.R) or plain vectors of entities.
new_component <- function(analytical_var = "", variable_name = "", complement = "",
                          effect = "", start_period = NA, end_period = NA,
                          cs = list(), cs_children = list(), follow_tb = TRUE, follow_cs = TRUE,
                          cs_labels = list()) {
  cs <- cs %||% list()
  cs <- cs[intersect(names(cs), MFF_DIMS)] %>% map(as_cs_filter) %>% compact()
  as_date_or_na <- function(d) if (is_valid_value(d)) parse_dates_flexible(d)[1] else NA

  list(
    analytical_var = str_trim(convert_to_char(analytical_var)),
    # Labels go into the VOF name: no leading, trailing or repeated spaces.
    variable_name  = str_squish(convert_to_char(variable_name)),
    complement     = str_squish(convert_to_char(complement)),
    effect         = str_squish(convert_to_char(effect)),
    start_period   = as_date_or_na(start_period),
    end_period     = as_date_or_na(end_period),
    cs             = cs,
    cs_children    = cs_children %||% list(),
    # Whether the row follows the TimeBreaks / CS Groups of the Media tab
    # (FALSE when the analyst gives it its own dates / filter).
    follow_tb      = !isFALSE(follow_tb),
    follow_cs      = !isFALSE(follow_cs),
    # Custom CS flag by dimension ("MyGroup" -> "--g MyGroup"); empty = automatic.
    cs_labels      = cs_labels %||% list() %>% map(~ str_squish(convert_to_char(.x))) %>% keep(nzchar)
  )
}

# Older VOFs stored "Backcasted" at the end of the media channel.
strip_backcasted_label <- function(x) str_trim(str_remove(x, regex(str_c("\\s*\\b", BACKCASTED_LABEL, "$"), ignore_case = TRUE)))

# Name without the metric suffix ("X --g top5 ---Imps" or "X --g top5---Imps" -> "X --g top5").
vof_base_name <- function(name) str_remove(name, str_c(METRIC_SEP_REGEX, "[^-]+$"))

# ------------------------------------------------------------------------------
# Metrics
# ------------------------------------------------------------------------------
metric_terms <- function() {
  METRICS %>%
    mutate(term = map2(Metric, Aliases, ~ c(.x, if (nzchar(.y)) str_split(.y, fixed("|"))[[1]]))) %>%
    unnest(term)
}

word_regex <- function(term) str_c("(?i)(?<![A-Za-z0-9])", str_replace_all(term, "([.()\\[\\]+*?^$|{}])", "\\\\\\1"), "(?![A-Za-z0-9])")

# Metric of an MFF variable, read from its VariableName part. The longest term
# found wins ("Net Cost" over "Cost", "Completed Views" over "Views").
# Returns a one-row tibble (Metric, Type, Suffix, term) or NULL.
detect_metric <- function(analytical_var, long_dims) {
  hit <- detect_metrics(analytical_var, long_dims)
  if (is.na(hit$term[1])) NULL else hit[1, c("Metric", "Type", "Suffix", "term")]
}

# Vectorised version: one row per variable (NA columns when no metric found).
# Terms are tried longest first, each one over all names at once.
detect_metrics <- function(vars, long_dims) {
  parsed <- parse_mff_names(vars, long_dims)
  base <- if_else(parsed$is_mff, parsed$base, vars)
  terms <- metric_terms() %>% arrange(desc(nchar(term)))
  idx <- rep(NA_integer_, length(vars))
  for (i in seq_len(nrow(terms))) {
    todo <- is.na(idx)
    if (!any(todo)) break
    idx[todo][str_detect(base[todo], word_regex(terms$term[i]))] <- i
  }
  terms[idx, c("Metric", "Type", "Suffix", "term")]
}

metric_info <- function(metric) {
  row <- METRICS %>% filter(str_to_lower(Metric) == str_to_lower(metric))
  if (nrow(row) == 0) NULL else row[1, c("Metric", "Type", "Suffix")]
}

# Metric of the whole VOF from its components (all must agree on the suffix).
# `override` (a METRICS$Metric value) fills components whose metric cannot be
# detected. Returns list(metric, type, suffix, errors).
resolve_vof_metric <- function(components, long_dims, override = NULL) {
  detected <- map(components, ~ detect_metric(.x$analytical_var, long_dims))
  ov <- if (is_valid_value(override)) metric_info(override) else NULL
  errors <- character(0)
  detected <- imap(detected, function(m, i) {
    if (!is.null(m)) return(m)
    if (!is.null(ov)) return(ov)
    errors <<- c(errors, sprintf("Row %d: the metric of '%s' could not be detected. Choose it in VOF Settings.",
                                 i, components[[i]]$analytical_var))
    NULL
  })
  if (length(errors) > 0) return(list(errors = errors))
  suffixes <- unique(map_chr(detected, "Suffix"))
  if (length(suffixes) > 1)
    return(list(errors = sprintf("Components mix metrics (%s). A VOF must sum a single metric.",
                                 str_c(unique(map_chr(detected, "Metric")), collapse = ", "))))
  first <- detected[[1]]
  list(metric = if (first$Type == "Spend") "Spend" else first$Metric, type = first$Type,
       suffix = first$Suffix, errors = character(0))
}

# The metric must only appear as the name suffix: it cannot be part of the
# VariableName or Complement typed by the user.
validate_metric_not_in_labels <- function(components, metric) {
  terms <- metric_terms()
  own <- if (metric$type == "Spend") terms %>% filter(Type == "Spend") else terms %>% filter(Suffix == metric$suffix)
  words <- unique(c(own$term, own$Suffix))
  imap(components, function(comp, i) {
    map(c(VariableName = comp$variable_name, Complement = comp$complement), function(label) {
      found <- keep(words, ~ str_detect(label, word_regex(.x)))
      if (length(found) > 0) found[1] else NULL
    }) %>%
      compact() %>%
      imap_chr(~ sprintf("Row %d: '%s' appears in the %s; the metric is added as the name suffix ( ---%s), remove it from the %s.",
                         i, .x, .y, metric$suffix, .y))
  }) %>% unlist() %>% as.character()
}

# Metric type of each MFF variable ("Activity", "Spend" or NA when no metric of
# the dictionary is found), for the Activity / Spend filter of the builder.
mff_metric_types <- function(vars, long_dims) detect_metrics(vars, long_dims)$Type

# ------------------------------------------------------------------------------
# Suggested VariableName
# ------------------------------------------------------------------------------
# Root of an MFF variable: its VariableName part without the metric
# ("Social Video Impressions_Watches_Total" -> "Social Video").
mff_root_name <- function(analytical_var, long_dims) {
  if (!is_valid_value(analytical_var)) return("")
  parsed <- parse_mff_names(analytical_var, long_dims)
  base <- if (isTRUE(parsed$is_mff[1])) parsed$base[1] else analytical_var
  m <- detect_metric(analytical_var, long_dims)
  root <- if (is.null(m)) base else str_remove(base, word_regex(m$term))
  root <- str_squish(root)
  if (root == "") str_squish(base) else root
}

# Writes a pre-filled VariableName following the rules of VOF names:
# 1. usual channel abbreviations (CHANNEL_ABBREVIATIONS), any separator
#    between words: "ONLINE VIDEO-CTV" -> "CTV", "P-MAX" -> "Pmax";
# 2. special characters (- _ ( ) , . / & + ...) become spaces; apostrophes
#    are dropped ("Macy's" -> "Macys");
# 3. casing: acronyms in capitals (NAME_ACRONYMS), brand casing
#    (NAME_SPECIAL_CASING), words with digits in capitals (GA4), and/of/the/...
#    in lower case unless first, every other word with only the first letter
#    in capitals. "DIRECT TELEVISION" -> "Direct TV".
format_variable_name <- function(name) {
  name <- str_squish(convert_to_char(name))
  if (name == "") return("")
  abbr <- CHANNEL_ABBREVIATIONS %>%
    separate_rows(Phrase, sep = "\\|") %>%
    mutate(Phrase = str_squish(Phrase)) %>%
    arrange(desc(nchar(Phrase)))
  for (i in seq_len(nrow(abbr))) {
    words <- str_split(abbr$Phrase[i], " ")[[1]] %>% map_chr(~ str_replace_all(.x, "([.+*?^$(){}\\[\\]\\\\])", "\\\\\\1"))
    pattern <- str_c("(?<![[:alnum:]])", str_c(words, collapse = "[^[:alnum:]]+"), "(?![[:alnum:]])")
    name <- str_replace_all(name, regex(pattern, ignore_case = TRUE), abbr$Abbreviation[i])
  }
  name <- name %>%
    str_remove_all("['\u2019`]") %>%
    str_replace_all("[^[:alnum:] ]", " ") %>%
    str_squish()
  if (name == "") return("")
  words <- str_split(name, " ")[[1]]
  format_word <- function(w, first) {
    lw <- str_to_lower(w)
    if (str_to_upper(w) %in% NAME_ACRONYMS) return(str_to_upper(w))
    if (lw %in% names(NAME_SPECIAL_CASING)) return(NAME_SPECIAL_CASING[[lw]])
    if (str_detect(w, "[0-9]") && str_detect(w, "[[:alpha:]]")) return(str_to_upper(w))
    if (!first && lw %in% TITLE_CASE_SMALL_WORDS) return(lw)
    str_c(str_to_upper(str_sub(w, 1, 1)), str_to_lower(str_sub(w, 2)))
  }
  map2_chr(words, seq_along(words) == 1, format_word) %>% str_c(collapse = " ")
}

# Pre-filled VariableName of a selected variable: formatted root.
suggested_root_name <- function(analytical_var, long_dims) format_variable_name(mff_root_name(analytical_var, long_dims))

# Words shared at the start of every name ("Social Video", "Social Display" -> "Social").
common_word_prefix <- function(x) {
  words <- str_split(x, " ")
  n <- min(lengths(words))
  k <- 0
  while (k < n && n_distinct(map_chr(words, k + 1)) == 1) k <- k + 1
  if (k == 0) "" else str_c(words[[1]][seq_len(k)], collapse = " ")
}

# VariableName suggested for every Component Row from the roots of the rows:
# - one root (or all equal): the root itself;
# - different roots that start with the same words: "Total <shared words>"
#   (Social Video + Social Display -> "Total Social");
# - otherwise each row keeps its own root (the name joins them with "and").
# Rows without a variable get "".
suggest_variable_names <- function(roots) {
  roots <- str_squish(replace_na(as.character(roots), ""))
  used <- unique(roots[nzchar(roots)])
  if (length(used) < 2) return(roots)
  prefix <- common_word_prefix(used)
  if (prefix == "") return(roots)
  total <- if (str_detect(prefix, regex("^Total\\b", ignore_case = TRUE))) prefix else str_c("Total ", prefix)
  if_else(nzchar(roots), total, "")
}

# ------------------------------------------------------------------------------
# Name parts
# ------------------------------------------------------------------------------
effect_values <- function(components) components %>% map_chr("effect") %>% keep(nzchar) %>% unique()

period_values <- function(components) {
  list(starts = map_chr(components, ~ format_date_iso(.x$start_period)),
       ends   = map_chr(components, ~ format_date_iso(.x$end_period)))
}

# Shared filter of a dimension across components (NULL when they differ or
# when some component does not filter that dimension).
common_cs_filter <- function(components, dim) {
  filters <- map(components, ~ .x$cs[[dim]])
  if (some(filters, is.null)) return(NULL)
  keys <- map_chr(filters, cs_filter_key)
  if (n_distinct(keys) == 1) filters[[1]] else NULL
}

active_cs_dims <- function(components) keep(MFF_DIMS, function(d) some(components, ~ !is.null(.x$cs[[d]])))

# VOF name without the metric suffix. `cs_labels` is a named list (dimension ->
# flag label) for dimensions with a shared filter.
build_media_vof_base_name <- function(components, cs_labels = list(), backcasted = FALSE) {
  names_part <- components %>% map_chr("variable_name") %>% unique() %>% str_c(collapse = NAME_AND)
  complements <- components %>% map_chr("complement") %>% keep(nzchar) %>% unique()
  base <- if (length(complements) > 0) str_c(names_part, NAME_COMPLEMENT_SEP, str_c(complements, collapse = NAME_AND)) else names_part
  if (backcasted) base <- str_c(BACKCASTED_LABEL, " ", base)

  effects <- effect_values(components)
  effect_tag <- if (length(effects) > 1) "--Both" else if (length(effects) == 1) str_c("--", effects) else ""

  p <- period_values(components)
  date_tag <- ""
  if (any(p$starts != "") || any(p$ends != "")) {
    date_tag <- if (n_distinct(p$starts) == 1 && n_distinct(p$ends) == 1)
      str_c("--p ", format_date_tag(p$starts[1]), "-", format_date_tag(p$ends[1])) else "--p"
  }

  cs_tags <- active_cs_dims(components) %>%
    map_chr(function(d) {
      lab <- cs_labels[[d]]
      if (is_valid_value(lab)) str_c(CS_NAME_TAGS[[d]], " ", lab) else CS_NAME_TAGS[[d]]
    })

  c(base, effect_tag, date_tag, cs_tags) %>% keep(nzchar) %>% str_c(collapse = " ") %>% str_squish()
}

vof_full_name <- function(base_name, suffix) str_c(str_squish(base_name), METRIC_SEP, str_squish(suffix))

# Human-readable title used as the "#### title ####" section header.
build_media_vof_title <- function(components, metric = NULL, backcasted = FALSE) {
  m_desc <- components %>% map_chr("variable_name") %>% keep(nzchar) %>% unique() %>% str_c(collapse = " and ")
  if (backcasted) m_desc <- str_c(BACKCASTED_LABEL, " ", m_desc)
  if (!is.null(metric)) m_desc <- str_c(m_desc, " (", metric, ")")

  effects <- effect_values(components)
  e_desc <- if (length(effects) > 0) str_c("adjusted by ", if (length(effects) > 1) "Both" else effects) else ""

  p <- period_values(components)
  p_desc <- ""
  if (any(p$starts != "") || any(p$ends != "")) {
    if (n_distinct(p$starts) == 1 && n_distinct(p$ends) == 1) {
      s <- p$starts[1]; e <- p$ends[1]
      p_desc <- if (s != "" && e != "") str_c("filtered from ", s, " to ", e)
                else if (s != "") str_c("filtered from ", s) else str_c("filtered until ", e)
    } else p_desc <- "filtered by different periods"
  }
  dims <- active_cs_dims(components)
  cs_desc <- if (length(dims) > 0) str_c("and by ", str_c(str_to_lower(dims), collapse = " and ")) else ""

  base_t <- str_c(keep(c(m_desc, e_desc), nzchar), collapse = ", ")
  filt_t <- str_c(keep(c(p_desc, cs_desc), nzchar), collapse = " ")
  (if (filt_t != "") str_c(base_t, ", ", filt_t) else base_t) %>% str_squish()
}
