# R/core/07_metadata.R
# VOF Metadata: schema normalisation, history updates, old-file import,
# purification against Model Details and final export.
#
# Every metadata column is stored as character ("" for missing), dates as ISO
# strings. Keeping a single type everywhere avoids bind errors such as
# Date vs character after restoring the browser history.

metadata_dim_columns <- function(cs_dims) as.vector(outer(METADATA_DIM_SUFFIXES, cs_dims, function(s, d) str_c(d, s)))

# Exported columns (the internal code column is added by normalize_metadata).
metadata_columns <- function(cs_dims = character(0)) c(METADATA_STATIC_COLS, metadata_dim_columns(cs_dims), METADATA_END_COLS)

empty_metadata <- function(cs_dims = character(0)) {
  c(metadata_columns(cs_dims), METADATA_CODE_COL) %>%
    set_names() %>%
    map(~ character(0)) %>%
    as_tibble()
}

# Brings older metadata files to the current schema:
#  * renamed columns (MediaChannel, SubChannel, Backcasting_*, <Dim>_Filter,
#    <Dim>_Group, plural dimension names);
#  * Metric: "MetricName" values, or the metric detected from the root for old
#    "Activity" rows;
#  * InitCode/FeedCode: dropped; the FeedCode of non-media rows is kept as the
#    internal code column (it cannot be rebuilt from the metadata).
migrate_metadata <- function(df) {
  dim_renames <- unlist(map(c(MFF_DIMS, names(METADATA_LEGACY_DIM_COLS)), function(d) {
    base <- METADATA_LEGACY_DIM_COLS[d] %|NA|% d
    set_names(str_c(base, METADATA_LEGACY_DIM_SUFFIXES), str_c(d, names(METADATA_LEGACY_DIM_SUFFIXES)))
  }))
  renames <- c(METADATA_LEGACY_DIM_COLS, METADATA_LEGACY_COLS, dim_renames)
  legacy <- renames[names(renames) %in% names(df) & !renames %in% names(df)]
  df <- rename(df, !!!set_names(names(legacy), unname(legacy)))

  if ("MetricName" %in% names(df)) {
    df <- df %>% mutate(Metric = if_else(MetricName != "", MetricName, Metric)) %>% select(-MetricName)
  }
  if (all(c("Metric", "AnalyticalVariableName") %in% names(df))) {
    df <- df %>% mutate(Metric = map2_chr(Metric, AnalyticalVariableName, function(m, v) {
      if (m != "Activity") return(m)
      d <- detect_metric(v, character(0))
      if (is.null(d)) m else if (d$Type == "Spend") "Spend" else d$Metric
    }))
  }
  if ("FeedCode" %in% names(df)) {
    # Files without a Type column only had media VOFs.
    is_media <- if ("Type" %in% names(df)) df$Type %in% c("", METADATA_TYPES[["media"]]) else rep(TRUE, nrow(df))
    if (!METADATA_CODE_COL %in% names(df)) df[[METADATA_CODE_COL]] <- ""
    init <- if ("InitCode" %in% names(df)) df$InitCode else ""
    old_code <- str_trim(str_c(init, "\n", df$FeedCode))
    df[[METADATA_CODE_COL]] <- if_else(!is_media & df[[METADATA_CODE_COL]] == "", old_code, df[[METADATA_CODE_COL]])
  }
  select(df, -any_of(c("InitCode", "FeedCode")))
}

# Normalises any metadata-like data.frame to the app's schema (all character,
# ISO dates, current column names and order, internal code column last).
normalize_metadata <- function(df, cs_dims = NULL) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(empty_metadata(cs_dims %||% character(0)))
  df <- df %>%
    as_tibble(.name_repair = "minimal") %>%
    mutate(across(where(~ inherits(.x, c("Date", "POSIXt"))), ~ format(as_date(.x), "%Y-%m-%d")),
           across(everything(), ~ replace_na(as.character(.x), ""))) %>%
    migrate_metadata()
  dims <- cs_dims %||% intersect(MFF_DIMS, names(df))
  for (col in setdiff(c(metadata_columns(dims), METADATA_CODE_COL), names(df))) df[[col]] <- ""
  # Older metadata had no filter type: values meant "include, exact match".
  for (d in dims) {
    fcol <- str_c(d, "Filter")
    df[[fcol]] <- if_else(df[[d]] != "" & df[[fcol]] == "", "Include|Exact", df[[fcol]])
  }

  df %>%
    mutate(across(all_of(METADATA_DATE_COLS), function(v) {
             parsed <- parse_dates_flexible(v)
             if_else(v != "" & !is.na(parsed), format(parsed, "%Y-%m-%d"), v)
           }),
           Type = if_else(Type == "", METADATA_TYPES[["media"]], Type)) %>%
    select(all_of(c(metadata_columns(dims), METADATA_CODE_COL)))
}

# Media component rebuilt from a metadata row (the metadata alone is enough to
# regenerate the code, as downstream processes will do).
component_from_metadata_row <- function(row, cs_dims) {
  cs <- cs_dims %>% set_names() %>% map(~ cs_filter_from_meta(row[[.x]], row[[str_c(.x, "Filter")]])) %>% compact()
  new_component(analytical_var = row$AnalyticalVariableName, variable_name = row$VofVariableName,
                complement = row$VofComplement, effect = row$Effect,
                start_period = row$MinPeriod, end_period = row$MaxPeriod, cs = cs)
}

# Adds new rows to the history. Rows of a VOF that is generated again replace
# the previous ones (matched by MainModelVariableName), so re-generating never
# duplicates a variable.
upsert_metadata <- function(history, new_rows) {
  new_rows <- normalize_metadata(new_rows)
  history <- normalize_metadata(history)
  if (nrow(new_rows) == 0) return(history)
  history %>%
    filter(!MainModelVariableName %in% new_rows$MainModelVariableName) %>%
    bind_rows(new_rows) %>%
    normalize_metadata()
}

# Old metadata only adds the VOFs that are not in the history yet.
add_missing_metadata <- function(history, old_rows) {
  history <- normalize_metadata(history)
  new_rows <- normalize_metadata(old_rows) %>% filter(!MainModelVariableName %in% history$MainModelVariableName)
  list(metadata = normalize_metadata(bind_rows(history, new_rows)),
       n_added = n_distinct(new_rows$MainModelVariableName),
       n_skipped = n_distinct(old_rows$MainModelVariableName) - n_distinct(new_rows$MainModelVariableName))
}

read_metadata_file <- function(path, file_name, cs_dims = NULL) {
  raw <- read_table_file(path, file_name, as_text = TRUE)
  from_code <- legacy_metadata_from_code(raw, cs_dims)
  if (!is.null(from_code)) return(from_code)
  normalize_metadata(raw, cs_dims)
}

# The oldest metadata files (no Type column; InitCode / FeedCode columns) wrote
# the CS values as one comma-separated text and dates as d/m/Y, and their names
# could differ from the VOF their code creates. Their code is what AWB runs,
# so the media rows are rebuilt from it with the Restore parser (filters,
# periods, labels, metric). Rows whose code cannot be read are migrated
# column by column. Returns NULL when the file is not of that kind; otherwise
# the metadata with attr(, "notes") for the analyst.
legacy_metadata_from_code <- function(raw, cs_dims = NULL) {
  if (!"FeedCode" %in% names(raw) || "Type" %in% names(raw) || nrow(raw) == 0) return(NULL)
  raw <- raw %>% mutate(across(everything(), ~ replace_na(as.character(.x), "")))
  # Rows repeated with the same code (a VOF saved twice) would add a root twice.
  n_raw <- nrow(raw)
  raw <- raw %>% distinct(FeedCode, .keep_all = TRUE)
  n_dup <- n_raw - nrow(raw)
  init <- if ("InitCode" %in% names(raw)) raw$InitCode else rep("", nrow(raw))
  code <- str_trim(str_c(init, "\n", raw$FeedCode))
  # VOF each row's code creates: Data[, 'X'] <- 0 / Data[..., 'X'] <- rowSums(...)
  # (read from FeedCode first: it is the line that fills the VOF)
  find_target <- function(x) str_match(x, "\\[[^\\]]*['\"]([^'\"]+)['\"]\\s*\\]\\s*(<-|=)")[, 2]
  target <- coalesce(find_target(raw$FeedCode), find_target(init))
  vofs <- tryCatch(parse_vof_code(str_c(code, collapse = "\n")), error = function(e) list())
  dims <- cs_dims %||% intersect(MFF_DIMS, names(raw))
  rebuilt <- if (length(vofs) > 0) parsed_vofs_to_metadata(vofs, dims) else NULL
  ok <- !is.na(target) & target %in% names(vofs)
  rest <- normalize_metadata(raw[!ok, , drop = FALSE], dims)
  out <- normalize_metadata(bind_rows(rebuilt, rest), dims)
  renamed <- unique(tibble(from = raw$MainModelVariableName[ok], to = target[ok]) %>% filter(from != to))
  notes <- c(
    sprintf("Old metadata format: %d VOFs rebuilt from their code (filters, periods and metric as AWB runs them).", n_distinct(target[ok])),
    if (nrow(renamed) > 0) sprintf("%d names in the file differ from the VOF their code creates; the name of the code is used (review these VOFs in VOF QA): %s.",
                                   nrow(renamed), str_c(sprintf("'%s' -> '%s'", renamed$from, renamed$to), collapse = "; ")),
    if (n_dup > 0) sprintf("%d repeated rows (same code as another row) were ignored.", n_dup),
    if (any(!ok)) sprintf("%d rows without readable code were migrated column by column.", sum(!ok))
  )
  attr(out, "notes") <- notes
  out
}

# ------------------------------------------------------------------------------
# Purifier
# ------------------------------------------------------------------------------
read_model_details <- function(path, file_name) {
  read_table_file(path, file_name, as_text = TRUE) %>%
    mutate(across(everything(), ~ replace_na(.x, "")))
}

details_var_col <- function(details) if ("VariableName" %in% names(details)) "VariableName" else names(details)[1]

# Keeps only the metadata of variables used by the model (Type not "NONE"),
# plus their spend twins, and lists expected VOFs missing from the metadata.
purify_metadata <- function(metadata, details, cs_dims) {
  metadata <- normalize_metadata(metadata)
  details <- details %>%
    rename(.var = all_of(details_var_col(details))) %>%
    mutate(.type = if ("Type" %in% names(.)) str_to_lower(Type) else "")

  valid_vars <- details %>% filter(!str_detect(.type, "none")) %>% pull(.var) %>% unique()

  # Expected VOFs: used, independent variables with no longitudinal values
  # (MFF variables always carry them), excluding non-media controls.
  long_present <- intersect(longitudinal_dims(cs_dims), names(details))
  expected <- details %>%
    filter(!str_detect(.type, "none|dep"),
           if_all(all_of(long_present), ~ str_trim(.x) == "")) %>%
    pull(.var) %>%
    unique() %>%
    discard(~ str_detect(.x, PURIFY_NON_MEDIA_PATTERN))

  # Twins (activity/spend) share the name without the "---<metric>" suffix:
  # the twin of a used variable is kept too.
  meta_names <- metadata$MainModelVariableName
  base_names <- vof_base_name(meta_names)
  valid_bases <- unique(c(valid_vars, vof_base_name(valid_vars)))
  missing <- setdiff(expected, unique(c(meta_names, base_names)))
  missing <- c(keep(missing, ~ str_detect(.x, MEDIA_KEYWORDS_PATTERN)), discard(missing, ~ str_detect(.x, MEDIA_KEYWORDS_PATTERN)))

  keep_rows <- meta_names %in% valid_vars | base_names %in% valid_bases
  list(metadata = metadata[keep_rows, ], missing = missing,
       rows_before = nrow(metadata), rows_after = sum(keep_rows))
}

# ------------------------------------------------------------------------------
# Export
# ------------------------------------------------------------------------------
export_metadata <- function(metadata, cs_dims) {
  normalize_metadata(metadata, cs_dims) %>% select(-all_of(METADATA_CODE_COL)) %>% distinct()
}

metadata_file_name <- function(time = Sys.time()) str_c(format(time, "%y%m%d%H%M%S"), " - VOF Metadata.csv")

# ------------------------------------------------------------------------------
# Browser persistence (JSON)
# ------------------------------------------------------------------------------
metadata_to_json <- function(metadata) jsonlite::toJSON(normalize_metadata(metadata), dataframe = "rows")

metadata_from_json <- function(json) {
  df <- jsonlite::fromJSON(json, flatten = TRUE)
  if (!is.data.frame(df)) df <- bind_rows(map(df, as_tibble))
  normalize_metadata(df)
}

# Code of every VOF of the metadata, in history order: tibble(name, type,
# code, components). Media VOFs are rebuilt from their metadata columns
# (filters, periods, backcasting); KPI / weight / external VOFs use their
# stored code ("" when it is not available).
vof_code_blocks <- function(metadata, cs_dims) {
  metadata <- normalize_metadata(metadata, cs_dims)
  empty <- tibble(name = character(0), type = character(0), code = character(0), components = list())
  if (nrow(metadata) == 0) return(empty)
  metadata %>%
    mutate(.order = match(MainModelVariableName, unique(MainModelVariableName))) %>%
    group_by(.order, MainModelVariableName) %>%
    group_map(function(rows, key) {
      v <- key$MainModelVariableName
      type <- rows$Type[1]
      if (all(rows$Type == METADATA_TYPES[["media"]])) {
        comps <- map(seq_len(nrow(rows)), ~ component_from_metadata_row(rows[.x, ], cs_dims))
        bc <- c(rows$BackcastingStart[1], rows$BackcastingEnd[1])
        code <- c(sprintf("#### %s ####", v), build_init_line(v), map_chr(comps, ~ build_feed_line(v, .x)),
                  if (all(bc != "")) c("", build_backcasting_code(v, cs_dims, bc[1], bc[2]))) %>% str_c(collapse = "\n")
      } else {
        comps <- list()
        code <- keep(rows[[METADATA_CODE_COL]], nzchar) %>% unique() %>% str_c(collapse = "\n")
      }
      tibble(name = v, type = type, code = code, components = list(comps),
             roots = list(unique(keep(rows$AnalyticalVariableName, nzchar))), metric = rows$Metric[1])
    }) %>%
    list_rbind()
}

# Full code of the project (one block per VOF, in history order).
project_code_from_metadata <- function(metadata, cs_dims) {
  blocks <- vof_code_blocks(metadata, cs_dims)
  if (nrow(blocks) == 0) return("")
  blocks %>%
    mutate(code = if_else(code == "", sprintf("#### %s ####\n# Code not available: imported from a metadata file without code.", name), code)) %>%
    pull(code) %>%
    str_c(collapse = "\n\n") %>%
    str_c("\n")
}
