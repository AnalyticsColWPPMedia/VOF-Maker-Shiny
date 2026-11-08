# R/core/02_analytical.R
# Reading the Analytical dataset and understanding its structure:
# cross-sectional vs longitudinal dimensions, and MFF variables vs VOFs.
#
# AWB builds every MFF column of the Analytical as
#   VariableName_<longitudinal dims in MFF_DIMS order>
# (cross-sectional dimensions are rows, so they are not part of the name).
# VOFs are created on the fly and only carry a VariableName, so the number of
# trailing "_<value>" tokens is what tells both kinds of columns apart.

read_analytical <- function(path, file_name) {
  ext <- str_to_lower(tools::file_ext(file_name))
  df <- if (ext %in% c("rdata", "rda")) {
    env <- new.env()
    obj_names <- load(path, envir = env)
    dfs <- keep(obj_names, ~ is.data.frame(env[[.x]]))
    if (length(dfs) == 0) stop("The .RData file does not contain any data.frame.")
    env[[dfs[1]]]
  } else if (ext == "csv") {
    # readr keeps names such as "CRM Clicks_Watches_Total" intact.
    read_csv(path, show_col_types = FALSE, progress = FALSE)
  } else {
    stop("Unsupported Analytical file type: .", ext)
  }
  if (!PERIOD_COL %in% names(df)) stop("The Analytical dataset has no '", PERIOD_COL, "' column.")
  df %>%
    as_tibble(.name_repair = "minimal") %>%
    mutate(!!PERIOD_COL := parse_dates_flexible(.data[[PERIOD_COL]]),
           across(any_of(MFF_DIMS), as.character))
}

# Cross-sectional dimensions present in the Analytical, in MFF order.
detect_cs_dims <- function(df_or_cols) {
  cols <- if (is.data.frame(df_or_cols)) names(df_or_cols) else df_or_cols
  MFF_DIMS[MFF_DIMS %in% cols]
}

longitudinal_dims <- function(cs_dims) setdiff(MFF_DIMS, cs_dims)

# Splits column names into VariableName + longitudinal dimension values.
# Returns a tibble with one row per name: column, is_mff, base and one column
# per longitudinal dimension (NA when the name is not an MFF variable). The base
# is everything before the last n tokens, so a VariableName that itself
# contains "_" is still parsed correctly.
parse_mff_names <- function(col_names, long_dims) {
  n <- length(long_dims)
  if (n == 0) {
    # Every MFF dimension is cross-sectional: AWB adds no suffix, so MFF
    # variables and VOFs cannot be told apart by name.
    return(tibble(column = col_names, is_mff = TRUE, base = col_names))
  }
  pattern <- str_c("^(.+)", strrep(str_c(MFF_NAME_SEP, "([^", MFF_NAME_SEP, "]+)"), n), "$")
  m <- str_match(col_names, pattern)
  out <- tibble(column = col_names, is_mff = !is.na(m[, 1]), base = m[, 2])
  for (j in seq_len(n)) out[[long_dims[j]]] <- m[, j + 2]
  out
}

# Non-dimension, non-period columns of the Analytical.
variable_columns <- function(df) setdiff(names(df), c(PERIOD_COL, MFF_DIMS))

# MFF variables (the only valid inputs for new VOFs, so VOFs are never nested).
list_mff_vars <- function(df, cs_dims = detect_cs_dims(df)) {
  parse_mff_names(variable_columns(df), longitudinal_dims(cs_dims)) %>%
    filter(is_mff) %>%
    pull(column)
}

# Columns that are already VOFs (no longitudinal suffix).
list_vof_vars <- function(df, cs_dims = detect_cs_dims(df)) {
  setdiff(variable_columns(df), list_mff_vars(df, cs_dims))
}

# Rebuilds an MFF column name from its parts.
build_mff_name <- function(base, dim_values) str_c(c(base, dim_values), collapse = MFF_NAME_SEP)

# Stable identifier of a project, built from the Analytical's column names.
# Used to keep the browser-stored history of each project separate.
project_signature <- function(df) {
  tmp <- tempfile()
  on.exit(unlink(tmp))
  write_lines(sort(names(df)), tmp)
  unname(tools::md5sum(tmp))
}

# Sorted unique, non-empty values of a CS dimension.
cs_dim_values <- function(df, dim) {
  if (is.null(df) || !dim %in% names(df)) return(character(0))
  sort(unique(clean_values(df[[dim]])))
}

# Sorted unique periods.
analytical_periods <- function(df) {
  df[[PERIOD_COL]] %>% unique() %>% sort() %>% discard(is.na)
}
