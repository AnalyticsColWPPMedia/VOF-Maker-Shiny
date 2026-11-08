# R/core/01_helpers.R
# Generic helpers with no business logic: value checks, R-literal quoting for
# the generated code, date parsing and file reading.

`%||%` <- function(a, b) if (is.null(a)) b else a

# TRUE when x carries a usable value (not NULL, empty, all-NA or all-blank).
is_valid_value <- function(x) {
  if (is.null(x) || length(x) == 0) return(FALSE)
  if (inherits(x, "Date")) return(!all(is.na(x)))
  if (all(is.na(x))) return(FALSE)
  !all(str_trim(as.character(x)) == "")
}

convert_to_char <- function(x) if (is_valid_value(x)) as.character(x) else ""

# Keeps only usable elements of a character vector.
clean_values <- function(x) {
  if (is.null(x)) return(character(0))
  x <- as.character(x)
  x[!is.na(x) & str_trim(x) != ""]
}

# ------------------------------------------------------------------------------
# R literals for generated code (the generated code itself is base R)
# ------------------------------------------------------------------------------
# Single-quoted R string literal, escaping backslashes and quotes, so values such
# as "McDonald's" never break the generated code.
r_str <- function(x) {
  # With fixed() patterns the replacement is literal: "\\\\" is two backslashes.
  x <- as.character(x) %>%
    str_replace_all(fixed("\\"), "\\\\") %>%
    str_replace_all(fixed("'"), "\\'")
  str_c("'", x, "'")
}

# c('a', 'b', ...) literal.
r_str_vec <- function(x) str_c("c(", str_c(r_str(x), collapse = ", "), ")")

# Data[['col']] accessor (safe for any column name).
r_col <- function(col, df = "Data") str_c(df, "[[", r_str(col), "]]")

# ------------------------------------------------------------------------------
# Text
# ------------------------------------------------------------------------------
# Title Case following the team convention: short words (and, of, the, ...)
# stay in lower case unless they are the first word.
to_title_case <- function(text) {
  words <- text %>% str_replace_all("[_-]", " ") %>% str_squish() %>% str_split(" ") %>% .[[1]]
  if (length(words) == 0 || identical(words, "")) return("")
  out <- str_to_title(words)
  small <- str_to_lower(words) %in% TITLE_CASE_SMALL_WORDS & seq_along(words) > 1
  out[small] <- str_to_lower(words[small])
  str_c(out, collapse = " ")
}

# ------------------------------------------------------------------------------
# Dates
# ------------------------------------------------------------------------------
# Parses a vector of dates coming from CSV/XLSX/RData: Date, POSIXct, Excel
# serial numbers (numeric or text) and character dates. For character input
# every candidate order is tried on the whole vector and the one that parses the
# most values wins (ties go to the earliest order: ISO, then US, then day-first),
# instead of guessing element by element.
DATE_PARSERS <- list(ymd = lubridate::ymd, mdy = lubridate::mdy, dmy = lubridate::dmy)

parse_dates_flexible <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as_date(x))
  if (is.factor(x)) x <- as.character(x)
  if (is.numeric(x)) {
    # Excel serial dates (1900 system) between years ~1955 and ~2100.
    out <- as_date(rep(NA_real_, length(x)))
    ok <- !is.na(x) & x > 20000 & x < 73000
    out[ok] <- as_date(x[ok], origin = "1899-12-30")
    return(out)
  }
  x <- str_trim(as.character(x))
  x[x %in% c("", "NA")] <- NA
  if (all(is.na(x))) return(as_date(x))
  if (all(str_detect(x[!is.na(x)], "^\\d{5}(\\.\\d+)?$"))) return(parse_dates_flexible(as.numeric(x)))
  x <- str_remove(x, "[ T]\\d{1,2}:\\d{2}(:\\d{2})?.*$")  # drop a time component

  candidates <- map(DATE_PARSERS, function(f) {
    d <- suppressWarnings(f(x, quiet = TRUE))
    d[!is.na(d) & (year(d) < 1900 | year(d) > 2100)] <- NA  # implausible years
    d
  })
  n_ok <- map_int(candidates, ~ sum(!is.na(.x)))
  candidates[[which.max(n_ok)]]
}

# ISO string ("YYYY-MM-DD") or "" for a single date-like value.
format_date_iso <- function(x) {
  if (!is_valid_value(x)) return("")
  d <- parse_dates_flexible(x)[1]
  if (is.na(d)) "" else format(d, "%Y-%m-%d")
}

# Compact date used in VOF names ("YYYYMMDD") or "".
format_date_tag <- function(x) {
  if (!is_valid_value(x)) return("")
  d <- parse_dates_flexible(x)[1]
  if (is.na(d)) "" else format(d, "%Y%m%d")
}

# ------------------------------------------------------------------------------
# Files
# ------------------------------------------------------------------------------
# Reads a CSV/XLSX upload keeping column names untouched. `as_text = TRUE`
# reads every column as character and untouched (metadata, model details).
read_table_file <- function(path, file_name, as_text = FALSE) {
  ext <- str_to_lower(tools::file_ext(file_name))
  if (ext == "csv") {
    read_csv(path, col_types = if (as_text) cols(.default = col_character()) else cols(),
             na = c("", "NA"), trim_ws = !as_text, show_col_types = FALSE, progress = FALSE)
  } else if (ext %in% c("xlsx", "xls")) {
    read_excel(path, col_types = if (as_text) "text" else NULL, .name_repair = "minimal")
  } else {
    stop("Unsupported file type: .", ext)
  }
}
