# R/modules/mod_history.R
# Reactive store of the project's VOF Metadata (the "history"). Every tab writes
# to it through these functions so the update rules live in one place
# (see upsert_metadata() in R/core/07_metadata.R).
#
# It also remembers what was last downloaded, to tell the analyst how many VOFs
# are new or changed since then (download buttons, warning when leaving).

history_store <- function() {
  data <- reactiveVal(empty_metadata())
  saved <- reactiveVal(set_names(character(0), character(0)))   # VOF -> signature at the last download

  # One signature per VOF (all its metadata rows), to tell new / changed VOFs.
  signatures <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(set_names(character(0), character(0)))
    rows <- apply(as.matrix(df), 1, str_c, collapse = "\u001f")
    sig <- tapply(rows, df$MainModelVariableName, str_c, collapse = "\u001e")
    set_names(as.character(sig), names(sig))
  }
  mark_saved_names <- function(names) {
    cur <- signatures(isolate(data()))
    s <- isolate(saved())
    s[intersect(names, names(cur))] <- cur[intersect(names, names(cur))]
    saved(s)
  }

  list(
    data    = data,
    # New rows replace previous rows of the same VOF.
    upsert  = function(rows) data(upsert_metadata(data(), rows)),
    # Old metadata only fills VOFs that are not in the history yet. Rows that
    # come from a file are already saved somewhere: they do not count as new.
    add_missing = function(rows, saved_already = TRUE) {
      res <- add_missing_metadata(data(), rows)
      data(res$metadata)
      if (saved_already) mark_saved_names(unique(rows$MainModelVariableName))
      res[c("n_added", "n_skipped")]
    },
    replace = function(rows, saved_already = FALSE) {
      data(normalize_metadata(rows))
      if (saved_already) saved(signatures(isolate(data())))
    },
    clear   = function() { data(empty_metadata()); saved(set_names(character(0), character(0))) },
    n_rows  = function() nrow(data()),
    n_vofs  = function() n_distinct(data()$MainModelVariableName),
    # Everything in the history was just downloaded.
    mark_saved = function() saved(signatures(isolate(data()))),
    # VOFs new, changed or removed since the last download.
    n_unsaved = reactive({
      cur <- signatures(data()); s <- saved()
      changed <- names(cur)[is.na(s[names(cur)]) | s[names(cur)] != cur]
      length(changed) + length(setdiff(names(s), names(cur)))
    })
  )
}

# Shows a list of messages as one message (see app_notify()).
notify_messages <- function(msgs, type = "warning", duration = 10) {
  msgs <- keep(msgs, nzchar)
  if (length(msgs) == 0) return(invisible())
  app_notify(if (length(msgs) == 1) msgs else tags$ul(map(msgs, tags$li)), type = type, duration = duration)
}
