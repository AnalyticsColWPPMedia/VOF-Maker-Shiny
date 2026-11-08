# R/modules/mod_metadata_download.R
# "Download VOF Metadata" button. The same button sits next to every Generate
# button and in Data Management: all of them download the whole history and
# show how many VOFs are new or changed since the last download.

metadata_download_ui <- function(id, label = "Download VOF Metadata", class = "btn btn-success custom-download-btn") {
  ns <- NS(id)
  downloadButton(ns("download"),
                 tagList(label, span(class = "vof-unsaved-badge", textOutput(ns("unsaved"), inline = TRUE))),
                 class = class)
}

metadata_download_server <- function(id, history, upload) {
  moduleServer(id, function(input, output, session) {
    output$unsaved <- renderText({
      n <- history$n_unsaved()
      if (n > 0) sprintf("%d new", n) else ""
    })
    output$download <- downloadHandler(
      filename = function() metadata_file_name(),
      content = function(file) {
        dims <- tryCatch(upload$cs_dims(), error = function(e) intersect(MFF_DIMS, names(history$data())))
        write_csv(export_metadata(history$data(), dims), file, na = "", quote = "all")
        history$mark_saved()
      }
    )
  })
}
