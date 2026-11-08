# server.R
# Wiring between modules. All business rules live in R/core; all reactive
# behaviour lives in R/modules.

function(input, output, session) {
  history <- history_store()
  upload  <- data_upload_server("upload", history)

  observe({
    has_data <- !is.null(tryCatch(upload$data(), error = function(e) NULL))
    shinyjs::toggleState("main_app_content", condition = has_data)
  })

  media <- media_tab_server("media", upload, history)
  kpi_tab_server("kpi", upload, history)
  external_tab_server("external", upload, history)
  dummies_tab_server("dummies", upload, history)
  qa_tab_server("qa", upload, history)
  project_actions_server("project", upload, history, media)
}
