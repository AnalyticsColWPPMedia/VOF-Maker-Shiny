# R/modules/mod_data_upload.R
# Data Management: Analytical dataset, old VOF Metadata and Model Details.
# Exposes the Analytical and its derived structure to the other modules.

data_upload_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(4, div(class = "upload-zone", h5("Analytical Dataset"),
                  fileInput(ns("file_analytical"), NULL, accept = c(".RData", ".rda", ".csv"),
                            buttonLabel = "Browse...", placeholder = "No file selected"),
                  uiOutput(ns("analytical_summary")))),
    column(4, div(class = "upload-zone", h5("VOF Metadata (Optional)"),
                  fileInput(ns("file_metadata"), NULL, accept = c(".xlsx", ".csv"),
                            buttonLabel = "Browse...", placeholder = "Add Old Metadata to History"))),
    column(4, div(class = "upload-zone", h5("Model Details (For Purifier)"),
                  fileInput(ns("file_details"), NULL, accept = c(".xlsx", ".csv"),
                            buttonLabel = "Browse...", placeholder = "Purify based on this file")))
  )
}

data_upload_server <- function(id, history) {
  moduleServer(id, function(input, output, session) {

    analytical <- reactive({
      req(input$file_analytical)
      tryCatch(read_analytical(input$file_analytical$datapath, input$file_analytical$name),
               error = function(e) {
                 app_notify(paste("Error reading the Analytical:", conditionMessage(e)), type = "error")
                 NULL
               })
    })

    cs_dims   <- reactive({ req(analytical()); detect_cs_dims(analytical()) })
    long_dims <- reactive(longitudinal_dims(cs_dims()))
    mff_vars  <- reactive({ req(analytical()); list_mff_vars(analytical(), cs_dims()) })
    vof_vars  <- reactive({ req(analytical()); list_vof_vars(analytical(), cs_dims()) })
    project_key <- reactive({ req(analytical()); project_signature(analytical()) })

    output$analytical_summary <- renderUI({
      req(analytical())
      tags$small(class = "text-muted", sprintf(
        "%s rows | CS: %s | %d MFF variables | %d existing VOFs",
        format(nrow(analytical()), big.mark = ","),
        if (length(cs_dims()) > 0) paste(cs_dims(), collapse = ", ") else "none",
        length(mff_vars()), length(vof_vars())))
    })

    # Old metadata is merged into the history right away.
    observeEvent(input$file_metadata, {
      f <- input$file_metadata
      tryCatch({
        old <- read_metadata_file(f$datapath, f$name)
        res <- history$add_missing(old)
        app_notify(sprintf("Old metadata loaded: %d VOFs added to the history (%d already existed).",
                                 res$n_added, res$n_skipped),
                         type = "message", duration = 8)
      }, error = function(e) app_notify(paste("Error reading the metadata:", conditionMessage(e)), type = "error"))
      shinyjs::reset("file_metadata")
    })

    details <- reactive({
      req(input$file_details)
      tryCatch(read_model_details(input$file_details$datapath, input$file_details$name),
               error = function(e) {
                 app_notify(paste("Error reading Model Details:", conditionMessage(e)), type = "error")
                 NULL
               })
    })

    list(data = analytical, cs_dims = cs_dims, long_dims = long_dims, mff_vars = mff_vars,
         vof_vars = vof_vars, project_key = project_key, details = details,
         has_details = reactive(!is.null(input$file_details)))
  })
}
