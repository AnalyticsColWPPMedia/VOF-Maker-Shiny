# R/modules/mod_external_tab.R
# External Sources tab: external series aligned to the Analytical periods.

external_tab_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$br(),
    div(class = "section-card", h4("External Data Upload & Mapping", class = "card-title"),
        fluidRow(
          column(3, fileInput(ns("file"), "Upload Data (CSV/XLSX)", accept = c(".csv", ".xlsx", ".xls"))),
          column(3, textInput(ns("var_name"), "Variable Name", placeholder = "e.g., Google Trends")),
          column(2, selectInput(ns("period_col"), "Period Column", choices = NULL)),
          column(2, selectInput(ns("value_col"), "Value Column", choices = NULL))
        ),
        fluidRow(column(6),
                 column(3, selectInput(ns("entity_col"), "External Entity Column (Optional)", choices = c("None" = ""))),
                 column(3, uiOutput(ns("target_ui"))))
    ),
    div(class = "section-card", h4("Metadata", class = "card-title"),
        fluidRow(column(4, textInput(ns("source"), "Source (URL, platform, etc.)", width = "100%")),
                 column(4, textInput(ns("notes"), "Specific notes", width = "100%")),
                 column(4, textInput(ns("why"), "Why use VOF?", width = "100%")))),
    div(class = "section-card", h4("Diagnosis & Settings", class = "card-title"),
        uiOutput(ns("diagnosis_ui")),
        uiOutput(ns("agg_ui"))),
    div(class = "section-card", h4("Actions & Output", class = "card-title"),
        div(class = "vof-action-row",
            actionButton(ns("generate_btn"), "Generate VOF & Save", class = "btn btn-primary vof-btn-wide"),
            metadata_download_ui(ns("meta_dl"))),
        tags$hr(),
        textAreaInput(ns("code_area"), "Generated Code", value = "", rows = 12, width = "100%", resize = "vertical"))
  )
}

external_tab_server <- function(id, upload, history) {
  moduleServer(id, function(input, output, session) {
    metadata_download_server("meta_dl", history, upload)
    ns <- session$ns

    ext_data <- reactive({
      req(input$file)
      tryCatch(read_table_file(input$file$datapath, input$file$name), error = function(e) {
        app_notify(paste("Error reading external file:", conditionMessage(e)), type = "error"); NULL
      })
    })

    observeEvent(ext_data(), {
      cols <- names(ext_data())
      updateSelectInput(session, "period_col", choices = c("", cols))
      updateSelectInput(session, "value_col", choices = c("", cols))
      updateSelectInput(session, "entity_col", choices = c("None" = "", cols))
    })

    output$target_ui <- renderUI({
      req(input$entity_col)
      selectInput(ns("target_dim"), "Target Analytical Entity", choices = c("", upload$cs_dims()))
    })

    entity_col <- reactive(input$entity_col %||% "")
    target_dim <- reactive(if (entity_col() == "") "" else input$target_dim %||% "")

    diagnosis <- reactive({
      req(ext_data(), upload$data(), is_valid_value(input$period_col), is_valid_value(input$value_col))
      diagnose_external(ext_data(), input$period_col, input$value_col, upload$data(), entity_col(), target_dim())
    })

    observe({
      ok <- tryCatch(isTRUE(diagnosis()$ok), error = function(e) FALSE)
      shinyjs::toggleState("generate_btn", condition = ok)
    })

    output$diagnosis_ui <- renderUI({
      d <- diagnosis()
      tagList(map(d$messages, ~ div(class = str_c("alert alert-", .x$level), .x$text)),
              if (length(d$missing_entities) > 0)
                tags$ul(class = "vof-scroll-list", map(d$missing_entities, tags$li)))
    })

    output$agg_ui <- renderUI({
      req(diagnosis()$needs_agg)
      radioButtons(ns("agg_method"), "Temporal Aggregation Method:", choices = c("Sum" = "sum", "Mean" = "mean"), inline = TRUE)
    })

    observeEvent(input$generate_btn, {
      res <- generate_external_vof(input$var_name, ext_data(), input$period_col, input$value_col, upload$data(),
                                   entity_col = entity_col(), target_dim = target_dim(),
                                   agg = input$agg_method %||% "sum", source = input$source,
                                   notes = input$notes, why = input$why, cs_dims = upload$cs_dims())
      if (length(res$errors) > 0) { notify_messages(res$errors, type = "error"); return() }
      updateTextAreaInput(session, "code_area", value = res$code)
      history$upsert(res$metadata)
      app_notify("External VOF saved to history!", type = "message")
      notify_messages(res$warnings)
    })
  })
}
