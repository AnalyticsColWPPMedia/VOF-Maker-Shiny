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
    div(class = "section-card", h4("Preview in the Analytical", class = "card-title"),
        uiOutput(ns("preview_controls")),
        div(class = "vof-dt-card", DT::DTOutput(ns("preview_table")))),
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

    # Generate needs a valid diagnosis and, for coarser data, an expansion method.
    observe({
      d <- tryCatch(diagnosis(), error = function(e) NULL)
      ok <- isTRUE(d$ok) && (!isTRUE(d$needs_expand) || is_valid_value(input$expand_method))
      shinyjs::toggleState("generate_btn", condition = ok)
    })

    output$diagnosis_ui <- renderUI({
      d <- diagnosis()
      tagList(map(d$messages, ~ div(class = str_c("alert alert-", .x$level), .x$text,
                                     if (length(.x$items) > 0) div(class = "vof-diag-items", tags$ul(map(.x$items, tags$li))))),
              if (length(d$missing_entities) > 0)
                tags$ul(class = "vof-scroll-list", map(d$missing_entities, tags$li)))
    })

    output$agg_ui <- renderUI({
      d <- diagnosis()
      expand <- if (isTRUE(d$needs_expand)) {
        l <- d$expand_labels
        choices <- set_names(unname(EXPAND_METHODS), c(
          sprintf("Replicate: every %s of the %s gets its value (levels, indexes, rates, prices)", str_remove(l$unit, "s$"), l$span),
          sprintf("Divide equally: the value is divided by the number of %s that start in the %s (totals)", l$unit, l$span),
          sprintf("Divide by days: each %s gets the share of the days it has in the %s; %s across two %ss take part of each (totals)",
                  str_remove(l$unit, "s$"), l$span, l$unit, l$span)))
        div(class = "vof-expand-choice",
            radioButtons(ns("expand_method"), sprintf("How to expand %s data to the Analytical %s (required):", l$freq, l$unit),
                         choices = choices, selected = isolate(input$expand_method) %||% character(0), width = "100%"))
      }
      agg <- if (isTRUE(d$needs_agg))
        radioButtons(ns("agg_method"), "Temporal Aggregation Method:", choices = c("Sum" = "sum", "Mean" = "mean"), inline = TRUE)
      tagList(expand, agg)
    })

    # --- Preview: the variable as it will be in the Analytical ------------------------
    preview_agg <- reactive({
      d <- diagnosis(); req(isTRUE(d$ok))
      req(!isTRUE(d$needs_expand) || is_valid_value(input$expand_method))
      tryCatch(aggregate_external(ext_data(), input$period_col, input$value_col, analytical_periods(upload$data()),
                                  entity_col(), input$agg_method %||% "sum", input$expand_method),
               error = function(e) NULL)
    })
    # With entities: only those with data can be previewed; the rest get 0.
    preview_entities <- reactive({
      agg <- preview_agg(); req(agg, target_dim() != "", "Entity" %in% names(agg))
      ana_vals <- upload$data()[[target_dim()]] %>% as.character() %>% str_trim() %>% clean_values() %>% unique() %>% sort()
      list(with_data = intersect(ana_vals, unique(agg$Entity)), zero = setdiff(ana_vals, unique(agg$Entity)))
    })
    output$preview_controls <- renderUI({
      d <- tryCatch(diagnosis(), error = function(e) NULL)
      if (is.null(d) || !isTRUE(d$ok)) return(tags$small(class = "text-muted", "Select the columns (and fix the diagnosis) to see the preview."))
      if (isTRUE(d$needs_expand) && !is_valid_value(input$expand_method))
        return(tags$small(class = "text-muted", "Choose how to expand the data to see the preview."))
      if (target_dim() == "")
        return(tags$small(class = "text-muted", "Same values for every entity of the Analytical. First and last 3 months; periods without external data are 0."))
      pe <- preview_entities()
      tagList(
        fluidRow(column(4, selectInput(ns("preview_entity"), str_c(target_dim(), " (entities with data)"), choices = pe$with_data,
                                       selected = intersect(isolate(input$preview_entity), pe$with_data) %>% first(default = pe$with_data[1])))),
        if (length(pe$zero) > 0) tags$small(class = "text-muted",
                                            sprintf("%d entities have no external data and are 0 in every period: %s.", length(pe$zero), str_c(pe$zero, collapse = ", "))))
    })
    output$preview_table <- DT::renderDT({
      agg <- preview_agg(); req(agg)
      entity <- if (target_dim() != "") { req(input$preview_entity); input$preview_entity } else NULL
      tbl <- external_preview(agg, analytical_periods(upload$data()), entity) %>%
        mutate(Period = format(Period), Value = round(Value, 6))
      name <- str_squish(input$var_name %||% "")
      names(tbl)[names(tbl) == "Value"] <- if (nzchar(name)) name else "New variable"
      DT::datatable(tbl, rownames = FALSE, class = "compact stripe hover", selection = "none",
                    options = list(dom = "t", paging = FALSE, scrollY = "260px", scrollX = TRUE, scrollCollapse = TRUE,
                                   ordering = FALSE, rowGroup = NULL))
    })

    observeEvent(input$generate_btn, {
      res <- generate_external_vof(input$var_name, ext_data(), input$period_col, input$value_col, upload$data(),
                                   entity_col = entity_col(), target_dim = target_dim(),
                                   agg = input$agg_method %||% "sum", source = input$source,
                                   notes = input$notes, why = input$why, cs_dims = upload$cs_dims(),
                                   expand = input$expand_method)
      if (length(res$errors) > 0) { notify_messages(res$errors, type = "error"); return() }
      updateTextAreaInput(session, "code_area", value = res$code)
      history$upsert(res$metadata)
      app_notify("External VOF saved to history!", type = "message")
      notify_messages(res$warnings)
    })
  })
}
