# R/modules/mod_kpi_tab.R
# KPI tab: log of the KPI, business cycles and weight variable.

kpi_tab_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$br(),
    div(class = "section-card", h4("KPI Selection & Transformation", class = "card-title"),
        fluidRow(
          column(6, uiOutput(ns("kpi_selector_ui"))),
          column(4, checkboxInput(ns("gen_log"), "Transform KPI to Log", TRUE),
                    checkboxInput(ns("gen_weight"), "Create Weight Variable", TRUE))
        )
    ),
    conditionalPanel(sprintf("input['%s'] == true", ns("gen_weight")),
      div(class = "section-card", h4("Business Cycle Settings", class = "card-title"),
          fluidRow(
            column(6, radioButtons(ns("cycle_method"), "Business cycle method:", choices = CYCLE_METHODS,
                                   selected = "trailing")),
            column(3, textInput(ns("cycle_label"), "Cycle label (prefix)", value = "FY")),
            column(3, conditionalPanel(sprintf("input['%s'] == 'trailing'", ns("cycle_method")),
                                       numericInput(ns("n_cycles"), "Number of cycles (empty = all complete)",
                                                    value = NA, min = 1, step = 1)))
          ),
          conditionalPanel(sprintf("input['%s'] != 'custom'", ns("cycle_method")), uiOutput(ns("cycle_preview"))),
          conditionalPanel(sprintf("input['%s'] == 'custom'", ns("cycle_method")),
            tags$hr(),
            fluidRow(column(6, fileInput(ns("upload_cycles"), "Upload cycle settings (csv)", accept = ".csv"))),
            fluidRow(class = "vof-fy-header", column(3, tags$b("Calendar year")), column(4, tags$b("Start")), column(5, tags$b("End"))),
            uiOutput(ns("cycle_rows_ui")), tags$br(),
            downloadButton(ns("download_cycles"), "Download cycle settings", class = "btn btn-info custom-download-btn"))
      )
    ),
    div(class = "section-card", h4("Actions & Output (KPI)", class = "card-title"),
        div(class = "vof-action-row",
            actionButton(ns("generate_btn"), "Generate KPI Code & Save", class = "btn btn-primary vof-btn-wide"),
            metadata_download_ui(ns("meta_dl"))),
        tags$hr(),
        textAreaInput(ns("code_area"), NULL, value = "", rows = 12, width = "100%", resize = "vertical"))
  )
}

kpi_tab_server <- function(id, upload, history) {
  moduleServer(id, function(input, output, session) {
    metadata_download_server("meta_dl", history, upload)
    ns <- session$ns
    row_id <- function(prefix, y) str_c(prefix, "_", y)

    output$kpi_selector_ui <- renderUI({
      selectizeInput(ns("kpi_var"), "Select the KPI Variable (MFF)", choices = c("", upload$mff_vars()), width = "100%")
    })

    periods <- reactive({ req(upload$data()); analytical_periods(upload$data()) })
    label <- reactive(input$cycle_label %||% "FY")

    # --- Custom table ----------------------------------------------------------
    default_table <- reactive(default_cycle_table(periods()))

    output$cycle_rows_ui <- renderUI({
      tbl <- default_table(); req(nrow(tbl) > 0)
      tagList(pmap(tbl, function(Year_Cal, Include, Start, End) {
        fluidRow(class = "vof-fy-row",
                 column(3, checkboxInput(ns(row_id("cyc_inc", Year_Cal)), as.character(Year_Cal), value = Include)),
                 column(4, dateInput(ns(row_id("cyc_start", Year_Cal)), NULL, value = Start)),
                 column(5, dateInput(ns(row_id("cyc_end", Year_Cal)), NULL, value = End)))
      }))
    })

    cycle_table <- reactive({
      default_table()$Year_Cal %>%
        map(function(y) {
          inc <- input[[row_id("cyc_inc", y)]]; s <- input[[row_id("cyc_start", y)]]; e <- input[[row_id("cyc_end", y)]]
          if (is.null(inc) || is.null(s) || is.null(e)) return(NULL)
          tibble(Year_Cal = y, Include = inc, Start = as_date(s), End = as_date(e))
        }) %>%
        list_rbind()
    })

    observeEvent(input$upload_cycles, {
      tbl <- tryCatch(read_csv(input$upload_cycles$datapath, show_col_types = FALSE), error = function(e) NULL)
      if (is.null(tbl) || !all(c("Year_Cal", "Include", "Start", "End") %in% names(tbl))) {
        app_notify("The file needs the columns Year_Cal, Include, Start and End.", type = "error"); return()
      }
      tbl <- tbl %>% mutate(Start = parse_dates_flexible(Start), End = parse_dates_flexible(End))
      pwalk(tbl, function(Year_Cal, Include, Start, End, ...) {
        updateCheckboxInput(session, row_id("cyc_inc", Year_Cal), value = as.logical(Include))
        updateDateInput(session, row_id("cyc_start", Year_Cal), value = Start)
        updateDateInput(session, row_id("cyc_end", Year_Cal), value = End)
      })
      unknown <- setdiff(tbl$Year_Cal, default_table()$Year_Cal)
      if (length(unknown) > 0)
        app_notify(str_c("These years are not in the Analytical and were ignored: ", str_c(unknown, collapse = ", ")),
                         type = "warning")
    })

    output$download_cycles <- downloadHandler(
      filename = function() "Business Cycle Settings.csv",
      content = function(file) write_csv(cycle_table(), file)
    )

    # --- Cycles for the selected method ------------------------------------------
    cycles <- reactive({
      switch(input$cycle_method %||% "trailing",
        trailing = {
          if (!is_valid_value(input$kpi_var)) return(NULL)
          df <- upload$data()
          n <- if (is_valid_value(input$n_cycles) && input$n_cycles >= 1) floor(input$n_cycles) else Inf
          build_trailing_cycles(df[[PERIOD_COL]], df[[input$kpi_var]], label = label(), n_cycles = n)
        },
        calendar = build_calendar_cycles(periods(), label = label()),
        custom = custom_cycles(cycle_table(), label = label()))
    })

    output$cycle_preview <- renderUI({
      if (identical(input$cycle_method, "trailing") && !is_valid_value(input$kpi_var))
        return(tags$p(class = "text-muted", "Select a KPI to preview the cycles (they are anchored on the last date with KPI > 0)."))
      cyc <- cycles()
      if (is.null(cyc) || nrow(cyc) == 0)
        return(div(class = "alert alert-danger", "Not enough data to build a complete 52-week cycle. Use calendar years or a custom table."))
      counts <- cycle_period_counts(cyc, periods())
      tags$ul(pmap(counts, function(Label, Start, End, Periods) {
        tags$li(sprintf("%s: %s to %s (%d periods)%s", Label, Start, End, Periods,
                        if (Periods < WEEKS_PER_CYCLE) " - incomplete" else ""))
      }))
    })

    observeEvent(input$generate_btn, {
      header <- str_c("# Business Cycle (", names(CYCLE_METHODS)[CYCLE_METHODS == input$cycle_method], ")")
      res <- generate_kpi_code(input$kpi_var, upload$mff_vars(), upload$cs_dims(),
                               gen_log = isTRUE(input$gen_log), gen_weight = isTRUE(input$gen_weight),
                               cycles = if (isTRUE(input$gen_weight)) cycles(), cycles_header = header)
      if (length(res$errors) > 0) { notify_messages(res$errors, type = "error"); return() }
      updateTextAreaInput(session, "code_area", value = res$code)
      history$upsert(res$metadata)
      app_notify("KPI variables saved to history!", type = "message")
      notify_messages(res$warnings)
    })
  })
}
