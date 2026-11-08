# R/modules/mod_dummies_tab.R
# Dummies tab:
#   1. KPI & market: KPI in levels (log scale detected and confirmed) and the
#      holiday calendar of the project's market.
#   2. Pattern detection on the deviation of the KPI from its trend: holidays
#      (event study), yearly seasonality windows, sub-annual patterns,
#      atypical points, and how each pattern shows up across entities.
#      Suggestions only: the analyst adds what the model needs.
#   3. Dummy builder: dummies from the calendar, custom dates, an events file
#      or the suggestions; shape (ramp/decay) and CS scope; code generation.

# Categorical slots (dataviz reference palette) by pattern type; diverging
# blue/red with a neutral middle for deviations (heatmaps).
PATTERN_COLORS <- c("#eb6834", "#6250d6", "#1baf7a", "#e34948") %>% set_names(unname(PATTERN_TYPES))
DIVERGING <- list(list(0, "#e34948"), list(0.5, "#f0efec"), list(1, "#2a78d6"))
pct_label <- function(x) if_else(is.na(x), "", sprintf("%+.0f%%", 100 * x))

dummies_tab_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$br(),
    div(class = "section-card", h4("KPI & Market", class = "card-title"),
        fluidRow(
          column(4, selectizeInput(ns("kpi"), "KPI (MFF variable or existing VOF)", choices = NULL, width = "100%")),
          column(4, checkboxInput(ns("is_log"), "The KPI is in log scale (peaks are searched on exp(KPI))", FALSE),
                    uiOutput(ns("log_note"))),
          column(2, selectInput(ns("market"), "Market (holiday calendar)", choices = NULL)),
          column(2, fileInput(ns("custom_calendar"), "Extra dates (csv: Date, Name)", accept = c(".csv", ".xlsx")))
        )),
    div(class = "section-card", h4("Pattern Detection", class = "card-title"),
        fluidRow(
          column(3, selectizeInput(ns("level"), "Series", choices = NULL, width = "100%"),
                    selectInput(ns("scale"), "Deviation from the trend", choices = DEV_SCALES)),
          column(3, sliderInput(ns("min_lift"), "Minimum effect (% of the normal level)", min = 3, max = 50, value = 10, step = 1, post = "%")),
          column(3, sliderInput(ns("min_consistency"), "Minimum consistency (share of years / occurrences)", min = 0.5, max = 1, value = 0.67, step = 0.01)),
          column(2, sliderInput(ns("z_threshold"), "Atypical: robust z threshold", min = 2, max = 6, value = 3.5, step = 0.5)),
          column(1, uiOutput(ns("dummy_count")))
        ),
        tags$small(class = "text-muted",
                   "Effects are measured as the deviation of the KPI from a robust trend (13-period running median). ",
                   "Holidays: event study around each date of the calendar, aligned on the real date of every year. ",
                   "Seasonality: weeks of the year with a consistent deviation across years. Suggestions only: add what the model needs."),
        plotly::plotlyOutput(ns("overview_chart"), height = "380px"),
        div(class = "vof-pattern-legend",
            imap(PATTERN_TYPES[c("holiday", "season")], ~ span(span(class = "vof-swatch", style = str_c("background:", PATTERN_COLORS[[.x]])),
                                                              str_c(.x, " weeks (shaded)"))),
            span(span(class = "vof-swatch", style = str_c("background:", PATTERN_COLORS[[PATTERN_TYPES[["atypical"]]]])), "Atypical point")),
        tabsetPanel(id = ns("pattern_tabs"), type = "tabs",
          tabPanel(textOutput(ns("tab_holidays"), inline = TRUE), value = "holidays",
                   fluidRow(column(7, div(class = "vof-dt-card", DT::DTOutput(ns("holidays_table")))),
                            column(5, plotly::plotlyOutput(ns("event_chart"), height = "330px"))),
                   actionButton(ns("add_holidays"), "Add selected to the builder", icon = icon("plus"), class = "btn btn-primary")),
          tabPanel(textOutput(ns("tab_seasons"), inline = TRUE), value = "seasons",
                   plotly::plotlyOutput(ns("season_heatmap"), height = "300px"),
                   div(class = "vof-dt-card", DT::DTOutput(ns("seasons_table"))),
                   actionButton(ns("add_seasons"), "Add selected to the builder", icon = icon("plus"), class = "btn btn-primary")),
          tabPanel(textOutput(ns("tab_subannual"), inline = TRUE), value = "subannual",
                   tags$small(class = "text-muted", "Measured after removing the yearly profile: month turns, first/last full week of the month, 4-4-5 quarter weeks and the strongest periodicity of the autocorrelation."),
                   div(class = "vof-dt-card", DT::DTOutput(ns("subannual_table"))),
                   actionButton(ns("add_subannual"), "Add selected to the builder", icon = icon("plus"), class = "btn btn-primary")),
          tabPanel(textOutput(ns("tab_atypical"), inline = TRUE), value = "atypical",
                   tags$small(class = "text-muted", "Single-year points still out of range after the yearly profile, outside the holidays and seasonal windows found. Use them only when the model needs them."),
                   div(class = "vof-dt-card", DT::DTOutput(ns("atypical_table"))),
                   actionButton(ns("add_atypical"), "Add selected to the builder", icon = icon("plus"), class = "btn btn-primary")),
          tabPanel("Entities", value = "entities",
                   tags$small(class = "text-muted", "How the patterns found on the selected series show up in every entity. Useful to choose between a dummy for all entities or only for some."),
                   br(), actionButton(ns("run_entities"), "Analyze entities", icon = icon("search"), class = "btn btn-default"),
                   div(class = "vof-dt-card", DT::DTOutput(ns("entities_table"))),
                   plotly::plotlyOutput(ns("entities_heatmap"), height = "420px"),
                   actionButton(ns("add_entities"), "Add selected only for the entities with the effect", icon = icon("plus"), class = "btn btn-primary"))
        )),
    div(class = "section-card", h4("Dummy Builder", class = "card-title"),
        tabsetPanel(id = ns("source_tabs"), type = "pills",
          tabPanel("From calendar", br(),
                   fluidRow(column(8, selectizeInput(ns("calendar_events"), "Holidays and special dates of the market (all years in the data)",
                                                     choices = NULL, multiple = TRUE, width = "100%")),
                            column(4, br(), actionButton(ns("add_calendar"), "Add as Holiday dummies", icon = icon("plus"), class = "btn btn-primary")))),
          tabPanel("Custom dates", br(),
                   fluidRow(column(3, textInput(ns("custom_name"), "Event name", placeholder = "e.g. Price Increase May 2025")),
                            column(2, selectInput(ns("custom_category"), "Category", choices = DUMMY_CATEGORIES, selected = "Business")),
                            column(4, textAreaInput(ns("custom_dates"), "Dates (YYYY-MM-DD, comma or line separated)", rows = 2)),
                            column(3, dateRangeInput(ns("custom_range"), "Or a period (every week = 1)", start = NA, end = NA))),
                   actionButton(ns("add_custom"), "Add dummy", icon = icon("plus"), class = "btn btn-primary")),
          tabPanel("Events file", br(),
                   fluidRow(column(5, fileInput(ns("events_file"), "Events (csv/xlsx): Name, Category, Start, End, optional CS columns", accept = c(".csv", ".xlsx"))),
                            column(3, br(), downloadButton(ns("events_template"), "Download template", class = "btn btn-info custom-download-btn"))))
        ),
        tags$hr(),
        h5("Dummies to generate", class = "vof-subsection-title"),
        uiOutput(ns("conflicts_note")),
        div(class = "vof-dt-card", DT::DTOutput(ns("pending_table"))),
        actionButton(ns("remove_pending"), "Remove selected", icon = icon("trash"), class = "btn btn-danger btn-sm"),
        uiOutput(ns("editor_ui")),
        tags$hr(),
        div(class = "vof-action-row",
            actionButton(ns("generate"), "Generate Dummies Code & Save", class = "btn btn-primary vof-btn-wide"),
            metadata_download_ui(ns("meta_dl"))),
        tags$br(), tags$br(),
        textAreaInput(ns("code_area"), NULL, value = "", rows = 12, width = "100%", resize = "vertical"))
  )
}

dummies_tab_server <- function(id, upload, history) {
  moduleServer(id, function(input, output, session) {
    metadata_download_server("meta_dl", history, upload)
    ns <- session$ns
    base_calendar <- read_holiday_calendar()
    custom_calendar <- reactiveVal(NULL)
    pending <- reactiveVal(list())

    # --- KPI & market ------------------------------------------------------------
    observe({
      df <- upload$data(); req(df)
      vars <- variable_columns(df)
      kpi_hist <- history$data() %>% filter(Type == METADATA_TYPES[["kpi_log"]]) %>% pull(MainModelVariableName)
      guess <- c(intersect(kpi_hist, vars), keep(vars, ~ str_detect(.x, regex("sales|revenue|kpi", ignore_case = TRUE))))
      keep_or <- function(current, default) if (is_valid_value(current)) current else default
      updateSelectizeInput(session, "kpi", choices = c("", vars), selected = keep_or(isolate(input$kpi), first(guess, default = "")))
      updateSelectInput(session, "market", choices = market_choices(base_calendar), selected = keep_or(isolate(input$market), "US"))
    })

    log_check <- reactive({ req(input$kpi, upload$data()); detect_log_scale(input$kpi, upload$data()[[input$kpi]]) })
    observeEvent(input$kpi, { req(input$kpi); updateCheckboxInput(session, "is_log", value = log_check()$is_log) })
    output$log_note <- renderUI({
      lc <- log_check()
      tags$small(class = "text-muted", if (lc$is_log) str_c("Detected as log: ", lc$reason, ". Uncheck if it is in levels.")
                                       else "Detected in levels. Check it if the KPI comes in logs from the MFF.")
    })

    observeEvent(input$custom_calendar, {
      tryCatch({
        custom_calendar(read_custom_calendar(input$custom_calendar$datapath, input$custom_calendar$name))
        app_notify(sprintf("%d extra dates loaded.", nrow(custom_calendar())), type = "message")
      }, error = function(e) app_notify(conditionMessage(e), type = "error"))
    })

    periods <- reactive({ req(upload$data()); analytical_periods(upload$data()) })
    events <- reactive({
      req(input$market)
      cal <- bind_rows(base_calendar, custom_calendar())
      calendar_in_periods(cal, c(input$market, "CUSTOM"), periods())
    })

    observe({
      ev <- events()
      choices <- ev %>% count(Name, Category) %>% arrange(Category != "Public holiday", Name) %>%
        mutate(label = str_c(Name, " (", Category, ", ", n, " dates)"))
      updateSelectizeInput(session, "calendar_events", choices = set_names(choices$Name, choices$label))
    })

    # --- Peak detection ------------------------------------------------------------
    level_data <- reactive({ req(input$kpi, upload$data()); kpi_level_series(upload$data(), input$kpi, upload$cs_dims(), isTRUE(input$is_log)) })
    observe({
      ents <- sort(unique(level_data()$Entity))
      updateSelectizeInput(session, "level", choices = c("Total of all entities" = "__total__", ents),
                           selected = if (is_valid_value(isolate(input$level))) isolate(input$level) else "__total__")
    })
    series <- reactive({
      lv <- level_data()
      if (is.null(input$level) || input$level %in% c("", "__total__")) total_series(lv)
      else lv %>% filter(Entity == input$level) %>% select(Period, value) %>% arrange(Period)
    })
    # Default scale: relative for positive KPIs (sales, traffic), additive when the
    # series can be negative (net adds, indexes). Re-evaluated when the log flag changes.
    observeEvent(list(input$kpi, input$is_log), {
      req(input$kpi, upload$data())
      updateSelectInput(session, "scale", selected = choose_scale(total_series(level_data())$value))
    }, priority = -1)

    patterns <- reactive({
      detect_patterns(series(), events(), scale = input$scale %||% "relative", min_lift = input$min_lift / 100,
                      min_consistency = input$min_consistency, z_threshold = input$z_threshold)
    })
    level_entity <- reactive(if (is.null(input$level) || input$level %in% c("", "__total__")) "" else input$level)

    output$dummy_count <- renderUI({
      n <- history$data() %>% filter(Type == METADATA_TYPES[["dummy"]]) %>% pull(MainModelVariableName) %>% n_distinct()
      div(class = "vof-dummy-count", tags$b(n), tags$br(), "dummies", tags$br(),
          tags$small(class = "text-muted", sprintf("%d periods", length(periods()))))
    })

    count_label <- function(name, tbl) {
      n <- if (nrow(tbl) == 0) 0 else sum(tbl$Passes)
      str_c(name, " (", n, ")")
    }
    output$tab_holidays <- renderText(count_label("Holidays", patterns()$holidays))
    output$tab_seasons <- renderText(count_label("Seasonality", patterns()$seasons))
    output$tab_subannual <- renderText(count_label("Sub-annual", patterns()$subannual))
    output$tab_atypical <- renderText(count_label("Atypical", patterns()$atypical))

    # --- Overview chart: KPI, trend and the windows of the patterns found --------------
    output$overview_chart <- plotly::renderPlotly({
      pt <- patterns(); dev <- pt$dev
      p <- plotly::plot_ly() %>%
        plotly::add_lines(data = dev, x = ~Period, y = ~value, name = "KPI (levels)", line = list(color = "#2a78d6", width = 2),
                          hovertemplate = "%{y:,.2f}<extra>KPI</extra>") %>%
        plotly::add_lines(data = dev, x = ~Period, y = ~trend, name = "Trend", line = list(color = "#898781", width = 1.5, dash = "dash"),
                          hovertemplate = "%{y:,.2f}<extra>Trend</extra>")
      step <- median_step(dev$Period) %|NA|% 7
      shade <- function(tbl, type) {
        if (nrow(tbl) == 0) return(list())
        tbl <- tbl %>% filter(Passes)
        map(seq_len(nrow(tbl)), function(i) map(tbl$Dates[[i]], function(d) list(
          type = "rect", xref = "x", yref = "paper", x0 = d - step / 2, x1 = d + step / 2, y0 = 0, y1 = 1,
          fillcolor = PATTERN_COLORS[[type]], opacity = 0.18, line = list(width = 0), layer = "below"))) %>% flatten()
      }
      shapes <- c(shade(pt$holidays, PATTERN_TYPES[["holiday"]]), shade(pt$seasons, PATTERN_TYPES[["season"]]))
      at <- pt$atypical
      if (nrow(at) > 0) {
        at_pts <- tibble(Period = as_date(unlist(at$Dates)), label = at$Pattern, lift = pct_label(at$Lift)) %>%
          left_join(dev %>% select(Period, value), by = "Period")
        p <- plotly::add_markers(p, data = at_pts, x = ~Period, y = ~value, name = "Atypical",
                                 marker = list(color = PATTERN_COLORS[[PATTERN_TYPES[["atypical"]]]], size = 11, symbol = "diamond",
                                               line = list(color = "#ffffff", width = 2)),
                                 text = ~str_c(label, " (", lift, " vs normal)"), hovertemplate = "%{text}<extra></extra>")
      }
      p %>% plotly::layout(shapes = shapes, hovermode = "x unified", legend = list(orientation = "h", y = -0.2),
                           xaxis = list(title = "", gridcolor = "#ecebe7"), yaxis = list(title = "", gridcolor = "#ecebe7", rangemode = "tozero")) %>%
        plotly::config(displaylogo = FALSE)
    })

    dt_table <- function(tbl, message, selected = NULL) {
      if (is.null(tbl) || nrow(tbl) == 0) tbl <- tibble(Message = message)
      DT::datatable(tbl, rownames = FALSE, class = "compact stripe hover", selection = list(mode = "multiple", selected = selected),
                    options = list(scrollX = TRUE, scrollY = "300px", scrollCollapse = TRUE, pageLength = 100, dom = "ftip"))
    }
    dates_label <- function(d) str_c(format(sort(d)), collapse = ", ")

    # --- Holidays ------------------------------------------------------------------------
    output$holidays_table <- DT::renderDT({
      h <- patterns()$holidays
      tbl <- if (nrow(h) > 0) h %>% transmute(
        Holiday = Pattern, Suggested = if_else(Passes, "Yes", ""), Effect = pct_label(Lift),
        `Peak week` = case_when(Offset == 0 ~ "Event week", Offset < 0 ~ str_c(-Offset, " week before"), TRUE ~ str_c(Offset, " week after")),
        Consistency = sprintf("%.0f%% of %d years", 100 * Consistency, Years),
        `Suggested shape` = str_squish(str_c(if_else(Ramp > 0, str_c("Ramp", Ramp), ""), " ", if_else(Decay > 0, str_c("Decay", Decay), ""))),
        `Same weeks as` = SameWeeks)
      dt_table(tbl, "No calendar dates inside the data.", selected = if (nrow(h) > 0) which(h$Passes) else NULL)
    })

    output$event_chart <- plotly::renderPlotly({
      h <- patterns()$holidays; req(nrow(h) > 0)
      i <- input$holidays_table_row_last_clicked %||% 1   # the row the analyst clicked
      if (i > nrow(h)) i <- 1
      row <- h[i, ]; mat <- row$Matrix[[1]]; ks <- row$Profile[[1]]$k
      p <- plotly::plot_ly()
      for (j in seq_len(nrow(mat))) {
        p <- plotly::add_lines(p, x = ks, y = mat[j, ] * 100, name = as.character(year(row$EventPeriods[[1]][j])),
                               line = list(color = "#b5b3ad", width = 1.2),
                               hovertemplate = str_c("%{y:+.0f}%<extra>", year(row$EventPeriods[[1]][j]), "</extra>"))
      }
      p %>% plotly::add_lines(x = ks, y = row$Profile[[1]]$median * 100, name = "Median", line = list(color = "#2a78d6", width = 3),
                              hovertemplate = "%{y:+.0f}%<extra>Median</extra>") %>%
        plotly::layout(title = list(text = str_c(row$Pattern, ": deviation around the event"), x = 0, font = list(size = 13)),
                       xaxis = list(title = "Periods from the event week", dtick = 1, zeroline = TRUE),
                       yaxis = list(title = "% vs normal level", ticksuffix = "%", zeroline = TRUE, zerolinecolor = "#898781"),
                       shapes = list(list(type = "rect", xref = "x", yref = "paper", x0 = row$Offset - 0.5, x1 = row$Offset + 0.5,
                                          y0 = 0, y1 = 1, fillcolor = PATTERN_COLORS[[1]], opacity = 0.12, line = list(width = 0))),
                       showlegend = FALSE, hovermode = "x unified") %>%
        plotly::config(displaylogo = FALSE)
    })

    # --- Seasonality ---------------------------------------------------------------------
    output$season_heatmap <- plotly::renderPlotly({
      m <- seasonality_matrix(patterns()$dev)
      lim <- max(0.05, stats::quantile(abs(m$dev), 0.95, na.rm = TRUE))
      plotly::plot_ly(m, x = ~woy, y = ~factor(year), z = ~pmax(pmin(dev, lim), -lim) * 100, type = "heatmap",
                      colorscale = DIVERGING, zmid = 0, zmin = -lim * 100, zmax = lim * 100,
                      text = ~sprintf("%s week %d: %+.0f%%", year, woy, 100 * dev), hovertemplate = "%{text}<extra></extra>",
                      colorbar = list(title = "% vs normal", ticksuffix = "%")) %>%
        plotly::layout(title = list(text = "Deviation by week of the year (blue above normal, red below)", x = 0, font = list(size = 13)),
                       xaxis = list(title = "Week of the year", dtick = 4), yaxis = list(title = "")) %>%
        plotly::config(displaylogo = FALSE)
    })

    output$seasons_table <- DT::renderDT({
      sw <- patterns()$seasons
      tbl <- if (nrow(sw) > 0) sw %>% transmute(
        Window = Pattern, Weeks = str_c(Start, "-", End), Effect = pct_label(Lift), Strongest = pct_label(Peak),
        Consistency = sprintf("%.0f%% of %d years", 100 * Consistency, Years),
        `Includes holidays` = Covers, `Suggested dummy` = str_c("Season ", Pattern))
      dt_table(tbl, "No consistent yearly window with these thresholds.")
    })

    # --- Sub-annual / atypical ----------------------------------------------------------
    output$subannual_table <- DT::renderDT({
      sa <- patterns()$subannual
      tbl <- if (nrow(sa) > 0) sa %>% transmute(Pattern, Suggested = if_else(Passes, "Yes", ""), Effect = pct_label(Lift),
                                                Consistency = sprintf("%.0f%% of %d periods", 100 * Consistency, Occurrences),
                                                `Suggested dummy` = str_c("Season ", Pattern))
      dt_table(tbl, "Not enough data.", selected = if (nrow(sa) > 0) which(sa$Passes) else NULL)
    })

    output$atypical_table <- DT::renderDT({
      at <- patterns()$atypical
      tbl <- if (nrow(at) > 0) at %>% transmute(Period = map_chr(Dates, dates_label), Effect = pct_label(Lift),
                                                Strength = round(abs(z), 1), `Suggested dummy` = str_c("Business Atypical ", Pattern))
      dt_table(tbl, "No atypical points with these thresholds.")
    })

    # --- Entities ----------------------------------------------------------------------------
    entity_effects <- reactiveVal(NULL)
    observeEvent(list(input$level, input$kpi, input$market), entity_effects(NULL))
    observeEvent(input$run_entities, {
      pt <- patterns()
      pats <- bind_rows(pt$holidays %>% filter(Passes), pt$seasons, pt$subannual %>% filter(Passes))
      if (nrow(pats) == 0) { app_notify("No patterns to analyze with these thresholds.", type = "warning"); return() }
      withProgress(message = "Analyzing entities", {
        entity_effects(list(patterns = pats,
                            effects = entity_pattern_effects(level_data(), pats, input$scale, input$min_lift / 100, input$min_consistency)))
      })
    })
    entity_summary <- reactive({ ee <- entity_effects(); req(ee); entity_pattern_summary(ee$effects) })

    output$entities_table <- DT::renderDT({
      ee <- entity_effects()
      if (is.null(ee)) return(dt_table(NULL, "Click 'Analyze entities' to compute the patterns in every entity."))
      dt_table(entity_summary() %>% transmute(Pattern, Type, `Total effect` = pct_label(`Total lift`),
                                             `Entities with the effect` = str_c(`Entities with the effect`, " of ", Entities),
                                             `Median entity effect` = pct_label(`Median entity lift`), `Without the effect`), "")
    })

    output$entities_heatmap <- plotly::renderPlotly({
      ee <- entity_effects(); req(ee)
      e <- ee$effects %>% mutate(Pattern = factor(Pattern, levels = entity_summary()$Pattern))
      lim <- max(0.05, stats::quantile(abs(e$Lift), 0.95, na.rm = TRUE))
      plotly::plot_ly(e, x = ~Pattern, y = ~Entity, z = ~pmax(pmin(Lift, lim), -lim) * 100, type = "heatmap",
                      colorscale = DIVERGING, zmid = 0, zmin = -lim * 100, zmax = lim * 100,
                      text = ~sprintf("%s - %s: %+.0f%%%s", Entity, Pattern, 100 * Lift, if_else(Effect, "", " (no consistent effect)")),
                      hovertemplate = "%{text}<extra></extra>", colorbar = list(title = "% vs normal", ticksuffix = "%")) %>%
        plotly::layout(xaxis = list(title = "", tickangle = -30, automargin = TRUE), yaxis = list(title = "", automargin = TRUE),
                       margin = list(b = 120)) %>%
        plotly::config(displaylogo = FALSE)
    })

    # --- From detected patterns to dummies -----------------------------------------------
    entity_cs <- function(entities) {
      dims <- upload$cs_dims()
      if (length(entities) == 0 || length(dims) == 0 || all(entities == "")) return(list())
      parts <- str_split(entities, fixed(" | "))
      dims %>% set_names() %>% imap(function(d, nm) unique(map_chr(parts, ~ .x[match(nm, dims)])))
    }
    add_from <- function(tbl, rows) {
      if (length(rows) == 0 || nrow(tbl) == 0) { app_notify("Select rows in the table first.", type = "warning"); return() }
      cs <- entity_cs(level_entity())
      add_pending(map(rows, ~ pattern_dummy(tbl[.x, ], cs)))
    }
    observeEvent(input$add_holidays, add_from(patterns()$holidays, input$holidays_table_rows_selected))
    observeEvent(input$add_seasons, add_from(patterns()$seasons, input$seasons_table_rows_selected))
    observeEvent(input$add_subannual, add_from(patterns()$subannual, input$subannual_table_rows_selected))
    observeEvent(input$add_atypical, add_from(patterns()$atypical, input$atypical_table_rows_selected))
    observeEvent(input$add_entities, {
      ee <- entity_effects(); sel <- input$entities_table_rows_selected
      if (is.null(ee) || length(sel) == 0) { app_notify("Analyze the entities and select patterns first.", type = "warning"); return() }
      summ <- entity_summary()
      dummies <- map(sel, function(i) {
        pat <- summ$Pattern[i]
        ents <- ee$effects %>% filter(Pattern == pat, Effect) %>% pull(Entity)
        row <- ee$patterns %>% filter(Pattern == pat) %>% slice(1)
        d <- pattern_dummy(row, entity_cs(ents))
        d$note <- str_c(d$note, ", ", length(ents), " entities")
        d
      })
      add_pending(dummies)
    })

    add_pending <- function(dummies) {
      dummies <- keep(dummies, ~ length(.x$dates) > 0 || length(.x$ranges %||% list()) > 0)
      if (length(dummies) == 0) { app_notify("Nothing to add.", type = "warning"); return() }
      current <- pending()
      new_names <- map_chr(dummies, dummy_name)
      current <- keep(current, ~ !dummy_name(.x) %in% new_names)
      pending(c(current, dummies))
      app_notify(sprintf("%d dummies added to the builder.", length(dummies)), type = "message")
    }

    # --- Builder sources ---------------------------------------------------------------
    observeEvent(input$add_calendar, {
      req(input$calendar_events)
      ev <- events()
      add_pending(map(input$calendar_events, ~ new_dummy(.x, "Holiday", dates = ev$Date[ev$Name == .x], note = "Calendar")))
    })

    observeEvent(input$add_custom, {
      dates <- str_split(input$custom_dates %||% "", "[,;\\s]+")[[1]] %>% clean_values() %>% parse_dates_flexible()
      rng <- input$custom_range
      ranges <- if (length(rng) == 2 && !any(is.na(rng))) list(list(start = rng[1], end = rng[2])) else NULL
      if (!nzchar(input$custom_name %||% "")) { app_notify("Type the event name.", type = "error"); return() }
      add_pending(list(new_dummy(input$custom_name, input$custom_category, dates = dates[!is.na(dates)], ranges = ranges, note = "Custom")))
    })

    observeEvent(input$events_file, {
      tryCatch(add_pending(read_events_file(input$events_file$datapath, input$events_file$name, upload$cs_dims())),
               error = function(e) app_notify(conditionMessage(e), type = "error"))
    })
    output$events_template <- downloadHandler(filename = function() "Events template.csv",
                                              content = function(file) write_csv(EVENTS_TEMPLATE, file))

    # --- Conflicts between dummies (collinearity) -------------------------------------
    # Every pending dummy against the other pending ones and the dummies saved in
    # the metadata, on the column it produces in the Analytical.
    conflicts <- reactive({
      p <- pending(); req(upload$data())
      if (length(p) == 0) return(dummy_conflicts(tibble(name = character(0)), NULL, NULL, character(0)))
      gen <- map(p, ~ generate_dummy(.x, periods(), upload$cs_dims()))
      pend <- tibble(name = map_chr(p, dummy_name), category = map_chr(p, "category"), code = map_chr(gen, ~ .x$code %||% ""))
      h <- history$data()
      saved <- if (nrow(h) > 0 && METADATA_CODE_COL %in% names(h)) {
        h %>% filter(Type == METADATA_TYPES[["dummy"]]) %>% distinct(MainModelVariableName, .keep_all = TRUE) %>%
          transmute(name = MainModelVariableName, category = VofVariableName, code = .data[[METADATA_CODE_COL]])
      }
      dummy_conflicts(pend, saved, upload$data(), upload$cs_dims())
    })
    blocked_pairs <- reactive(conflicts() %>% filter(Level == DUMMY_CONFLICT_LEVELS[["blocked"]]))

    output$conflicts_note <- renderUI({
      b <- blocked_pairs()
      if (nrow(b) == 0) return(NULL)
      div(class = "vof-dummy-blocked", icon("ban"),
          sprintf("%d pair(s) of dummies switch on in the same periods: remove one of each pair before generating (%s).",
                  nrow(b), str_c(sprintf("%s / %s%s", b$Dummy, b$Other, if_else(b$OtherSource == "saved", " (saved)", "")), collapse = "; ")))
    })

    # --- Pending dummies ------------------------------------------------------------------
    output$pending_table <- DT::renderDT({
      p <- pending()
      tbl <- if (length(p) == 0) tibble(Message = "Add dummies from the suggestions, the calendar, custom dates or an events file.") else
        tibble(Name = map_chr(p, dummy_name), Category = map_chr(p, "category"),
               Dates = map_chr(p, ~ str_c(c(format(head(.x$dates, 4)), if (length(.x$dates) > 4) "...",
                                            map_chr(.x$ranges %||% list(), function(r) str_c(r$start, " to ", r$end))), collapse = ", ")),
               Shape = map_chr(p, ~ if (nzchar(shape_suffix(.x$shape))) shape_suffix(.x$shape) else "Exact period"),
               Scope = map_chr(p, ~ if (length(.x$cs) == 0) "All entities" else str_c(imap_chr(.x$cs, ~ describe_cs_filter(.y, .x)), collapse = "; ")),
               Source = map_chr(p, "note"),
               Conflicts = dummy_conflict_summary(conflicts(), map_chr(p, dummy_name))) %>%
        mutate(Level = coalesce(str_extract(Conflicts, "^[A-Za-z]+"), ""))
      hidden <- if ("Level" %in% names(tbl)) list(list(visible = FALSE, targets = which(names(tbl) == "Level") - 1)) else list()
      dt <- DT::datatable(tbl, rownames = FALSE, class = "compact stripe hover", selection = "single",
                          options = list(scrollX = TRUE, scrollY = "220px", scrollCollapse = TRUE, pageLength = 100, dom = "tip",
                                         columnDefs = hidden))
      if ("Level" %in% names(tbl))
        dt <- dt %>% DT::formatStyle("Conflicts", valueColumns = "Level",
                                     backgroundColor = DT::styleEqual(unname(DUMMY_CONFLICT_LEVELS), c("#fdecea", "#fdf3e7", "#eef4fc")))
      dt
    })

    selected_index <- reactive({
      i <- input$pending_table_rows_selected
      if (length(i) == 1 && i <= length(pending())) i else NULL
    })

    observeEvent(input$remove_pending, {
      i <- selected_index(); req(i)
      p <- pending(); p[[i]] <- NULL; pending(p)
    })

    # Editor of the selected dummy: name, category, shape and CS scope.
    output$editor_ui <- renderUI({
      i <- selected_index()
      if (is.null(i)) return(tags$p(class = "text-muted", "Select a dummy in the table to edit its shape and scope."))
      d <- isolate(pending())[[i]]
      dims <- upload$cs_dims()
      param_ui <- function(side) {
        sh <- d$shape
        tagList(
          numericInput(ns(str_c(side, "_n")), str_c(if (side == "ramp") "Ramp: periods before" else "Decay: periods after"),
                       value = sh[[side]], min = 0, max = 26, step = 1),
          selectInput(ns(str_c(side, "_profile")), "Profile", choices = DUMMY_PROFILES, selected = sh[[str_c(side, "_profile")]]),
          numericInput(ns(str_c(side, "_param")), if (side == "ramp") "Linear: first value / Exponential: rate" else "Linear: last value / Exponential: rate",
                       value = sh[[str_c(side, "_param")]], min = 0, max = 1, step = 0.05),
          textInput(ns(str_c(side, "_weights")), "Custom weights (comma separated)",
                    value = str_c(sh[[str_c(side, "_weights")]] %||% character(0), collapse = ", ")))
      }
      div(class = "vof-dummy-editor",
          h5(class = "vof-subsection-title", str_c("Edit: ", dummy_name(d))),
          fluidRow(
            column(3, textInput(ns("edit_event"), "Event name", value = d$event),
                      selectInput(ns("edit_category"), "Category", choices = DUMMY_CATEGORIES, selected = d$category)),
            column(3, param_ui("ramp")),
            column(3, param_ui("decay")),
            column(3, map(dims, function(dim) tagList(
              shinyWidgets::radioGroupButtons(ns(str_c("edit_mode_", dim)), dim, choices = CS_FILTER_MODES,
                                              selected = d$cs[[dim]]$mode %||% "include", size = "xs"),
              shinyWidgets::pickerInput(ns(str_c("edit_cs_", dim)), NULL, choices = cs_dim_values(upload$data(), dim),
                                        selected = if (!is.null(d$cs[[dim]])) d$cs[[dim]]$levels[[1]]$values, multiple = TRUE,
                                        options = list(`actions-box` = TRUE, `live-search` = TRUE, `none-selected-text` = "All entities")))))
          ),
          actionButton(ns("apply_edit"), "Apply changes", icon = icon("check"), class = "btn btn-success btn-sm"),
          plotly::plotlyOutput(ns("weights_chart"), height = "220px"))
    })

    edited_dummy <- reactive({
      i <- selected_index(); req(i)
      d <- pending()[[i]]
      shape <- list(ramp = input$ramp_n %||% 0, ramp_profile = input$ramp_profile %||% "linear",
                    ramp_param = input$ramp_param %||% NA, ramp_weights = str_split(input$ramp_weights %||% "", ",")[[1]] %>% str_trim() %>% clean_values(),
                    decay = input$decay_n %||% 0, decay_profile = input$decay_profile %||% "linear",
                    decay_param = input$decay_param %||% NA, decay_weights = str_split(input$decay_weights %||% "", ",")[[1]] %>% str_trim() %>% clean_values())
      shape$ramp <- max(0, floor(shape$ramp %|NA|% 0)); shape$decay <- max(0, floor(shape$decay %|NA|% 0))
      cs <- upload$cs_dims() %>% set_names() %>% map(function(dim) {
        vals <- input[[str_c("edit_cs_", dim)]]
        if (length(vals) == 0) return(NULL)
        new_cs_filter(vals, input[[str_c("edit_mode_", dim)]] %||% "include")
      }) %>% compact()
      new_dummy(input$edit_event %||% d$event, input$edit_category %||% d$category, dates = d$dates, ranges = d$ranges,
                shape = shape, cs = cs, note = d$note)
    })

    observeEvent(input$apply_edit, {
      i <- selected_index(); req(i)
      d <- tryCatch({ x <- edited_dummy(); dummy_weights(x, periods()); x }, error = function(e) { app_notify(conditionMessage(e), type = "error"); NULL })
      req(d)
      p <- pending(); p[[i]] <- d; pending(p)
      app_notify(str_c("Updated: ", dummy_name(d)), type = "message")
    })

    output$weights_chart <- plotly::renderPlotly({
      d <- edited_dummy()
      w <- tryCatch(dummy_weights(d, periods()), error = function(e) NULL)
      req(w, nrow(w) > 0)
      plotly::plot_ly(w, x = ~Period, y = ~weight, type = "bar", marker = list(color = "#2a78d6"),
                      hovertemplate = "%{x}: %{y}<extra></extra>") %>%
        plotly::layout(title = list(text = str_c("Weights: ", dummy_name(d)), x = 0, font = list(size = 13)),
                       xaxis = list(title = ""), yaxis = list(title = "", range = c(0, 1.05)), bargap = 0.3) %>%
        plotly::config(displaylogo = FALSE)
    })

    # --- Generation ------------------------------------------------------------------------
    observeEvent(input$generate, {
      p <- pending()
      if (length(p) == 0) { app_notify("Add dummies first.", type = "warning"); return() }
      b <- blocked_pairs()
      if (nrow(b) > 0) {
        notify_messages(sprintf("'%s' and '%s'%s %s. Remove one of them.", b$Dummy, b$Other,
                                if_else(b$OtherSource == "saved", " (saved in the metadata)", ""), b$Detail), type = "error")
        return()
      }
      existing <- unique(c(names(upload$data()), history$data()$MainModelVariableName))
      res <- map(p, ~ generate_dummy(.x, periods(), upload$cs_dims(), existing))
      errors <- res %>% map("errors") %>% unlist()
      ok <- keep(res, ~ length(.x$errors) == 0)
      if (length(ok) > 0) {
        history$upsert(map(ok, "metadata") %>% list_rbind())
        updateTextAreaInput(session, "code_area", value = str_c(str_c(map_chr(ok, "code"), collapse = "\n\n"), "\n"))
        pending(keep(p, function(d) !dummy_name(d) %in% map_chr(ok, "name")))
        app_notify(sprintf("%d dummies saved to history.", length(ok)), type = "message")
      }
      notify_messages(c(errors, unlist(map(ok, "warnings"))), type = if (length(errors) > 0) "error" else "warning")
    })
  })
}
