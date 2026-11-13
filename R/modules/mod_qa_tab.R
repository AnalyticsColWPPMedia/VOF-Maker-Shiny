# R/modules/mod_qa_tab.R
# VOF QA tab. The unit of review is a VOF group: a VOF and its twin (same name
# without the "---<metric>" suffix), so the twins are checked together.
#   * Charts:  each VOF of the group vs its root variables (stacked areas by
#              default when it sums variables, or lines), AWB values when the
#              VOF already exists, and the period cuts.
#   * Summary: how the VOF is built (checks + components), entities, and the
#              status of every VOF of the project.

# Fixed categorical order (dataviz reference palette): slot 1 is the VOF, the
# following slots go to the root variables in the order they appear.
QA_COLORS <- list(vof = "#2a78d6", awb = "#52514e", cut = "#898781",
                  roots = c("#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300", "#6250d6", "#e34948"),
                  other = "#b5b3ad")
QA_STATUS <- tibble::tribble(
  ~Status,            ~icon,                  ~color,    ~meaning,
  "OK",               "check-circle",         "#0ca30c", "The code runs and the values look consistent.",
  "Check",            "info-circle",          "#fab219", "The VOF has missing (NA) or negative values. VOFs start at 0, so this points to NAs or negatives in the roots.",
  "All zero",         "exclamation-triangle", "#ec835a", "A media VOF that is 0 everywhere: its filters (periods or entities) leave out all the data.",
  "Differs from AWB", "exclamation-triangle", "#ec835a", "The VOF already exists in the Analytical with other values: it was built differently in AWB, or the configuration changed.",
  "Error",            "times-circle",         "#d03b3b", "The code fails or is not available (non-media VOF imported from a metadata file without code)."
)

QA_MAX_GROUP <- 4

status_badge <- function(status, label = status) {
  st <- QA_STATUS %>% filter(Status == !!status)
  span(class = "vof-qa-badge", style = str_c("border-color:", st$color),
       icon(st$icon, style = str_c("color:", st$color)), label)
}

qa_tab_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$br(),
    div(class = "section-card", h4("VOF Selection", class = "card-title"),
        fluidRow(
          column(5, selectizeInput(ns("group"), "VOF (activity and spend twins are reviewed together)", choices = NULL, width = "100%")),
          column(4, uiOutput(ns("entity_ui"))),
          column(3, uiOutput(ns("view_ui")))
        ),
        uiOutput(ns("header_ui"))
    ),
    tabsetPanel(id = ns("qa_tabs"), type = "tabs",
      tabPanel("Charts", uiOutput(ns("charts_ui"))),
      tabPanel("Summary",
        uiOutput(ns("build_ui")),
        div(class = "section-card", h4("Entities", class = "card-title"),
            tags$small(class = "text-muted", "Whether the filters keep each cross-sectional entity and whether the VOFs have data there (totals over all periods)."),
            div(class = "vof-dt-card", DT::DTOutput(ns("entity_table")))),
        div(class = "section-card", h4("Status of all VOFs", class = "card-title"),
            uiOutput(ns("status_legend")),
            div(class = "vof-dt-card", DT::DTOutput(ns("checks_table")))))
    )
  )
}

qa_tab_server <- function(id, upload, history) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Lazy: only recomputed when a QA output is visible and the history changes.
    qa <- reactive({
      req(upload$data(), nrow(history$data()) > 0)
      qa_compute(upload$data(), history$data(), upload$cs_dims())
    })
    checks <- reactive(qa_checks(qa()))

    observe({
      groups <- qa_groups(qa())
      selected <- isolate(input$group)
      updateSelectizeInput(session, "group", choices = groups,
                           selected = if (!is.null(selected) && selected %in% groups) selected else last(groups))
    })

    group_blocks <- reactive({ req(input$group); qa_group_blocks(qa(), input$group) })
    kind <- reactive({
      comps <- group_blocks()$components %>% keep(~ length(.x) > 0)
      qa_vof_kind(if (length(comps) > 0) comps[[1]] else list())
    })

    entities <- reactive({ req(upload$data()); sort(unique(qa_entity(upload$data(), upload$cs_dims()))) })
    output$entity_ui <- renderUI({
      shinyWidgets::pickerInput(ns("entities"), "Cross-sectional entities (none = total of all)",
                                choices = entities(), multiple = TRUE, width = "100%",
                                options = list(`actions-box` = TRUE, `live-search` = TRUE,
                                               `selected-text-format` = "count > 2", `none-selected-text` = "All entities (total)"))
    })

    # Stacked areas only make sense when the VOF sums several variables.
    # The Y axis from 0 can be switched off to see series that move in a narrow
    # band (e.g. an index between 90 and 100).
    output$view_ui <- renderUI({
      tagList(
        if (kind()$sum)
          shinyWidgets::radioGroupButtons(ns("view"), "Chart", choices = c("Stacked areas" = "stack", "Lines" = "lines"),
                                          selected = isolate(input$view) %||% "stack", size = "sm"),
        checkboxInput(ns("y_zero"), "Y axis from 0", value = isolate(input$y_zero) %||% TRUE))
    })

    # --- Header: type of VOF, status of each twin, CS filter notice -----------------
    output$header_ui <- renderUI({
      b <- group_blocks(); req(nrow(b) > 0)
      ch <- checks() %>% filter(VOF %in% b$name)
      k <- kind()
      cs_note <- NULL
      if (k$cs) {
        comps <- b$components[[1]]
        dims <- unique(unlist(map(comps, ~ names(.x$cs))))
        cs_note <- div(class = "vof-qa-note", icon("filter"),
                       sprintf(" This VOF filters by %s: entities left out by the filter are 0. If you select entities in the chart, some of them may be 0.",
                               str_c(dims, collapse = " and ")))
      }
      tagList(
        div(class = "vof-qa-status",
            if (length(k$labels) > 0) span(class = "vof-qa-kinds", map(k$labels, ~ span(class = "vof-qa-kind", .x))),
            map(seq_len(nrow(ch)), function(i) status_badge(ch$Status[i], str_c(ch$Metric[i], ": ", ch$Status[i])))),
        cs_note)
    })

    # --- Charts -------------------------------------------------------------------
    build_chart <- function(vof, metric, view, components) {
      s <- qa_series(qa(), vof, input$entities)
      if (is.null(s)) return(plotly::plotly_empty() %>% plotly::layout(title = list(text = str_c(vof, ": not computed"))))
      roots <- unique(s$series[s$kind == "Root"])
      # More roots than categorical slots fold into "Other roots".
      if (length(roots) > length(QA_COLORS$roots)) {
        keep_roots <- s %>% filter(kind == "Root") %>% group_by(series) %>% summarise(t = sum(value)) %>%
          arrange(desc(t)) %>% slice_head(n = length(QA_COLORS$roots) - 1) %>% pull(series)
        s <- s %>% mutate(series = if_else(kind == "Root" & !series %in% keep_roots, "Other roots", series)) %>%
          group_by(Period, series, kind) %>% summarise(value = sum(value), .groups = "drop")
        roots <- c(keep_roots, "Other roots")
      }
      colors <- set_names(c(QA_COLORS$roots, QA_COLORS$other)[seq_along(roots)], roots)
      if ("Other roots" %in% roots) colors[["Other roots"]] <- QA_COLORS$other

      # A KPI in log and its original variable have different scales: the roots
      # go to their own chart below.
      vof_type <- qa()$blocks$type[qa()$blocks$name == vof][1]
      split_roots <- identical(vof_type, METADATA_TYPES[["kpi_log"]]) && length(roots) > 0
      y_axis <- list(title = "", gridcolor = "#ecebe7", rangemode = if (isFALSE(input$y_zero)) "normal" else "tozero")
      p_roots <- plotly::plot_ly()
      p <- plotly::plot_ly()
      for (r in roots) {
        d <- s %>% filter(kind == "Root", series == r) %>% arrange(Period)
        if (identical(view, "stack")) {
          p <- plotly::add_trace(p, data = d, x = ~Period, y = ~value, name = r, type = "scatter", mode = "lines",
                                 stackgroup = "roots", fillcolor = colors[[r]], line = list(color = "#ffffff", width = 1),
                                 hovertemplate = str_c("%{y:,.2f}<extra>", r, "</extra>"))
        } else if (split_roots) {
          p_roots <- plotly::add_lines(p_roots, data = d, x = ~Period, y = ~value, name = str_c(r, " (original)"),
                                       line = list(color = colors[[r]], width = 1.5),
                                       hovertemplate = str_c("%{y:,.2f}<extra>", r, "</extra>"))
        } else {
          p <- plotly::add_lines(p, data = d, x = ~Period, y = ~value, name = r, line = list(color = colors[[r]], width = 1.5),
                                 hovertemplate = str_c("%{y:,.2f}<extra>", r, "</extra>"))
        }
      }
      awb <- s %>% filter(kind == "AWB")
      if (nrow(awb) > 0)
        p <- plotly::add_lines(p, data = awb, x = ~Period, y = ~value, name = "AWB (Analytical)",
                               line = list(color = QA_COLORS$awb, width = 2, dash = "dash"),
                               hovertemplate = "%{y:,.2f}<extra>AWB (Analytical)</extra>")
      p <- plotly::add_lines(p, data = s %>% filter(kind == "VOF"), x = ~Period, y = ~value, name = str_c("VOF (", metric, ")"),
                             line = list(color = QA_COLORS$vof, width = 3),
                             hovertemplate = str_c("%{y:,.2f}<extra>VOF</extra>"))

      cuts <- qa_period_cuts(components)
      shapes <- map(seq_len(nrow(cuts)), function(i) list(type = "line", xref = "x", yref = "paper", y0 = 0, y1 = 1,
                                                          x0 = cuts$date[i], x1 = cuts$date[i],
                                                          line = list(color = QA_COLORS$cut, width = 1, dash = "dot")))
      notes <- map(seq_len(nrow(cuts)), function(i) list(x = cuts$date[i], y = 1, xref = "x", yref = "paper", showarrow = FALSE,
                                                         text = str_c(if (cuts$edge[i] == "start") "starts " else "ends ", cuts$date[i]),
                                                         font = list(size = 10, color = "#52514e"), yanchor = "bottom"))
      scope <- if (length(input$entities) == 0) "total of all entities" else str_c("total of ", length(input$entities), " selected entities")
      if (split_roots) {
        p <- plotly::subplot(p %>% plotly::layout(yaxis = y_axis), p_roots %>% plotly::layout(yaxis = y_axis),
                             nrows = 2, shareX = TRUE, heights = c(0.55, 0.45), titleY = FALSE, margin = 0.06)
        return(p %>% plotly::layout(
          title = list(text = str_c(metric, "<br><sup>", vof, " - ", scope, " (top: VOF in log; bottom: original variable)</sup>"),
                       x = 0, font = list(size = 15)),
          hovermode = "x unified", legend = list(orientation = "h", y = -0.12),
          xaxis2 = list(title = "", gridcolor = "#ecebe7"), yaxis = y_axis, yaxis2 = y_axis,
          shapes = shapes, annotations = notes, margin = list(t = 80)) %>%
          plotly::config(displaylogo = FALSE))
      }
      p %>% plotly::layout(
        title = list(text = str_c(metric, "<br><sup>", vof, " - ", scope, "</sup>"), x = 0, font = list(size = 15)),
        hovermode = "x unified", legend = list(orientation = "h", y = -0.18),
        xaxis = list(title = "", gridcolor = "#ecebe7"), yaxis = y_axis,
        shapes = shapes, annotations = notes, margin = list(t = 80)) %>%
        plotly::config(displaylogo = FALSE)
    }

    output$charts_ui <- renderUI({
      b <- group_blocks() %>% head(QA_MAX_GROUP); req(nrow(b) > 0)
      div(class = "section-card",
          map(seq_len(nrow(b)), function(i) tagList(if (i > 1) tags$hr(), plotly::plotlyOutput(ns(str_c("chart_", i)),
                                                                                  height = if (identical(b$type[i], METADATA_TYPES[["kpi_log"]])) "620px" else "420px"))))
    })
    # A group holds a VOF and its twin (up to QA_MAX_GROUP for older projects).
    walk(seq_len(QA_MAX_GROUP), function(j) {
      output[[str_c("chart_", j)]] <- plotly::renderPlotly({
        bb <- group_blocks(); req(nrow(bb) >= j)
        view <- if (kind()$sum) input$view %||% "stack" else "lines"
        build_chart(bb$name[j], bb$metric[j], view, bb$components[[j]])
      })
    })

    # --- Summary: how the VOF is built -------------------------------------------------
    checks_table_ui <- function(tbl) {
      tags$table(class = "table table-condensed vof-check-table",
        tags$tbody(map(seq_len(nrow(tbl)), function(i) tags$tr(
          tags$td(if (tbl$OK[i]) icon("check-circle", style = "color:#0ca30c") else icon("times-circle", style = "color:#d03b3b")),
          tags$td(tags$b(tbl$Check[i])), tags$td(tbl$Result[i]), tags$td(class = "text-muted", tbl$Detail[i])))))
    }

    output$build_ui <- renderUI({
      b <- group_blocks() %>% head(QA_MAX_GROUP); req(nrow(b) > 0)
      twin <- qa_twin_check(qa(), input$group, upload$long_dims())
      div(class = "section-card", h4("How this VOF is built", class = "card-title"),
          if (length(kind()$labels) > 0) p(tags$b("Type: "), str_c(kind()$labels, collapse = " + ")),
          if (b$type[1] == METADATA_TYPES[["media"]]) checks_table_ui(twin),
          map(seq_len(nrow(b)), function(i) tagList(
            h5(class = "vof-qa-subtitle", str_c(b$metric[i], ": ", b$name[i])),
            checks_table_ui(qa_build_checks(qa(), b$name[i])),
            if (length(b$components[[i]]) > 0) div(class = "vof-table-card", tableOutput(ns(str_c("components_", i)))))))
    })
    walk(seq_len(QA_MAX_GROUP), function(j) {
      output[[str_c("components_", j)]] <- renderTable({
        bb <- group_blocks(); req(nrow(bb) >= j)
        qa_components_table(qa(), bb$name[j]) %>%
          mutate(Contribution = format(round(Contribution, 2), big.mark = ","),
                 `Share of VOF` = sprintf("%.1f%%", 100 * `Share of VOF`))
      }, striped = TRUE, spacing = "xs")
    })

    output$entity_table <- DT::renderDT({
      tbl <- qa_group_entities(qa(), input$group); req(tbl)
      num <- names(tbl)[str_ends(names(tbl), " total")]
      DT::datatable(tbl, rownames = FALSE, class = "compact stripe hover", selection = "none",
                    options = list(scrollX = TRUE, scrollY = "360px", scrollCollapse = TRUE, pageLength = 100, dom = "ftip")) %>%
        DT::formatRound(num, digits = 2)
    })

    # --- Summary: status of all VOFs ----------------------------------------------------
    output$status_legend <- renderUI({
      div(class = "vof-qa-legend",
          map(seq_len(nrow(QA_STATUS)), function(i) div(status_badge(QA_STATUS$Status[i]), span(QA_STATUS$meaning[i]))))
    })

    output$checks_table <- DT::renderDT({
      tbl <- checks() %>%
        left_join(QA_STATUS %>% select(Status, icon, color), by = "Status") %>%
        mutate(Status = str_c('<span style="color:', color, '">', map_chr(icon, ~ as.character(shiny::icon(.x))), "</span> ", Status)) %>%
        select(-icon, -color, -Group)
      DT::datatable(tbl, rownames = FALSE, escape = FALSE, class = "compact stripe hover", selection = "single",
                    caption = "Click a row to review that VOF.",
                    options = list(scrollX = TRUE, scrollY = "360px", scrollCollapse = TRUE, pageLength = 100, dom = "ftip")) %>%
        DT::formatRound("Total", digits = 2)
    })

    observeEvent(input$checks_table_rows_selected, {
      sel <- checks()$Group[input$checks_table_rows_selected]
      if (length(sel) == 1) updateSelectizeInput(session, "group", selected = sel)
    })
  })
}
