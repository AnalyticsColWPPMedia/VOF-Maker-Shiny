# R/modules/mod_variable_row.R
# One Component Row of the media Variable Builder: an MFF variable with its
# labels, period and cross-sectional filters (include/exclude; exact or, with
# CS Children, positional). Backcasting is set per VOF in the media tab.

variable_row_ui <- function(id, visibility = list(), comp = NULL) {
  ns <- NS(id)
  comp <- comp %||% new_component()
  show_if <- function(elem, flag) if (isTRUE(flag)) elem else shinyjs::hidden(elem)
  date_or_na <- function(d) if (is_valid_value(d)) d else NA
  # value = NA renders an empty date box; Shiny warns about it, harmlessly.
  dateInput <- function(...) suppressWarnings(shiny::dateInput(...))

  tags$div(
    id = ns("panel_container"), class = "panel vof-row",
    fluidRow(
      column(10, uiOutput(ns("analytical_var_ui"))),
      column(1, actionButton(ns("clear_filters_btn"), "", icon = icon("eraser"), class = "btn-warning vof-row-btn",
                             title = "Clear Date & CS Filters")),
      column(1, actionButton(ns("remove_btn"), "", icon = icon("trash"), class = "btn-danger vof-row-btn",
                             title = "Remove Row"))
    ),
    fluidRow(
      column(3, textInput(ns("variable_name"), "VariableName", value = comp$variable_name)),
      show_if(div(id = ns("col_complement"), class = "col-sm-2",
                  textInput(ns("complement"), "Complement", value = comp$complement)), visibility$complement),
      show_if(div(id = ns("col_effect"), class = "col-sm-2",
                  selectInput(ns("effect"), "Effect", choices = EFFECT_CHOICES, selected = comp$effect)), visibility$effect),
      show_if(div(id = ns("col_period_start"), class = "col-sm-2",
                  div(class = "vof-datepicker", dateInput(ns("start_period"), "Start Period", value = date_or_na(comp$start_period)))),
              visibility$period),
      show_if(div(id = ns("col_period_end"), class = "col-sm-2",
                  div(class = "vof-datepicker", dateInput(ns("end_period"), "End Period", value = date_or_na(comp$end_period)))),
              visibility$period),
      # TimeBreaks of the Media tab (shown instead of the dates when the row follows them).
      shinyjs::hidden(div(id = ns("col_tb_info"), class = "col-sm-9", uiOutput(ns("tb_info"))))
    ),
    show_if(div(id = ns("wrap_cs"), uiOutput(ns("cs_ui")), uiOutput(ns("cs_preview"))), visibility$cs)
  )
}

# settings:  reactive list(complement, effect, period, cs, shortest) of options.
# children:  reactiveVal list(enabled, sep) of the CS Children configuration.
# on_remove: callback(id) invoked when the user deletes the row.
# var_choices: function() with the variables this row may offer (type filter,
#   without the ones used in other rows); the row adds its own selection.
# split_info: reactive list(tb_enabled, tb_text, cs_enabled, cs_dim, cs_text)
#   with the TimeBreaks / CS Groups of the Media tab. The row follows them
#   unless the analyst switches it to custom dates / its own filter.
variable_row_server <- function(id, mff_vars, settings, children, cs_dims, analytical,
                                initial = NULL, on_remove = function(id) NULL,
                                var_choices = function() mff_vars(),
                                split_info = reactive(list())) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    initial <- initial %||% new_component()
    observers <- list()
    track <- function(obs) { observers[[length(observers) + 1]] <<- obs; invisible(obs) }

    # --- TimeBreaks / CS Groups (sister VOFs) ---------------------------------------
    follow_tb <- reactiveVal(!isFALSE(initial$follow_tb))
    follow_cs <- reactiveVal(!isFALSE(initial$follow_cs))
    tb_active <- reactive(isTRUE(settings()$period) && isTRUE(split_info()$tb_enabled))
    # Dimension of the CS Groups (and the one this row hands over to them).
    # reactiveVal: the CS pickers re-render only when these really change.
    split_dim <- reactiveVal("")
    group_dim <- reactiveVal("")
    track(observe({
      si <- split_info()
      d <- if (isTRUE(si$cs_enabled)) si$cs_dim %||% "" else ""
      split_dim(d)
      group_dim(if (follow_cs()) d else "")
    }))

    output$tb_info <- renderUI({
      si <- split_info()
      if (follow_tb()) {
        div(class = "vof-split-row", icon("scissors"), span(si$tb_text),
            actionLink(ns("custom_dates"), "Custom dates", icon = icon("pen")))
      } else {
        div(class = "vof-split-row vof-split-off", icon("triangle-exclamation"),
            span("Custom dates: this row does not follow the TimeBreaks, so no sister VOFs are created."),
            actionLink(ns("use_tb"), "Use TimeBreaks", icon = icon("scissors")))
      }
    })
    output$cs_group_info <- renderUI({
      div(class = "vof-split-row", icon("object-group"), span(split_info()$cs_text),
          actionLink(ns("custom_cs"), "Custom filter", icon = icon("pen")))
    })
    track(observeEvent(input$custom_dates, follow_tb(FALSE)))
    track(observeEvent(input$use_tb, follow_tb(TRUE)))
    track(observeEvent(input$custom_cs, follow_cs(FALSE)))
    track(observeEvent(input$use_cs, follow_cs(TRUE)))

    child_id  <- function(dim, p) str_c("cs_child_", str_to_lower(dim), "_", p)
    parent_id <- function(dim) str_c("cs_", str_to_lower(dim))
    mode_id   <- function(dim) str_c("cs_mode_", str_to_lower(dim))
    name_id   <- function(dim) str_c("cs_name_", str_to_lower(dim))
    picker <- function(input_id, label, choices, selected) {
      shinyWidgets::pickerInput(ns(input_id), label, choices = choices, selected = selected, multiple = TRUE,
                                options = list(`actions-box` = TRUE, `live-search` = TRUE,
                                               `selected-text-format` = "count > 2", size = 10),
                                width = "100%")
    }

    # Entities shown as selected for a restored/inherited filter: the pickers
    # always hold the entities the user picks (the mode says keep or drop).
    initial_parent <- function(dim, entities) {
      f <- initial$cs[[dim]]
      if (is.null(f)) return(NULL)
      cs_filter_resolve(modifyList(f, list(mode = "include")), entities)
    }

    output$analytical_var_ui <- renderUI({
      selected <- isolate(input$analytical_var) %||% initial$analytical_var
      choices <- var_choices()
      if (nzchar(selected) && !selected %in% choices) choices <- c(selected, choices)
      selectizeInput(ns("analytical_var"), "Analytical Variable (MFF)", choices = c("", choices),
                     selected = selected, width = "100%")
    })

    # VariableName suggested by the app (root of the variable, or "Total ..."
    # when the rows share it). The app only overwrites a name it wrote itself:
    # once the analyst types a different name, the row keeps it. Restored rows
    # keep their stored name.
    auto_written <- c("")
    suggest_name <- function(value) {
      current <- isolate(input$variable_name) %||% initial$variable_name
      if (!current %in% auto_written || identical(current, value)) return(invisible())
      auto_written <<- unique(c(auto_written, value))
      updateTextInput(session, "variable_name", value = value)
    }

    track(observe({
      s <- settings()
      shinyjs::toggle("col_complement", condition = isTRUE(s$complement))
      shinyjs::toggle("col_effect", condition = isTRUE(s$effect))
      own_dates <- isTRUE(s$period) && !(tb_active() && follow_tb())
      shinyjs::toggle("col_period_start", condition = own_dates)
      shinyjs::toggle("col_period_end", condition = own_dates)
      shinyjs::toggle("col_tb_info", condition = tb_active())
      shinyjs::toggle("wrap_cs", condition = isTRUE(s$cs))
    }))

    # Re-rendered only when the dimensions, data or CS Children setup change;
    # current selections are kept across re-renders.
    output$cs_ui <- renderUI({
      dims <- cs_dims(); df <- analytical(); ch <- children()
      gd <- group_dim(); sd <- split_dim()
      if (length(dims) == 0) return(NULL)
      children_on <- isTRUE(ch$enabled) && nzchar(ch$sep %||% "")
      blocks <- map(dims, function(dim) {
        # The CS Groups decide the entities of their dimension.
        if (dim == gd) return(div(class = "vof-cs-block", div(class = "vof-cs-header", tags$b(dim)), uiOutput(ns("cs_group_info"))))
        back_link <- if (dim == sd) actionLink(ns("use_cs"), "Use CS Groups", icon = icon("object-group"))
        # Optional custom flag of this filter ("--g MyGroup"); empty = automatic.
        name_box <- div(class = "vof-cs-name",
                        textInput(ns(name_id(dim)), NULL, value = isolate(input[[name_id(dim)]]) %||% (initial$cs_labels[[dim]] %||% ""),
                                  placeholder = "Group name (optional)", width = "220px"))
        values <- cs_dim_values(df, dim)
        current <- isolate(input[[parent_id(dim)]])
        selected <- if (!is.null(current)) current else initial_parent(dim, values)
        mode <- isolate(input[[mode_id(dim)]]) %||% (initial$cs[[dim]]$mode %||% "include")
        mode_switch <- shinyWidgets::radioGroupButtons(ns(mode_id(dim)), NULL, choices = CS_FILTER_MODES,
                                                       selected = mode, size = "xs", status = "default")
        if (children_on) {
          parts <- cs_children_parts(values, ch$sep)
          implied <- cs_filter_to_children(initial$cs[[dim]], values, ch$sep) %||% cs_children_from_parent(selected, ch$sep)
          pickers <- map(seq_along(parts), function(p) {
            cur <- isolate(input[[child_id(dim, p)]])
            sel <- if (!is.null(cur)) cur else initial$cs_children[[child_id(dim, p)]] %||% (if (length(implied) >= p) implied[[p]])
            column(2, picker(child_id(dim, p), str_c(dim, " - CS Child ", LETTERS[p]), parts[[p]], sel))
          })
          div(class = "vof-cs-block",
              div(class = "vof-cs-header", tags$b(dim), mode_switch, name_box, back_link),
              do.call(fluidRow, pickers),
              # The parent picker stays (hidden) with the resolved selection.
              shinyjs::hidden(picker(parent_id(dim), dim, values, selected)))
        } else {
          div(class = "vof-cs-block vof-cs-inline",
              div(class = "vof-cs-header", tags$b(dim), mode_switch, name_box, back_link),
              picker(parent_id(dim), NULL, values, selected))
        }
      })
      div(class = "vof-cs-container", blocks)
    })

    # CS Children -> parent selection (what the children select).
    track(observe({
      ch <- children()
      req(isTRUE(ch$enabled), nzchar(ch$sep %||% ""))
      df <- analytical()
      for (dim in cs_dims()) {
        values <- cs_dim_values(df, dim)
        n_parts <- length(cs_children_parts(values, ch$sep))
        if (n_parts == 0) next
        sels <- map(seq_len(n_parts), ~ input[[child_id(dim, .x)]])
        if (every(sels, is.null)) next  # pickers not rendered yet
        shinyWidgets::updatePickerInput(session, parent_id(dim), selected = cs_children_filter(values, ch$sep, sels))
      }
    }))

    track(observeEvent(input$clear_filters_btn, {
      suppressWarnings({  # value = NA empties the box; Shiny warns about it, harmlessly
        updateDateInput(session, "start_period", value = NA)
        updateDateInput(session, "end_period", value = NA)
      })
      for (dim in cs_dims()) {
        shinyWidgets::updatePickerInput(session, parent_id(dim), selected = character(0))
        shinyWidgets::updateRadioGroupButtons(session, mode_id(dim), selected = "include")
        for (p in seq_len(MAX_CS_CHILDREN)) shinyWidgets::updatePickerInput(session, child_id(dim, p), selected = character(0))
      }
    }))

    track(observeEvent(input$remove_btn, on_remove(id), ignoreInit = TRUE, once = TRUE))

    # Filters of the row. With CS Children: positional patterns built from the
    # child selections (exact list when patterns cannot reproduce them).
    cs_filters <- reactive({
      s <- settings()
      if (!isTRUE(s$cs)) return(list(filters = list(), notes = character(0)))
      ch <- children(); df <- analytical()
      notes <- character(0)
      gd <- group_dim()
      filters <- cs_dims() %>% set_names() %>% map(function(dim) {
        if (dim == gd) return(NULL)  # set by the CS Groups of each sister
        values <- cs_dim_values(df, dim)
        mode <- input[[mode_id(dim)]] %||% "include"
        if (isTRUE(ch$enabled) && nzchar(ch$sep %||% "")) {
          n_parts <- length(cs_children_parts(values, ch$sep))
          sels <- map(seq_len(n_parts), ~ input[[child_id(dim, .x)]])
          if (some(sels, ~ length(.x) > 0)) {
            res <- cs_children_to_filter(values, ch$sep, sels, mode)
            if (res$fallback) notes <<- c(notes, sprintf("%s: the children selection cannot be written as positional patterns; the exact list of entities is used.", dim))
            return(res$filter)
          }
        }
        f <- as_cs_filter(input[[parent_id(dim)]])
        if (is.null(f)) return(NULL)
        if (mode == "exclude") f$mode <- "exclude"
        if (isTRUE(s$shortest)) f <- shortest_exact_filter(f, values)
        f
      }) %>% compact()
      list(filters = filters, notes = notes)
    })

    # What each filter keeps, so the analyst sees the effect of patterns and
    # exclusions before generating.
    output$cs_preview <- renderUI({
      res <- cs_filters()
      if (length(res$filters) == 0) return(NULL)
      df <- analytical()
      items <- imap(res$filters, function(f, dim) {
        kept <- cs_filter_resolve(f, cs_dim_values(df, dim))
        total <- length(cs_dim_values(df, dim))
        tags$li(tags$b(dim), sprintf(" keeps %d of %d: ", length(kept), total),
                tags$code(cs_filter_expr(dim, f)),
                tags$small(class = "text-muted", str_c(" (", str_c(head(kept, 8), collapse = ", "),
                                                       if (length(kept) > 8) ", ..." else "", ")")))
      })
      div(class = "vof-cs-preview", tags$ul(unname(items)),
          if (length(res$notes) > 0) div(class = "text-warning", res$notes))
    })

    component <- reactive({
      s <- settings()
      cs_children <- list()
      if (isTRUE(s$cs) && isTRUE(children()$enabled)) {
        for (d in cs_dims()) for (p in seq_len(MAX_CS_CHILDREN)) {
          v <- input[[child_id(d, p)]]
          if (!is.null(v)) cs_children[[child_id(d, p)]] <- v
        }
      }
      new_component(
        analytical_var = input$analytical_var,
        variable_name  = input$variable_name,
        complement     = if (isTRUE(s$complement)) input$complement else "",
        effect         = if (isTRUE(s$effect)) input$effect else "",
        start_period   = if (isTRUE(s$period) && !(tb_active() && follow_tb())) input$start_period else NA,
        end_period     = if (isTRUE(s$period) && !(tb_active() && follow_tb())) input$end_period else NA,
        cs             = cs_filters()$filters,
        cs_children    = cs_children,
        follow_tb      = follow_tb(),
        follow_cs      = follow_cs(),
        cs_labels      = (names(cs_filters()$filters) %||% character(0)) %>% set_names() %>% map(~ input[[name_id(.x)]] %||% "")
      )
    })

    list(id = id, component = component, suggest_name = suggest_name,
         selected_var = reactive(input$analytical_var %||% initial$analytical_var),
         destroy = function() walk(observers, ~ .x$destroy()))
  })
}
