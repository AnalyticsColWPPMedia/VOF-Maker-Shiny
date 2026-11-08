# R/modules/mod_media_tab.R
# Media tab: configuration, Variable Builder (dynamic Component Rows), VOF-level
# settings (backcasting, metric) and generation of the media VOF and its twin.

media_tab_ui <- function(id) {
  ns <- NS(id)
  # value = NA renders an empty date box; Shiny warns about it, harmlessly.
  date_input <- function(...) suppressWarnings(dateInput(...))
  tagList(
    tags$br(),
    div(class = "section-card", h4("Configuration Options", class = "card-title"),
        fluidRow(
          column(3, checkboxInput(ns("show_complement"), "Complement", FALSE)),
          column(3, checkboxInput(ns("show_effect"), "Effect", FALSE)),
          column(3, checkboxInput(ns("show_period"), "Period", FALSE)),
          column(3, checkboxInput(ns("show_cs"), "Cross Sectional", FALSE))
        ),
        conditionalPanel(sprintf("input['%s'] == true", ns("show_period")),
          tags$hr(class = "vof-subsection-divider"),
          h5("TimeBreaks (sister VOFs by period)", class = "vof-subsection-title"),
          checkboxInput(ns("tb_enabled"), "Split the VOF into TimeBreaks", FALSE),
          conditionalPanel(sprintf("input['%s'] == true", ns("tb_enabled")),
            div(class = "vof-split-block",
              fluidRow(
                column(2, numericInput(ns("tb_n"), "Number of TimeBreaks", value = 2, min = 2, max = SPLIT_MAX_PARTS, step = 1)),
                column(10, uiOutput(ns("tb_cuts_ui")))
              ),
              # Open ends by default; a start and/or an end close the first / last TimeBreak.
              div(class = "vof-split-ends",
                  checkboxInput(ns("tb_use_start"), "Start date (closes TimeBreak 1)", FALSE),
                  conditionalPanel(sprintf("input['%s'] == true", ns("tb_use_start")),
                                   div(class = "vof-datepicker vof-split-cut", date_input(ns("tb_start"), NULL, value = NA))),
                  checkboxInput(ns("tb_use_end"), "End date (closes the last TimeBreak)", FALSE),
                  conditionalPanel(sprintf("input['%s'] == true", ns("tb_use_end")),
                                   div(class = "vof-datepicker vof-split-cut", date_input(ns("tb_end"), NULL, value = NA)))),
              tags$small(class = "text-muted",
                         "A cut date is the last day of the TimeBreak on its left; the next TimeBreak starts the day after. ",
                         "Any calendar date works (it does not need to be a period of the Analytical)."),
              uiOutput(ns("tb_summary"))))
        ),
        conditionalPanel(sprintf("input['%s'] == true", ns("show_cs")),
          tags$hr(class = "vof-subsection-divider"),
          h5("CS Children (Cross-Sectional Splitter)", class = "vof-subsection-title"),
          div(class = "vof-inline-controls",
              checkboxInput(ns("enable_cs_children"), "Enable CS Children", FALSE),
              conditionalPanel(sprintf("input['%s'] == true", ns("enable_cs_children")),
                               textInput(ns("cs_separator"), "Separator Token", value = " - ", width = "150px"))),
          h5("CS Groups (sister VOFs by entities)", class = "vof-subsection-title"),
          checkboxInput(ns("csg_enabled"), "Split the VOF into groups of entities", FALSE),
          conditionalPanel(sprintf("input['%s'] == true", ns("csg_enabled")),
            div(class = "vof-split-block",
              fluidRow(
                column(2, selectInput(ns("csg_dim"), "Dimension", choices = NULL)),
                column(2, numericInput(ns("csg_n"), "Number of groups", value = 1, min = 1, max = SPLIT_MAX_PARTS - 1, step = 1)),
                column(8, tags$small(class = "text-muted vof-split-help",
                                     "Each group is a sister VOF, defined like the CS filter of a Component Row (Include / Exclude; ",
                                     "with CS Children enabled, by children: starts with / contains / ends with). The entities in no group form the ",
                                     tags$b(CS_ALL_OTHERS_LABEL), " VOF (only when some are left). Names follow the usual flags: ",
                                     tags$code("--g San"), ", ", tags$code("--g not San"), ", ", tags$code("--g top3"), ", ", tags$code("--g group2"),
                                     ". Other dimensions keep the filters of each row."))
              ),
              uiOutput(ns("csg_groups_ui")),
              uiOutput(ns("csg_others_ui")),
              uiOutput(ns("csg_summary"))))
        )
    ),
    div(class = "section-card", h4("Variable Builder", class = "card-title"),
        div(class = "vof-var-type",
            tags$b("Show variables"),
            shinyWidgets::radioGroupButtons(ns("var_type"), NULL, choices = VAR_TYPE_FILTERS, selected = "Activity",
                                            size = "sm", status = "default"),
            tags$small(class = "text-muted", "Activity / Spend follow the metric dictionary; variables without a known metric are listed under All. A variable used in one row is not offered in the others.")),
        div(id = ns("rows_container")), tags$br(),
        actionButton(ns("add_row_btn"), "Add Component Row", class = "btn btn-primary", icon = icon("plus"))
    ),
    div(class = "section-card", h4("VOF Settings", class = "card-title"),
        fluidRow(
          column(3, checkboxInput(ns("enable_backcasting"),
                                  tags$span(class = "vof-backcasting-label", "Apply Backcasting to this VOF"), FALSE)),
          column(3, conditionalPanel(sprintf("input['%s'] == true", ns("enable_backcasting")),
                                     div(class = "vof-datepicker", date_input(ns("bc_start"), "Calibration Start", value = NA)))),
          column(3, conditionalPanel(sprintf("input['%s'] == true", ns("enable_backcasting")),
                                     div(class = "vof-datepicker", date_input(ns("bc_end"), "Calibration End", value = NA)))),
          column(3, selectInput(ns("metric_override"), "Metric (only if not detected)",
                                choices = c("Auto-detect" = "", METRICS$Metric)),
                 actionLink(ns("dictionary_btn"), "Naming dictionary", icon = icon("book")))
        ),
        checkboxInput(ns("shortest_filter"), "CS filters: write the shortest list (switch to Exclude when it lists fewer entities)", FALSE),
        conditionalPanel(sprintf("input['%s'] == true", ns("enable_backcasting")),
                         tags$small(class = "text-muted",
                                    "Zeros before the calibration window are imputed with the mean of the window (by cross-section), after summing all components. Applies to the activity and spend VOFs."))
    ),
    div(class = "section-card", h4("Actions & Output", class = "card-title"),
        fluidRow(
          column(4, actionButton(ns("generate_btn"), "Generate VOF Code & Save", class = "btn btn-primary btn-block"),
                 div(class = "vof-centered", checkboxInput(ns("auto_twin"), "Auto-generate twin VOF (Activity / Spend)", value = TRUE))),
          column(4, actionButton(ns("next_btn"), "Next VOF (Clear UI)", class = "btn btn-success btn-block",
                                 icon = icon("arrow-right"))),
          column(4, metadata_download_ui(ns("meta_dl"), class = "btn btn-success custom-download-btn btn-block"))
        ),
        tags$hr(),
        h5("Generated Code (Current VOF)", class = "vof-output-title"),
        textAreaInput(ns("code_area"), NULL, value = "", rows = 12, width = "100%", resize = "vertical")
    ),
    # Always visible while the rows are configured (sticky at the bottom).
    div(class = "vof-name-preview", uiOutput(ns("name_preview")))
  )
}

VAR_TYPE_FILTERS <- c("Activity" = "Activity", "Spend" = "Spend", "All" = "All")

# value = NA renders an empty date box; Shiny warns about it, harmlessly.
split_date_input <- function(...) suppressWarnings(dateInput(...))

# Keeps the parts the analyst left checked; parts added since the last render
# (more TimeBreaks / groups) start checked.
keep_selection <- function(previous, available, previous_available) {
  if (is.null(previous)) return(available)
  c(intersect(previous, available), setdiff(available, previous_available))
}

media_tab_server <- function(id, upload, history) {
  moduleServer(id, function(input, output, session) {
    metadata_download_server("meta_dl", history, upload)
    ns <- session$ns

    settings <- reactive(list(complement = isTRUE(input$show_complement), effect = isTRUE(input$show_effect),
                              period = isTRUE(input$show_period), cs = isTRUE(input$show_cs),
                              shortest = isTRUE(input$shortest_filter)))
    # reactiveVal only invalidates when the value really changes, so rows do
    # not re-render their CS pickers when unrelated options are toggled.
    children <- reactiveVal(list(enabled = FALSE, sep = " - "))
    observe(children(list(enabled = isTRUE(input$enable_cs_children), sep = input$cs_separator %||% "")))

    backcasting <- reactive(list(enabled = isTRUE(input$enable_backcasting),
                                 start = input$bc_start, end = input$bc_end))

    rows <- reactiveVal(list())
    counter <- 0

    # --- Sister VOFs: TimeBreaks ---------------------------------------------------
    tb_n <- reactive(min(SPLIT_MAX_PARTS, max(2L, as.integer(input$tb_n %||% 2) %|NA|% 2L)))
    output$tb_cuts_ui <- renderUI({
      n <- tb_n()
      div(class = "vof-split-cuts", map(seq_len(n - 1), function(i) {
        id <- str_c("tb_cut_", i); cur <- isolate(input[[id]])
        div(class = "vof-datepicker vof-split-cut",
            split_date_input(ns(id), sprintf("Cut %d (last day of TimeBreak %d)", i, i), value = if (is_valid_value(cur)) cur else NA))
      }))
    })
    tb_spec <- reactive({
      if (!isTRUE(input$show_period) || !isTRUE(input$tb_enabled)) return(list(enabled = FALSE))
      list(enabled = TRUE, start = if (isTRUE(input$tb_use_start)) input$tb_start else NA,
           end = if (isTRUE(input$tb_use_end)) input$tb_end else NA,
           cuts = map(seq_len(tb_n() - 1), ~ input[[str_c("tb_cut_", .x)]]))
    })
    tb_parts <- reactive({ sp <- tb_spec(); req(sp$enabled); time_break_parts(sp) })
    tb_selected <- reactiveVal(NULL)       # NULL = every TimeBreak
    tb_shown <- character(0)
    observeEvent(input$tb_selected, tb_selected(input$tb_selected %||% character(0)), ignoreNULL = FALSE, ignoreInit = TRUE)
    output$tb_summary <- renderUI({
      tp <- tb_parts()
      if (length(tp$errors) > 0) return(div(class = "vof-split-msg", icon("circle-exclamation"), str_c(tp$errors, collapse = " ")))
      cov <- time_break_coverage(tp$parts, analytical_periods(upload$data()))
      idx <- as.character(cov$index)
      sel <- keep_selection(isolate(tb_selected()), idx, tb_shown)
      tb_shown <<- idx
      names <- pmap(cov, function(index, label, n, first, last, ...) {
        span(tags$b("TimeBreak ", index), tags$code(str_c("--p ", label)),
             tags$small(class = "text-muted", if (n > 0) sprintf("%d periods, %s to %s", n, first, last) else "no periods of the Analytical"))
      })
      checkboxGroupInput(ns("tb_selected"), "Generate", choiceNames = names, choiceValues = idx, selected = sel)
    })

    # --- Sister VOFs: CS Groups ------------------------------------------------------
    observe({
      dims <- upload$cs_dims()
      current <- isolate(input$csg_dim)
      updateSelectInput(session, "csg_dim", choices = dims,
                        selected = if (is_valid_value(current) && current %in% dims) current else first(dims, default = NULL))
    })
    csg_n <- reactive(min(SPLIT_MAX_PARTS - 1L, max(1L, as.integer(input$csg_n %||% 1) %|NA|% 1L)))
    csg_entities <- reactive({ req(input$csg_dim); cs_dim_values(upload$data(), input$csg_dim) })
    # Each group is defined like the CS filter of a Component Row: Include /
    # Exclude and either entities or, with CS Children, one picker per child.
    gid <- function(i, what) str_c("csg_g", i, "_", what)
    children_on <- reactive({ ch <- children(); isTRUE(ch$enabled) && nzchar(ch$sep %||% "") })
    output$csg_groups_ui <- renderUI({
      values <- csg_entities(); n <- csg_n(); ch <- children(); use_children <- children_on()
      picker <- function(id, label, choices) {
        shinyWidgets::pickerInput(ns(id), label, choices = choices, multiple = TRUE,
                                  selected = intersect(isolate(input[[id]]) %||% character(0), choices),
                                  options = list(`actions-box` = TRUE, `live-search` = TRUE, `selected-text-format` = "count > 2", size = 10),
                                  width = "100%")
      }
      div(class = "vof-split-groups", map(seq_len(n), function(i) {
        mode <- shinyWidgets::radioGroupButtons(ns(gid(i, "mode")), NULL, choices = CS_FILTER_MODES,
                                                selected = isolate(input[[gid(i, "mode")]]) %||% "include", size = "xs", status = "default")
        pickers <- if (use_children) {
          parts <- cs_children_parts(values, ch$sep)
          div(class = "vof-split-children", map(seq_along(parts), function(p)
            div(class = "vof-split-child", picker(gid(i, str_c("child_", p)), str_c("CS Child ", LETTERS[p]), parts[[p]]))))
        } else picker(gid(i, "entities"), NULL, values)
        name_box <- div(class = "vof-cs-name",
                        textInput(ns(gid(i, "name")), NULL, value = isolate(input[[gid(i, "name")]]) %||% "",
                                  placeholder = "Name (optional)", width = "220px"))
        div(class = "vof-split-group", div(class = "vof-cs-header", tags$b("Group ", i), mode, name_box), pickers)
      }))
    })
    # All Others can be named only when some entities are left out of the groups.
    output$csg_others_ui <- renderUI({
      # Groups only (not the names), so typing the name does not re-render the box.
      req(isTRUE(input$show_cs), isTRUE(input$csg_enabled), is_valid_value(input$csg_dim))
      cp <- cs_group_parts(list(dim = input$csg_dim, groups = group_filters()$filters), csg_entities())
      if (length(cp$errors) > 0 || !"others" %in% cp$parts$index) return(NULL)
      n_left <- length(cp$parts$kept[[which(cp$parts$index == "others")]])
      div(class = "vof-split-group vof-split-others",
          div(class = "vof-cs-header", tags$b(CS_ALL_OTHERS_LABEL),
              div(class = "vof-cs-name", textInput(ns("csg_others_name"), NULL, value = isolate(input$csg_others_name) %||% "",
                                                   placeholder = "Name (optional)", width = "220px")),
              tags$small(class = "text-muted", sprintf("%d entities in no group.", n_left))))
    })
    group_filters <- reactive({
      values <- csg_entities(); ch <- children(); use_children <- children_on()
      notes <- character(0)
      filters <- map(seq_len(csg_n()), function(i) {
        mode <- input[[gid(i, "mode")]] %||% "include"
        if (use_children) {
          sels <- map(seq_along(cs_children_parts(values, ch$sep)), ~ input[[gid(i, str_c("child_", .x))]])
          if (!some(sels, ~ length(.x) > 0)) return(NULL)
          res <- cs_children_to_filter(values, ch$sep, sels, mode)
          if (res$fallback) notes <<- c(notes, sprintf("Group %d: the children selection cannot be written as positional patterns; the exact list of entities is used.", i))
          return(res$filter)
        }
        f <- as_cs_filter(input[[gid(i, "entities")]])
        if (!is.null(f) && mode == "exclude") f$mode <- "exclude"
        f
      })
      list(filters = filters, notes = notes)
    })
    cs_spec <- reactive({
      if (!isTRUE(input$show_cs) || !isTRUE(input$csg_enabled) || !is_valid_value(input$csg_dim)) return(list(enabled = FALSE))
      gf <- group_filters()
      list(enabled = TRUE, dim = input$csg_dim, groups = gf$filters, notes = gf$notes,
           names = map(seq_len(csg_n()), ~ input[[gid(.x, "name")]] %||% ""), others_name = input$csg_others_name %||% "")
    })
    csg_parts <- reactive({ sp <- cs_spec(); req(sp$enabled); cs_group_parts(sp, csg_entities()) })
    csg_selected <- reactiveVal(NULL)      # NULL = every group
    csg_shown <- character(0)
    observeEvent(input$csg_selected, csg_selected(input$csg_selected %||% character(0)), ignoreNULL = FALSE, ignoreInit = TRUE)
    output$csg_summary <- renderUI({
      cp <- csg_parts()
      if (length(cp$errors) > 0) return(div(class = "vof-split-msg", icon("circle-exclamation"), str_c(cp$errors, collapse = " ")))
      parts <- cp$parts
      sel <- keep_selection(isolate(csg_selected()), parts$index, csg_shown)
      csg_shown <<- parts$index
      dim <- input$csg_dim
      names <- pmap(list(parts$label, parts$kept, parts$filter), function(lab, kept, f) {
        span(tags$b(lab), tags$code(cs_filter_expr(dim, f)),
             tags$small(class = "text-muted", sprintf("%d entities: %s%s", length(kept), str_c(head(kept, 4), collapse = ", "),
                                                      if (length(kept) > 4) ", ..." else "")))
      })
      tagList(checkboxGroupInput(ns("csg_selected"), "Generate", choiceNames = names, choiceValues = parts$index, selected = sel),
              if (length(c(cs_spec()$notes, cp$notes)) > 0) div(class = "vof-split-msg", str_c(c(cs_spec()$notes, cp$notes), collapse = " ")))
    })

    split_config <- reactive({
      tb <- tb_spec(); cs <- cs_spec()
      if (isTRUE(tb$enabled)) tb$selected <- tb_selected()
      if (isTRUE(cs$enabled)) cs$selected <- csg_selected()
      new_split(tb = tb, cs = cs)
    })
    # What every row shows about the split (and whether it follows it).
    split_info <- reactive({
      tb <- tb_spec(); cs <- cs_spec()
      tb_text <- if (isTRUE(tb$enabled)) { tp <- time_break_parts(tb); if (length(tp$errors) > 0) "TimeBreaks: set the cut dates in Configuration Options." else time_break_text(tp$parts) } else ""
      cs_text <- if (isTRUE(cs$enabled)) { cp <- cs_group_parts(cs, csg_entities()); if (length(cp$errors) > 0) "CS Groups: select the entities of every group in Configuration Options." else cs_group_text(cs$dim, cp$parts) } else ""
      list(tb_enabled = isTRUE(tb$enabled), tb_text = tb_text,
           cs_enabled = isTRUE(cs$enabled), cs_dim = if (isTRUE(cs$enabled)) cs$dim else "", cs_text = cs_text)
    })

    # --- Variables offered in the rows -----------------------------------------
    var_types <- reactive({
      vars <- upload$mff_vars(); req(vars)
      tibble(var = vars, type = mff_metric_types(vars, upload$long_dims()))
    })
    vars_of_type <- reactive({
      vt <- var_types()
      if (identical(input$var_type, "All")) vt$var else vt$var[vt$type %in% (input$var_type %||% "Activity")]
    })
    # Variables available to one row: the type filter minus the variables
    # selected in the other rows (its own selection is added by the row).
    choices_for <- function(row_id) function() {
      others <- rows()[setdiff(names(rows()), row_id)] %>% map_chr(~ .x$selected_var() %||% "")
      setdiff(vars_of_type(), others)
    }

    # Suggested VariableName of every row from the roots of the variables.
    observe({
      rs <- rows(); req(length(rs) > 0)
      roots <- map_chr(rs, ~ suggested_root_name(.x$selected_var(), upload$long_dims()))
      walk2(rs, suggest_variable_names(roots), ~ .x$suggest_name(.y))
    })

    remove_row <- function(row_id) {
      current <- rows()
      if (is.null(current[[row_id]])) return(invisible())
      current[[row_id]]$destroy()
      removeUI(selector = str_c("#", ns(NS(row_id, "panel_container"))))
      current[[row_id]] <- NULL
      rows(current)
    }

    insert_row <- function(comp = NULL, visibility = isolate(settings())) {
      counter <<- counter + 1
      row_id <- str_c("row_", counter)
      # immediate: the row must exist in the page before its observers show or
      # hide parts of it (dates vs TimeBreaks, CS Groups).
      insertUI(selector = str_c("#", ns("rows_container")), where = "beforeEnd",
               ui = variable_row_ui(ns(row_id), visibility, comp), immediate = TRUE)
      handle <- variable_row_server(row_id, upload$mff_vars, settings, children, upload$cs_dims, upload$data,
                                    initial = comp, on_remove = remove_row, var_choices = choices_for(row_id),
                                    split_info = split_info)
      current <- isolate(rows())
      current[[row_id]] <- handle
      rows(current)
    }

    clear_rows <- function() walk(names(isolate(rows())), remove_row)

    set_backcasting <- function(bc = list(enabled = FALSE)) {
      updateCheckboxInput(session, "enable_backcasting", value = isTRUE(bc$enabled))
      # value = NA empties the box; Shiny warns about it, harmlessly.
      suppressWarnings({
        updateDateInput(session, "bc_start", value = if (isTRUE(bc$enabled)) bc$start else NA)
        updateDateInput(session, "bc_end", value = if (isTRUE(bc$enabled)) bc$end else NA)
      })
    }

    reset_builder <- function(add_empty_row = TRUE) {
      clear_rows()
      walk(c("show_complement", "show_effect", "show_period", "show_cs", "tb_enabled", "csg_enabled"),
           ~ updateCheckboxInput(session, .x, value = FALSE))
      set_backcasting()
      updateSelectInput(session, "metric_override", selected = "")
      updateTextAreaInput(session, "code_area", value = "")
      if (add_empty_row) insert_row(visibility = list())
    }

    # Loads a VOF (components + backcasting) into the builder (Restore Project).
    load_vof <- function(components, backcasting = list(enabled = FALSE), name = NULL) {
      clear_rows()
      # Custom CS flags written in the name ("--g MyGroup") go back to the rows;
      # automatic ones (patterns, topN, "... etc", groupN) are recomputed.
      if (!is.null(name)) {
        df <- isolate(upload$data()); dims <- isolate(upload$cs_dims())
        labels <- name_cs_labels(name, dims)
        roots <- map_chr(components, "analytical_var")
        sep <- isolate(input$cs_separator) %||% " - "
        components <- map(components, function(comp) {
          for (d in names(labels)) {
            if (!nzchar(labels[[d]]) || is.null(comp$cs[[d]])) next
            if (is_custom_cs_label(labels[[d]], d, comp$cs[[d]], cs_dim_values(df, d), cs_root_totals(df, d, roots), sep))
              comp$cs_labels[[d]] <- labels[[d]]
          }
          comp
        })
      }
      # Restored VOFs come back one by one, with their own dates and filters.
      walk(c("tb_enabled", "csg_enabled"), ~ updateCheckboxInput(session, .x, value = FALSE))
      vis <- list(
        complement = some(components, ~ .x$complement != ""),
        effect = some(components, ~ .x$effect != ""),
        period = some(components, ~ is_valid_value(.x$start_period) || is_valid_value(.x$end_period)),
        cs = some(components, ~ length(.x$cs) > 0)
      )
      positional <- some(components, function(comp) some(comp$cs, ~ !cs_filter_is_exact(.x)))
      updateCheckboxInput(session, "show_complement", value = vis$complement)
      updateCheckboxInput(session, "show_effect", value = vis$effect)
      updateCheckboxInput(session, "show_period", value = vis$period)
      updateCheckboxInput(session, "show_cs", value = vis$cs)
      if (positional) {
        updateCheckboxInput(session, "enable_cs_children", value = TRUE)
        children(list(enabled = TRUE, sep = isolate(input$cs_separator) %||% " - "))
      }
      set_backcasting(backcasting)
      walk(components, ~ insert_row(.x, vis))
    }

    # First row as soon as an Analytical is loaded.
    observeEvent(upload$data(), {
      if (length(rows()) == 0) insert_row()
    })

    # New rows inherit the period and cross-sectional filters of the last one.
    observeEvent(input$add_row_btn, {
      if (is.null(upload$data())) { app_notify("Upload the Analytical first.", type = "warning"); return() }
      current <- rows()
      inherit <- if (length(current) > 0) {
        modifyList(current[[length(current)]]$component(),
                   list(analytical_var = "", variable_name = "", complement = "", effect = ""))
      }
      insert_row(inherit)
    })

    # --- Live preview of the VOF names -------------------------------------------
    preview_inputs <- reactive({
      list(components = map(rows(), ~ .x$component()) %>% unname(),
           twin = isTRUE(input$auto_twin), backcasting = backcasting(), metric = input$metric_override,
           cs_sep = if (isTRUE(input$enable_cs_children)) input$cs_separator, split = split_config())
    }) %>% debounce(400)

    preview <- reactive({
      p <- preview_inputs(); req(upload$data())
      tryCatch(
        preview_media_vof(p$components, upload$cs_dims(), upload$mff_vars(), upload$long_dims(),
                          generate_twin = p$twin, backcasting = p$backcasting, metric_override = p$metric,
                          context = list(analytical = upload$data(), history = history$data(), cs_sep = p$cs_sep),
                          split = p$split),
        error = function(e) list(status = "incomplete", vofs = NULL, messages = conditionMessage(e)))
    })

    output$name_preview <- renderUI({
      pv <- preview()
      head <- span(class = "vof-name-preview-title", icon("tag"), "VOF name")
      if (pv$status == "empty")
        return(div(head, span(class = "text-muted", "Select a variable in a Component Row to see the name.")))
      if (pv$status == "incomplete")
        return(div(head, span(class = "vof-name-preview-missing", icon("circle-exclamation"), str_c(pv$messages, collapse = " "))))
      name_chip <- function(r) {
        span(class = "vof-name-preview-item",
             span(class = "vof-name-preview-type", if (r$Configured) r$Type else str_c(r$Type, " twin")),
             tags$code(r$Name),
             if (r$InHistory) span(class = "vof-name-preview-note", "already in the history: generating replaces it"),
             if (!r$Configured && r$Type == "Activity" && isTRUE(pv$twin_pending)) span(class = "vof-name-preview-note", "activity twin to confirm when generating"))
      }
      by_sister <- base::split(pv$vofs, factor(pv$vofs$Sibling, levels = unique(pv$vofs$Sibling)))
      lines <- map(by_sister, function(v) {
        tags$div(class = "vof-name-preview-line",
                 if (pv$n_sisters > 1) span(class = "vof-name-preview-sister", v$Sibling[1]),
                 map(seq_len(nrow(v)), ~ name_chip(v[.x, ])))
      })
      if (pv$n_sisters > 1) head <- span(class = "vof-name-preview-title", icon("tags"),
                                         sprintf("%d sister VOFs, %d VOFs", pv$n_sisters, nrow(pv$vofs)))
      div(head, div(class = "vof-name-preview-names", unname(lines)),
          if (length(pv$messages) > 0) div(class = "vof-name-preview-note", str_c(pv$messages, collapse = " ")))
    })

    # --- Generation (with an optional twin-choice dialog) ----------------------
    pending <- reactiveVal(NULL)  # components + options waiting for twin choices

    run_generation <- function(components, twin_choices = NULL) {
      res <- generate_split_vofs(
        components, upload$cs_dims(), upload$mff_vars(), upload$long_dims(),
        generate_twin = isTRUE(input$auto_twin), backcasting = backcasting(),
        metric_override = input$metric_override, twin_choices = twin_choices,
        context = list(analytical = upload$data(), history = history$data(),
                       cs_sep = if (isTRUE(input$enable_cs_children)) input$cs_separator),
        split = split_config())
      if (length(res$errors) > 0) { notify_messages(res$errors, type = "error"); return() }
      if (!is.null(res$needs_choice)) {
        pending(list(components = components, options = res$needs_choice))
        show_twin_dialog(res$needs_choice)
        return()
      }
      updateTextAreaInput(session, "code_area", value = res$code)
      replaced <- any(unique(res$metadata$MainModelVariableName) %in% history$data()$MainModelVariableName)
      history$upsert(res$metadata)
      n <- n_distinct(res$metadata$MainModelVariableName)
      app_notify(str_c(if (res$n_sisters > 1) sprintf("%d sister VOFs (%d VOFs) saved to history", res$n_sisters, n)
                             else if (n > 1) sprintf("%d VOFs saved to history", n) else "VOF saved to history",
                             if (replaced) " (previous versions replaced)." else "."), type = "message")
      notify_messages(res$warnings)
    }

    show_twin_dialog <- function(options) {
      showModal(modalDialog(
        title = "Choose the activity twin", size = "l",
        p("These spend variables have several activity counterparts. Choose the one to model as the activity twin",
          " (pre-selected with the channel rules of the metric dictionary)."),
        imap(options, function(o, i) {
          choices <- set_names(o$candidates$var, str_c(o$candidates$Metric, ": ", o$candidates$var))
          radioButtons(ns(str_c("twin_choice_", i)), str_c("Row ", i, " - ", o$analytical_var),
                       choices = choices, selected = o$preselected)
        }) %>% unname(),
        footer = tagList(modalButton("Cancel"), actionButton(ns("twin_confirm"), "Generate", class = "btn btn-primary"))
      ))
    }

    observeEvent(input$generate_btn, {
      if (length(rows()) == 0) { app_notify("Add at least one Component Row.", type = "warning"); return() }
      run_generation(map(rows(), ~ .x$component()) %>% unname())
    })

    observeEvent(input$twin_confirm, {
      p <- pending(); req(p)
      choices <- names(p$options) %>% set_names() %>% map(~ input[[str_c("twin_choice_", .x)]])
      removeModal()
      pending(NULL)
      run_generation(p$components, choices)
    })

    observeEvent(input$next_btn, {
      reset_builder()
      app_notify("UI Cleared.", type = "warning")
    })

    # --- Naming dictionary (metrics, channel rules, abbreviations) --------------
    observeEvent(input$dictionary_btn, {
      showModal(modalDialog(
        title = "Naming dictionary", size = "l", easyClose = TRUE,
        p("Every media VOF ends with ", tags$code(" ---<suffix>"), " (with a space before, to keep it apart from the tags). Words of 7+ letters are abbreviated to 4 letters; every spend synonym becomes ", tags$code(" ---Spend"), "."),
        div(class = "vof-table-card", tableOutput(ns("metric_dictionary_table"))),
        h5("Preferred activity metric by channel (twin of a spend VOF)"),
        div(class = "vof-table-card", tableOutput(ns("channel_rules_table"))),
        h5("Pre-filled VariableName"),
        p("The VariableName is pre-filled with the root of the selected variable (its name without the metric), ",
          "with these channel abbreviations, special characters ( - _ ( ) , . / & ...) replaced by spaces, ",
          "the first letter of each word in capitals and the rest in lower case, and these words always in capitals. Edit ", tags$code("CHANNEL_ABBREVIATIONS"), " / ", tags$code("NAME_ACRONYMS"),
          " in ", tags$code("R/core/00_constants.R"), " to add cases."),
        div(class = "vof-table-card", tableOutput(ns("abbreviations_table"))),
        p(tags$b("Always in capitals: "), str_c(NAME_ACRONYMS, collapse = ", "), ". ",
          tags$b("Brand casing: "), str_c(unique(NAME_SPECIAL_CASING), collapse = ", "), "."),
        footer = modalButton("Close")))
    })
    output$metric_dictionary_table <- renderTable(
      METRICS %>% transmute(Metric, Type, `Name suffix` = str_c(METRIC_SEP, Suffix), `Also detected as` = str_replace_all(Aliases, fixed("|"), ", ")),
      striped = TRUE, hover = TRUE, spacing = "xs")
    output$channel_rules_table <- renderTable(
      CHANNEL_METRIC_RULES %>% transmute(`Channel (VariableName contains)` = str_replace_all(Pattern, fixed("|"), ", "), `Preferred activity metric` = Metric),
      striped = TRUE, hover = TRUE, spacing = "xs")

    output$abbreviations_table <- renderTable(
      CHANNEL_ABBREVIATIONS %>% transmute(`Channel (any case)` = str_replace_all(Phrase, fixed("|"), ", "), `Written as` = Abbreviation),
      striped = TRUE, hover = TRUE, spacing = "xs")

    # These are called from other modules (Restore / Reset): they must run in
    # this module's session so new rows are namespaced under "media-".
    list(
      load_vof = function(components, backcasting = list(enabled = FALSE), name = NULL)
        withReactiveDomain(session, load_vof(components, backcasting, name)),
      reset = function() withReactiveDomain(session, reset_builder(add_empty_row = !is.null(isolate(upload$data()))))
    )
  })
}
