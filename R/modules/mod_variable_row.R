# R/modules/mod_variable_row.R

variableRowUI <- function(id, initial_state = list(sub=FALSE, effect=FALSE, period=FALSE, cs=FALSE), values = NULL) {
  ns <- NS(id)
  
  init_vis <- function(elem, show) {
    if(isTRUE(show)) elem else shinyjs::hidden(elem)
  }
  
  get_val <- function(key, default = "") {
    if(!is.null(values) && !is.null(values[[key]])) values[[key]] else default
  }
  
  get_date <- function(key) {
    val <- get_val(key, NA)
    if(is.na(val) || val == "NA" || val == "") NA else val
  }
  
  tags$div(
    id = ns("panel_container"),
    class = "panel", 
    style = "border: 1px solid #e0e0e0; padding: 15px; margin-bottom: 15px; border-radius: 8px; background-color: white;",
    
    fluidRow(
      column(11, uiOutput(ns("analytical_var_ui"))),
      column(1, actionButton(ns("remove_btn"), "", icon = icon("trash"), class = "btn-danger", style = "margin-top: 25px; width: 100%;"))
    ),
    
    fluidRow(
      column(3, textInput(ns("media_channel"), "Media Channel", value = get_val("media_channel"))),
      init_vis(tags$div(id = ns("col_sub"), class = "col-sm-2", textInput(ns("sub_channel"), "Sub Channel", value = get_val("sub_channel"))), initial_state$sub),
      init_vis(tags$div(id = ns("col_effect"), class = "col-sm-2", selectInput(ns("effect"), "Effect", choices = c("", "Direct", "Halo"), selected = get_val("effect"))), initial_state$effect),
      init_vis(tags$div(id = ns("col_period_start"), class = "col-sm-2", div(class = "vof-datepicker", dateInput(ns("start_period"), "Start Period", value = get_date("start_period")))), initial_state$period),
      init_vis(tags$div(id = ns("col_period_end"), class = "col-sm-2", div(class = "vof-datepicker", dateInput(ns("end_period"), "End Period", value = get_date("end_period")))), initial_state$period)
    ),
    
    init_vis(tags$div(id = ns("wrap_cs"), uiOutput(ns("cs_ui"))), initial_state$cs)
  )
}

variableRowServer <- function(id, analytical_cols, global_settings, cs_detect, analytical_data, restore_data = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$analytical_var_ui <- renderUI({
      cols <- analytical_cols()
      if(is.null(cols)) cols <- character(0)
      sel <- if(!is.null(restore_data)) restore_data$analytical_var else NULL
      selectizeInput(ns("analytical_var"), "Analytical Variable", choices = cols, selected = sel, width = "100%")
    })
    
    observe({
      req(global_settings())
      config <- global_settings()
      shinyjs::toggle("col_sub", condition = config$incl_sub_channel)
      shinyjs::toggle("col_effect", condition = config$incl_effect)
      shinyjs::toggle("col_period_start", condition = config$incl_period)
      shinyjs::toggle("col_period_end", condition = config$incl_period)
      shinyjs::toggle("wrap_cs", condition = config$incl_cross_sectional)
    })
    
    output$cs_ui <- renderUI({
      cols_avail <- cs_detect()
      df_data <- analytical_data() 
      if(length(cols_avail) == 0) return(NULL)
      
      ui_elems <- list()
      possible_cs <- c("Geography", "Product", "Campaign", "Outlet", "Creative")
      
      for(cs in possible_cs){
        if(cs %in% cols_avail){
          input_id <- paste0("cs_", tolower(cs))
          # CAMBIO: Label limpio, solo el nombre de la dimensión
          label <- cs 
          choice_values <- if(!is.null(df_data)) sort(unique(df_data[[cs]])) else NULL
          
          sel_vals <- NULL
          if(!is.null(restore_data)) {
            key <- paste0("cs_", tolower(cs))
            if(!is.null(restore_data[[key]])) sel_vals <- restore_data[[key]]
          }
          
          ui_elems[[length(ui_elems) + 1]] <- column(2, selectizeInput(ns(input_id), label, choices = choice_values, selected = sel_vals, multiple = TRUE, options = list(create = FALSE)))
        }
      }
      do.call(fluidRow, ui_elems)
    })
    
    return(list(
      delete_signal = reactive(input$remove_btn),
      id = id,
      get_data = reactive({
        config <- global_settings()
        safe_char <- function(val) if(!is.null(val)) val else ""
        safe_date <- function(val) if(!is.null(val)) val else NA
        safe_cs   <- function(val) if(!is.null(val)) val else NULL
        
        list(
          analytical_var = safe_char(input$analytical_var),
          media_channel  = safe_char(input$media_channel),
          sub_channel    = if(config$incl_sub_channel) safe_char(input$sub_channel) else "",
          effect         = if(config$incl_effect) safe_char(input$effect) else "",
          start_period   = if(config$incl_period) safe_date(input$start_period) else NA,
          end_period     = if(config$incl_period) safe_date(input$end_period) else NA,
          cs_geography   = if(config$incl_cross_sectional) safe_cs(input$cs_geography) else NULL,
          cs_product     = if(config$incl_cross_sectional) safe_cs(input$cs_product) else NULL,
          cs_campaign    = if(config$incl_cross_sectional) safe_cs(input$cs_campaign) else NULL,
          cs_outlet      = if(config$incl_cross_sectional) safe_cs(input$cs_outlet) else NULL,
          cs_creative    = if(config$incl_cross_sectional) safe_cs(input$cs_creative) else NULL
        )
      })
    ))
  })
}