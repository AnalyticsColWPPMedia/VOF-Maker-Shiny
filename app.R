# app.R

source("global.R")
library(dplyr)
library(stringr)

# ==============================================================================
# FUNCIONES AUXILIARES (KPI Naming)
# ==============================================================================
to_title_case <- function(text) {
  text <- gsub("[_\\-]", " ", text)
  s <- strsplit(text, " ")[[1]]
  s <- paste(toupper(substring(s, 1, 1)), tolower(substring(s, 2)), sep = "", collapse = " ")
  return(trimws(s))
}

get_kpi_log_name <- function(raw_var_name, all_analytical_cols, cs_dims) {
  mff_dims <- c("Geography", "Product", "Campaign", "Outlet", "Creative")
  longitudinal_dims <- setdiff(mff_dims, cs_dims)
  
  var_cols <- all_analytical_cols[grepl("_", all_analytical_cols)]
  
  if(length(var_cols) == 0 || !grepl("_", raw_var_name)) {
    return(paste("Log", to_title_case(raw_var_name)))
  }
  
  splits <- strsplit(var_cols, "_")
  max_len <- max(sapply(splits, length))
  
  if(max_len != (length(longitudinal_dims) + 1)) {
    base_name <- strsplit(raw_var_name, "_")[[1]][1]
    return(paste("Log", to_title_case(base_name)))
  }
  
  meta_df <- do.call(rbind, lapply(splits, function(x) {
    length(x) <- max_len 
    return(x)
  }))
  
  usable_indices <- c()
  for (i in 2:max_len) {
    unique_vals <- unique(meta_df[, i])
    if (length(unique_vals) > 1) {
      usable_indices <- c(usable_indices, i)
    }
  }
  
  raw_splits <- strsplit(raw_var_name, "_")[[1]]
  var_name_base <- raw_splits[1]
  usable_vals <- raw_splits[usable_indices]
  usable_vals <- usable_vals[!is.na(usable_vals)]
  
  final_components <- c("Log", to_title_case(var_name_base), sapply(usable_vals, to_title_case))
  
  return(paste(final_components, collapse = " "))
}

# ==============================================================================
# UI
# ==============================================================================
ui <- fluidPage(
  title = "VOF Maker",
  useShinyjs(),
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(src = "custom.js")
  ),
  
  div(class = "app-header",
      div(class = "header-left", tags$img(src = "wpp_media_logo_white.png", height = "50px")),
      div(class = "header-center", h1("VOF Maker", class = "app-title"), div("By Advanced Analytics Colombia", class = "app-subtitle")),
      div(class = "header-right", tags$img(src = "wpp_media_logo_white.png", height = "50px"))
  ),
  
  div(class = "section-card", h4("Data Management", class = "card-title"),
      fluidRow(
        column(4, div(class = "upload-zone", h5("Analytical Dataset"), fileInput("file_analytical", NULL, accept = c(".RData", ".csv"), buttonLabel = "Browse...", placeholder = "No file selected"))),
        column(4, div(class = "upload-zone", h5("VOF Metadata (Optional)"), fileInput("file_metadata", NULL, accept = c(".xlsx", ".csv"), buttonLabel = "Browse...", placeholder = "Add Old Metadata to History"))),
        column(4, div(class = "upload-zone", h5("Model Details (For Purifier)"), fileInput("file_details", NULL, accept = c(".xlsx", ".csv"), buttonLabel = "Browse...", placeholder = "Purify based on this file")))
      )
  ),
  
  div(id = "main_app_content",
      
      tabsetPanel(id = "main_tabs", type = "pills",
                  
                  # === PESTAÑA 1: MEDIA ===
                  tabPanel("Media",
                           tags$br(),
                           div(class = "section-card", h4("Configuration Options", class = "card-title"),
                               fluidRow(
                                 column(3, checkboxInput("show_sub_channel", "Sub Channel", FALSE)),
                                 column(3, checkboxInput("show_effect", "Effect", FALSE)),
                                 column(3, checkboxInput("show_period", "Period", FALSE)),
                                 column(3, checkboxInput("show_cs", "Cross Sectional", FALSE))
                               )
                           ),
                           div(class = "section-card", h4("Variable Builder", class = "card-title"),
                               div(id = "modules_container"), tags$br(),
                               actionButton("add_variable_btn", "Add Component Row", class = "btn btn-primary", icon = icon("plus"))
                           ),
                           div(class = "section-card", h4("Actions & Output", class = "card-title"),
                               fluidRow(
                                 column(3, actionButton("generate_vof_btn", "Generate VOF Code & Save", class = "btn btn-primary", style = "width: 100%;"), tags$div(style = "margin-top: 10px; text-align: center;", checkboxInput("auto_spend", "Auto-generate Spend VOFs", value = TRUE))),
                                 column(3, actionButton("clear_ui_btn", "Next VOF (Clear UI)", class = "btn btn-success", style = "width: 100%;", icon = icon("arrow-right"))),
                                 column(2, actionButton("import_code_modal_btn", "Restore Project", class = "btn btn-info", style = "width: 100%;", icon = icon("history")), tags$div(style = "margin-top: 5px;", actionButton("import_legacy_modal_btn", "Import Legacy R", class = "btn btn-secondary", style = "width: 100%; background-color: #8e44ad; border-color: #8e44ad; color: white;", icon = icon("code")))),
                                 column(2, actionButton("purify_btn", "Purify Project", class = "btn btn-warning", style = "width: 100%; color: white;", icon = icon("filter"))),
                                 column(2, actionButton("reset_all_btn", "Reset All", class = "btn btn-danger", style = "width: 100%;", icon = icon("trash")))
                               ),
                               uiOutput("history_status_ui"), tags$hr(),
                               h5("Generated Code (Current VOF)", style="font-weight: 600; color: #555;"),
                               textAreaInput("final_code_area", NULL, value = "", rows = 12, width = "100%", resize = "vertical"), tags$br(),
                               downloadButton("download_csv", "Download Full Metadata CSV", class = "btn btn-primary custom-download-btn", style = "width: 100%;")
                           )
                  ),
                  
                  # === PESTAÑA 2: KPI ===
                  tabPanel("KPI",
                           tags$br(),
                           div(class = "section-card", h4("KPI Selection & Transformation", class = "card-title"),
                               fluidRow(
                                 column(4, uiOutput("kpi_selector_ui")),
                                 column(4, 
                                        checkboxInput("gen_log", "Transform KPI to Log", TRUE),
                                        checkboxInput("gen_weight", "Create Weight Variable", TRUE)
                                 )
                               )
                           ),
                           
                           conditionalPanel(
                             condition = "input.gen_weight == true",
                             div(class = "section-card", h4("Fiscal Year Settings", class = "card-title"),
                                 fluidRow(
                                   column(12,
                                          radioButtons("fy_method", "Select Business Cycle Method:",
                                                       choices = c("Trailing 52-Week Cycles" = "auto",
                                                                   "Custom FY Table (Manual/Upload)" = "custom"),
                                                       selected = "custom",
                                                       inline = TRUE)
                                   )
                                 ),
                                 conditionalPanel(
                                   condition = "input.fy_method == 'custom'",
                                   tags$hr(),
                                   fluidRow(
                                     column(6, textInput("fy_label", "Fiscal Year Label (text)", value = "FY")),
                                     column(6, fileInput("upload_fy", "Upload FY settings (csv)", accept = ".csv"))
                                   ),
                                   uiOutput("fy_dynamic_rows_ui"),
                                   tags$br(),
                                   downloadButton("download_fy", "Download FY settings", class = "btn btn-info custom-download-btn")
                                 )
                             )
                           ),
                           
                           div(class = "section-card", h4("Actions & Output (KPI)", class = "card-title"),
                               actionButton("generate_kpi_btn", "Generate KPI Code & Save", class = "btn btn-primary", style="width: 250px;"),
                               tags$hr(),
                               textAreaInput("kpi_code_area", NULL, value = "", rows = 12, width = "100%", resize = "vertical")
                           )
                  ),
                  
                  # === PESTAÑA 3: EXTERNAL SOURCES ===
                  tabPanel("External Sources",
                           tags$br(),
                           div(class = "section-card", h4("External Data Upload & Mapping", class = "card-title"),
                               fluidRow(
                                 column(3, fileInput("ext_file", "Upload Data (CSV/XLSX)", accept = c(".csv", ".xlsx", ".xls"))),
                                 column(3, textInput("ext_var_name", "Variable Name", placeholder = "e.g., Google_Trends")),
                                 column(2, selectInput("ext_period_col", "Period Column", choices = NULL)),
                                 column(2, selectInput("ext_val_col", "Value Column", choices = NULL))
                               ),
                               fluidRow(
                                 column(6),
                                 column(3, selectInput("ext_cs_col", "External Entity Column (Optional)", choices = c("None" = ""))),
                                 column(3, uiOutput("ext_cs_target_ui"))
                               )
                           ),
                           div(class = "section-card", h4("Metadata", class = "card-title"),
                               fluidRow(
                                 column(4, textInput("ext_source", "Source (URL, platform, etc.)", width = "100%")),
                                 column(4, textInput("ext_notes", "Specific notes", width = "100%")),
                                 column(4, textInput("ext_why", "Why use VOF?", width = "100%"))
                               )
                           ),
                           div(class = "section-card", h4("Diagnosis & Settings", class = "card-title"),
                               uiOutput("ext_diagnosis_ui")
                           ),
                           div(class = "section-card", h4("Actions & Output", class = "card-title"),
                               actionButton("generate_ext_btn", "Generate VOF", class = "btn btn-primary", style = "width: 250px;"),
                               tags$hr(),
                               textAreaInput("ext_code_area", "Generated Code", value = "", rows = 12, width = "100%", resize = "vertical")
                           )
                  )
      )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================
server <- function(input, output, session) {
  
  observe({
    if (is.null(input$file_analytical)) {
      shinyjs::disable("main_app_content")
    } else {
      shinyjs::enable("main_app_content")
    }
  })
  
  analytical_data <- reactive({
    req(input$file_analytical)
    ext <- tools::file_ext(input$file_analytical$name)
    tryCatch({
      if(ext == "csv") read.csv(input$file_analytical$datapath, stringsAsFactors = FALSE)
      else if(ext == "RData") { e <- new.env(); load(input$file_analytical$datapath, envir = e); e[[ls(e)[1]]] }
    }, error = function(e) { showNotification(paste("Error:", e$message), type = "error"); NULL })
  })
  
  analytical_cols <- reactive({
    req(analytical_data())
    cols <- colnames(analytical_data())
    grep("_Total", cols, ignore.case = TRUE, value = TRUE)
  })
  
  cs_detect <- reactive({
    req(analytical_data())
    cols <- colnames(analytical_data())
    c("Geography", "Product", "Campaign", "Outlet", "Creative")[c("Geography", "Product", "Campaign", "Outlet", "Creative") %in% cols]
  })
  
  global_settings <- reactive({
    list(incl_sub_channel = input$show_sub_channel, incl_effect = input$show_effect, incl_period = input$show_period, incl_cross_sectional = input$show_cs)
  })
  
  vof_history_df <- reactiveVal(data.frame()) 
  
  # --- PESTAÑA KPI: SELECTORES Y WIDGETS ---
  
  output$kpi_selector_ui <- renderUI({
    req(analytical_data())
    cols <- colnames(analytical_data())
    var_cols <- cols[grepl("_", cols)] 
    if (length(var_cols) == 0) var_cols <- cols
    selectInput("kpi_var", "Select the KPI Variable", choices = var_cols, width = "100%")
  })
  
  r_detected_years <- reactive({
    req(analytical_data())
    df <- analytical_data()
    if (!"Period" %in% names(df)) return(NULL)
    yrs <- sort(unique(as.numeric(format(as.Date(df$Period), "%Y"))))
    return(yrs[!is.na(yrs)])
  })
  
  output$fy_dynamic_rows_ui <- renderUI({
    yrs <- r_detected_years()
    req(yrs)
    df <- analytical_data()
    all_dates <- as.Date(df$Period)
    
    ui_list <- lapply(yrs, function(y) {
      year_dates <- all_dates[format(all_dates, "%Y") == as.character(y)]
      year_dates <- year_dates[!is.na(year_dates)]
      
      if (length(year_dates) > 0) {
        default_start <- min(year_dates)
        default_end <- max(year_dates)
      } else {
        default_start <- as.Date(paste0(y, "-04-01"))
        default_end <- as.Date(paste0(y + 1, "-03-31"))
      }
      
      fluidRow(style = "margin-bottom: 5px; display: flex; align-items: center;",
               column(3, checkboxInput(paste0("fy_inc_", y), label = as.character(y), value = TRUE)),
               column(4, dateInput(paste0("fy_start_", y), label = NULL, value = default_start)),
               column(5, dateInput(paste0("fy_end_", y), label = NULL, value = default_end))
      )
    })
    do.call(tagList, ui_list)
  })
  
  r_fy_mapping <- reactive({
    yrs <- r_detected_years()
    req(yrs)
    df_map <- data.frame(Year_Cal = integer(), Include = logical(), Start = as.Date(character()), End = as.Date(character()))
    for (y in yrs) {
      inc_val <- input[[paste0("fy_inc_", y)]]
      start_val <- input[[paste0("fy_start_", y)]]
      end_val <- input[[paste0("fy_end_", y)]]
      if (!is.null(inc_val) && !is.null(start_val) && !is.null(end_val)) {
        df_map <- rbind(df_map, data.frame(Year_Cal = y, Include = inc_val, Start = as.Date(start_val), End = as.Date(end_val)))
      }
    }
    return(df_map)
  })
  
  observeEvent(input$upload_fy, {
    req(input$upload_fy)
    fy_csv <- read.csv(input$upload_fy$datapath)
    if (all(c("Year_Cal", "Include", "Start", "End") %in% names(fy_csv))) {
      for (i in 1:nrow(fy_csv)) {
        y <- fy_csv$Year_Cal[i]
        updateCheckboxInput(session, paste0("fy_inc_", y), value = as.logical(fy_csv$Include[i]))
        updateDateInput(session, paste0("fy_start_", y), value = as.Date(fy_csv$Start[i]))
        updateDateInput(session, paste0("fy_end_", y), value = as.Date(fy_csv$End[i]))
      }
    }
  })
  
  output$download_fy <- downloadHandler(
    filename = function() { "FY Settings.csv" },
    content = function(file) { req(r_fy_mapping()); write.csv(r_fy_mapping(), file, row.names = FALSE) }
  )
  
  # --- PESTAÑA KPI: GENERACIÓN DE CÓDIGO ---
  observeEvent(input$generate_kpi_btn, {
    req(input$kpi_var)
    df_analytical <- analytical_data()
    all_cols <- colnames(df_analytical)
    active_cs <- cs_detect()
    
    log_var_name <- get_kpi_log_name(input$kpi_var, all_cols, active_cs)
    
    code_text <- ""
    new_rows_metadata <- data.frame()
    
    if (isTRUE(input$gen_log)) {
      log_code <- glue::glue(
        "#### DepVar ####\n",
        "Data$`{log_var_name}` = 0\n",
        "Data$`{log_var_name}` = ifelse(Data$`{input$kpi_var}` <= 0, 0, log(Data$`{input$kpi_var}`))"
      )
      code_text <- paste0(code_text, log_code, "\n\n")
      
      row_log <- data.frame(
        Type = "KPI_Log", MainModelVariableName = log_var_name, AnalyticalVariableName = input$kpi_var,
        MediaChannel = "", SubChannel = "", Effect = "", MinPeriod = "", MaxPeriod = "", Metric = "KPI",
        InitCode = glue::glue("Data$`{log_var_name}` = 0"),
        FeedCode = glue::glue("Data$`{log_var_name}` = ifelse(Data$`{input$kpi_var}` <= 0, 0, log(Data$`{input$kpi_var}`))"),
        stringsAsFactors = FALSE
      )
      if (length(active_cs) > 0) { for(d in active_cs) row_log[[d]] <- "" }
      new_rows_metadata <- bind_rows(new_rows_metadata, row_log)
    }
    
    if (isTRUE(input$gen_weight)) {
      fy_code <- ""
      
      if (input$fy_method == "auto") {
        req("Period" %in% all_cols)
        valid_dates <- as.Date(df_analytical$Period[!is.na(df_analytical[[input$kpi_var]]) & df_analytical[[input$kpi_var]] > 0])
        
        if (length(valid_dates) > 0) {
          valid_dates <- sort(valid_dates)
          max_date <- max(valid_dates, na.rm = TRUE)
          max_year <- as.numeric(format(max_date, "%Y"))
          
          get_closest_date <- function(target, available_dates) {
            available_dates[which.min(abs(available_dates - target))]
          }
          
          c1_end <- max_date
          c1_start <- get_closest_date(c1_end - 364, valid_dates) 
          lbl1 <- paste0("FY", substr(as.character(max_year), 3, 4))
          
          c2_end <- get_closest_date(c1_start - 7, valid_dates)
          c2_start <- get_closest_date(c2_end - 364, valid_dates)
          lbl2 <- paste0("FY", substr(as.character(max_year - 1), 3, 4))
          
          c3_end <- get_closest_date(c2_start - 7, valid_dates)
          c3_start <- get_closest_date(c3_end - 364, valid_dates)
          lbl3 <- paste0("FY", substr(as.character(max_year - 2), 3, 4))
          
          fy_code <- glue::glue(
            "# Temporal Business Cycle (Trailing 52-Week Cycles)\n",
            "Data$FiscalYear = ifelse(Data$Period >= as.Date('{c3_start}') & Data$Period <= as.Date('{c3_end}'), '{lbl3}',\n",
            "                         ifelse(Data$Period >= as.Date('{c2_start}') & Data$Period <= as.Date('{c2_end}'), '{lbl2}',\n",
            "                                ifelse(Data$Period >= as.Date('{c1_start}') & Data$Period <= as.Date('{c1_end}'), '{lbl1}',\n",
            "                                       'OUT OF MODEL SCOPE')))"
          )
        } else {
          fy_code <- "# Error: No valid dates found with numbers greater than 0 in the selected KPI variable."
        }
        
      } else {
        fy_mapping_df <- r_fy_mapping()
        if (!is.null(fy_mapping_df) && nrow(fy_mapping_df) > 0) {
          fy_mapping_active <- fy_mapping_df[fy_mapping_df$Include == TRUE, ]
          
          if (nrow(fy_mapping_active) > 0) {
            fy_code_lines <- c()
            for (i in 1:nrow(fy_mapping_active)) {
              yr_suffix <- substr(as.character(fy_mapping_active$Year_Cal[i]), 3, 4)
              fy_label_val <- paste0(input$fy_label, yr_suffix)
              s_date <- fy_mapping_active$Start[i]
              e_date <- fy_mapping_active$End[i]
              
              cond <- glue::glue("Data$Period >= as.Date('{s_date}') & Data$Period <= as.Date('{e_date}')")
              
              if (i == 1) {
                fy_code_lines <- c(fy_code_lines, glue::glue("Data$FiscalYear = ifelse({cond}, '{fy_label_val}',"))
              } else {
                padding <- paste(rep(" ", 25), collapse = "")
                fy_code_lines <- c(fy_code_lines, glue::glue("{padding}ifelse({cond}, '{fy_label_val}',"))
              }
            }
            padding <- paste(rep(" ", 25), collapse = "")
            closing <- paste0(padding, "'OUT OF MODEL SCOPE'", paste(rep(")", nrow(fy_mapping_active)), collapse = ""))
            fy_code_lines <- c(fy_code_lines, closing)
            fy_code <- paste(c("# Temporal Business Cycle (Custom FY Table)", fy_code_lines), collapse = "\n")
            
          } else {
            fy_code <- "# Error: No active Fiscal Years selected in custom settings."
          }
        } else {
          fy_code <- "# Warning: Custom FY Table is empty."
        }
      }
      
      cs_input_string <- ""
      if (length(active_cs) > 0) {
        cs_input_string <- paste(sapply(active_cs, function(d) glue::glue("Data${d}")), collapse = ", ")
        cs_input_string <- paste0(cs_input_string, ", ")
      }
      
      weight_code <- glue::glue(
        "#### Weight Variable ####\n\n",
        "{fy_code}\n\n",
        "Data$`Weight Variable MMM` <- ave(\n",
        "  # Target Variable (DepVar Transformed)\n",
        "  Data$`{if(isTRUE(input$gen_log)) log_var_name else input$kpi_var}`,\n",
        "  # Cross-sectional dimensions + Temporal Business Cycle\n",
        "  {cs_input_string}Data$FiscalYear,\n",
        "  FUN = function(x) {{\n",
        "    valid_x <- x[!is.na(x) & x != 0]\n",
        "    if(length(valid_x) == 0) {{\n",
        "      return(0)\n",
        "    }} else {{\n",
        "      return(mean(valid_x))\n",
        "    }}\n",
        "  }}\n",
        ")"
      )
      code_text <- paste0(code_text, weight_code)
      
      row_weight <- data.frame(
        Type = "Weight_Var", MainModelVariableName = "Weight Variable MMM", AnalyticalVariableName = if(isTRUE(input$gen_log)) log_var_name else input$kpi_var,
        MediaChannel = "", SubChannel = "", Effect = "", MinPeriod = "", MaxPeriod = "", Metric = "Weight",
        InitCode = "", FeedCode = as.character(weight_code), stringsAsFactors = FALSE
      )
      if (length(active_cs) > 0) { for(d in active_cs) row_weight[[d]] <- "" }
      new_rows_metadata <- bind_rows(new_rows_metadata, row_weight)
    }
    
    updateTextAreaInput(session, "kpi_code_area", value = code_text)
    
    if (nrow(new_rows_metadata) > 0) {
      vof_history_df(bind_rows(vof_history_df(), new_rows_metadata))
      showNotification("KPI Variables successfully saved to history!", type = "message")
    }
  })
  
  # --- BLOQUES PREEXISTENTES (Media, Importación, Descargas) ---
  observeEvent(input$restore_trigger, {
    req(input$restore_trigger)
    tryCatch({
      restored_df <- jsonlite::fromJSON(input$restore_trigger, flatten = TRUE)
      if(!is.data.frame(restored_df)) restored_df <- dplyr::bind_rows(restored_df)
      if("MinPeriod" %in% names(restored_df)) restored_df$MinPeriod <- suppressWarnings(as.Date(restored_df$MinPeriod))
      if("MaxPeriod" %in% names(restored_df)) restored_df$MaxPeriod <- suppressWarnings(as.Date(restored_df$MaxPeriod))
      if(nrow(restored_df) > 0){
        vof_history_df(restored_df)
        showNotification(paste("Restored", nrow(restored_df), "VOFs from browser history."), type = "message", duration = 5)
      }
    }, error = function(e) {
      session$sendCustomMessage("clearBrowserStorage", "reset")
      vof_history_df(data.frame())
    })
  })
  
  observeEvent(vof_history_df(), {
    current_data <- vof_history_df()
    if(nrow(current_data) > 0){
      json_data <- jsonlite::toJSON(current_data, dataframe = "rows")
      session$sendCustomMessage("saveToBrowser", json_data)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$import_code_modal_btn, {
    showModal(modalDialog(
      title = "Restore from VOF Code", size = "l",
      p("Paste your generated R code below..."),
      textAreaInput("paste_code_area", "Paste Code Here", rows = 15, width = "100%", placeholder = "#### Title ####\nData[...] <- ..."),
      footer = tagList(modalButton("Cancel"), actionButton("process_import_btn", "Process & Restore", class = "btn btn-success"))
    ))
  })
  
  observeEvent(input$import_legacy_modal_btn, {
    showModal(modalDialog(
      title = "Import Legacy R Code (Phase 1)", size = "l",
      p("Paste your raw R code below. The app will extract VOF definitions based on 'rowSums' and standard filtering syntax."),
      p(tags$small("Supported: Cumulative rowSums, Date filters (Period), CS filters (tolower %in%). Unstructured code will be skipped.")),
      textAreaInput("paste_legacy_area", "Paste R Code Here", rows = 15, width = "100%", placeholder = "Data[, 'VOF'] <- rowSums(...)"),
      footer = tagList(modalButton("Cancel"), actionButton("process_legacy_btn", "Process Legacy Code", class = "btn btn-success"))
    ))
  })
  
  observeEvent(input$process_legacy_btn, {
    req(input$paste_legacy_area)
    req(analytical_cols()) 
    
    legacy_meta <- parse_legacy_r_code(
      code_text = input$paste_legacy_area, 
      analytical_cols_list = analytical_cols(),
      active_cs_dims = cs_detect()
    )
    
    if (nrow(legacy_meta) == 0) {
      showNotification("No valid legacy definitions found or no matching roots.", type = "error")
      return()
    }
    
    removeModal()
    current_hist <- vof_history_df()
    new_hist <- bind_rows(current_hist, legacy_meta)
    vof_history_df(new_hist)
    showNotification(glue::glue("Successfully extracted {nrow(legacy_meta)} metadata rows from Legacy Code."), type = "message", duration = 8)
  })
  
  observeEvent(input$process_import_btn, {
    req(input$paste_code_area)
    vofs_found <- parse_vof_code_to_list(input$paste_code_area)
    if(length(vofs_found) == 0) { showNotification("No valid VOFs found.", type = "error"); return() }
    removeModal()
    
    new_meta_accum <- data.frame()
    vof_names <- names(vofs_found)
    
    for(vname in vof_names) {
      rows_data <- vofs_found[[vname]]
      is_spend <- grepl("---Spend$", vname, ignore.case = TRUE)
      
      for(i in seq_along(rows_data)) {
        rows_data[[i]]$vof_name_override <- vname
      }
      
      meta_df <- generate_vof_data_from_list(
        all_modules_data = rows_data, 
        active_cs_dims = cs_detect(), 
        output_type = "data.frame",
        generate_spend = FALSE, 
        all_analytical_vars = analytical_cols()
      )
      
      if (!is.null(meta_df) && nrow(meta_df) > 0) {
        if (is_spend) meta_df$Metric <- "Spend"
        new_meta_accum <- bind_rows(new_meta_accum, meta_df)
      }
    }
    
    vof_history_df(bind_rows(vof_history_df(), new_meta_accum))
    
    vof_names_base <- vof_names[!grepl("---Spend$", vof_names, ignore.case = TRUE)]
    if(length(vof_names_base) > 0) {
      last_vof_name <- vof_names_base[length(vof_names_base)]
    } else {
      last_vof_name <- vof_names[length(vof_names)]
    }
    
    last_vof_rows <- vofs_found[[last_vof_name]]
    current_mods <- modules_list()
    for(mod_id in names(current_mods)){ removeUI(selector = paste0("#", NS(mod_id, "panel_container"))) }
    modules_list(list()) 
    
    for(row_data_iter in last_vof_rows) {
      local({
        my_row_data <- row_data_iter 
        counter(counter() + 1)
        new_id <- paste0("var_mod_", counter())
        
        has_sub <- my_row_data$sub_channel != ""
        has_eff <- my_row_data$effect != ""
        has_per <- !is.na(my_row_data$start_period) || !is.na(my_row_data$end_period)
        has_cs  <- any(grepl("cs_", names(my_row_data)))
        
        if(has_sub) updateCheckboxInput(session, "show_sub_channel", value = TRUE)
        if(has_eff) updateCheckboxInput(session, "show_effect", value = TRUE)
        if(has_per) updateCheckboxInput(session, "show_period", value = TRUE)
        if(has_cs)  updateCheckboxInput(session, "show_cs", value = TRUE)
        
        current_state <- list(sub=has_sub, effect=has_eff, period=has_per, cs=has_cs)
        values_list <- list(media_channel=my_row_data$media_channel, sub_channel=my_row_data$sub_channel, effect=my_row_data$effect, start_period=my_row_data$start_period, end_period=my_row_data$end_period)
        
        insertUI(selector = "#modules_container", where = "beforeEnd", ui = variableRowUI(new_id, initial_state = current_state, values = values_list))
        mod_instance <- variableRowServer(new_id, analytical_cols, global_settings, cs_detect, analytical_data, restore_data = my_row_data)
        
        curr <- modules_list()
        curr[[new_id]] <- mod_instance
        modules_list(curr)
      })
    }
    showNotification(glue::glue("Restored {length(vof_names)} elements. Loaded '{last_vof_name}' to UI."), type = "message", duration = 8)
  })
  
  modules_list <- reactiveVal(list())
  counter <- reactiveVal(0)
  
  observeEvent(input$add_variable_btn, {
    if(is.null(input$file_analytical)) { showNotification("Upload data first", type = "warning"); return() }
    
    # --- LÓGICA DE HERENCIA: Extraer filtros del componente anterior ---
    curr_mods <- modules_list()
    inherit_values <- NULL
    
    if (length(curr_mods) > 0) {
      last_mod_id <- names(curr_mods)[length(curr_mods)]
      last_data <- curr_mods[[last_mod_id]]$get_data()
      
      inherit_values <- list(
        start_period = last_data$start_period,
        end_period   = last_data$end_period,
        cs_geography = last_data$cs_geography,
        cs_product   = last_data$cs_product,
        cs_campaign  = last_data$cs_campaign,
        cs_outlet    = last_data$cs_outlet,
        cs_creative  = last_data$cs_creative
      )
    }
    
    counter(counter() + 1)
    new_id <- paste0("var_mod_", counter())
    current_state <- list(sub = isTRUE(input$show_sub_channel), effect = isTRUE(input$show_effect), period = isTRUE(input$show_period), cs = isTRUE(input$show_cs))
    
    # Inyectar los valores heredados tanto en la UI como en el Server
    insertUI(selector = "#modules_container", where = "beforeEnd", ui = variableRowUI(new_id, initial_state = current_state, values = inherit_values))
    mod_instance <- variableRowServer(new_id, analytical_cols, global_settings, cs_detect, analytical_data, restore_data = inherit_values)
    
    current <- modules_list()
    current[[new_id]] <- mod_instance
    modules_list(current)
  })
  
  
  observe({
    current_mods <- modules_list()
    for(mod_id in names(current_mods)){
      mod <- current_mods[[mod_id]]
      local({
        my_mod <- mod; my_id <- mod_id
        observeEvent(my_mod$delete_signal(), {
          removeUI(selector = paste0("#", NS(my_id, "panel_container")))
          all_mods <- modules_list()
          all_mods[[my_id]] <- NULL
          modules_list(all_mods)
        }, ignoreInit = TRUE, once = TRUE)
      })
    }
  })
  
  observeEvent(input$generate_vof_btn, {
    req(length(modules_list()) > 0)
    all_inputs_data <- lapply(modules_list(), function(mod) mod$get_data())
    code_text <- generate_vof_data_from_list(all_inputs_data, output_type = "text", generate_spend = input$auto_spend, all_analytical_vars = analytical_cols())
    updateTextAreaInput(session, "final_code_area", value = code_text)
    current_meta <- generate_vof_data_from_list(all_inputs_data, active_cs_dims = cs_detect(), output_type = "data.frame", generate_spend = input$auto_spend, all_analytical_vars = analytical_cols())
    vof_history_df(bind_rows(vof_history_df(), current_meta))
    showNotification("VOF saved to history!", type = "message")
  })
  
  observeEvent(input$clear_ui_btn, {
    current_mods <- modules_list()
    for(mod_id in names(current_mods)){ removeUI(selector = paste0("#", NS(mod_id, "panel_container"))) }
    modules_list(list()) 
    updateCheckboxInput(session, "show_sub_channel", value = FALSE)
    updateCheckboxInput(session, "show_effect", value = FALSE)
    updateCheckboxInput(session, "show_period", value = FALSE)
    updateCheckboxInput(session, "show_cs", value = FALSE)
    updateTextAreaInput(session, "final_code_area", value = "")
    showNotification("UI Cleared.", type = "warning")
  })
  
  observeEvent(input$reset_all_btn, {
    showModal(modalDialog(title = "Reset Project", "Delete all history?", footer = tagList(modalButton("Cancel"), actionButton("confirm_reset", "Yes, Delete", class = "btn btn-danger"))))
  })
  
  observeEvent(input$confirm_reset, {
    removeModal()
    click("clear_ui_btn") 
    vof_history_df(data.frame())
    session$sendCustomMessage("clearBrowserStorage", "reset")
    showNotification("Project reset.", type = "error")
  })
  
  # ============================================================================
  # LÓGICA DE PURIFICACIÓN ACTUALIZADA CON FILTROS AVANZADOS
  # ============================================================================
  observeEvent(input$purify_btn, {
    req(input$file_details)
    full_meta <- vof_history_df()
    
    if(!is.null(input$file_metadata)){
      try({
        old_path <- input$file_metadata$datapath; is_csv <- tools::file_ext(input$file_metadata$name) == "csv"
        old_meta <- if(is_csv) read.csv(old_path, stringsAsFactors = FALSE) else as.data.frame(readxl::read_xlsx(old_path))
        old_meta[is.na(old_meta)] <- ""
        full_meta <- bind_rows(old_meta, full_meta)
      }, silent = TRUE)
    }
    
    if(nrow(full_meta) == 0) { showNotification("No metadata to purify.", type = "warning"); return() }
    
    tryCatch({
      ext <- tools::file_ext(input$file_details$name)
      details_df <- if(ext == "csv") read.csv(input$file_details$datapath, stringsAsFactors = FALSE) else as.data.frame(readxl::read_xlsx(input$file_details$datapath))
      
      # Conservar lógica de purificación original (ignorando None)
      if ("Type" %in% names(details_df)) {
        details_df_base <- details_df %>% filter(!str_detect(str_to_lower(Type), "none"))
      } else {
        details_df_base <- details_df
      }
      valid_vars <- as.character(details_df_base[[1]])
      
      # --- INICIO: LÓGICA PARA CONSTRUIR EL WARNING ---
      details_warning <- details_df
      
      # 1. Filtro por 'Type'
      if ("Type" %in% names(details_warning)) {
        details_warning <- details_warning %>% 
          filter(!str_detect(str_to_lower(Type), 'none|dep'))
      }
      
      # 2. Filtro por Dimensiones Longitudinales (Deben ser NA o vacías)
      mff_dims <- c('Geography', 'Product', 'Campaign', 'Outlet', 'Creative')
      active_cs <- cs_detect()
      longitudinal_dims <- setdiff(mff_dims, active_cs)
      
      for (ldim in longitudinal_dims) {
        if (ldim %in% names(details_warning)) {
          details_warning <- details_warning[is.na(details_warning[[ldim]]) | trimws(as.character(details_warning[[ldim]])) == "", ]
        }
      }
      
      warning_vars <- as.character(details_warning[[1]])
      
      # 3. Excluir sufijos no relacionados a medios
      warning_vars <- warning_vars[!str_detect(warning_vars, "(?i)Business|Macro|Seasonal|Season|Holiday|")]
      
      # Determinar cuáles de estas variables calculadas no están en la app
      found_in_meta <- unique(c(
        full_meta$MainModelVariableName,
        str_remove(full_meta$MainModelVariableName[str_ends(full_meta$MainModelVariableName, "---Spend")], "---Spend")
      ))
      
      missing_vars <- setdiff(warning_vars, found_in_meta)
      
      # 4. Ordenar poniendo los medios al inicio de la lista
      media_keywords <- "(?i)Display|Banner|Native|OLV|Online Video|Connected TV|CTV|Magazine|Newspaper|Event|Social|Search|OOH|TV|Radio|Cinema|Digital|Audio|Podcast|Influencer|Affiliate|Programmatic|VOD|Youtube|Meta|Facebook|Instagram|TikTok|Google|Bing|Pinterest|Snapchat"
      
      if(length(missing_vars) > 0) {
        media_missing <- missing_vars[str_detect(missing_vars, media_keywords)]
        other_missing <- missing_vars[!str_detect(missing_vars, media_keywords)]
        missing_vars <- c(media_missing, other_missing)
      }
      # --- FIN: LÓGICA PARA CONSTRUIR EL WARNING ---
      
      # Purificar (usando valid_vars original para no borrar nada por accidente)
      full_meta_purified <- full_meta %>%
        filter(
          MainModelVariableName %in% valid_vars |
            (str_ends(MainModelVariableName, "---Spend") & str_remove(MainModelVariableName, "---Spend") %in% valid_vars)
        )
      
      rows_before <- nrow(full_meta)
      rows_after <- nrow(full_meta_purified)
      vof_history_df(full_meta_purified)
      
      shinyjs::reset("file_metadata")
      shinyjs::runjs("Shiny.setInputValue('file_metadata', null);")
      
      if (length(missing_vars) > 0) {
        
        display_vars <- missing_vars
        if (length(missing_vars) > 15) {
          display_vars <- c(missing_vars[1:15], paste("... and", length(missing_vars) - 15, "more"))
        }
        
        showModal(modalDialog(
          title = tagList(icon("exclamation-triangle", style="color: #f39c12;"), " Warning: Missing Media VOFs"),
          p(style="font-weight: bold;", "Based on your Details file, the following expected Media variables were NOT found in your VOF Metadata:"),
          tags$div(style = "max-height: 200px; overflow-y: auto; background-color: #f8f9fa; padding: 10px; border: 1px solid #dee2e6; border-radius: 4px;",
                   tags$ul(style = "margin-bottom: 0;", lapply(display_vars, tags$li))),
          tags$br(),
          p("The purification process has been completed with the matching variables. You can continue, but note your metadata might be missing standard media builds."),
          easyClose = FALSE,
          footer = modalButton("I Understand & Continue")
        ))
      } else {
        showNotification(glue::glue("Purified Project! Reduced from {rows_before} to {rows_after} variables."), type = "message", duration = 8)
      }
      
    }, error = function(e) { showNotification(paste("Error reading Details file:", e$message), type = "error") })
  })
  
  output$history_status_ui <- renderUI({
    count <- nrow(vof_history_df())
    if(count > 0) div(class = "history-box", icon("database"), paste(count, "rows of metadata stored.")) else NULL
  })
  
  output$download_csv <- downloadHandler(
    filename = function() { paste0(format(Sys.time(), "%y%m%d%H%M%S"), " - VOF Metadata - your_proyect.csv") },
    content = function(file) {
      final_data <- vof_history_df()
      active_dims <- cs_detect()
      
      if(nrow(final_data) == 0 && length(modules_list()) > 0){
        all_inputs <- lapply(modules_list(), function(mod) mod$get_data())
        final_data <- generate_vof_data_from_list(all_inputs, active_cs_dims = active_dims, output_type = "data.frame", generate_spend = input$auto_spend, all_analytical_vars = analytical_cols())
      }
      
      if(!is.null(input$file_metadata)){
        try({
          old_path <- input$file_metadata$datapath; is_csv <- tools::file_ext(input$file_metadata$name) == "csv"
          old_meta <- if(is_csv) read.csv(old_path, stringsAsFactors = FALSE) else as.data.frame(readxl::read_xlsx(old_path))
          old_meta[is.na(old_meta)] <- ""
          
          map_old_to_new <- c("Geographies"="Geography", "Products"="Product", "Campaigns"="Campaign", "Outlets"="Outlet", "Creatives"="Creative")
          for(old_col in names(map_old_to_new)) {
            new_col_target <- map_old_to_new[[old_col]]
            if(new_col_target %in% active_dims && old_col %in% names(old_meta)) old_meta[[new_col_target]] <- as.character(old_meta[[old_col]])
          }
          
          cols_static <- c("Type", "MainModelVariableName", "AnalyticalVariableName", "MediaChannel", "SubChannel", "Effect", "MinPeriod", "MaxPeriod")
          cols_end <- c("Metric", "InitCode", "FeedCode")
          all_expected <- unique(c(cols_static, active_dims, cols_end))
          for(col in all_expected) { if(!col %in% names(old_meta)) old_meta[[col]] <- "" }
          
          cols_final_order <- c(cols_static, active_dims, "Metric", "InitCode", "FeedCode")
          old_meta_clean <- old_meta[, intersect(cols_final_order, names(old_meta)), drop=FALSE]
          final_data <- bind_rows(old_meta_clean, final_data)
        }, silent = TRUE)
      }
      
      if(nrow(final_data) > 0) {
        final_data[is.na(final_data)] <- ""
        
        if (!"Type" %in% names(final_data)) {
          final_data$Type <- "Media"
        } else {
          final_data$Type[final_data$Type == ""] <- "Media"
        }
        
        cols_static <- c("Type", "MainModelVariableName", "AnalyticalVariableName", "MediaChannel", "SubChannel", "Effect", "MinPeriod", "MaxPeriod")
        cols_final_order <- c(cols_static, active_dims, "Metric", "InitCode", "FeedCode")
        final_data <- final_data[, intersect(cols_final_order, names(final_data)), drop=FALSE]
        
        final_data <- dplyr::distinct(final_data)
      }
      
      req(nrow(final_data) > 0)
      write.csv(final_data, file, row.names = FALSE)
    }
  )
  
  # === EXTERNAL SOURCES LOGIC ===
  
  ext_data <- reactive({
    req(input$ext_file)
    ext <- tools::file_ext(input$ext_file$name)
    tryCatch({
      if(ext == "csv") {
        read.csv(input$ext_file$datapath, stringsAsFactors = FALSE)
      } else {
        as.data.frame(readxl::read_excel(input$ext_file$datapath))
      }
    }, error = function(e) {
      showNotification(paste("Error reading external file:", e$message), type = "error")
      NULL
    })
  })
  
  observeEvent(ext_data(), {
    req(ext_data())
    cols <- colnames(ext_data())
    updateSelectInput(session, "ext_period_col", choices = c("", cols))
    updateSelectInput(session, "ext_val_col", choices = c("", cols))
    updateSelectInput(session, "ext_cs_col", choices = c("None" = "", cols))
  })
  
  output$ext_cs_target_ui <- renderUI({
    req(input$ext_cs_col, input$ext_cs_col != "")
    active_cs <- cs_detect()
    selectInput("ext_cs_target", "Target Analytical Entity", choices = c("", active_cs))
  })
  
  output$ext_diagnosis_ui <- renderUI({
    req(ext_data(), input$ext_period_col, input$ext_val_col, analytical_data())
    
    df_ext <- ext_data()
    df_ana <- analytical_data()
    
    if(!("Period" %in% names(df_ana))) {
      return(tags$div(class = "alert alert-danger", "Analytical Dataset missing 'Period' column."))
    }
    
    ext_dates <- suppressWarnings(as.Date(as.character(df_ext[[input$ext_period_col]]), tryFormats = c("%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y", "%Y/%m/%d")))
    ana_dates <- sort(unique(as.Date(df_ana$Period)))
    ana_dates <- ana_dates[!is.na(ana_dates)]
    
    if(all(is.na(ext_dates))) {
      shinyjs::disable("generate_ext_btn")
      return(tags$div(class = "alert alert-danger", "Could not parse dates in the selected Period Column. Ensure they are in a standard format (e.g., YYYY-MM-DD)."))
    }
    
    ext_dates_clean <- ext_dates[!is.na(ext_dates)]
    matched_dates <- sum(ext_dates_clean %in% ana_dates)
    match_pct <- round((matched_dates / length(ext_dates_clean)) * 100, 1)
    
    msg_date <- tags$p(glue::glue("Date alignment: {match_pct}% of external dates match directly with analytical dates."))
    
    agg_ui <- NULL
    if(match_pct < 100 || length(unique(ext_dates_clean)) > length(ana_dates)) {
      msg_date <- tagList(msg_date, tags$p("Temporal misalignment or frequency mismatch detected. The app will align external dates to the nearest analytical date.", style="color: #e67e22; font-weight: bold;"))
      agg_ui <- radioButtons("ext_agg_method", "Select Temporal Aggregation Method:", choices = c("Sum" = "sum", "Mean" = "mean"), inline = TRUE)
    }
    
    cs_ui_msg <- NULL
    cs_ok <- TRUE
    
    if(input$ext_cs_col != "") {
      if(is.null(input$ext_cs_target) || input$ext_cs_target == "") {
        cs_ok <- FALSE
        cs_ui_msg <- tags$div(class = "alert alert-warning", "Please select a Target Analytical Entity for mapping.")
      } else {
        ext_cs_vals <- unique(trimws(as.character(df_ext[[input$ext_cs_col]])))
        ana_cs_vals <- unique(trimws(as.character(df_ana[[input$ext_cs_target]])))
        
        missing_in_ana <- setdiff(ext_cs_vals, ana_cs_vals)
        if(length(missing_in_ana) > 0) {
          cs_ok <- FALSE
          cs_ui_msg <- tags$div(class = "alert alert-danger",
                                tags$strong("Cross-Sectional Mismatch!"),
                                tags$p(glue::glue("The following entities exist in '{input$ext_cs_col}' but NOT in '{input$ext_cs_target}'. Code generation is blocked until this is fixed:")),
                                tags$ul(style = "max-height: 150px; overflow-y: auto;", lapply(missing_in_ana, tags$li))
          )
        } else {
          cs_ui_msg <- tags$div(class = "alert alert-success", "Cross-Sectional entities mapped perfectly!")
        }
      }
    }
    
    if(cs_ok) {
      shinyjs::enable("generate_ext_btn")
    } else {
      shinyjs::disable("generate_ext_btn")
    }
    
    tagList(
      tags$div(class = "alert alert-info", msg_date),
      agg_ui,
      cs_ui_msg
    )
  })
  
  observeEvent(input$generate_ext_btn, {
    req(input$ext_var_name, input$ext_period_col, input$ext_val_col)
    
    df_ext <- ext_data()
    df_ana <- analytical_data()
    
    ext_dates <- suppressWarnings(as.Date(as.character(df_ext[[input$ext_period_col]]), tryFormats = c("%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y", "%Y/%m/%d")))
    ana_dates <- sort(unique(as.Date(df_ana$Period)))
    ana_dates <- ana_dates[!is.na(ana_dates)]
    
    mapped_dates <- sapply(ext_dates, function(d) {
      if(is.na(d)) return(NA)
      ana_dates[which.min(abs(ana_dates - d))]
    })
    
    tmp <- data.frame(
      MappedDate = as.Date(mapped_dates, origin = "1970-01-01"),
      Value = as.numeric(df_ext[[input$ext_val_col]])
    )
    
    has_cs <- input$ext_cs_col != "" && !is.null(input$ext_cs_target) && input$ext_cs_target != ""
    if(has_cs) {
      tmp$Entity <- trimws(as.character(df_ext[[input$ext_cs_col]]))
    }
    
    tmp <- tmp[!is.na(tmp$MappedDate) & !is.na(tmp$Value), ]
    
    agg_func <- if(!is.null(input$ext_agg_method) && input$ext_agg_method == "mean") mean else sum
    
    if(has_cs) {
      agg_df <- aggregate(Value ~ MappedDate + Entity, data = tmp, FUN = agg_func)
    } else {
      agg_df <- aggregate(Value ~ MappedDate, data = tmp, FUN = agg_func)
    }
    
    if(has_cs) agg_df <- agg_df[order(agg_df$Entity, agg_df$MappedDate), ]
    else agg_df <- agg_df[order(agg_df$MappedDate), ]
    
    date_str <- paste(sprintf("'%s'", agg_df$MappedDate), collapse=", ")
    val_str <- paste(round(agg_df$Value, 4), collapse=", ")
    
    if(has_cs) {
      ent_str <- paste(sprintf("'%s'", agg_df$Entity), collapse=", ")
      df_code <- glue::glue("tmp_ext <- data.frame(
  Period = as.Date(c({date_str})),
  Entity = c({ent_str}),
  Value = c({val_str})
)")
      
      match_code <- glue::glue("
# 2. Create composite keys for exact mapping
key_data <- paste(Data$Period, Data$`{input$ext_cs_target}`, sep = '_')
key_ext <- paste(tmp_ext$Period, tmp_ext$Entity, sep = '_')

# 3. Map values directly using composite keys (match is safe and idempotent)
Data$`{input$ext_var_name}` <- tmp_ext$Value[match(key_data, key_ext)]
")
    } else {
      df_code <- glue::glue("tmp_ext <- data.frame(
  Period = as.Date(c({date_str})),
  Value = c({val_str})
)")
      
      match_code <- glue::glue("
# 2. Map values directly using match to avoid merge conflicts (.x/.y)
Data$`{input$ext_var_name}` <- tmp_ext$Value[match(Data$Period, tmp_ext$Period)]
")
    }
    
    final_code <- glue::glue("
#### External Variable: {input$ext_var_name} ####
# Source: {input$ext_source}
# Notes: {input$ext_notes}
# Why: {input$ext_why}

# 1. External data (Aggregated internally)
{df_code}
{match_code}
# Replace NAs with 0
Data$`{input$ext_var_name}`[is.na(Data$`{input$ext_var_name}`)] <- 0
")
    
    updateTextAreaInput(session, "ext_code_area", value = final_code)
    showNotification("External Data Code generated successfully!", type = "message")
  })
}

shinyApp(ui, server)