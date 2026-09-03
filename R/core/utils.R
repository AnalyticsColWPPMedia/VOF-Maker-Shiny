# R/core/utils.R

library(dplyr)
library(stringr)
library(lubridate)
library(glue)

# ==============================================================================
# 1. HELPERS GENÉRICOS
# ==============================================================================
is_valid_value <- function(x) {
  if (is.null(x)) return(FALSE)
  if (length(x) == 0) return(FALSE)
  if (inherits(x, "Date")) return(!all(is.na(x)))
  if (all(is.na(x))) return(FALSE)
  x_char <- as.character(x)
  if (all(trimws(x_char) == "")) return(FALSE)
  return(TRUE)
}
convert_to_char <- function(x) { if(is_valid_value(x)) as.character(x) else "" }
helper_period_vof_name <- function(date_var){
  if (!is_valid_value(date_var)) return("")
  d <- as.Date(date_var)
  sprintf("%04d%02d%02d", year(d), month(d), day(d))
}

detect_media_metric <- function(analytical_var, fallback = "Activity") {
  if (!is_valid_value(analytical_var)) return(fallback)
  normalized_var <- analytical_var %>%
    as.character() %>%
    str_replace_all("[_-]+", " ") %>%
    str_squish() %>%
    str_to_lower()

  metric_aliases <- list(
    "Gross Cost" = "gross cost",
    "Net Cost" = "net cost",
    "Engagements" = "engagements",
    "Impressions" = "impressions",
    "Circulation" = "circulation",
    "Attendance" = c("attendance", "attandance"),
    "Engagement" = "engagement",
    "Sessions" = "sessions",
    "Actions" = "actions",
    "Clicks" = "clicks",
    "Visits" = "visits",
    "Spend" = "spend",
    "Views" = "views",
    "Reach" = "reach",
    "GRPs" = "grps",
    "Cost" = "cost"
  )

  matches <- list()
  for (metric_name in names(metric_aliases)) {
    for (alias in metric_aliases[[metric_name]]) {
      locations <- str_locate_all(normalized_var, fixed(alias))[[1]]
      if (nrow(locations) > 0) {
        for (location_index in seq_len(nrow(locations))) {
          matches[[length(matches) + 1]] <- data.frame(
            metric = metric_name,
            start = locations[location_index, "start"],
            end = locations[location_index, "end"],
            length = locations[location_index, "end"] - locations[location_index, "start"] + 1,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }

  if (length(matches) == 0) return(fallback)

  matches_df <- bind_rows(matches) %>%
    arrange(start, desc(length))
  selected_rows <- integer(0)

  for (match_index in seq_len(nrow(matches_df))) {
    overlaps_existing <- if (length(selected_rows) == 0) {
      FALSE
    } else {
      any(
        matches_df$start[match_index] <= matches_df$end[selected_rows] &
          matches_df$end[match_index] >= matches_df$start[selected_rows]
      )
    }
    if (!overlaps_existing) selected_rows <- c(selected_rows, match_index)
  }

  detected_metrics <- matches_df$metric[selected_rows]
  detected_metrics <- detected_metrics[!duplicated(detected_metrics)]
  paste(detected_metrics, collapse = "/")
}

normalize_media_metrics <- function(metadata_df) {
  if (is.null(metadata_df) || !is.data.frame(metadata_df) || nrow(metadata_df) == 0 ||
      !"AnalyticalVariableName" %in% names(metadata_df)) {
    return(metadata_df)
  }

  if (!"Metric" %in% names(metadata_df)) metadata_df$Metric <- ""
  metadata_df$Metric <- as.character(metadata_df$Metric)
  if (!"Type" %in% names(metadata_df)) {
    is_media <- rep(TRUE, nrow(metadata_df))
  } else {
    metadata_type <- as.character(metadata_df$Type)
    is_media <- is.na(metadata_type) | trimws(metadata_type) == "" |
      str_to_lower(trimws(metadata_type)) == "media"
  }

  media_rows <- which(is_media)
  for (row_index in media_rows) {
    current_metric <- as.character(metadata_df$Metric[row_index])
    fallback <- if (!is.na(current_metric) && trimws(current_metric) != "") current_metric else "Activity"
    metadata_df$Metric[row_index] <- detect_media_metric(
      metadata_df$AnalyticalVariableName[row_index],
      fallback
    )
  }

  metadata_df
}

normalize_modeled_var_type <- function(metadata_df) {
  if (is.null(metadata_df) || !is.data.frame(metadata_df) || nrow(metadata_df) == 0) {
    return(metadata_df)
  }

  valid_types <- c("Modeled", "ForEfficiencyCalculation")
  if (!"ModeledVarType" %in% names(metadata_df)) {
    metadata_df$ModeledVarType <- NA_character_
  } else {
    metadata_df$ModeledVarType <- as.character(metadata_df$ModeledVarType)
  }

  modeled_names <- if ("MainModelVariableName" %in% names(metadata_df)) {
    as.character(metadata_df$MainModelVariableName)
  } else {
    rep("", nrow(metadata_df))
  }
  all_names_lower <- str_to_lower(modeled_names)
  base_names_lower <- str_to_lower(str_remove(modeled_names, regex("---Spend$", ignore_case = TRUE)))
  is_paired_spend <- str_detect(modeled_names, regex("---Spend$", ignore_case = TRUE)) &
    base_names_lower %in% all_names_lower

  needs_inference <- is.na(metadata_df$ModeledVarType) |
    trimws(metadata_df$ModeledVarType) == "" |
    !metadata_df$ModeledVarType %in% valid_types
  metadata_df$ModeledVarType[needs_inference] <- ifelse(
    is_paired_spend[needs_inference],
    "ForEfficiencyCalculation",
    "Modeled"
  )

  metadata_df
}

find_pair_variable <- function(activity_var, all_vars) {
  if (is.null(all_vars) || length(all_vars) == 0) return(NULL)
  
  act_keywords <- c("Impressions", "Clicks", "GRPs", "Views", "Sessions", "Reach", "Actions", "Visits", "Circulation")
  spend_keywords <- c("Spend", "Cost", "Net Cost", "Gross Cost")
  
  # Creamos una versión en minúsculas de todas las variables para hacer el match sin importar el case
  all_vars_lower <- tolower(all_vars)
  
  for (act in act_keywords) {
    if (grepl(act, activity_var, ignore.case = TRUE)) {
      for (sp in spend_keywords) {
        
        # 1. Hacemos el reemplazo de la palabra (ignore.case funciona aquí)
        candidate <- sub(act, sp, activity_var, ignore.case = TRUE)
        
        # 2. Buscamos el candidato en minúscula dentro de nuestra lista en minúscula
        match_idx <- which(all_vars_lower == tolower(candidate))
        
        # 3. Si hay match, retornamos la variable con su formato ORIGINAL (respetando sus mayúsculas reales)
        if (length(match_idx) > 0) {
          return(all_vars[match_idx[1]])
        }
      }
    }
  }
  return(NULL)
}
# ==============================================================================
# 2. LÓGICA DE INTERSECCIÓN & NOMBRES
# ==============================================================================
get_common_value <- function(list_of_values) {
  if(length(list_of_values) == 0) return(NULL)
  first_val <- list_of_values[[1]]
  if(!is_valid_value(first_val)) { if(all(sapply(list_of_values, function(x) !is_valid_value(x)))) return(NULL) else return(NULL) }
  for(i in seq_along(list_of_values)) { if(!is_valid_value(list_of_values[[i]]) || !setequal(first_val, list_of_values[[i]])) return(NULL) }
  return(first_val)
}
generate_vof_name_cs_str <- function(cs_vals, key){
  str_mapping <- c("cs_geography"="--g", "cs_product"="--prod", "cs_campaign"="--camp", "cs_outlet"="--out", "cs_creative"="--creat")
  if(!is_valid_value(cs_vals)) return("")
  vals <- sort(unique(cs_vals)); tag <- str_mapping[key]; if(is.na(tag)) return("")
  if(length(vals) > 1) glue::glue("{tag} {vals[1]} etc") else glue::glue("{tag} {vals[1]}")
}

# ==============================================================================
# 3. GENERACIÓN DE FILTROS & CÓDIGO
# ==============================================================================
generate_filter_string <- function(start_date, end_date, cs_list) {
  p_filters <- c()
  if(is_valid_value(start_date)) p_filters <- c(p_filters, glue::glue("Data$Period >= as.Date('{start_date}')"))
  if(is_valid_value(end_date))   p_filters <- c(p_filters, glue::glue("Data$Period <= as.Date('{end_date}')"))
  col_mapping <- c("cs_geography"="Geography", "cs_product"="Product", "cs_campaign"="Campaign", "cs_outlet"="Outlet", "cs_creative"="Creative")
  cs_filters <- sapply(names(cs_list), function(key){
    vals <- cs_list[[key]]
    if(!is_valid_value(vals)) return("")
    col_name <- col_mapping[key]
    val_string <- paste(sapply(vals, function(x) glue::glue("'{x}'")), collapse = ", ")
    glue::glue("tolower(Data${col_name}) %in% tolower(c({val_string}))")
  })
  all_filters <- c(p_filters, cs_filters[cs_filters != ""])
  if(length(all_filters) > 0) return(paste(all_filters, collapse = " & ")) else return("")
}

generate_vof_data_from_list <- function(all_modules_data, 
                                        active_cs_dims = NULL, 
                                        output_type = "text",
                                        generate_spend = FALSE, 
                                        all_analytical_vars = NULL) {
  
  if(length(all_modules_data) == 0) return(NULL)
  
  # --- 1. Calcular Nombres y Títulos (BASE ACTIVITY) ---
  temp_df <- do.call(rbind, lapply(all_modules_data, function(x) { data.frame(media=trimws(x$media_channel), sub=trimws(x$sub_channel), stringsAsFactors=FALSE) }))
  media_groups <- split(temp_df$sub, temp_df$media); media_groups <- lapply(media_groups, function(subs) unique(subs[subs != ""])); media_names <- names(media_groups)
  if (all(sapply(media_groups, function(subs) setequal(subs, media_groups[[1]]))) && length(media_groups) > 1) {
    name_base <- if(paste(media_groups[[1]], collapse="-") != "") paste(paste(media_names, collapse="-"), paste(media_groups[[1]], collapse="-")) else paste(media_names, collapse="-")
  } else {
    name_base <- paste(sapply(media_names, function(m) { s <- paste(media_groups[[m]], collapse="-"); if(s!="") paste(m, s) else m }), collapse="-")
  }
  
  all_effects <- sapply(all_modules_data, function(x) x$effect); unique_effects <- unique(all_effects[all_effects!=""])
  effect_tag <- if(length(unique_effects)>1) "--Both" else if(length(unique_effects)==1) glue::glue("--{unique_effects}") else ""
  
  starts_vec <- sapply(all_modules_data, function(x) if(is_valid_value(x$start_period)) as.character(x$start_period) else "")
  ends_vec <- sapply(all_modules_data, function(x) if(is_valid_value(x$end_period)) as.character(x$end_period) else "")
  has_period <- any(starts_vec!="") || any(ends_vec!="")
  date_tag <- if(length(unique(starts_vec))==1 && length(unique(ends_vec))==1 && has_period) {
    s <- helper_period_vof_name(if(starts_vec[1]!="") starts_vec[1] else NA)
    e <- helper_period_vof_name(if(ends_vec[1]!="") ends_vec[1] else NA)
    if(s!="" || e!="") glue::glue("--p {s}-{e}") else ""
  } else if(has_period) "--p" else ""
  
  cs_tags <- c(); cs_keys <- c("cs_geography", "cs_product", "cs_campaign", "cs_outlet", "cs_creative")
  for(k in cs_keys){
    vals <- lapply(all_modules_data, function(x) x[[k]]); comm <- get_common_value(vals)
    if(!is.null(comm)) cs_tags <- c(cs_tags, generate_vof_name_cs_str(comm, k)) else if(any(sapply(vals, is_valid_value))) cs_tags <- c(cs_tags, switch(k, cs_geography="--g", cs_product="--prod", cs_campaign="--camp", cs_outlet="--out", cs_creative="--creat"))
  }
  
  # Nombre BASE (Activity)
  # Override opcional para el parser legacy
  vof_name_activity <- if(!is.null(all_modules_data[[1]]$vof_name_override)) all_modules_data[[1]]$vof_name_override else trimws(paste(c(name_base, effect_tag, date_tag, cs_tags)[c(name_base, effect_tag, date_tag, cs_tags) != ""], collapse = " "))
  
  # Generar Título Humano
  m_desc <- paste(unique(temp_df$media[temp_df$media!=""]), collapse=" and ")
  e_desc <- if(length(unique_effects)>0) paste("adjusted by", if(length(unique_effects)>1) "Both" else unique_effects) else ""
  p_desc <- if(has_period) { if(length(unique(starts_vec))==1 && length(unique(ends_vec))==1) {
    s<-starts_vec[1]; e<-ends_vec[1]; if(s!=""&&e!="") glue::glue("filtered from {s} to {e}") else if(s!="") glue::glue("filtered from {s}") else glue::glue("filtered until {e}")
  } else "filtered by different periods" } else ""
  
  cs_act <- c(); map <- c("cs_geography"="geography", "cs_product"="product", "cs_campaign"="campaign", "cs_outlet"="outlet", "cs_creative"="creative")
  for(k in names(map)) if(any(sapply(lapply(all_modules_data, function(x) x[[k]]), is_valid_value))) cs_act <- c(cs_act, map[k])
  cs_desc <- if(length(cs_act)>0) paste("and by", paste(cs_act, collapse=" and ")) else ""
  
  base_t <- paste(c(m_desc, e_desc)[c(m_desc, e_desc)!=""], collapse=", ")
  filt_t <- paste(c(p_desc, cs_desc)[c(p_desc, cs_desc)!=""], collapse=" ")
  final_title <- gsub("\\s+", " ", trimws(if(filt_t!="") paste0(base_t, ", ", filt_t) else base_t))
  
  # --- PREPARACIÓN DE SPEND ---
  spend_modules_data <- list()
  if(generate_spend && !is.null(all_analytical_vars)) {
    for(mod in all_modules_data) {
      spend_var <- find_pair_variable(mod$analytical_var, all_analytical_vars)
      if(!is.null(spend_var)) {
        mod_spend <- mod
        mod_spend$analytical_var <- spend_var
        spend_modules_data[[length(spend_modules_data)+1]] <- mod_spend
      }
    }
  }
  
  has_spend <- length(spend_modules_data) > 0
  vof_name_spend <- if(has_spend) paste0(vof_name_activity, "---Spend") else ""
  
  # --- 2. GENERACIÓN DE CÓDIGO ---
  code_lines_act <- c(glue::glue("Data[, '{vof_name_activity}'] <- 0"))
  for(mod in all_modules_data){
    cs_l <- list(cs_geography=mod$cs_geography, cs_product=mod$cs_product, cs_campaign=mod$cs_campaign, cs_outlet=mod$cs_outlet, cs_creative=mod$cs_creative)
    f_str <- generate_filter_string(mod$start_period, mod$end_period, cs_l)
    avar <- mod$analytical_var
    meta_comment <- glue::glue("# Media Channel: {convert_to_char(mod$media_channel)}; Sub Channel: {convert_to_char(mod$sub_channel)}; Effect: {convert_to_char(mod$effect)}")
    line <- if(f_str != "") glue::glue("Data[{f_str}, '{vof_name_activity}'] <- rowSums(Data[{f_str}, c('{vof_name_activity}', '{avar}')], na.rm = TRUE) {meta_comment}") else glue::glue("Data[, '{vof_name_activity}'] <- rowSums(Data[, c('{vof_name_activity}', '{avar}')], na.rm = TRUE) {meta_comment}")
    code_lines_act <- c(code_lines_act, line)
  }
  
  full_block_code <- paste(c(glue::glue("#### {final_title} ####"), code_lines_act, ""), collapse = "\n")
  
  if(has_spend) {
    code_lines_spd <- c(glue::glue("Data[, '{vof_name_spend}'] <- 0"))
    for(mod in spend_modules_data){
      cs_l <- list(cs_geography=mod$cs_geography, cs_product=mod$cs_product, cs_campaign=mod$cs_campaign, cs_outlet=mod$cs_outlet, cs_creative=mod$cs_creative)
      f_str <- generate_filter_string(mod$start_period, mod$end_period, cs_l)
      avar <- mod$analytical_var
      meta_comment <- glue::glue("# Media Channel: {convert_to_char(mod$media_channel)}; Sub Channel: {convert_to_char(mod$sub_channel)}; Effect: {convert_to_char(mod$effect)}")
      line <- if(f_str != "") glue::glue("Data[{f_str}, '{vof_name_spend}'] <- rowSums(Data[{f_str}, c('{vof_name_spend}', '{avar}')], na.rm = TRUE) {meta_comment}") else glue::glue("Data[, '{vof_name_spend}'] <- rowSums(Data[, c('{vof_name_spend}', '{avar}')], na.rm = TRUE) {meta_comment}")
      code_lines_spd <- c(code_lines_spd, line)
    }
    spend_title <- paste(final_title, "(Spend)")
    full_block_code <- paste(full_block_code, paste(c(glue::glue("#### {spend_title} ####"), code_lines_spd, ""), collapse = "\n"), sep="\n")
  }
  
  if(output_type == "data.frame") {
    
    build_df_rows <- function(modules, v_name, full_code_block, metric_fallback, modeled_var_type) {
      lapply(modules, function(mod) {
        cs_list <- list(cs_geography=mod$cs_geography, cs_product=mod$cs_product, cs_campaign=mod$cs_campaign, cs_outlet=mod$cs_outlet, cs_creative=mod$cs_creative)
        filter_str <- generate_filter_string(mod$start_period, mod$end_period, cs_list)
        meta_comment <- glue::glue("# Media Channel: {convert_to_char(mod$media_channel)}; Sub Channel: {convert_to_char(mod$sub_channel)}; Effect: {convert_to_char(mod$effect)}")
        spec_feed <- if(filter_str != "") glue::glue("Data[{filter_str}, '{v_name}'] <- rowSums(Data[{filter_str}, c('{v_name}', '{mod$analytical_var}')], na.rm = TRUE) {meta_comment}") else glue::glue("Data[, '{v_name}'] <- rowSums(Data[, c('{v_name}', '{mod$analytical_var}')], na.rm = TRUE) {meta_comment}")
        
        row_df <- data.frame(
          MainModelVariableName = v_name,
          AnalyticalVariableName = convert_to_char(mod$analytical_var), 
          MediaChannel = convert_to_char(mod$media_channel),
          SubChannel = convert_to_char(mod$sub_channel), 
          Effect = convert_to_char(mod$effect),
          MinPeriod = ifelse(is_valid_value(mod$start_period), as.character(mod$start_period), ""),
          MaxPeriod = ifelse(is_valid_value(mod$end_period), as.character(mod$end_period), ""),
          stringsAsFactors = FALSE
        )
        if(!is.null(active_cs_dims)) for(d in active_cs_dims) { k<-paste0("cs_",tolower(d)); row_df[[d]] <- if(k %in% names(mod) && is_valid_value(mod[[k]])) paste(mod[[k]], collapse=", ") else "" }
        
        row_df$ModeledVarType <- modeled_var_type
        row_df$Metric <- detect_media_metric(mod$analytical_var, metric_fallback)
        row_df$InitCode <- glue::glue("Data[, '{v_name}'] <- 0")
        row_df$FeedCode <- spec_feed
        
        return(row_df)
      })
    }
    
    rows_act <- build_df_rows(all_modules_data, vof_name_activity, full_block_code, "Activity", "Modeled")
    rows_spd <- list()
    if(has_spend) {
      rows_spd <- build_df_rows(spend_modules_data, vof_name_spend, full_block_code, "Spend", "ForEfficiencyCalculation")
    }
    
    final_df <- bind_rows(c(rows_act, rows_spd))
    final_df$Type <- "Media" # NUEVO: Asignación por defecto para VOFs de Medios
    
    cols_static <- c("Type", "MainModelVariableName", "AnalyticalVariableName", "MediaChannel", "SubChannel", "Effect", "MinPeriod", "MaxPeriod")
    
    cols_static <- c("MainModelVariableName", "AnalyticalVariableName", "MediaChannel", "SubChannel", "Effect", "MinPeriod", "MaxPeriod")
    cols_dyn <- if(!is.null(active_cs_dims)) active_cs_dims else character(0)
    cols_end <- c("ModeledVarType", "Metric", "InitCode", "FeedCode")
    
    expected_order <- c(cols_static, cols_dyn, cols_end)
    return(final_df[, intersect(expected_order, names(final_df)), drop=FALSE])
    
  } else {
    return(full_block_code)
  }
}



# ==============================================================================
# 4. PARSER: IMPORTAR DESDE CÓDIGO
# ==============================================================================
parse_vof_code_to_list <- function(code_text) {
  lines <- str_split(code_text, "\n")[[1]]
  vof_list <- list()
  
  for(line in lines) {
    if(grepl("<- rowSums", line) && grepl("# Media Channel:", line)) {
      tryCatch({
        vof_name <- str_match(line, "Data\\[.*, '([^']+)'\\] <-")[2]
        if(is.na(vof_name)) vof_name <- str_match(line, "Data\\[, '([^']+)'\\] <-")[2]
        analytical_var <- str_match(line, "c\\s*\\(\\s*'[^']+'\\s*,\\s*'([^']+)'\\s*\\)")[,2]
        meta_part <- str_split(line, "# Media Channel:")[[1]][2]
        media_ch <- str_trim(str_match(meta_part, "^([^;]+);")[2])
        sub_ch   <- str_trim(str_match(meta_part, "Sub Channel: ([^;]+);")[2])
        effect   <- str_trim(str_match(meta_part, "Effect: (.*)$")[2])
        start_p <- NA; end_p <- NA
        s_match <- str_match(line, "Data\\$Period >= as.Date\\('([^']+)'\\)"); if(!is.na(s_match[2])) start_p <- s_match[2]
        e_match <- str_match(line, "Data\\$Period <= as.Date\\('([^']+)'\\)"); if(!is.na(e_match[2])) end_p <- e_match[2]
        
        cs_data <- list(); known_dims <- c("Geography", "Product", "Campaign", "Outlet", "Creative")
        for(dim in known_dims) {
          pattern <- paste0("tolower\\(Data\\$", dim, "\\) %in% tolower\\(c\\(([^)]+)\\)\\)")
          match <- str_match(line, pattern)
          if(!is.na(match[2])) {
            raw_vals <- match[2]; vals <- str_split(raw_vals, ",")[[1]]; vals <- gsub("'", "", vals); vals <- str_trim(vals)
            cs_data[[paste0("cs_", tolower(dim))]] <- vals
          }
        }
        
        row_data <- list(analytical_var = analytical_var, media_channel = media_ch, sub_channel = if(!is.na(sub_ch) && sub_ch != "NA") sub_ch else "", effect = if(!is.na(effect) && effect != "NA") effect else "", start_period = start_p, end_period = end_p)
        row_data <- c(row_data, cs_data)
        
        if(is.null(vof_list[[vof_name]])) vof_list[[vof_name]] <- list()
        vof_list[[vof_name]][[length(vof_list[[vof_name]]) + 1]] <- row_data
        
      }, error = function(e) { print(paste("Error parsing line:", line, "-", e$message)) })
    }
  }
  return(vof_list)
}

# ==============================================================================
# 5. PARSER: LEGACY R CODE (FASE 3: DEDUCCIÓN DE MEDIA CHANNEL INTELIGENTE)
# ==============================================================================

parse_legacy_r_code <- function(code_text, analytical_cols_list, active_cs_dims) {
  
  lines <- str_split(code_text, "\n")[[1]]
  definitions_map <- list()
  vof_names_ordered <- c()
  known_dims <- c("Geography", "Product", "Campaign", "Outlet", "Creative")
  
  # --- PASO 1: LEER Y MAPEAR (Idéntico) ---
  for (line in lines) {
    line <- trimws(line)
    if (!grepl("<-", line) || grepl("<- 0", line) || startsWith(line, "#")) next
    
    lhs_part <- str_split(line, "<-")[[1]][1]
    vof_match <- str_match(lhs_part, "Data\\[.*['\"]([^'\"]+)['\"].*\\]")
    vof_name <- vof_match[2]
    if (is.na(vof_name)) { vof_match <- str_match(lhs_part, "Data\\[.*['\"]([^'\"]+)['\"]\\]"); vof_name <- vof_match[2] }
    if (is.na(vof_name)) next
    
    rhs_part <- str_split(line, "<-")[[1]][2]
    raw_vars <- str_extract_all(rhs_part, "['\"`][^'\"`]+['\"`]")[[1]]
    clean_vars <- gsub("['\"`]", "", raw_vars)
    components <- clean_vars[clean_vars != vof_name & !grepl("^\\d{4}-\\d{2}-\\d{2}", clean_vars)]
    
    if (length(components) == 0) next
    
    start_p <- NA; end_p <- NA
    s_match <- str_match(line, "Period\\s*>=\\s*as.Date\\(['\"]([^'\"]+)['\"]\\)"); if (!is.na(s_match[2])) start_p <- s_match[2]
    e_match <- str_match(line, "Period\\s*<=\\s*as.Date\\(['\"]([^'\"]+)['\"]\\)"); if (!is.na(e_match[2])) end_p <- e_match[2]
    
    cs_filters <- list()
    for (dim in known_dims) {
      pattern_dim <- paste0("Data\\$", dim, ".*?c\\(([^)]+)\\)")
      match_dim <- str_match(line, pattern_dim)
      if (!is.na(match_dim[2])) {
        raw_vals <- match_dim[2]; vals <- str_split(raw_vals, ",")[[1]]; vals <- gsub("^\\s*['\"]|['\"]\\s*$", "", trimws(vals)); vals <- gsub("tolower\\(|\\)", "", vals)
        if(length(vals) > 0 && vals[1] != "") cs_filters[[paste0("cs_", tolower(dim))]] <- vals
      }
    }
    
    def_block <- list(vars = components, filters = list(start = start_p, end = end_p, cs = cs_filters))
    if (is.null(definitions_map[[vof_name]])) {
      definitions_map[[vof_name]] <- list(def_block)
      vof_names_ordered <- c(vof_names_ordered, vof_name)
    } else {
      definitions_map[[vof_name]][[length(definitions_map[[vof_name]]) + 1]] <- def_block
    }
  }
  vof_names_ordered <- unique(vof_names_ordered)
  
  # --- PASO 2: FUNCIONES DE RESOLUCIÓN (Idéntico) ---
  merge_filters <- function(parent_f, child_f) {
    new_start <- parent_f$start
    if (!is.na(child_f$start)) { if (is.na(new_start) || child_f$start > new_start) new_start <- child_f$start }
    new_end <- parent_f$end
    if (!is.na(child_f$end)) { if (is.na(new_end) || child_f$end < new_end) new_end <- child_f$end }
    new_cs <- parent_f$cs
    for (k in names(child_f$cs)) { new_cs[[k]] <- child_f$cs[[k]] }
    list(start = new_start, end = new_end, cs = new_cs)
  }
  
  resolve_vof <- function(var_name, accum_filters, path = c()) {
    if (var_name %in% analytical_cols_list) return(list(list(root = var_name, filters = accum_filters)))
    if (var_name %in% path || is.null(definitions_map[[var_name]])) return(list())
    
    resolved_leaves <- list()
    definitions <- definitions_map[[var_name]]
    for (block in definitions) {
      current_merged_filters <- merge_filters(accum_filters, block$filters)
      for (component in block$vars) {
        leaves <- resolve_vof(component, current_merged_filters, c(path, var_name))
        resolved_leaves <- c(resolved_leaves, leaves)
      }
    }
    return(resolved_leaves)
  }
  
  # --- PASO 3: GENERACIÓN Y SEMÁNTICA (CORREGIDA STRICT MODE) ---
  final_rows <- list()
  
  for (vof_name in vof_names_ordered) {
    
    # 1. Separación Base vs Tags
    # Usamos el doble guion como punto de corte estructural
    parts <- str_split(vof_name, "--")[[1]]
    
    base_name_full <- parts[1] # Lo que está antes del primer -- es el Media Channel base
    
    # 2. Detección Estricta de Effect
    # Miramos lo que sigue al primer --.
    # Si existe, lo limpiamos y miramos si EMPIEZA con Direct o Halo.
    effect_val <- ""
    
    if (length(parts) > 1) {
      # Tomamos el segundo segmento (inmediatamente después del Media Channel)
      tag_segment <- trimws(parts[2])
      
      # Verificamos inicio estricto (Case Insensitive pero inicio de string ^)
      if (grepl("^Direct", tag_segment, ignore.case = TRUE)) {
        effect_val <- "Direct"
      } else if (grepl("^Halo", tag_segment, ignore.case = TRUE)) {
        effect_val <- "Halo"
      }
      # Si es "Indirect", "Both", o flags de fecha/geo (--p, --g), no hará match con ^Direct/^Halo.
    }
    
    # 3. Resolver Raíces
    empty_filters <- list(start = NA, end = NA, cs = list())
    roots_data <- resolve_vof(vof_name, empty_filters)
    if (length(roots_data) == 0) next
    
    # 4. Tokenización Inteligente (Media Channel)
    if (grepl("\\s+and\\s+", base_name_full, ignore.case = TRUE)) {
      tokens <- str_split(base_name_full, "\\s+and\\s+")[[1]]
    } else {
      tokens <- str_split(base_name_full, "\\s*-\\s*")[[1]]
    }
    tokens <- trimws(tokens[tokens != ""])
    
    roots_map <- rep(NA, length(roots_data))
    
    # Ronda 1 (Direct Match)
    for (i in seq_along(roots_data)) {
      root_var <- roots_data[[i]]$root
      matches <- c()
      for (tok in tokens) if (grepl(tok, root_var, ignore.case = TRUE)) matches <- c(matches, tok)
      if (length(matches) >= 1) roots_map[i] <- matches[which.max(nchar(matches))]
    }
    
    # Ronda 2 (Descarte)
    used_tokens <- unique(na.omit(roots_map))
    unused_tokens <- setdiff(tokens, used_tokens)
    unassigned_indices <- which(is.na(roots_map))
    if (length(unused_tokens) == 1 && length(unassigned_indices) > 0) roots_map[unassigned_indices] <- unused_tokens[1]
    
    # Ronda 3 (Fallback)
    roots_map[is.na(roots_map)] <- base_name_full
    
    # 5. GENERACIÓN DE FILAS
    for (i in seq_along(roots_data)) {
      item <- roots_data[[i]]
      root_var <- item$root
      filters <- item$filters
      final_media_channel <- roots_map[i]
      
      input_simulation <- list(
        analytical_var = root_var,
        media_channel = final_media_channel,
        sub_channel = "",
        effect = effect_val,
        start_period = if(!is.na(filters$start)) as.Date(filters$start) else NULL,
        end_period = if(!is.na(filters$end)) as.Date(filters$end) else NULL
      )
      
      if(!is.null(active_cs_dims)) {
        for(d in active_cs_dims) {
          k <- paste0("cs_", tolower(d))
          if(k %in% names(filters$cs)) input_simulation[[k]] <- filters$cs[[k]] else input_simulation[[k]] <- NULL
        }
      }
      
      # Generador Estándar (Crea nombre y código limpios/nuevos)
      standard_row_df <- generate_vof_data_from_list(
        all_modules_data = list(input_simulation),
        active_cs_dims = active_cs_dims,
        output_type = "data.frame",
        generate_spend = FALSE, 
        all_analytical_vars = analytical_cols_list
      )
      
      # FIX FINAL: Sobreescribir MainModelVariableName con el nombre Legacy
      if(!is.null(standard_row_df) && nrow(standard_row_df) > 0) {
        standard_row_df$MainModelVariableName <- vof_name
        legacy_base_name <- sub("---Spend$", "", vof_name, ignore.case = TRUE)
        is_paired_spend <- grepl("---Spend$", vof_name, ignore.case = TRUE) &&
          tolower(legacy_base_name) %in% tolower(vof_names_ordered)
        if(is_paired_spend) standard_row_df$ModeledVarType <- "ForEfficiencyCalculation"
        final_rows[[length(final_rows) + 1]] <- standard_row_df
      }
    }
  }
  
  if (length(final_rows) > 0) {
    res_df <- bind_rows(final_rows)
    return(res_df)
  } else {
    return(data.frame())
  }
}



# --- NUEVAS FUNCIONES PARA KPI Y MFF ---

# Convertir a Title Case
to_title_case <- function(text) {
  # Reemplaza guiones o guiones bajos por espacios, y pone la primera en mayúscula
  text <- gsub("[_\\-]", " ", text)
  s <- strsplit(text, " ")[[1]]
  s <- paste(toupper(substring(s, 1, 1)), tolower(substring(s, 2)), sep = "", collapse = " ")
  return(trimws(s))
}

# Lógica principal de dimensiones MFF
get_kpi_log_name <- function(raw_var_name, all_analytical_cols, cs_dims) {
  mff_dims <- c("Geography", "Product", "Campaign", "Outlet", "Creative")
  longitudinal_dims <- setdiff(mff_dims, cs_dims)
  
  # Filtramos solo columnas que parezcan ser variables analíticas (tienen guiones bajos)
  # Excluimos Period, o dimensiones CS puras si las hay
  var_cols <- all_analytical_cols[grepl("_", all_analytical_cols)]
  
  if(length(var_cols) == 0 || !grepl("_", raw_var_name)) {
    return(paste("LOG", to_title_case(raw_var_name)))
  }
  
  # Crear un dataframe temporal separando los nombres por "_"
  splits <- strsplit(var_cols, "_")
  
  # Asumimos que la estructura estricta es: VariableName_Long1_Long2_...
  # Encontramos la longitud máxima para crear la tabla
  max_len <- max(sapply(splits, length))
  
  # Si la estructura no coincide con [VarName + LongDims], hacemos un fallback seguro
  if(max_len != (length(longitudinal_dims) + 1)) {
    base_name <- strsplit(raw_var_name, "_")[[1]][1]
    return(paste("LOG", to_title_case(base_name)))
  }
  
  # Armamos tabla de metadatos de las columnas
  meta_df <- do.call(rbind, lapply(splits, function(x) {
    length(x) <- max_len # Rellenar con NA si alguna es más corta
    return(x)
  }))
  
  # La columna 1 es VariableName. Las siguientes son longitudinal_dims.
  usable_indices <- c()
  for (i in 2:max_len) {
    unique_vals <- unique(meta_df[, i])
    # Regla: Si tiene más de 1 valor único, es usable
    if (length(unique_vals) > 1) {
      usable_indices <- c(usable_indices, i)
    }
  }
  
  # Ahora construimos el nombre para la variable seleccionada
  raw_splits <- strsplit(raw_var_name, "_")[[1]]
  
  var_name_base <- raw_splits[1]
  usable_vals <- raw_splits[usable_indices]
  
  # Filtramos NAs por si acaso
  usable_vals <- usable_vals[!is.na(usable_vals)]
  
  final_components <- c("LOG", to_title_case(var_name_base), sapply(usable_vals, to_title_case))
  
  return(paste(final_components, collapse = " "))
}
