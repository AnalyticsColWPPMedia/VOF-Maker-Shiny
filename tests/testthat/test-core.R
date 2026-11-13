ana <- make_analytical()
cs <- detect_cs_dims(ana)
long <- longitudinal_dims(cs)
mff <- list_mff_vars(ana, cs)

run_code <- function(code, data = ana) {
  # Default R packages only (base, stats, utils...), as in AWB; no app helpers.
  env <- new.env(parent = as.environment("package:stats"))
  env$Data <- data
  eval(parse(text = code), envir = env)
  env$Data
}

test_that("MFF variables are told apart from VOFs by their longitudinal suffix", {
  expect_equal(cs, "Geography")
  expect_true("My_Var_With_Underscores_Watches_Total_Total_Total" %in% mff)
  expect_false("Existing VOF --Direct" %in% mff)
  expect_false("Google_Trends" %in% mff)  # one "_" but 4 longitudinal dims expected
  p <- parse_mff_names("My_Var_With_Underscores_Watches_Total_Total_Total", long)
  expect_equal(p$base, "My_Var_With_Underscores")
  expect_equal(p$Product, "Watches")
})

test_that("dates are parsed with the format that fits the whole vector", {
  expect_equal(parse_dates_flexible(c("1/6/2025", "3/31/2024")), as.Date(c("2025-01-06", "2024-03-31")))
  expect_equal(parse_dates_flexible(c("31/03/2024", "06/01/2025")), as.Date(c("2024-03-31", "2025-01-06")))
  expect_equal(parse_dates_flexible("2024-01-01 00:00:00"), as.Date("2024-01-01"))
  expect_equal(parse_dates_flexible(45292), as.Date("2024-01-01"))
})

test_that("quotes in values never break the generated code", {
  comp <- new_component("TV GRPs_Watches_Total_Total_Total", "TV", cs = list(Geography = "McDonald's Land"))
  res <- generate_media_vof(list(comp), cs, mff, long)
  out <- run_code(res$code)
  geo <- ana$Geography == "McDonald's Land"
  expect_equal(out[[res$activity_name]], ifelse(geo, ana[["TV GRPs_Watches_Total_Total_Total"]], 0))
  expect_equal(out[[res$spend_name]], ifelse(geo, ana[["TV Spend_Watches_Total_Total_Total"]], 0))
})

test_that("spend pairing uses whole words and reports unpaired components", {
  expect_equal(find_spend_pair("Search Clicks_Watches_Total_Total_Total", mff, long), "Search Cost_Watches_Total_Total_Total")
  expect_null(detect_metric("Social Transactions_Watches_Total_Total_Total", long))  # "Actions" is not a word here
  comps <- list(new_component("TV GRPs_Watches_Total_Total_Total", "TV"),
                new_component("Radio GRPs_Watches_Total_Total_Total", "Radio"))
  res <- generate_media_vof(comps, cs, mff, long)
  expect_match(res$warnings, "No spend twin found for: Radio GRPs", all = FALSE)
  expect_equal(sum(res$metadata$Metric == "Spend"), 1)
})

test_that("new naming: 'and', complement, metric suffix, backcasted prefix", {
  comps <- list(new_component("TV GRPs_Watches_Total_Total_Total", "TV", "Watches", "Direct"),
                new_component("Radio GRPs_Watches_Total_Total_Total", "Radio", "Collection", "Direct"))
  res <- generate_media_vof(comps, cs, mff, long, generate_twin = FALSE)
  expect_equal(res$name, "TV and Radio - Watches and Collection --Direct ---GRPs")
  bc <- generate_media_vof(comps[1], cs, mff, long, backcasting = list(enabled = TRUE, start = "2023-01-04", end = "2023-02-01"))
  expect_equal(bc$name, "Backcasted TV - Watches --Direct ---GRPs")
  expect_equal(bc$twin_name, "Backcasted TV - Watches --Direct ---Spend")
  expect_equal(unique(bc$metadata$VofVariableName), "TV")
})

test_that("metric checks: mixed metrics, unknown metric, metric inside labels", {
  mixed <- list(new_component("TV GRPs_Watches_Total_Total_Total", "TV"),
                new_component("Search Clicks_Watches_Total_Total_Total", "Search"))
  expect_match(generate_media_vof(mixed, cs, mff, long)$errors, "mix metrics")
  unknown <- list(new_component("Sales_Watches_Total_Total_Total", "Other"))
  expect_match(generate_media_vof(unknown, cs, mff, long)$errors, "could not be detected")
  expect_equal(generate_media_vof(unknown, cs, mff, long, generate_twin = FALSE, metric_override = "Units")$name, "Other ---Units")
  in_label <- list(new_component("TV Spend_Watches_Total_Total_Total", "TV Spend"))
  expect_match(generate_media_vof(in_label, cs, mff, long)$errors, "appears in the VariableName")
})

test_that("spend configured -> activity twin, asking when there are several candidates", {
  spend <- list(new_component("Search Cost_Watches_Total_Total_Total", "Paid Search"))
  first <- generate_media_vof(spend, cs, mff, long)
  expect_length(first$needs_choice, 1)
  expect_equal(first$needs_choice[[1]]$preselected, "Search Clicks_Watches_Total_Total_Total")  # channel rule
  done <- generate_media_vof(spend, cs, mff, long, twin_choices = list(`1` = first$needs_choice[[1]]$preselected))
  expect_equal(done$name, "Paid Search ---Spend")
  expect_equal(done$twin_name, "Paid Search ---Clicks")
  expect_equal(done$activity_name, "Paid Search ---Clicks")
  expect_equal(unique(done$metadata$Metric), c("Clicks", "Spend"))
})

test_that("exclusion and CS Children positional filters run in base R and round-trip", {
  ex <- new_component("TV GRPs_Watches_Total_Total_Total", "TV", cs = list(Geography = new_cs_filter("Atlanta", "exclude")))
  res <- generate_media_vof(list(ex), cs, mff, long, generate_twin = FALSE, context = list(analytical = ana))
  expect_equal(res$name, "TV --g not Atlanta ---GRPs")
  out <- run_code(res$code)
  expect_equal(out[[res$name]], ifelse(ana$Geography != "Atlanta", ana[["TV GRPs_Watches_Total_Total_Total"]], 0))
  expect_equal(res$metadata$GeographyFilter, "Exclude|Exact")

  geos <- cs_dim_values(ana, "Geography")
  f <- cs_children_to_filter(geos, " - ", list("Boston"))
  expect_false(f$fallback)
  expect_equal(f$filter$levels[[1]], list(match = "StartsWith", values = "Boston - "))
  ch <- new_component("TV GRPs_Watches_Total_Total_Total", "TV", cs = list(Geography = f$filter))
  res2 <- generate_media_vof(list(ch), cs, mff, long, generate_twin = FALSE, context = list(analytical = ana, cs_sep = " - "))
  expect_equal(res2$name, "TV --g Boston ---GRPs")
  out2 <- run_code(res2$code)
  expect_equal(out2[[res2$name]], ifelse(startsWith(ana$Geography, "Boston - "), ana[["TV GRPs_Watches_Total_Total_Total"]], 0))
  expect_equal(res2$metadata$Geography, "'Boston - '")
  expect_equal(res2$metadata$GeographyFilter, "Include|StartsWith")
  back <- parse_vof_code(res2$code)[[1]]$components[[1]]
  expect_equal(cs_filter_key(back$cs$Geography), cs_filter_key(f$filter))
})

test_that("name flags: topN, collisions become groups, same set reuses its label", {
  totals <- cs_root_totals(ana, "Geography", "TV GRPs_Watches_Total_Total_Total") %>% arrange(desc(total))
  top2 <- new_component("TV GRPs_Watches_Total_Total_Total", "TV", cs = list(Geography = totals$entity[1:2]))
  r1 <- generate_media_vof(list(top2), cs, mff, long, generate_twin = FALSE, context = list(analytical = ana))
  expect_equal(r1$cs_labels$Geography, "top2")

  a <- new_component("TV GRPs_Watches_Total_Total_Total", "TV", cs = list(Geography = c("Atlanta", "Boston - North")))
  b <- new_component("TV GRPs_Watches_Total_Total_Total", "TV", cs = list(Geography = c("Atlanta", "Boston - South")))
  ra <- generate_media_vof(list(a), cs, mff, long, generate_twin = FALSE, context = list(analytical = ana))
  hist <- upsert_metadata(empty_metadata(cs), ra$metadata)
  rb <- generate_media_vof(list(b), cs, mff, long, generate_twin = FALSE, context = list(analytical = ana, history = hist))
  if (ra$cs_labels$Geography == "Atlanta etc") {
    expect_equal(rb$cs_labels$Geography, "group1")
    expect_match(rb$warnings, "already names a different set", all = FALSE)
  }
  ra2 <- generate_media_vof(list(a), cs, mff, long, generate_twin = FALSE, context = list(analytical = ana, history = hist))
  expect_equal(ra2$cs_labels$Geography, ra$cs_labels$Geography)
})

test_that("metadata keeps the Type column and character dates", {
  comp <- new_component("TV GRPs_Watches_Total_Total_Total", "TV", start_period = as.Date("2023-01-04"))
  res <- generate_media_vof(list(comp), cs, mff, long, generate_twin = FALSE)
  expect_equal(unique(res$metadata$Type), "Media")
  hist <- metadata_from_json(as.character(metadata_to_json(res$metadata)))
  expect_true(all(map_lgl(hist, is.character)))
  expect_silent(upsert_metadata(hist, res$metadata))  # Date vs character bug
})

test_that("upsert replaces a regenerated VOF instead of duplicating it", {
  comp <- new_component("TV GRPs_Watches_Total_Total_Total", "TV")
  m1 <- generate_media_vof(list(comp), cs, mff, long)$metadata
  h <- upsert_metadata(empty_metadata(), m1)
  h <- upsert_metadata(h, m1)
  expect_equal(nrow(h), nrow(m1))
})

test_that("backcasting (VOF level) runs in base R, imputes by entity and survives an empty window", {
  comp <- new_component("TV GRPs_Watches_Total_Total_Total", "TV")
  bc <- list(enabled = TRUE, start = "2023-01-04", end = "2023-06-28")
  d <- ana
  d[["TV GRPs_Watches_Total_Total_Total"]][d$Period < as.Date("2023-01-04")] <- 0
  res <- generate_media_vof(list(comp), cs, mff, long, backcasting = bc)
  expect_match(res$activity_name, "^Backcasted TV")
  expect_equal(unique(res$metadata$BackcastingStart), "2023-01-04")
  expect_equal(sum(str_count(res$code, "BACKCASTING IMPUTATION")), 2)  # activity + spend
  out <- run_code(res$code, d)
  v <- out[[res$activity_name]]
  atl <- d$Geography == "Atlanta"
  win <- atl & d$Period >= as.Date("2023-01-04") & d$Period <= as.Date("2023-06-28") & d[["TV GRPs_Watches_Total_Total_Total"]] != 0
  expect_equal(unique(v[atl & d$Period < as.Date("2023-01-04")]), mean(d[["TV GRPs_Watches_Total_Total_Total"]][win]))

  empty_bc <- list(enabled = TRUE, start = "2030-01-01", end = "2030-06-01")
  expect_silent(run_code(generate_media_vof(list(comp), cs, mff, long, FALSE, backcasting = empty_bc)$code, d))
})

test_that("incomplete backcasting windows block generation", {
  comp <- new_component("TV GRPs_Watches_Total_Total_Total", "TV")
  res <- generate_media_vof(list(comp), cs, mff, long, backcasting = list(enabled = TRUE, start = "2023-01-04", end = NA))
  expect_match(res$errors, "incomplete", all = FALSE)
})

test_that("restore and legacy parsers handle tricky values and nested VOFs", {
  comp <- new_component("TV GRPs_Watches_Total_Total_Total", "TV", start_period = "2023-01-04",
                        cs = list(Geography = c("McDonald's Land", "Boston - North")))
  res <- generate_media_vof(list(comp), cs, mff, long,
                            backcasting = list(enabled = TRUE, start = "2023-01-04", end = "2023-03-01"))
  p <- parse_vof_code(res$code)
  expect_equal(names(p), c(res$activity_name, res$spend_name))
  back <- p[[1]]$components[[1]]
  expect_setequal(back$cs$Geography$levels[[1]]$values, c("McDonald's Land", "Boston - North"))
  expect_equal(back$variable_name, "TV")
  expect_true(p[[1]]$backcasting$enabled)
  expect_equal(p[[1]]$backcasting$start, as.Date("2023-01-04"))
  expect_equal(project_code_from_metadata(parsed_vofs_to_metadata(p, cs), cs), project_code_from_metadata(res$metadata, cs))

  legacy <- paste(
    "Data$`Inner` <- 0",
    "Data[Data$Geography %in% c('Atlanta', 'Boston - North'), 'Inner'] <- Data[Data$Geography %in% c('Atlanta', 'Boston - North'), 'TV GRPs_Watches_Total_Total_Total']",
    "Data[, 'TV-Search --Direct'] <- rowSums(Data[Data$Period >= as.Date('2023-01-04'),",
    "    c('Inner', 'Search Clicks_Watches_Total_Total_Total')], na.rm = TRUE)",
    "Data[!(Data$Geography %in% c('Atlanta')), 'Neg'] <- Data[, 'Sales_Watches_Total_Total_Total']",
    sep = "\n")
  l <- parse_legacy_r_code(legacy, mff, cs)
  m <- l$metadata[l$metadata$MainModelVariableName == "TV-Search --Direct", ]
  expect_setequal(m$AnalyticalVariableName, c("TV GRPs_Watches_Total_Total_Total", "Search Clicks_Watches_Total_Total_Total"))
  expect_equal(m$VofVariableName[m$AnalyticalVariableName == "TV GRPs_Watches_Total_Total_Total"], "TV")
  expect_equal(m$Geography[m$AnalyticalVariableName == "TV GRPs_Watches_Total_Total_Total"], "Atlanta, Boston - North")
  expect_equal(unique(m$Effect), "Direct")
  neg <- l$metadata[l$metadata$MainModelVariableName == "Neg", ]
  expect_equal(neg$GeographyFilter, "Exclude|Exact")
})

test_that("purify warns about missing VOFs (regex bug) and keeps spend twins", {
  meta <- normalize_metadata(data.frame(
    MainModelVariableName = c("TV --Direct", "TV --Direct ---Spend", "Unused VOF", "Radio ---GRPs", "Radio ---Spend"),
    AnalyticalVariableName = "x", stringsAsFactors = FALSE))
  details <- data.frame(VariableName = c("TV --Direct", "Radio --Direct", "Holiday X", "Unused VOF", "Sales", "Radio ---GRPs"),
                        Product = c("", "", "", "", "Watches", ""), Campaign = "", Outlet = "", Creative = "",
                        Type = c("2. IN", "2. IN", "2. IN", "6. NONE (Low)", "1. DEP", "2. IN"), stringsAsFactors = FALSE)
  res <- purify_metadata(meta, details, cs)
  expect_equal(res$missing, "Radio --Direct")
  expect_setequal(res$metadata$MainModelVariableName, c("TV --Direct", "TV --Direct ---Spend", "Radio ---GRPs", "Radio ---Spend"))
})

test_that("KPI: log name, business cycles (trailing / calendar / custom), weight code runs", {
  expect_equal(build_kpi_log_name("Sales_Watches_Total_Total_Total", mff, cs), "Sales Watches Log")
  per <- analytical_periods(ana)                     # 170 weeks -> 3 complete cycles
  cyc <- build_trailing_cycles(ana$Period, ana[["Sales_Watches_Total_Total_Total"]], "FY")
  expect_equal(nrow(cyc), 3)
  expect_equal(cycle_period_counts(cyc, per)$Periods, c(52, 52, 52))
  expect_equal(max(cyc$End), max(per))
  expect_true(all(cyc$Start[-1] > cyc$End[-3]))
  expect_equal(nrow(build_trailing_cycles(ana$Period, ana[["Sales_Watches_Total_Total_Total"]], "FY", n_cycles = 2)), 2)

  cal <- build_calendar_cycles(per, "BC")
  expect_equal(cal$Label, c("BC22", "BC23", "BC24", "BC25"))
  tbl <- default_cycle_table(per)
  tbl$Include[1] <- FALSE
  expect_equal(custom_cycles(tbl, "FY")$Label, c("FY23", "FY24", "FY25"))
  res <- generate_kpi_code("Sales_Watches_Total_Total_Total", mff, cs, TRUE, TRUE, cyc)
  out <- run_code(res$code)
  expect_true(all(c("Sales Watches Log", "FiscalYear", "Weight Variable MMM") %in% names(out)))
  expect_equal(sort(unique(res$metadata$Type)), c("KPI_Log", "Weight_Var"))
})

test_that("external daily data goes to the week that contains each date", {
  days <- seq(as.Date("2023-01-04"), as.Date("2023-01-31"), by = "day")
  ext <- data.frame(date = format(days, "%d/%m/%Y"), value = 1, stringsAsFactors = FALSE)
  agg <- aggregate_external(ext, "date", "value", analytical_periods(ana))
  expect_equal(agg$Value, c(7, 7, 7, 7))       # Wed-Tue weeks, no spill-over
  expect_equal(agg$Period, as.Date(c("2023-01-04", "2023-01-11", "2023-01-18", "2023-01-25")))
  res <- generate_external_vof("Ext Var", ext, "date", "value", ana, cs_dims = cs)
  out <- run_code(res$code)
  expect_equal(sum(out[["Ext Var"]]), 28 * 4)   # 4 geographies
  expect_equal(res$metadata$Type, "External")
})

test_that("CS children filter and inverse", {
  vals <- c("Boston - North", "Boston - South", "Atlanta")
  parts <- cs_children_parts(vals, " - ")
  expect_equal(parts[[1]], c("Atlanta", "Boston"))
  expect_equal(cs_children_filter(vals, " - ", list("Boston", "South")), "Boston - South")
  expect_equal(cs_children_filter(vals, " - ", list(NULL, NULL)), character(0))
})

test_that("pattern flags strip the separator, also when it is a space", {
  expect_equal(cs_pattern_core(c("Boston ", " North ", " Old"), " "), c("Boston", "North", "Old"))
  expect_equal(cs_pattern_core(c("Boston - ", " - North - "), " - "), c("Boston", "North"))
  f <- list(mode = "exclude", levels = list(list(match = "StartsWith", values = c("Boston ", "San "))))
  expect_equal(cs_flag_label("Geography", f, c("Boston A", "San B", "Miami"), sep = " ")$label, "not Boston-San")
})

test_that("metadata schema: CamelCase, single Metric column, no code columns, old files migrated", {
  comp <- new_component("TV GRPs_Watches_Total_Total_Total", "TV", cs = list(Geography = new_cs_filter("Atlanta", "exclude")))
  res <- generate_media_vof(list(comp), cs, mff, long, backcasting = list(enabled = TRUE, start = "2023-01-04", end = "2023-02-01"),
                            context = list(analytical = ana))
  out <- export_metadata(res$metadata, cs)
  expect_false(any(str_detect(names(out), "_")))
  expect_false(any(c("InitCode", "FeedCode", "MetricName", METADATA_CODE_COL) %in% names(out)))
  expect_equal(out$Metric, c("GRPs", "Spend"))
  expect_true(all(c("BackcastingStart", "GeographyFilter", "GeographyGroup") %in% names(out)))

  # The exported metadata alone rebuilds the same code.
  rebuilt <- project_code_from_metadata(read_csv(I(format_csv(out)), col_types = cols(.default = "c")), cs)
  expect_equal(run_code(rebuilt)[[res$name]], run_code(res$code)[[res$name]])

  old <- tibble(Type = c("Media", "KPI_Log"), MainModelVariableName = c("TV --Direct", "Sales Log"),
                AnalyticalVariableName = c("TV GRPs_Watches_Total_Total_Total", "Sales_Watches_Total_Total_Total"),
                MediaChannel = "TV", SubChannel = "", Geographies = c("Atlanta", ""), Metric = c("Activity", "KPI"),
                Backcasting_Start = "", InitCode = c("x", "Data[['Sales Log']] <- 0"), FeedCode = c("y", "Data[['Sales Log']] <- 1"))
  m <- normalize_metadata(old)
  expect_equal(m$Metric, c("GRPs", "KPI"))
  expect_equal(m$VofVariableName[1], "TV")
  expect_equal(m$GeographyFilter, c("Include|Exact", ""))
  expect_match(m[[METADATA_CODE_COL]][2], "<- 1")
  kpi <- generate_kpi_code("Sales_Watches_Total_Total_Total", mff, cs, TRUE, FALSE)
  expect_match(project_code_from_metadata(kpi$metadata, cs), "Sales Watches Log")
})

test_that("children selections are rebuilt from positional filters on any child (inherited rows)", {
  ents <- c("Healthcare - Desktop Core", "Reach - Mobile Ews And Uis", "Unassigned - Mobile Ews And Uis")
  only_b <- cs_children_to_filter(ents, " - ", list(NULL, "Mobile Ews And Uis"))$filter
  ch <- cs_filter_to_children(only_b, ents, " - ")
  expect_length(ch, 2)
  expect_null(ch[[1]])
  expect_equal(ch[[2]], "Mobile Ews And Uis")
  both <- cs_children_to_filter(ents, " - ", list("Reach", "Mobile Ews And Uis"))$filter
  expect_equal(cs_filter_to_children(both, ents, " - "), list("Reach", "Mobile Ews And Uis"))
})

test_that("QA: twins grouped, kind, series, build checks, entities, twin consistency", {
  comps <- list(new_component("TV GRPs_Watches_Total_Total_Total", "TV", start_period = "2023-01-04",
                              cs = list(Geography = new_cs_filter("Atlanta", "exclude"))),
                new_component("Radio GRPs_Watches_Total_Total_Total", "Radio", start_period = "2023-01-04",
                              cs = list(Geography = new_cs_filter("Atlanta", "exclude"))))
  res <- generate_media_vof(comps, cs, mff, long, context = list(analytical = ana))
  hist <- upsert_metadata(empty_metadata(cs), res$metadata)
  d <- run_code(res$code); awb <- d[[res$name]]; awb[1] <- awb[1] + 5
  ana_awb <- ana; ana_awb[[res$name]] <- awb
  qa <- qa_compute(ana_awb, hist, cs)

  expect_equal(qa_groups(qa), vof_base_name(res$name))
  g <- qa_group_blocks(qa, qa_groups(qa))
  expect_equal(g$metric, c("GRPs", "Spend"))
  expect_equal(qa_vof_kind(g$components[[1]])$labels, c("Sum of variables", "Date filter", "Cross-sectional filter"))

  ch <- qa_checks(qa)
  expect_equal(ch$Status[ch$VOF == res$name], "Differs from AWB")

  s <- qa_series(qa, res$name)
  expect_equal(sum(s$value[s$kind == "VOF"]), sum(s$value[s$kind == "Root"]))  # stack edge = VOF

  bc <- qa_build_checks(qa, res$name)
  expect_true(bc$OK[bc$Check == "VOF = sum of its components"])
  expect_true(bc$OK[bc$Check == "Date filter respected"])
  expect_false(bc$OK[bc$Check == "Same values as the Analytical (AWB)"])

  ent <- qa_group_entities(qa, qa_groups(qa))
  expect_equal(ent$Status[ent$Entity == "Atlanta"], "Excluded by the filter")
  expect_true(all(c("GRPs total", "Spend total") %in% names(ent)))

  # Radio GRPs has no spend twin: the spend VOF lacks that component.
  tw <- qa_twin_check(qa, qa_groups(qa), long)
  expect_false(tw$OK)
  expect_match(tw$Detail, "Only in GRPs: Radio GRPs_Watches_Total_Total_Total")
  tv <- generate_media_vof(comps[1], cs, mff, long, context = list(analytical = ana))
  qa_tv <- qa_compute(ana, upsert_metadata(empty_metadata(cs), tv$metadata), cs)
  expect_true(qa_twin_check(qa_tv, qa_groups(qa_tv), long)$OK)

  broken <- hist; broken$Type[1] <- "External"  # no stored code -> reported, others still computed
  ch2 <- qa_checks(qa_compute(ana, broken, cs))
  expect_true("Error" %in% ch2$Status)
})

test_that("dummies: log detection, periods, shapes and team code format", {
  expect_true(detect_log_scale("LN Gross Sales", log(c(1000, 2000, 3000)))$is_log)
  expect_true(detect_log_scale("Sales", log(c(1000, 2000, 3000)))$is_log)       # by values
  expect_false(detect_log_scale("Sales", c(1000, 2000, 3000))$is_log)

  per <- analytical_periods(ana)                                                 # Wednesday weeks
  expect_equal(date_to_period(as.Date(c("2023-11-23", "2023-11-21")), per), as.Date(c("2023-11-22", "2023-11-15")))

  d <- new_dummy("Thanksgiving", "Holiday", dates = as.Date(c("2023-11-23", "2024-11-28")), shape = list(ramp = 2, ramp_param = 0.5))
  expect_equal(dummy_name(d), "Holiday Thanksgiving Ramp2")
  w <- dummy_weights(d, per)
  expect_equal(w$weight, c(0.5, 0.75, 1, 0.5, 0.75, 1))
  expect_equal(side_weights(3, "exponential", 0.5, NULL, "decay"), c(0.5, 0.25, 0.125))
  expect_equal(side_weights(3, "linear", NA, NULL, "ramp"), c(0.25, 0.5, 0.75))

  g <- generate_dummy(d, per, cs)
  expect_match(g$code, 'Data\\[,"Holiday Thanksgiving Ramp2"\\] = 0')
  expect_match(g$code, "Data\\[which\\(Data\\$Period %in% as.Date\\(c\\('2023-11-22'\\)\\)\\), \"Holiday Thanksgiving Ramp2\"\\] = 1")
  out <- run_code(g$code)
  expect_equal(sum(out[["Holiday Thanksgiving Ramp2"]]), 4 * sum(w$weight))       # 4 geographies
  expect_equal(g$metadata$Type, "Dummy")

  shock <- new_dummy("Hurricane", "Macro", dates = as.Date("2024-10-09"), shape = list(decay = 2),
                     cs = list(Geography = "Atlanta"))
  out2 <- run_code(generate_dummy(shock, per, cs)$code)
  expect_equal(sum(out2[["Macro Hurricane Decay2"]][ana$Geography != "Atlanta"]), 0)
  expect_equal(sum(out2[["Macro Hurricane Decay2"]]), 1 + 2/3 + 1/3)

  rng <- new_dummy("Covid Lockdown", "Macro", ranges = list(list(start = as.Date("2023-03-01"), end = as.Date("2023-03-29"))))
  expect_equal(nrow(dummy_weights(rng, per)), 5)
})

test_that("patterns: deviation from the trend, relative or additive", {
  per <- analytical_periods(ana)
  s <- tibble(Period = per, value = 100 + seq_along(per))               # pure trend -> no deviation
  expect_lt(max(abs(kpi_deviation(s, "relative")$dev)), 1e-9)
  expect_equal(choose_scale(c(-1, 2)), "additive")
  s$value[50] <- s$value[50] * 1.5
  expect_equal(kpi_deviation(s, "relative")$dev[50], 0.5, tolerance = 0.02)
  neg <- tibble(Period = per, value = c(rep(-5, 60), rep(5, length(per) - 60)))  # trend crosses 0
  expect_true(all(is.finite(kpi_deviation(neg, "relative")$dev)))
})

test_that("patterns: holidays by event study, yearly windows, atypical points", {
  per <- analytical_periods(ana)                                         # Wednesday weeks, 3+ years
  set.seed(5)
  v <- 100 * (1 + rnorm(length(per), sd = 0.01))
  sale <- per[week_of_year(per) == 30]                                   # an event every year...
  v[per %in% sale] <- v[per %in% sale] * 1.6
  v[match(sale, per) - 1] <- v[match(sale, per) - 1] * 1.2               # ...with a ramp the week before
  xmas <- which(week_of_year(per) %in% 50:51)
  v[xmas] <- v[xmas] * 1.5                                               # regular December window
  v[100] <- v[100] * 0.4                                                 # one-off drop
  ev <- tibble(Name = "Big Sale", Period = sale)
  r <- detect_patterns(tibble(Period = per, value = v), ev)

  h <- r$holidays %>% filter(Pattern == "Big Sale")
  expect_true(h$Passes); expect_equal(h$Offset, 0); expect_gte(h$Ramp, 1)
  expect_equal(h$Lift, 0.6, tolerance = 0.1)

  dec <- r$seasons %>% filter(str_detect(Pattern, "December"))
  expect_equal(nrow(dec), 1)
  expect_match(dec$Pattern, "Peak$")
  expect_setequal(dec$Dates[[1]], per[xmas])
  jul <- r$seasons %>% filter(str_detect(Pattern, "July"))              # a window around the holiday says so
  expect_true(all(str_detect(jul$Covers, "Big Sale")))

  expect_true(per[100] %in% as_date(unlist(r$atypical$Dates)))
  expect_lt(r$atypical$Lift[map_lgl(r$atypical$Dates, ~ per[100] %in% .x)], -0.3)

  d <- pattern_dummy(h, list(Geography = "Atlanta"))
  expect_equal(dummy_name(d), str_c("Holiday Big Sale Ramp", h$Ramp))
  expect_equal(pattern_dummy(dec, list())$category, "Season")
  g <- generate_dummy(d, per, cs)
  expect_equal(sum(run_code(g$code)[[dummy_name(d)]][ana$Geography != "Atlanta"]), 0)
})

test_that("patterns: sub-annual periodicity and entity summary", {
  per <- analytical_periods(ana)
  set.seed(9)
  v <- 100 * (1 + rnorm(length(per), sd = 0.01))
  every6 <- seq(3, length(per), by = 6)
  v[every6] <- v[every6] * 0.7                                           # dip every 6 weeks
  sa <- subannual_patterns(kpi_deviation(tibble(Period = per, value = v)))
  hit <- sa %>% filter(Pattern == "Every 6 Weeks Drop")
  expect_equal(sum(str_detect(sa$Pattern, "^Quarter Week")), 1)
  expect_equal(nrow(hit), 1); expect_true(hit$Passes); expect_lt(hit$Lift, -0.2)

  lvl <- crossing(Entity = c("A", "B"), Period = per) %>%
    mutate(value = if_else(Entity == "A" & week_of_year(Period) == 30, 160, 100))
  pats <- detect_patterns(total_series(lvl), tibble(Name = "Big Sale", Period = per[week_of_year(per) == 30]))$holidays %>% filter(Passes)
  eff <- entity_pattern_effects(lvl, pats)
  expect_equal(eff %>% filter(Effect) %>% pull(Entity), "A")
  summ <- entity_pattern_summary(eff)
  expect_equal(summ$`Entities with the effect`, 1)
})


test_that("media builder: variable roots, suggested names, type filter and clean names", {
  ld <- c("Product", "Campaign", "Outlet", "Creative")
  expect_equal(mff_root_name("Social Video Impressions_Watches_Total_Total_Total", ld), "Social Video")
  expect_equal(mff_root_name("Paid Search Spend_Total_Total_Total_Total", ld), "Paid Search")
  expect_equal(mff_root_name("Magazine Circulation Boutique Closed_Total_Total_Total_Total", ld), "Magazine Boutique Closed")
  expect_equal(mff_root_name("Gross Sales_Total_Total_Total_Total", ld), "Gross Sales")        # no metric: base
  expect_equal(mff_root_name("", ld), "")

  expect_equal(suggest_variable_names("Social Video"), "Social Video")
  expect_equal(suggest_variable_names(c("Social Video", "Social Display", "Social Document")), rep("Total Social", 3))
  expect_equal(suggest_variable_names(c("Social Video", "")), c("Social Video", ""))
  expect_equal(suggest_variable_names(c("TV", "Radio")), c("TV", "Radio"))
  expect_equal(suggest_variable_names(c("Display", "Display")), c("Display", "Display"))
  expect_equal(suggest_variable_names(c("Total Social A", "Total Social B")), rep("Total Social", 2))

  types <- mff_metric_types(c("Display Impressions_W_T_T_T", "Display Spend_W_T_T_T", "Gross Sales_W_T_T_T"), ld)
  expect_equal(types, c("Activity", "Spend", NA))

  comps <- list(new_component("x", "  Paid   Search ", complement = " Watches  ", effect = " Direct "))
  expect_equal(build_media_vof_base_name(comps), "Paid Search - Watches --Direct")
  expect_equal(vof_full_name(" Paid  Search ", "Clicks"), "Paid Search ---Clicks")
})

test_that("media builder: pre-filled VariableName casing and channel abbreviations", {
  expect_equal(format_variable_name("DIRECT TV"), "Direct TV")
  expect_equal(format_variable_name("Online Video"), "OLV")
  expect_equal(format_variable_name("CONNECTED TV"), "CTV")
  expect_equal(format_variable_name("connected video"), "CTV")
  expect_equal(format_variable_name("Advanced TV"), "ATV")
  expect_equal(format_variable_name("over the top"), "OTT")
  expect_equal(format_variable_name("Digital Out-of-Home"), "DOOH")
  expect_equal(format_variable_name("Out of Home Billboards"), "OOH Billboards")
  expect_equal(format_variable_name("Online Videos"), "Online Videos")            # whole words only
  expect_equal(format_variable_name("youtube BRAND"), "YouTube Brand")
  expect_equal(format_variable_name("Owned GA4 Sessions"), "Owned GA4 Sessions")
  expect_equal(format_variable_name("E-mail"), "Email")
  expect_equal(format_variable_name("ONLINE VIDEO-CTV"), "CTV")                    # recurring project naming
  expect_equal(format_variable_name("OLV_CTV"), "CTV")
  expect_equal(format_variable_name("P-MAX"), "Pmax")
  expect_equal(format_variable_name("PMAX"), "Pmax")
  expect_equal(format_variable_name("Google P-MAX Shopping"), "Google Pmax Shopping")
  expect_equal(format_variable_name("OLV"), "OLV")
  expect_equal(format_variable_name("Search (Brand)"), "Search Brand")              # special characters -> space
  expect_equal(format_variable_name("Display/Programmatic, Native."), "Display Programmatic Native")
  expect_equal(format_variable_name("Macy's_Promo"), "Macys Promo")
  expect_equal(format_variable_name("Share of Voice"), "Share of Voice")
  ld <- c("Product", "Campaign", "Outlet", "Creative")
  expect_equal(suggested_root_name("DIRECT TV GRPs_Watches_Total_Total_Total", ld), "Direct TV")
  expect_equal(suggested_root_name("Online Video Impressions_Watches_Total_Total_Total", ld), "OLV")
})

test_that("media builder: live preview of the VOF names", {
  expect_equal(preview_media_vof(list(new_component("", "")), cs, mff, long)$status, "empty")
  pv <- preview_media_vof(list(new_component("TV GRPs_Watches_Total_Total_Total", "")), cs, mff, long)
  expect_equal(pv$status, "incomplete"); expect_match(pv$messages, "VariableName is empty")
  comps <- list(new_component("TV GRPs_Watches_Total_Total_Total", "TV", complement = "Watches"), new_component("", ""))
  pv <- preview_media_vof(comps, cs, mff, long)                     # empty rows are ignored
  res <- generate_media_vof(comps[1], cs, mff, long)
  expect_equal(pv$status, "ok")
  expect_equal(pv$vofs$Name, c(res$activity_name, res$spend_name))
  expect_equal(pv$vofs$Name[pv$vofs$Configured], res$name)
  expect_false(any(pv$vofs$InHistory))
  pv <- preview_media_vof(comps, cs, mff, long, context = list(history = tibble(MainModelVariableName = res$activity_name)))
  expect_equal(pv$vofs$InHistory, c(TRUE, FALSE))
  pv <- preview_media_vof(comps, cs, mff, long, backcasting = list(enabled = TRUE, start = NA, end = NA))
  expect_match(pv$vofs$Name[1], "^Backcasted ")                      # incomplete window does not block the preview
})

test_that("sisters: TimeBreaks with open ends and cuts glued to the left", {
  tb <- list(enabled = TRUE, cuts = list("2024-06-30", "2025-06-30"))
  p <- time_break_parts(tb)$parts
  expect_equal(p$label, c("-20240630", "20240701-20250630", "20250701-"))
  expect_equal(p$start, as.Date(c(NA, "2024-07-01", "2025-07-01")))
  expect_equal(time_break_parts(list(enabled = TRUE, start = "2023-01-01", cuts = list("2024-06-30")))$parts$label,
               c("20230101-20240630", "20240701-"))
  expect_match(time_break_parts(list(cuts = list("2025-01-01", "2024-01-01")))$errors, "after the previous")
  expect_match(time_break_parts(list(cuts = list(NA)))$errors, "set every cut")

  per <- analytical_periods(ana)
  cut <- per[40] + 3                                              # a date that is not a period of the Analytical
  comps <- list(new_component("TV GRPs_Watches_Total_Total_Total", "TV"),
                new_component("Radio GRPs_Watches_Total_Total_Total", "Radio"))
  split <- new_split(tb = list(enabled = TRUE, cuts = list(cut)))
  res <- generate_split_vofs(comps, cs, mff, long, context = list(analytical = ana), split = split)
  expect_equal(res$n_sisters, 2)
  expect_equal(nrow(res$vofs), 4)                                  # each sister with its spend twin
  out <- run_code(res$code)
  act <- res$vofs %>% filter(Type == "Activity") %>% pull(Name)
  expect_match(act[1], str_c("--p -", format(cut, "%Y%m%d"), " ---GRPs"), fixed = TRUE)
  expect_match(act[2], str_c("--p ", format(cut + 1, "%Y%m%d"), "- ---GRPs"), fixed = TRUE)   # metric apart from the open date
  roots <- ana[["TV GRPs_Watches_Total_Total_Total"]] + ana[["Radio GRPs_Watches_Total_Total_Total"]]
  expect_equal(out[[act[1]]] + out[[act[2]]], roots)               # no gaps, no overlaps
  expect_equal(sum(out[[act[1]]][ana$Period > cut]), 0)

  # A row with its own dates: no sisters, the other rows use the first TimeBreak.
  comps[[2]]$follow_tb <- FALSE
  res <- generate_split_vofs(comps, cs, mff, long, context = list(analytical = ana), split = split)
  expect_equal(res$n_sisters, 1)
  expect_match(res$warnings, "Row 2 uses custom dates: no sister VOFs", all = FALSE)

  # Backcasting: only the oldest TimeBreak, window inside it.
  comps[[2]]$follow_tb <- TRUE
  bc <- list(enabled = TRUE, start = per[10], end = per[20])
  res <- generate_split_vofs(comps, cs, mff, long, backcasting = bc, context = list(analytical = ana), split = split)
  act <- res$vofs %>% filter(Type == "Activity") %>% pull(Name)
  expect_match(act[1], "^Backcasted "); expect_false(str_detect(act[2], "Backcasted"))
  bad <- generate_split_vofs(comps, cs, mff, long, backcasting = list(enabled = TRUE, start = per[10], end = per[60]),
                             context = list(analytical = ana), split = split)
  expect_match(bad$errors, "inside TimeBreak 1")
})

test_that("sisters: CS Groups, All Others and combinations with TimeBreaks", {
  comps <- list(new_component("TV GRPs_Watches_Total_Total_Total", "TV"))
  geos <- cs_dim_values(ana, "Geography")
  split <- new_split(cs = list(enabled = TRUE, dim = "Geography", groups = list("Atlanta")))
  res <- generate_split_vofs(comps, cs, mff, long, context = list(analytical = ana), split = split)
  act <- res$vofs %>% filter(Type == "Activity") %>% pull(Name)
  expect_equal(act, c("TV --g Atlanta ---GRPs", "TV --g not Atlanta ---GRPs"))   # All Others follows the flag rules
  out <- run_code(res$code)
  expect_equal(out[[act[1]]] + out[[act[2]]], ana[["TV GRPs_Watches_Total_Total_Total"]])
  expect_true(all(res$metadata$GeographyGroup[res$metadata$MainModelVariableName == act[2]] == "not Atlanta"))

  # Same flag for a different set -> groupK (as for any CS filter); same set -> same flag.
  split2 <- new_split(cs = list(enabled = TRUE, dim = "Geography", groups = list(c("Atlanta", "Boston - North"))))
  first <- generate_split_vofs(comps, cs, mff, long, context = list(analytical = ana), split = split2)
  expect_equal(first$vofs$Name[first$vofs$Type == "Activity"], c("TV --g Atlanta etc ---GRPs", "TV --g top2 ---GRPs"))  # the 2 left are the 2 largest
  split3 <- new_split(cs = list(enabled = TRUE, dim = "Geography", groups = list(c("Atlanta", "Boston - South"))))
  again <- generate_split_vofs(comps, cs, mff, long, context = list(analytical = ana, history = first$metadata), split = split3)
  expect_equal(again$vofs$Name[again$vofs$Type == "Activity"], c("TV --g group1 ---GRPs", "TV --g not Atlanta etc ---GRPs"))
  same <- generate_split_vofs(comps, cs, mff, long, context = list(analytical = ana, history = first$metadata), split = split2)
  expect_equal(same$vofs$Name, first$vofs$Name)
  expect_match(cs_group_parts(list(dim = "Geography", groups = list("Atlanta", "atlanta")), geos)$errors, "more than one group")

  # 2 TimeBreaks x 2 groups (selected) = 4 sisters, each with its twin.
  split3 <- new_split(tb = list(enabled = TRUE, cuts = list(analytical_periods(ana)[50])),
                      cs = list(enabled = TRUE, dim = "Geography", groups = list("Atlanta")))
  res <- generate_split_vofs(comps, cs, mff, long, context = list(analytical = ana), split = split3)
  expect_equal(res$n_sisters, 4); expect_equal(nrow(res$vofs), 8)
  out <- run_code(res$code)
  act <- res$vofs %>% filter(Type == "Activity") %>% pull(Name)
  expect_equal(reduce(map(act, ~ out[[.x]]), `+`), ana[["TV GRPs_Watches_Total_Total_Total"]])
  split3$cs$selected <- "1"; split3$tb$selected <- 2
  res <- generate_split_vofs(comps, cs, mff, long, context = list(analytical = ana), split = split3)
  expect_equal(res$n_sisters, 1)
  expect_match(res$vofs$Name[1], "--p \\d{8}- --g Atlanta ---GRPs")
})

test_that("QA: sister VOFs add up to their roots", {
  comps <- list(new_component("TV GRPs_Watches_Total_Total_Total", "TV"))
  split <- new_split(tb = list(enabled = TRUE, cuts = list(analytical_periods(ana)[60] + 2)),
                     cs = list(enabled = TRUE, dim = "Geography", groups = list("Atlanta")))
  res <- generate_split_vofs(comps, cs, mff, long, context = list(analytical = ana), split = split)
  qa <- qa_compute(ana, res$metadata, cs)
  act <- res$vofs %>% filter(Type == "Activity") %>% pull(Name)
  chk <- qa_build_checks(qa, act[1]) %>% filter(Check == "Sister VOFs add up to the roots")
  expect_true(chk$OK); expect_match(chk$Detail, "4 sister VOFs split by period and Geography")
  partial <- res$metadata %>% filter(MainModelVariableName != act[4])
  chk <- qa_build_checks(qa_compute(ana, partial, cs), act[1]) %>% filter(Check == "Sister VOFs add up to the roots")
  expect_false(chk$OK); expect_match(chk$Detail, "some sister is missing")
  single <- generate_media_vof(comps, cs, mff, long)
  expect_equal(nrow(qa_build_checks(qa_compute(ana, single$metadata, cs), single$name) %>% filter(str_detect(Check, "Sister"))), 0)
  expect_equal(sister_key("OLV and Display --p 20250701- --g All Others"), "OLV and Display")
})

test_that("sisters: CS Groups built with CS Children patterns and Include / Exclude", {
  comps <- list(new_component("TV GRPs_Watches_Total_Total_Total", "TV"))
  geos <- cs_dim_values(ana, "Geography")
  boston <- cs_children_to_filter(geos, " - ", list("Boston"))$filter          # starts with 'Boston - '
  split <- new_split(cs = list(enabled = TRUE, dim = "Geography", groups = list(boston)))
  res <- generate_split_vofs(comps, cs, mff, long, context = list(analytical = ana, cs_sep = " - "), split = split)
  act <- res$vofs %>% filter(Type == "Activity") %>% pull(Name)
  expect_equal(act, c("TV --g Boston ---GRPs", "TV --g not Boston ---GRPs"))
  expect_match(res$code, "!(startsWith(tolower(Data$Geography), tolower('Boston - ')))", fixed = TRUE)   # All Others keeps the pattern
  out <- run_code(res$code)
  expect_equal(out[[act[1]]] + out[[act[2]]], ana[["TV GRPs_Watches_Total_Total_Total"]])

  # Two groups of the same kind: one Exclude level; different kinds: exact list (with a note).
  two_exact <- cs_group_parts(list(dim = "Geography", groups = list("Atlanta", "McDonald's Land")), geos)
  others <- two_exact$parts$filter[[3]]
  expect_equal(others$mode, "exclude"); expect_equal(sort(others$levels[[1]]$values), c("Atlanta", "McDonald's Land"))
  mixed <- cs_group_parts(list(dim = "Geography", groups = list(boston, "Atlanta")), geos)
  expect_equal(mixed$parts$kept[[3]], "McDonald's Land")
  expect_match(mixed$notes, "exact list")
  # An Exclude group: All Others is the Include of the same entities.
  excl <- cs_group_parts(list(dim = "Geography", groups = list(new_cs_filter("Atlanta", mode = "exclude"))), geos)
  expect_equal(excl$parts$kept[[2]], "Atlanta")
  expect_match(cs_group_parts(list(dim = "Geography", groups = list(boston, "Boston - North")), geos)$errors, "more than one group")
  expect_match(cs_group_text("Geography", cs_group_parts(list(dim = "Geography", groups = list(boston)), geos)$parts),
               "Group 1: keeps startswith 'Boston - ' \\(2\\) \\| All Others: 2 entities")
})

test_that("sisters: TimeBreaks with only a start or only an end", {
  expect_equal(time_break_parts(list(start = "2023-01-01", cuts = list("2024-06-30")))$parts$label, c("20230101-20240630", "20240701-"))
  expect_equal(time_break_parts(list(end = "2025-12-31", cuts = list("2024-06-30")))$parts$label, c("-20240630", "20240701-20251231"))
})

test_that("QA: sisters closed by a TimeBreaks end date are compared within that window", {
  per <- analytical_periods(ana)
  comps <- list(new_component("TV GRPs_Watches_Total_Total_Total", "TV"))
  split <- new_split(tb = list(enabled = TRUE, cuts = list(per[60] + 2), end = per[120] + 2))
  res <- generate_split_vofs(comps, cs, mff, long, context = list(analytical = ana), split = split)
  qa <- qa_compute(ana, res$metadata, cs)
  chk <- qa_build_checks(qa, res$vofs$Name[1]) %>% filter(Check == "Sister VOFs add up to the roots")
  expect_true(chk$OK)
})

test_that("sisters: no All Others when every entity is in a group", {
  geos <- cs_dim_values(ana, "Geography")
  full <- cs_group_parts(list(dim = "Geography", groups = list(geos[1:2], geos[3:4])), geos)
  expect_equal(full$parts$label, c("Group 1", "Group 2"))
  expect_match(full$notes, "Every Geography is already in a group")
  expect_false(str_detect(cs_group_text("Geography", full$parts), "All Others"))
  comps <- list(new_component("TV GRPs_Watches_Total_Total_Total", "TV"))
  res <- generate_split_vofs(comps, cs, mff, long, context = list(analytical = ana),
                             split = new_split(cs = list(enabled = TRUE, dim = "Geography", groups = list(geos[1:2], geos[3:4]))))
  expect_equal(res$n_sisters, 2)
  expect_false(any(str_detect(res$vofs$Name, "All Others")))
  expect_match(res$warnings, "already in a group", all = FALSE)
})

test_that("CS flags: a pattern filter is named by its pattern even if the set had another flag", {
  comps <- list(new_component("TV GRPs_Watches_Total_Total_Total", "TV"))
  geos <- cs_dim_values(ana, "Geography")
  boston <- cs_children_to_filter(geos, " - ", list("Boston"))$filter
  exact <- generate_split_vofs(comps, cs, mff, long, context = list(analytical = ana),
                               split = new_split(cs = list(enabled = TRUE, dim = "Geography", groups = list(cs_filter_resolve(boston, geos)))))
  expect_equal(exact$vofs$Name[1], "TV --g Boston - North etc ---GRPs")
  pat <- generate_split_vofs(comps, cs, mff, long, context = list(analytical = ana, cs_sep = " - ", history = exact$metadata),
                             split = new_split(cs = list(enabled = TRUE, dim = "Geography", groups = list(boston))))
  act <- pat$vofs %>% filter(Type == "Activity") %>% pull(Name)
  expect_equal(act, c("TV --g Boston ---GRPs", "TV --g not Boston ---GRPs"))
  # Exact lists still reuse the flag of the same set.
  again <- generate_split_vofs(comps, cs, mff, long, context = list(analytical = ana, history = pat$metadata),
                               split = new_split(cs = list(enabled = TRUE, dim = "Geography", groups = list(cs_filter_resolve(boston, geos)))))
  expect_equal(again$vofs$Name[1], "TV --g Boston ---GRPs")
})

test_that("custom CS flags: rows, sister groups, validation, conflicts and restore", {
  expect_equal(clean_cs_label("  My  Group (A)/B ")$name, "My Group A B")
  expect_match(clean_cs_label("top3")$error, "reserved")
  expect_match(clean_cs_label("Group2")$error, "reserved")
  expect_match(clean_cs_label("not mine")$error, "reserved")
  expect_equal(clean_cs_label("Notable")$error, "")

  geos <- cs_dim_values(ana, "Geography")
  boston <- cs_children_to_filter(geos, " - ", list("Boston"))$filter
  comps <- list(new_component("TV GRPs_Watches_Total_Total_Total", "TV"))
  split <- new_split(cs = list(enabled = TRUE, dim = "Geography", groups = list(boston), names = list("North East"), others_name = "Rest"))
  res <- generate_split_vofs(comps, cs, mff, long, context = list(analytical = ana, cs_sep = " - "), split = split)
  act <- res$vofs %>% filter(Type == "Activity") %>% pull(Name)
  expect_equal(act, c("TV --g North East ---GRPs", "TV --g Rest ---GRPs"))
  expect_equal(unique(res$metadata$GeographyGroup), c("North East", "Rest"))
  expect_match(res$code, "startsWith(tolower(Data$Geography), tolower('Boston - '))", fixed = TRUE)   # the filter does not change
  expect_match(cs_group_text("Geography", cs_group_parts(split$cs, geos)$parts), "Group 1 \\(North East\\)")
  dup <- new_split(cs = list(enabled = TRUE, dim = "Geography", groups = list(boston), names = list("Rest"), others_name = "rest"))
  expect_match(generate_split_vofs(comps, cs, mff, long, context = list(analytical = ana), split = dup)$errors, "more than one group")

  # Rows without sisters: the custom name of a dimension.
  row <- new_component("TV GRPs_Watches_Total_Total_Total", "TV", cs = list(Geography = c("Atlanta", "McDonald's Land")),
                       cs_labels = list(Geography = "Key Markets"))
  one <- generate_media_vof(list(row), cs, mff, long, context = list(analytical = ana))
  expect_equal(one$activity_name, "TV --g Key Markets ---GRPs")
  # The same name for a different set is blocked; another name for the same set is allowed (with a note).
  other <- row; other$cs$Geography <- new_cs_filter("Atlanta")
  blocked <- generate_media_vof(list(other), cs, mff, long, context = list(analytical = ana, history = one$metadata))
  expect_match(blocked$errors, "already names a different set")
  renamed <- modifyList(row, list(cs_labels = list(Geography = "Core")))
  ok <- generate_media_vof(list(renamed), cs, mff, long, context = list(analytical = ana, history = one$metadata))
  expect_equal(ok$activity_name, "TV --g Core ---GRPs"); expect_match(ok$warnings, "also named 'Key Markets'", all = FALSE)
  # Exact lists of the same set reuse the custom name.
  plain <- modifyList(row, list(cs_labels = list()))
  expect_equal(generate_media_vof(list(plain), cs, mff, long, context = list(analytical = ana, history = one$metadata))$activity_name,
               "TV --g Key Markets ---GRPs")
  # Rows with different names for the same dimension.
  r2 <- new_component("Radio GRPs_Watches_Total_Total_Total", "Radio", cs = list(Geography = c("Atlanta", "McDonald's Land")),
                      cs_labels = list(Geography = "Other"))
  expect_match(generate_media_vof(list(row, r2), cs, mff, long, context = list(analytical = ana))$errors, "different Geography group names")

  # Restore: custom flags are told apart from automatic ones.
  expect_equal(name_cs_labels("TV --p -20240630 --g North East ---GRPs", "Geography")$Geography, "North East")
  expect_true(is_custom_cs_label("North East", "Geography", boston, geos, sep = " - "))
  expect_false(is_custom_cs_label("Boston", "Geography", boston, geos, sep = " - "))
  expect_false(is_custom_cs_label("not Boston", "Geography", modifyList(boston, list(mode = "exclude")), geos, sep = " - "))
  expect_false(is_custom_cs_label("group3", "Geography", boston, geos, sep = " - "))
})

test_that("metric suffix: a space before it; old names without the space are still read", {
  expect_equal(vof_full_name("Channel --p 20250101-", "Imps"), "Channel --p 20250101- ---Imps")
  expect_equal(vof_base_name("Channel --p 20250101- ---Imps"), "Channel --p 20250101-")
  expect_equal(vof_base_name("Channel --p 20250101----Imps"), "Channel --p 20250101-")       # old style
  expect_equal(vof_base_name("X --g top5---Spend"), "X --g top5")
  expect_equal(name_cs_labels("TV --g My Group ---Imps", "Geography")$Geography, "My Group")
  expect_equal(name_cs_labels("TV --g My Group---Imps", "Geography")$Geography, "My Group")
  expect_equal(metric_from_vof("Paid Search ---Clicks", list(), long)$metric, "Clicks")
  expect_equal(metric_from_vof("Paid Search---Spend", list(), long)$metric, "Spend")
})

test_that("dummy conflicts: only identical columns blocked, other overlaps warned with the weeks, seasons informative", {
  per <- analytical_periods(ana)                                   # Wednesday weeks
  yrs <- unique(year(per))
  d_on <- function(day) as.Date(sprintf("%d-%s", yrs, day))
  code_of <- function(d) generate_dummy(d, per, cs)$code
  mk <- function(...) { d <- new_dummy(...); tibble(name = dummy_name(d), category = d$category, code = code_of(d)) }
  # Two days of the same week every year (different dates, same period) -> blocked.
  a <- mk("Big Thursday", dates = per[week_of_year(per) == 47] + 1)
  b <- mk("Big Friday", dates = per[week_of_year(per) == 47] + 2)
  cf <- dummy_conflicts(bind_rows(a, b), NULL, ana, cs)
  expect_equal(cf$Level, "Blocked"); expect_match(cf$Detail, "same periods")
  b3 <- mk("Big Friday", dates = c(per[week_of_year(per) == 47] + 2, per[week_of_year(per) == 10]))   # plus other dates
  cf3 <- dummy_conflicts(bind_rows(a, b3), NULL, ana, cs)                  # same event weeks but not identical -> warning
  expect_equal(cf3$Level, "Warning"); expect_match(cf3$Detail, format(per[week_of_year(per) == 47][1]))
  # Same dates but different CS scopes -> no conflict.
  b_bos <- tibble(name = "Holiday Big Friday", category = "Holiday",
                  code = code_of(new_dummy("Big Friday", dates = per[week_of_year(per) == 47] + 2, cs = list(Geography = "Boston - North"))))
  a_atl <- tibble(name = "Holiday Big Thursday", category = "Holiday",
                  code = code_of(new_dummy("Big Thursday", dates = per[week_of_year(per) == 47] + 1, cs = list(Geography = "Atlanta"))))
  expect_equal(nrow(dummy_conflicts(bind_rows(a_atl, b_bos), NULL, ana, cs)), 0)
  # Coincide only one year -> warning with the year.
  wk <- per[week_of_year(per) == 20]
  c1 <- mk("Event One", dates = wk)
  c2 <- mk("Event Two", dates = c(wk[1], per[week_of_year(per) == 30][-1]))
  cf <- dummy_conflicts(bind_rows(c1, c2), NULL, ana, cs)
  expect_equal(cf$Level, "Warning"); expect_match(cf$Detail, str_c("coincide in ", format(wk[1])))
  # Ramp of one runs into the other -> warning.
  e1 <- mk("Eve", dates = per[week_of_year(per) == 50])
  e2 <- mk("Main", dates = per[week_of_year(per) == 51], shape = list(ramp = 2))
  cf <- dummy_conflicts(bind_rows(e1, e2), NULL, ana, cs)
  expect_equal(cf$Level, "Warning"); expect_match(cf$Detail, "ramp / decay overlap")
  # Season window containing a holiday -> info; against a saved dummy as well.
  s <- mk("Winter Peak", category = "Season", dates = per[week_of_year(per) %in% 49:52])
  cf <- dummy_conflicts(e1, s, ana, cs)
  expect_equal(cf$Level, "Info"); expect_equal(cf$OtherSource, "saved")
  # A saved dummy with the name of a pending one is being replaced: not compared.
  expect_equal(nrow(dummy_conflicts(a, a, ana, cs)), 0)
  expect_match(dummy_conflict_summary(dummy_conflicts(bind_rows(a, b), NULL, ana, cs), c(a$name, b$name))[2], "^Blocked: with Holiday Big Thursday")
})

test_that("dummy conflicts: a long event window that contains another event is a warning, not a block", {
  per <- analytical_periods(ana)
  code_of <- function(d) generate_dummy(d, per, cs)$code
  mk <- function(...) { d <- new_dummy(...); tibble(name = dummy_name(d), category = d$category, code = code_of(d)) }
  holiday <- mk("Juneteenth", category = "Business", dates = per[week_of_year(per) == 24])
  storm <- mk("Geopolitical Storm", category = "Macro", dates = per[60:90])
  cf <- dummy_conflicts(storm, holiday, ana, cs)
  shared <- intersect(per[60:90], per[week_of_year(per) == 24])
  expect_equal(cf$Level, "Warning")
  expect_match(cf$Detail, format(as.Date(shared[1])))
})

test_that("old metadata without Type is read from its columns (InitCode / FeedCode ignored)", {
  old <- tibble(
    MainModelVariableName = c("TV --Direct --g Atlanta etc", "TV --Direct --g Atlanta etc", "TV --Direct --g Atlanta etc---Spend",
                              "Search Cost Main", "TV --Direct --g Atlanta etc"),
    AnalyticalVariableName = c("TV GRPs_Watches_Total_Total_Total", "TV GRPs_Watches_Total_Total_Total", "TV Spend_Watches_Total_Total_Total",
                               "Search Cost_Watches_Total_Total_Total", "TV GRPs_Watches_Total_Total_Total"),
    MediaChannel = "TV", SubChannel = c("", "", "", "Brand", ""), Effect = "Direct", MinPeriod = "", MaxPeriod = "22/09/2024",
    Geography = "Atlanta, McDonald's Land", Metric = "Activity",
    InitCode = "Data[, 'Something else'] <- 0", FeedCode = c("wrong code 1", "wrong code 2", "x", "y", "wrong code 1"))
  path <- tempfile(fileext = ".csv"); write_csv(old, path)
  m <- read_metadata_file(path, "old.csv", cs)
  expect_equal(nrow(m), 3)                                                    # repeated rows dropped (code ignored)
  expect_equal(m$Metric, c("GRPs", "Spend", "Spend"))                         # ---Spend twin; cost root as main VOF
  expect_equal(unique(m$Type), "Media"); expect_equal(unique(m$MaxPeriod), "2024-09-22")
  expect_equal(unique(m$GeographyFilter), "Include|Exact"); expect_equal(m$VofComplement[3], "Brand")
  expect_true(all(m[[METADATA_CODE_COL]] == ""))
  expect_match(attr(m, "notes"), "InitCode / FeedCode ignored", all = FALSE)
  comp <- component_from_metadata_row(m[1, ], cs)
  expect_equal(cs_filter_resolve(comp$cs$Geography, cs_dim_values(ana, "Geography")), c("Atlanta", "McDonald's Land"))
})

test_that("dates: month and year only are read as the first day of the month", {
  expect_equal(parse_dates_flexible(c(" 10/2022", "11/2022")), as.Date(c("2022-10-01", "2022-11-01")))
  expect_equal(parse_dates_flexible(c("2022-10", "2022-11")), as.Date(c("2022-10-01", "2022-11-01")))
  expect_equal(parse_dates_flexible(c("Oct 2022", "Nov 2022")), as.Date(c("2022-10-01", "2022-11-01")))
  expect_equal(parse_dates_flexible(c("05/10/2022", "25/10/2022")), as.Date(c("2022-10-05", "2022-10-25")))
})

test_that("external: monthly data expanded to the weeks (replicate, divide, days)", {
  per <- analytical_periods(ana)                                    # weekly, Wednesday weeks
  months <- seq(floor_date(min(per), "month"), floor_date(max(per), "month"), by = "month")
  ext <- tibble(Month = format(months, "%m/%Y"), Value = seq_along(months) * 10)
  d <- diagnose_external(ext, "Month", "Value", ana)
  expect_true(d$needs_expand); expect_equal(d$expand_labels$freq, "monthly")
  expect_match(generate_external_vof("X", ext, "Month", "Value", ana)$errors, "Choose how to expand")

  rep <- aggregate_external(ext, "Month", "Value", per, expand = "replicate")
  expect_equal(nrow(rep), length(per))
  in_m2 <- per[floor_date(per, "month") == months[2]]
  expect_true(all(rep$Value[rep$Period %in% in_m2] == 20))

  div <- aggregate_external(ext, "Month", "Value", per, expand = "divide")
  expect_equal(sum(div$Value[div$Period %in% in_m2]), 20)            # the month total is kept
  expect_equal(unique(div$Value[div$Period %in% in_m2]), 20 / length(in_m2))

  days <- aggregate_external(ext, "Month", "Value", per, expand = "days")
  p <- per[which(floor_date(per, "month") != floor_date(per + 6, "month"))[2]]   # a week across two months
  m1 <- floor_date(p, "month"); m2 <- m1 %m+% months(1)
  d1 <- as.numeric(m2 - p); v1 <- ext$Value[months == m1]; v2 <- ext$Value[months == m2]
  expected <- v1 * d1 / days_in_month(m1) + v2 * (7 - d1) / days_in_month(m2)
  expect_equal(days$Value[days$Period == p], unname(expected))
  res <- generate_external_vof("Monthly X", ext, "Month", "Value", ana, expand = "divide")
  expect_match(res$code, "# Frequency: monthly data, value divided equally among the periods of its month", fixed = TRUE)
  expect_equal(sum(run_code(res$code)[["Monthly X"]][ana$Geography == ana$Geography[1]]), sum(div$Value))
})
