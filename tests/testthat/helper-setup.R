# Loads the core logic (no Shiny needed) and a small synthetic Analytical.
# testthat runs from tests/testthat, so the app root is two levels up.
app_root <- normalizePath(file.path("..", ".."))
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(stringr)
  library(tibble); library(readr); library(readxl); library(lubridate)
})
for (f in sort(list.files(file.path(app_root, "R", "core"), full.names = TRUE))) source(f)

# Weekly data starting on a Wednesday (week-start day must never be assumed).
make_analytical <- function() {
  periods <- seq(as.Date("2022-01-05"), by = "week", length.out = 170)
  geos <- c("Atlanta", "Boston - North", "Boston - South", "McDonald's Land")
  df <- expand.grid(Period = periods, Geography = geos, stringsAsFactors = FALSE)
  df <- df[order(df$Geography, df$Period), c("Geography", "Period")]
  set.seed(1)
  n <- nrow(df)
  df[["TV GRPs_Watches_Total_Total_Total"]]      <- round(runif(n, 0, 100))
  df[["TV Spend_Watches_Total_Total_Total"]]     <- round(runif(n, 0, 1000))
  df[["Search Clicks_Watches_Total_Total_Total"]] <- round(runif(n, 0, 50))
  df[["Search Cost_Watches_Total_Total_Total"]]  <- round(runif(n, 0, 500))
  df[["Social Transactions_Watches_Total_Total_Total"]] <- round(runif(n, 0, 5))
  df[["Sales_Watches_Total_Total_Total"]]        <- round(runif(n, 100, 1000))
  df[["Sales_Leather_Total_Total_Total"]]        <- round(runif(n, 100, 1000))
  df[["Radio GRPs_Watches_Total_Total_Total"]]   <- round(runif(n, 0, 80))
  df[["Search Impressions_Watches_Total_Total_Total"]] <- round(runif(n, 0, 5000))
  df[["Promo Units_Watches_Total_Total_Total"]]  <- round(runif(n, 0, 10))
  df[["My_Var_With_Underscores_Watches_Total_Total_Total"]] <- 1
  df[["Existing VOF --Direct"]] <- 0
  df[["Google_Trends"]] <- 0
  rownames(df) <- NULL
  df
}
