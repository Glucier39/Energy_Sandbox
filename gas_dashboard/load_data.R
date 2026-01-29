# ============================================================================
# LOAD_DATA.R - Minimal data loading for Shiny app
# ============================================================================

library(xgboost)
library(tidyquant)
library(riem)
library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)

# EIA API KEY
eia_api_key <- "UhdZNEINPQj9mdr8f6wOS5KQliGFaOlZYOOMBZdD"

# Load model
xgb_model <- xgb.load("~/Documents/GitHub/Energy_Sandbox/gas_dashboard/models/xgb_gasoline_price.model")

# Load metadata
model_metadata <- readRDS("~/Documents/GitHub/Energy_Sandbox/gas_dashboard/models/model_metadata.rds")
historical_sd <- readRDS("~/Documents/GitHub/Energy_Sandbox/gas_dashboard/data/historical_volatility.rds")

get_eia_stock <- function(old_series_id) {
  # Use the backward compatibility format: /v2/seriesid/V1_SERIES_ID
  url <- paste0(
    "https://api.eia.gov/v2/seriesid/", old_series_id,
    "?api_key=", eia_api_key
  )
  
  response <- GET(url)
  content_text <- content(response, "text", encoding = "UTF-8")
  parsed <- fromJSON(content_text)
  
  # Data is in a data frame, first row has most recent value
  return(as.numeric(parsed$response$data$value[1]))
}

# Fetch latest stocks (using old v1 series IDs with backward compatibility)
crude_stocks <- get_eia_stock("PET.WCESTUS1.W")
gas_stocks <- get_eia_stock("PET.WGTSTUS1.W")
distillate_stocks <- get_eia_stock("PET.WDISTUS1.W")

# Get gasoline prices (last 40 days for lags)
gulf_gas <- tq_get("DGASUSGULF", get = "economic.data", from = Sys.Date() - 40)
prices <- tail(gulf_gas$price, 30)
latest_price <- tail(prices, 1)
latest_date <- tail(gulf_gas$date, 1)

# Get weather (last 40 days for lags)
houston <- riem_measures(station = "IAH", date_start = Sys.Date() - 40, date_end = Sys.Date())

weather <- houston %>%
  mutate(date = as.Date(valid)) %>%
  group_by(date) %>%
  summarise(
    temp_avg = mean(tmpf, na.rm = TRUE),
    temp_max = max(tmpf, na.rm = TRUE),
    temp_min = min(tmpf, na.rm = TRUE),
    .groups = 'drop'
  )

temps_avg <- tail(weather$temp_avg, 30)
temps_max <- tail(weather$temp_max, 30)
temps_min <- tail(weather$temp_min, 30)

# Build latest market state
latest_market_state <- list(
  price = latest_price,
  date = latest_date,
  year = year(latest_date),
  
  price_lag1 = prices[length(prices) - 1],
  price_lag3 = prices[length(prices) - 3],
  price_lag7 = prices[length(prices) - 7],
  
  crude_stocks = crude_stocks,
  gas_stocks = gas_stocks,
  distillate_stocks = distillate_stocks,
  
  crude_lag7 = crude_stocks,
  gas_lag7 = gas_stocks,
  distillate_lag7 = distillate_stocks,
  
  temp_avg_lag1 = temps_avg[length(temps_avg) - 1],
  temp_avg_lag7 = temps_avg[length(temps_avg) - 7],
  temp_max_lag1 = temps_max[length(temps_max) - 1],
  temp_min_lag1 = temps_min[length(temps_min) - 1],
  
  month = as.numeric(month(latest_date)),
  is_summer = as.numeric(month(latest_date) %in% c(5, 6, 7, 8)),
  is_hurricane = as.numeric(month(latest_date) %in% c(8, 9, 10))
)
