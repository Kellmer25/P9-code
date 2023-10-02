### Dependencies
library(tidyverse)

### Functions
api_fetcher_stock <- function(tickers, multiplier, interval, from_date, to_date) {
  
  # Initiate API vector
  api_vector <- c()
  
  # Create API key for every ticker
  for (ticker in tickers) {
    api <- paste0("https://api.polygon.io/v2/aggs/ticker/", ticker, 
                  "/range/", multiplier,
                  "/", interval,
                  "/", from_date,
                  "/", to_date,
                  "?adjusted=true&sort=asc&limit=50000&apiKey=yeGNbrt0UkaKA_Qmopz_gxbCy1m2_sDD")
    api_vector <- c(api_vector, api)
  }
  
  # Return API vector
  return(api_vector)
}

data_cleaner <- function(df) {
  
  # Convert time to timestamp and arrange by timestamp
  df <- df %>% 
    dplyr::mutate(
      timestamp = lubridate::as_datetime(t / 1000)) %>%
    dplyr::select(
      timestamp,
      c) %>%
    dplyr::arrange(
      timestamp)
}

data_fetcher <- function(tickers, multipler, interval, from_date, to_date) {
  api_vector = api_fetcher_stock(tickers, multiplier, interval, from_date, to_date)
  
  for (api in api_vector) {
    
    # Create initial dataframe
    if (match(api, api_vector) == 1) {
      stock_df <- jsonlite::fromJSON(api)$results %>%
        data_cleaner()
    }
    # Create additional columns for every ticker
    else {
      stock_df <- jsonlite::fromJSON(api)$results %>%
        data_cleaner() %>%
        full_join(stock_df, by = "timestamp")
    }
  }
  # Rename columns and return
  colnames(stock_df) <- c("timestamp", rev(tickers))
  stock_df = stock_df %>%
    dplyr::arrange(timestamp)
  return(stock_df)
}

### Input
tickers = c("AAPL")
multiplier = "1"
interval = "minute"
from_date = "2023-09-11"
to_date = "2023-09-11"

### Main
df = data_fetcher(tickers, multiplier, interval, from_date, to_date)




###
library(httr)

login = list(
  "user" = "hej",
  "password" = "1234"
)

token = "eyJhbGciOiJFUzI1NiIsIng1dCI6IkRFNDc0QUQ1Q0NGRUFFRTlDRThCRDQ3ODlFRTZDOTEyRjVCM0UzOTQifQ.eyJvYWEiOiI3Nzc3NSIsImlzcyI6Im9hIiwiYWlkIjoiMTA5IiwidWlkIjoiNnJnbVR4NG5BMklBZFh4S3xKTXFYZz09IiwiY2lkIjoiNnJnbVR4NG5BMklBZFh4S3xKTXFYZz09IiwiaXNhIjoiRmFsc2UiLCJ0aWQiOiIyMDAyIiwic2lkIjoiZDIwYWY5YzU4NzJjNGJiM2JiNzNjZjI2NWZiYjUwNjgiLCJkZ2kiOiI4NCIsImV4cCI6IjE3MDQwMDc1MjkiLCJvYWwiOiIxRiIsImlpZCI6ImFiMTk2NTQ2MGE2MjQ0YzVkNjY3MDhkYmMxNzJmMTI4In0.x3Xnm760rLW7gDKIVnhPLjq0K1b96KT1p1T74RXv3wyByXD2pn9w0UX62Js3PggIBBjZ60ddKl2HXw2SyFfE5g"

res = GET("https://api.open-notify.org/astros.json")

res = GET("https://gateway.saxobank.com/sim/openapi/ref/v1/instruments?ExchangeId=NYSE&Keywords=Coca Cola&AssetTypes=Stock")
res = GET("https://gateway.saxobank.com/sim/openapi/chart/v1/charts?AssetType=FxSpot&Horizon=1&Uic=21")

test = rawToChar(res$content)

