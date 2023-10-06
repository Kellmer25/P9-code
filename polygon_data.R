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
