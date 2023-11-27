### Preliminaries -------------------------------------------------------------
suppressMessages({
  library(dplyr)
  library(lubridate)
  library(magrittr)
  library(tidyr)
  library(tictoc)
  library(data.table)
  library(highfrequency)
  library(ggplot2)
  library(xts)
  library(stringi)
})

### Functions -----------------------------------------------------------------
get_daily_RC <- function(refreshed_forex) {
  
  transformed_forex <- refreshed_forex %>% 
    dplyr::mutate(
      Start = row.names(.),
      Date = lubridate::date(Start)
    )
  
  get_one_day_RC <- function(Date_str, transformed_forex) {
    filt_data <- transformed_forex %>% 
      dplyr::filter(
        Date == Date_str
      ) %>% 
      dplyr::select(-Date, -Start) %>% 
      as.matrix() %>% 
      unname()
    print(Date_str)
    RC_res <- RC_est(Y = filt_data)
    return(RC_res)
  }
  
  res <- lapply(
    X = unique(transformed_forex$Date), 
    FUN = get_one_day_RC, 
    transformed_forex = transformed_forex
  ) %>% set_names(as.character(unique(transformed_forex$Date)))
}

### Data load -----------------------------------------------------------------
forex_data <- get_forex_data()

refreshed_forex <- refresh_list(forex_data)

### Get estimates -------------------------------------------------------------
RC_series <- get_daily_RC(refreshed_forex)

saveRDS(RC_series, "RC_series")
saveRDS(refreshed_forex, "refreshed_forex")



