### Preliminaries -------------------------------------------------------------
suppressMessages({
  library(dplyr)
  library(magrittr)
  library(readxl)
  library(lubridate)
})


### Functions -----------------------------------------------------------------
Get_Eikon <- function(filename,Date = "2023-08-25"){
  
  Data <- readxl::read_xlsx(
    paste0("Eikon\\",filename),
    skip = 5
  ) %>% 
    dplyr::select(
      Timestamp,
      "Price" = `Last Trade`,
      Volume
    ) %>% 
    dplyr::mutate(
      Timestamp = as.POSIXct(Timestamp, format = "%d-%b-%Y %H:%M:%OS")
    ) %>% 
    dplyr::filter(lubridate::date(Timestamp) %in% as.POSIXct(Date))
  
  return(Data)
}
