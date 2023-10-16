### Preliminaries -------------------------------------------------------------
suppressMessages({
  library(dplyr)
  library(magrittr)
  library(readxl)
  library(lubridate)
  library(xts)
  library(highfrequency)
})


### Functions -----------------------------------------------------------------
Get_Eikon <- function(filename,Date = c("2023-08-25")){
  Dates <- as.Date(Date)
  
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
    dplyr::filter(lubridate::date(Timestamp) %in% Dates)
  
  return(Data)
}

get_lobster_data = function(ticker) {
  ticker = toupper(ticker)
  message = read.csv(paste0("Message/", ticker, "_2012-06-21_34200000_57600000_message_1.csv")) %>%
    as.data.frame() %>%
    magrittr::set_colnames(c("time", "type", "order_id", "size", "price", "direction"))%>%
    dplyr::mutate(
      ms = round(time - 34200, 2), .after=1
    ) %>% dplyr::mutate(
      time = as.POSIXct(time, origin="2012-10-06 00:00:00"),
      price = round(price / 10000, 2)
    )
 
  orderbook = read.csv(paste0("Orderbook/", ticker, "_2012-06-21_34200000_57600000_orderbook_1.csv"), header=FALSE) %>%
    as.data.frame() %>%
    magrittr::set_colnames(c("ask", "ask_size", "bid", "bid_size")) %>%
    dplyr::mutate(
      ask = round(ask / 10000, 2),
      bid = round(bid / 10000, 2)
    ) %>% 
      dplyr::filter(row_number() <= n()-1)
  
  message = message %>%
    dplyr::mutate(bid = orderbook$bid, .before = price) %>%
    dplyr::mutate(ask = orderbook$ask, .after = price)
  
 return(message) 
}

get_polygon_time_series = function(ticker, multiplier="1", interval="minute", to_date="2023-09-11", from_date="2023-09-11") {
  ticker = toupper(ticker)
  api_key = paste0(
    "https://api.polygon.io/v2/aggs/ticker/", ticker, 
    "/range/", multiplier,
    "/", interval,
    "/", from_date,
    "/", to_date,
    "?adjusted=true&sort=asc&limit=50000&apiKey=yeGNbrt0UkaKA_Qmopz_gxbCy1m2_sDD"
  )
  
  stock_df = jsonlite::fromJSON(api_key)$results %>%
    dplyr::mutate(timestamp = lubridate::as_datetime(t / 1000)) %>%
    dplyr::select(timestamp, c) %>%
    dplyr::rename(price = c) %>% 
    dplyr::arrange(timestamp)
  
  start = stock_df[1, 1] %>% as.POSIXct()
  end = stock_df[nrow(stock_df), 1] %>% as.POSIXct()
  
  na_df = seq(from=start, to=end, by="min") %>%
    as.data.frame() %>%
    magrittr::set_colnames(c("timestamp"))
  
  stock_df = merge(stock_df, na_df, by="timestamp", all=TRUE)[1:2]
  return(stock_df)
}

get_forex_data = function(full=FALSE, refresh=FALSE, month="09") {
  paths = list.files(path="Forex/", pattern=NULL, all.files=FALSE, full.names=TRUE)
  month = paste0("2023", month)
  files = c()
  
  for (folder in paths) {
    csv_paths = list.files(path=folder, pattern=".csv", all.files=FALSE, full.names=TRUE)
    csv_paths = sort(csv_paths)
    matches = grepl(month, csv_paths)
    files = c(files, csv_paths[matches])
  }
  
  data = list()
  counter = 1
  len = length(files)
  
  if (isTRUE(full)) {
    for (file in files) {
      name = strsplit(file, "/")[[1]][2]
      cat("Fetching: ", name, " (", counter, "/", len, ")", sep="", end="\n")
      df = read.csv2(file=files[1], header=FALSE) %>%
        dplyr::mutate(V1 = strptime(V1, format="%Y%m%d %H%M%S")) %>%
        dplyr::select(V1, V2)
      df_xts = xts(df[,-1], order.by = df[,1])
      data[[name]] = df_xts
      counter = counter + 1
    }
  }
  else {
    for (file in files) {
      placeholder = list()
      name = strsplit(file, "/")[[1]][2]
      cat("Fetching: ", name, " (", counter, "/", len, ")", sep="", end="\n")
      df = read.csv2(file=files[1], header=FALSE) %>%
        dplyr::mutate(V1 = strptime(V1, format="%Y%m%d %H%M%S")) %>%
        dplyr::select(V1, V2)
      days = unique(as.Date(df[,1]))
      for (day in days) {
        sub_df = df %>% 
          dplyr::filter(as.Date(V1) == day)
        sub_df_xts = xts(sub_df[,-1], order.by = sub_df[,1])
        placeholder[[length(placeholder)+1]] = sub_df_xts
      }
      data[[name]] = placeholder
      counter = counter + 1
    }
  }
  if (isTRUE(refresh)) {
  }
  return(data)
}

save(test, file="forex_list2.RData")


test = get_forex_data()

test1 = refreshTime(test)



