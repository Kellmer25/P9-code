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

get_forex_data = function(from_month="01", to_month="09") {
  paths = list.files(path="Forex/", pattern=NULL, all.files=FALSE, full.names=TRUE)
  months = paste0("20230", seq(from_month, "08"))
  
  data = list()
  
  counter = 1
  len = length(paths)
  
  for (folder in paths) {
    name = strsplit(folder, "/")[[1]][2]
    cat("Fetching: ", name, " (", counter, "/", len, ")", sep="", end="\n")
    
    files = c()
    csv_paths = list.files(path=folder, pattern=".csv", all.files=FALSE, full.names=TRUE)
    csv_paths = sort(csv_paths)
    matches = c()
    for (i in 1:length(months)) {
      matches[i] = grepl(months[i], csv_paths)[i]
    }
    if (length(months) < length(csv_paths)) {
      matches[(length(months)+1):length(csv_paths)] = FALSE
    }
    files = csv_paths[matches]
   
    asset = list()
    for (file in files) {
      df = read.csv2(file=file, header=FALSE) %>%
        dplyr::mutate(V1 = strptime(V1, format="%Y%m%d %H%M%S")) %>%
        dplyr::mutate(V2 = log(as.numeric(V2))) %>% 
        dplyr::select(V1, V2)
      print(df[[1]][1])
      days = unique(as.Date(df[,1]))
      for (day in days) {
        day = as.Date(day)
        sub_df = df %>% 
          dplyr::filter(as.Date(V1) == day)
        sub_df_xts = xts(sub_df[,-1], order.by = sub_df[,1])
        
        day_name = strsplit(as.character(day), "-")[[1]][2:3]
        day_name = paste0(day_name[1], day_name[2])
        asset[[day_name]] = sub_df_xts
      }
    }
    data[[name]] = asset
    counter = counter + 1
  }
  return(data)
}


for (day in days) {
  print(day)
  day = as.Date(day)
  print(day)
  day = as.Date(day)
  print(day)
}

get_avg_time = function(data) {
  
}

refresh_list = function(data) {
  assets = names(data)
  days = names(data[["EURUSD"]])
  
  for (day in days) {
    for (asset in assets) {
      day_list = list()
    }
    
    
    
  
  }
  
  refreshTime()
  
  return(refresh_df)
}

test = get_forex_data()

tester = refreshTime(data)

saver = test

save(data, "forex_list.Rdata")



### Estimat ---------
RC_est <- function(Y) {
  if (is.null(nrow(Y)) && length(Y)!=0) {
    Y <- matrix(Y, ncol = 1)
  }
  
  n <- nrow(Y) - 1
  d <- ncol(Y)
  
  res <- matrix(rep(0,d^2), ncol = d)
  dY <- diff(Y)
  for (i in 1:n) {
    res <- res + dY[i,]%*%t(dY[i,])
  }
  return(res)
}

df1 = do.call(data.frame, test[[1]][1])
df1[[1]] = log(as.numeric(df1[[1]]))
colnames(df1) = "log-euraud"

downsample = df1 %>% 
  mutate(timestamp = as.POSIXct(row.names(.))) %>% 
  group_by(grp = cut(timestamp, breaks = "5 min")) %>% 
  summarise_all(first) %>% 
  select(-grp)

rc1 = RC_est(df1[[1]])
rc2 = RC_est(downsample[[1]])

avg_time = mean(diff(as.POSIXct(row.names(df1))))

