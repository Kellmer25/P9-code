### Preliminaries -------------------------------------------------------------
suppressMessages({
  library(dplyr)
  library(magrittr)
  library(readxl)
  library(lubridate)
  library(xts)
  library(highfrequency)
  library(kableExtra)
  library(cowplot)
  library(plotly)
  library(ggpubr)
  library(ggplot2)
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

get_ms_noise = function(refresh_data) {
  diff_y = diff(as.matrix(refresh_data))
  n = nrow(diff_y)
  res = 0
  for (i in 1:n) {
    res = res + diff_y[i,] %*% t(diff_y[i,])
  }
  res = res / (2*n)
  return(res)
}

### Lobster data ---------------------------------------------------------------
get_lobster_data = function(ticker) {
  ticker = toupper(ticker)
  message = read.csv(paste0("Message/", ticker, "_2012-06-21_34200000_57600000_message_1.csv")) %>%
    as.data.frame() %>%
    magrittr::set_colnames(c("time", "type", "order_id", "size", "price", "direction"))%>%
    dplyr::mutate(ms = round(time - 34200, 2), .after=1) %>% 
    dplyr::mutate(time = as.POSIXct(time, origin="2012-10-06 00:00:00"),
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

### Polygon --------------------------------------------------------------------
get_polygon_time_series = function(ticker, multiplier="1", interval="minute", from_date="2023-09-03", to_date="2023-09-29") {
  ticker = toupper(ticker)
  api_key = paste0(
    "https://api.polygon.io/v2/aggs/ticker/", ticker, 
    "/range/", multiplier,
    "/", interval,
    "/", from_date,
    "/", to_date,
    "?adjusted=true&sort=asc&limit=50000&apiKey=e8jrQvnDb_qc0yjciOUCIq1cZGVvnZYm"
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

### Forex data -----------------------------------------------------------------
get_forex_data_dfs = function(from_month="01", to_month="09") {
  paths = list.files(path="Forex/", pattern=NULL, all.files=FALSE, full.names=TRUE)
  months = paste0("20230", seq(from_month, to_month))
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
    asset = matrix(ncol=2) %>% 
      as.data.frame() %>% 
      magrittr::set_colnames(c("time", {{name}}))
    
    for (file in files) {
      df = read.csv2(file=file, header=FALSE) %>%
        dplyr::mutate(V1 = as.POSIXct(strptime(V1, format="%Y%m%d %H%M%S"), tz="UTC")) %>%
        dplyr::mutate(V2 = log(as.numeric(V2))) %>% 
        magrittr::set_colnames(c("time", {{name}})) %>% 
        dplyr::select(time, {{name}})
      asset = rbind(asset, df)
    }
    asset$time = as.POSIXct(asset$time, tz="utc", origin="1970-01-01")
    data[[name]] = asset %>% na.omit()
    counter = counter + 1
  }
  return(data)
}

get_forex_data = function(from_month="01", to_month="09") {
  paths = list.files(path="Forex/", pattern=NULL, all.files=FALSE, full.names=TRUE)
  months = paste0("20230", seq(from_month, to_month))
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
        dplyr::mutate(V1 = as.POSIXct(strptime(V1, format="%Y%m%d %H%M%S"), tz="UTC")) %>%
        dplyr::mutate(V2 = log(as.numeric(V2))) %>% 
        dplyr::select(V1, V2)
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

get_empirical_data = function(data) {
  assets = names(data)
  diff_list = list()
  acf_list = list()
  for (asset in assets) {
    asset_df = data[[asset]]
    times = asset_df$time[2:nrow(asset_df)]
    diffs = as.double(diff(asset_df$time))
    diff_df = data.frame(times, diffs)
    diff_list[[asset]] = diff_df
    acf_list[[asset]] = acf(asset_df[[asset]])$acf
  }
  return(list("diff"=diff_list, "acf"=acf_list))
}

refresh_list = function(data) {
  assets = names(data)
  days = names(data[["EURJPY"]])
  refresh_df = matrix(ncol=length(assets)+1) %>% 
    as.data.frame() %>% 
    magrittr::set_colnames(c(assets, "timestamp"))
  missed_days = c()
  for (day in days) {
    day_list = list()
    for (asset in assets) {
      day_list[[asset]] = data[[asset]][[day]]
    }
    if (length(day_list) < length(assets)) {
      missed_days = c(missed_days, day)
      next
    }
    refresh = refreshTime(day_list) %>% as.data.frame()
    refresh$timestamp = rownames(refresh)
    rownames(refresh) = NULL
    refresh_df = rbind(refresh_df, refresh)
  }
  refresh_df = refresh_df[2:nrow(refresh_df),]
  rownames(refresh_df) = refresh_df[["timestamp"]]
  refresh_df[["timestamp"]] = NULL
  return(refresh_df)
}

get_avg_time = function(data) {
  assets = names(data)
  times = matrix(ncol=3, nrow=length(assets)) %>% 
    as.data.frame() %>% 
    magrittr::set_colnames(c("obs", "avg", "avg_trading")) %>% 
    magrittr::set_rownames(assets)
  for (asset in assets) {
    hit_times = c()
    asset_list = data[[asset]]
    obs = 0
    time = 0
    for (list in asset_list) {
      obs = obs + length(index(list))
      time = time + length(index(list)) * mean(as.double(diff(as.POSIXct(index(list), origin="1970-01-01"))))
      hit_times = c(hit_times, index(list))
    }
    avg_time = mean(as.double(diff(as.POSIXct(hit_times, origin="1970-01-01"))))
    times[asset, ] = c(obs, avg_time, time / obs)
  }
  return(times)
}

get_comp_df = function(data, return_matrices=TRUE, return_diff=TRUE) {
  times = get_avg_time(data) %>% arrange(avg)
  assets = rownames(times) %>% rev()
  info = matrix(ncol=5, nrow=length(assets)-1) 
  matrices = list()
  diff_list = list()
  
  refresh_data = refresh_list(data)
  info[1,1] = assets[1]
  info[1,2] = ncol(refresh_data)
  info[1,3] = nrow(refresh_data)
  info[1,4] = mean(as.numeric(diff(as.POSIXct(rownames(refresh_data)))))
  info[1,5] = get_ms_noise(refresh_data) %>% diag() %>% norm(type="2") %>% format(scientific = TRUE)
  matrices[[info[1,1]]] = get_ms_noise(refresh_data)
  
  time = as.POSIXct(rownames(refresh_data))
  diff_data = diff(as.POSIXct(rownames(refresh_data)))
  diff_list[[info[1,1]]] = data.frame(time, diff_data)
  
  data_res = data
  counter = 2
  for (asset in assets[1:(length(assets)-2)]) {
    print(asset)
    data_res[[asset]] = NULL
    refresh_data = refresh_list(data_res)
    info[counter,1] = assets[[counter]]
    info[counter,2] = ncol(refresh_data)
    info[counter,3] = nrow(refresh_data)
    info[counter,4] = mean(as.numeric(diff(as.POSIXct(rownames(refresh_data)))))
    info[counter,5] = get_ms_noise(refresh_data) %>% diag() %>% norm(type="2") %>% format(scientific = TRUE)
    matrices[[info[counter,1]]] = get_ms_noise(refresh_data)
    
    time = as.POSIXct(rownames(refresh_data))
    diff_data = diff(as.POSIXct(rownames(refresh_data)))
    diff_list[[info[counter,1]]] = data.frame(time, diff_data)
    counter = counter + 1
  }
  info = info %>% as.data.frame() %>% 
    magrittr::set_colnames(c("least", "nr_assets", "obs", "avg", "ms"))
  if (isTRUE(return_matrices)) {
    return(list("info"=info, "matrices"=matrices, "diff"=diff))
  }
  return(info)
}

### SPX data -------------------------------------------------------------------
load_spx_data = function() {
  files = list.files(path="SPX/", pattern=NULL, all.files=FALSE, full.names=TRUE) %>% sort()
  
  df = read.csv2(file="SPX/DAT_NT_SPXUSD_T_LAST_202301.csv", header=FALSE) %>%
    dplyr::mutate(V1 = as.POSIXct(strptime(V1, format="%Y%m%d %H%M%S"), tz="UTC")) %>%
    dplyr::mutate(V2 = log(as.numeric(V2))) %>% 
    dplyr::select(V1, V2)
  
  for (file in files[2:length(files)]) {
    spx = read.csv2(file=file, header=FALSE) %>%
      dplyr::mutate(V1 = as.POSIXct(strptime(V1, format="%Y%m%d %H%M%S"), tz="UTC")) %>%
      dplyr::mutate(V2 = log(as.numeric(V2))) %>% 
      dplyr::select(V1, V2)
    df = rbind(df, spx)
  }
  colnames(df) = c("time", "spx")
  return(df)
}
  
### Stock data -----------------------------------------------------------------
get_polygon_df = function(tickers, multiplier="1", interval="minute", from_date="2023-09-03", to_date="2023-09-29") {
  df = FALSE
  for (ticker in tickers) {
    print(ticker)
    if (isFALSE(df)) {
      time_series = get_polygon_time_series(ticker, multiplier=multiplier, interval=interval, to_date=to_date, from_date=from_date)
      df = time_series
      colnames(time_series) = c("timestamp", ticker)
    } else {
      Sys.sleep(20)
      time_series = get_polygon_time_series(ticker, multiplier=multiplier, interval=interval, to_date=to_date, from_date=from_date)
      colnames(time_series) = c("timestamp", ticker)
      
      df = merge(df, time_series, by="timestamp", all=TRUE)
      
    }
  }
  df$timestamp = as.POSIXct(df$timestamp, tz="UTC")
  return(df)
}

get_stock_avg_time = function(stock_df) {
  tickers = colnames(stock_df)[2:ncol(stock_df)]
  time_df = matrix(ncol=3, nrow=ncol(stock_df)-1) %>% 
    as.data.frame() %>% 
    magrittr::set_rownames(tickers) %>% 
    magrittr::set_colnames(c("obs", "avg", "avg_trading"))
  
  for (ticker in tickers) {
    sub_df = stock_df[, c("timestamp", ticker)] %>% na.omit()
    time_df[ticker, 1] = nrow(sub_df)
    time_df[ticker, 2] = diff(sub_df$timestamp) %>% mean()
    time_df[ticker, 3] = diff(sub_df$timestamp) %>% ifelse(. > 120, 1, .) %>% mean()    
  }
  time_df = time_df %>% arrange(avg)
  return(time_df)
}

get_stock_refresh = function(stock_df) {
  tickers = colnames(stock_df)[2:ncol(stock_df)]
  observed = list()
  for (ticker in tickers) {
    observed[[ticker]] = FALSE
  }
  
  times = matrix(ncol=ncol(stock_df), nrow=nrow(stock_df))
  for (i in 1:nrow(stock_df)) {
    row = stock_df[i, ]
    for (ticker in tickers) {
      if (isFALSE(observed[[ticker]])) {
        if (is.na(row[[ticker]])) {
          next
        } else {
          observed[[ticker]] = row[[ticker]]
        }
      }
    }
    if (all(sapply(observed, function(x) x != FALSE))) {
      times[i, 1] = row[["timestamp"]]
      times[i, 2:ncol(times)] = unlist(unname(observed))
      observed = lapply(observed, function(x) FALSE)
    }
  }
  times = as.data.frame(times) %>% 
    na.omit() %>% 
    magrittr::set_colnames(c("timestamp", tickers)) %>% 
    dplyr::mutate(timestamp = as.POSIXct(timestamp, origin="1970-01-01", tz="UTC"))
  return(times)
}

get_refresh_avg_time = function(stock_df, return_matrix=TRUE) {
  times = get_stock_avg_time(stock_df)
  tickers = times %>% rownames(.) %>% rev()
  info = matrix(ncol=6, nrow=length(tickers)-1)
  
  refresh_data = get_stock_refresh(stock_df)
  rownames(refresh_data) = refresh_data$timestamp
  refresh_data$timestamp = NULL
  info[1, 1] = tickers[1]
  info[1, 2] = ncol(refresh_data)
  info[1, 3] = nrow(refresh_data)
  info[1, 4] = mean(as.numeric(diff(as.POSIXct(rownames(refresh_data)))))
  info[1, 5] = rownames(refresh_data) %>% as.POSIXct() %>% diff() %>% ifelse(. > 120, 1, .) %>% as.numeric %>% mean()
  info[1, 6] = get_ms_noise(refresh_data) %>% diag() %>% norm(type="2") %>% format(scientific = TRUE)
  
  data_res = stock_df
  counter = 2
  for (ticker in tickers[1:(length(tickers)-2)]) {
    print(ticker)
    data_res[[ticker]] = NULL
    refresh_data = get_stock_refresh(data_res)
    rownames(refresh_data) = refresh_data$timestamp
    refresh_data$timestamp = NULL
    info[counter, 1] = tickers[counter]
    info[counter, 2] = ncol(refresh_data)
    info[counter, 3] = nrow(refresh_data)
    info[counter, 4] = mean(as.numeric(diff(as.POSIXct(rownames(refresh_data)))))
    info[counter, 5] = rownames(refresh_data) %>% as.POSIXct() %>% diff() %>% ifelse(. > 120, 1, .) %>% as.numeric %>% mean()
    info[counter, 6] = get_ms_noise(refresh_data) %>% diag() %>% norm(type="2") %>% format(scientific = TRUE)
    counter = counter + 1
  }
  info = as.data.frame(info) %>%
    magrittr::set_colnames(c("least", "nr_assets", "obs", "avg", "avg_trading", "ms"))
  if (isTRUE(return_matrix)) {
    return(info, matrices)
  } else {
    return(info)
  }
}

### Simulation Study -----------------------------------------------------------
transform_results <- function(lambda_level, simulation_result) {
  
  res <- matrix(rep(0,5*6),nrow = 5, ncol = 6)
  for (i in 1:1000) {
    res <- res + simulation_result[[lambda_level]][[i]][[1]]
  }
  return(as.data.frame(1/1000*res))
}

results_to_table <- function(simulation_result){
  results <- lapply(
    1:5, 
    transform_results, 
    simulation_result = simulation_result
  )
  
  rbound_res <- purrr::map_dfr(results, function(x){return(x)}) %>% 
    dplyr::mutate(Noise = substr(rownames(.), 1, 3)) %>% 
    magrittr::set_rownames(NULL) %>% 
    dplyr::select(
      Noise,
      "bias" = RC_BIAS,
      "mae" = RC_MAE,
      "rmse" = RC_RMSE,
      "mbias" = MRC_BIAS,
      "mmae" = MRC_MAE,
      "mrmse" = MRC_RMSE
    ) %>% 
    mutate_if(is.numeric, round, 3) %>% 
    dplyr::add_row(
      Noise = "0",
      bias = 0,
      rmse = 0,
      mae = 0,
      mbias = 0,
      mrmse = 0,
      mmae = 0, 
      .before = 1
    ) %>% 
    dplyr::add_row(
      Noise = "0",
      bias = 0,
      rmse = 0,
      mae = 0,
      mbias = 0,
      mrmse = 0,
      mmae = 0, 
      .before = 7
    ) %>% 
    dplyr::add_row(
      Noise = "0",
      bias = 0,
      rmse = 0,
      mae = 0,
      mbias = 0,
      mrmse = 0,
      mmae = 0, 
      .before = 13
    ) %>% 
    dplyr::add_row(
      Noise = "0",
      bias = 0,
      rmse = 0,
      mae = 0,
      mbias = 0,
      mrmse = 0,
      mmae = 0, 
      .before = 19
    ) %>% 
    dplyr::add_row(
      Noise = "0",
      bias = 0,
      rmse = 0,
      mae = 0,
      mbias = 0,
      mrmse = 0,
      mmae = 0, 
      .before = 25
    )
  
  latex_table <- kableExtra::kbl(
    rbound_res, 
    longtable = T, 
    booktabs = T,
    format = "latex",
    linesep = c("", "", "", "", "", paste0("\\addlinespace")),
    caption = "Longtable"
  ) %>%
    kableExtra::add_header_above(c(" ", "RC" = 3, "MRC" = 3)) %>%
    kableExtra::kable_styling(
      latex_options = c(
        "repeat_header", 
        "striped"),
      stripe_index = c(1:6, 13:18, 25:30),
      position = "center"
    ) %>%
    kableExtra::column_spec(2:7, width = "2cm")
  
  clipr::write_clip(latex_table)
  
}

twobytwo <- function(lambda_level, simulation_result) {
  mrc_list <- list(
    "mrc_11" = c(1,1),
    "mrc_12" = c(1,2),
    "mrc_21" = c(2,1),
    "mrc_22" = c(2,2)
  )
  
  rc_list <- list(
    "rc_11" = c(1,1),
    "rc_12" = c(1,2),
    "rc_21" = c(2,1),
    "rc_22" = c(2,2)
  )
  
  mrc_res <- lapply(mrc_list, function(vec, lambda_level) {
    res <- rep(0, 1000)
    for (i in 1:1000) {
      res[i] <-
        simulation_result[[lambda_level]][[i]][['MRC']][['MS0']][vec[1],vec[2]] -
        simulation_result[[lambda_level]][[i]][['TrueCov']][vec[1],vec[2]]
    }
    return(res)
    
  }, lambda_level = lambda_level)
  
  rc_res <- lapply(rc_list, function(vec, lambda_level) {
    res <- rep(0, 1000)
    for (i in 1:1000) {
      res[i] <-
        simulation_result[[lambda_level]][[i]][['RC']][['MS0']][vec[1],vec[2]] -
        simulation_result[[lambda_level]][[i]][['TrueCov']][vec[1], vec[2]]
        
    }
    return(res)
    
  }, lambda_level = lambda_level)
  
  figs <- lapply(1:4, function(number) {
    x_bound <- 0.035
    binsw <- 0.003
    if (number %in% c(2,3)) {
      x_bound <- 0.25
      binsw <- 0.02
    }
    entry <- switch(number, 11, 12, 21, 22)
    
    color1 <- "#ea5545"
    color2 <- "#27aeef"
    
    df <- data.frame(
      "MRC" = mrc_res[[number]],
      "RC" = rc_res[[number]]
    ) %>% tidyr::gather(key = 'Estimator', value = result)
    
    fig <- ggplot(df, aes(x = result, color=Estimator, fill = Estimator)) +
      geom_histogram(alpha=0.4, position="identity", binwidth = binsw) +
      xlim(-x_bound,x_bound) +
      scale_color_manual(values=c(color1, color2)) +
      scale_fill_manual(values=c(color1, color2)) + 
      geom_vline(
        aes(xintercept=quantile(mrc_res[[number]], probs = 0.25)), 
        color=color1,
        linetype="dashed"
      ) +
      geom_vline(
        aes(xintercept=quantile(mrc_res[[number]], probs = 0.75)), 
        color=color1,
        linetype="dashed"
      ) +
      geom_vline(
        aes(xintercept=quantile(rc_res[[number]], probs = 0.25)), 
        color= color2,
        linetype="dashed"
      ) +
      geom_vline(
        aes(xintercept=quantile(rc_res[[number]], probs = 0.75)), 
        color = color2,
        linetype="dashed"
      ) + xlab("Bias") + ylab("Count") +
      ggtitle(bquote(Bias ~ of ~ MRC[.(entry)] ~ and ~ RC[.(entry)])) +
      scale_y_continuous(expand =  expansion(mult = c(0, 0.05)))
    
    return(fig)
  })
  final_plot <- ggarrange(
    figs[[1]],figs[[2]],figs[[3]],figs[[4]], 
    ncol=2, nrow=2, common.legend = TRUE, legend="bottom"
  )
  final_plot
}

twobytwo(lambda_level = 1, simulation_result = simulation_result)
twobytwo(lambda_level = 2, simulation_result = simulation_result)
twobytwo(lambda_level = 3, simulation_result = simulation_result)
twobytwo(lambda_level = 4, simulation_result = simulation_result)
twobytwo(lambda_level = 5, simulation_result = simulation_result)

stock_df = get_polygon_df(tickers)
times = get_stock_avg_time(stock_df)

refresh_stock = get_stock_refresh(stock_df)
info = get_refresh_avg_time(stock_df)

log_stock_df = stock_df
log_stock_df[2:ncol(log_stock_df)] = log(stock_df[2:ncol(stock_df)])

info = get_refresh_avg_time(log_stock_df)

# Save and load
save(data, file="log_forex_list.Rdata")
save(refresh_data, file="log_forex_refresh.RData")

# Save and load
save(data, file="forex_list.Rdata")
save(refresh_data, file="forex_refresh.RData")

# Save and load
save(data, file="100_logforex_list.Rdata")
save(refresh_data, file="100_logforex_refresh.RData")

