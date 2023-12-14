# Dependencies
source("helpfuns.R")

data = get_forex_data()

# Initial data analysis
data = get_forex_data_dfs()
info = get_empirical_data(data)


# Plots
asset = names(forex_dfs)[1]

forex_dfs[[asset]] %>% 
  ggplot(., aes(x=time)) +
  geom_line(aes_string(y=asset)) + 
  geom_vline(xintercept="2023-09-01 00:00:00") + 
  labs(y="Log-Price", 
       title=asset) + 
  theme_minimal()

# SPX
spx = df = read.csv2(file="SPX/DAT_NT_SPXUSD_T_LAST_202309.csv", header=FALSE) %>%
  dplyr::mutate(V1 = as.POSIXct(strptime(V1, format="%Y%m%d %H%M%S"), tz="UTC")) %>%
  dplyr::mutate(V2 = as.numeric(V2)) %>% 
  dplyr::select(V1, V2) %>% 
  magrittr::set_colnames(c("time", "spx"))

as.POSIXct(spx$time, origin="1970-01-01") %>% diff() %>% mean()
as.POSIXct(spx$time, origin="1970-01-01") %>% diff() %>% ifelse(. > 500, 1, .) %>% as.double %>% mean()
rownames(spx) = spx_data$time
spx$time = NULL
get_ms_noise(spx)

# Lobster
ticker = "MSFT"
lobster_data = get_lobster_data(ticker) %>% 
  mutate(time = round_date(time, unit="second")) %>% 
  dplyr::filter(type==4 | type==5) %>% 
  distinct(time, .keep_all=TRUE) %>% 
  mutate(price = log(price)) %>% 
  dplyr::select(time, price)

as.POSIXct(lobster_data$time, origin="1970-01-01") %>% diff() %>% mean()
nrow(lobster_data)
get_ms_noise(lobster_data$price)
((lobster_data[nrow(lobster_data), 2] - lobster_data[1, 2]) / lobster_data[1,2]) * 100

plot(lobster_data$time, lobster_data$price, main=ticker)

# Get stock data
# https://www.investing.com/equities/most-active-stocks
tickers = c("TSLA", "T", "AAPL", "PLTR", "AMD", "BAC", "AMZN", "NVDA", "PFE", "INTC")
stock_df = get_polygon_df(tickers)
times = get_stock_avg_time(stock_df)

refresh_stock = get_stock_refresh(stock_df)
info = get_refresh_avg_time(stock_df)

log_stock_df = stock_df
log_stock_df[2:ncol(log_stock_df)] = log(stock_df[2:ncol(stock_df)])

info = get_refresh_avg_time(log_stock_df)
