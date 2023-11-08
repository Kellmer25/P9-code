# Initial data analysis
data = get_forex_data_dfs()
info = get_empirical_data(data)

diff_data = info[["diff"]]
acf_data = info[["acf"]]

jpy_data = info[["diff"]][["EURJPY"]]

jpy_data %>% filter(diffs > 60) %>% View()
View(jpy_data[7180572:(7180572+2000), ])

ggplot(jpy_data[1:8000, ], aes(x=times, y=diffs)) + 
  geom_line()

# Get data and times
data = get_forex_data()
times = get_avg_time(data) %>% arrange(avg)

refresh_data = refresh_list(data)
info = get_comp_df(data)

test = get_comp_df(data)
matrices = test[["matrices"]]

for (matrix in matrices) {
  matrix %>% diag() %>% norm(type="2") %>% print()
}

# MS EURJPY
tester = matrix(ncol=2) %>% as.data.frame()
for (item in data$EURJPY) {
  item = item %>% 
    as.data.frame() %>% 
    magrittr::set_colnames(c("V2")) %>% 
    mutate(V1 = as.POSIXct(index(item), origin="1970-01-01", tz="utc"), .before=1)
  tester = rbind(tester, item)
}
tester = tester %>% na.omit()
get_ms_noise(tester$V2)

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
  mutate(price = price) %>% 
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

# Save and load
save(data, file="log_forex_list.Rdata")
save(refresh_data, file="log_forex_refresh.RData")
save(test, file="matrices.RData")
load(file="log_forex_refresh.RData")
load(file="log_forex_list.RData")


# Save and load
save(data, file="forex_df.Rdata")
save(refresh_data, file="forex_refresh_df.RData")
