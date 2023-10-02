### Dependencies ---------------------------------------------------------------
library(tidyverse)


### Functions ------------------------------------------------------------------


### Input ----------------------------------------------------------------------


### Main -----------------------------------------------------------------------
message = read_csv("Message/AMZN_2012-06-21_34200000_57600000_message_1.csv", col_names = FALSE) %>%
  as.data.frame() %>%
  magrittr::set_colnames(c("time", "type", "order_id", "size", "price", "direction")) %>%
  dplyr::mutate(time = as.POSIXct(time, origin="2012-06-21", tz="utc"),
                price = round(price / 10000, 2))

message_filter = message  %>%
  dplyr::filter(type %in% c(4, 5))

# Type count
message %>%
  dplyr::group_by(type) %>% 
  dplyr::count()

# Plot
plot(message$time, message$price, type = "l")
plot(message_filter$time, message_filter$price, type = "l")
