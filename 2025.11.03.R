library(ggplot2)
library(eurostat)
library(lubridate)
library(dplyr)
country <- "PL"
year <- "2023"
data1 <- get_eurostat("prc_hicp_manr", time_format = "date", cache = TRUE)
data2 <- data1 %>%
  filter(geo == country &
             lubridate::year(TIME_PERIOD) == year &
           coicop == "CP00") 
