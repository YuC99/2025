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



qwe <- c("CP00", "AP", "APF")
country <- c("PL", "CZ", "DE", "FR")
year <- "2023"
data2 <- data1 %>%
  filter(geo %in% country &
           lubridate::year(TIME_PERIOD) == year &
           coicop %in% qwe) 

summary(data2$values)
mean(data2$values)
median(data2$values)
sd(data2$values)
range(data2$values)

