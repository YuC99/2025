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



qwe <- c("CP00")
country <- c("PL", "CZ", "DE", "FR", "IT", "ES", "NL")
year <- "2023"
data2 <- data1 %>%
  filter(geo %in% country &
           lubridate::year(TIME_PERIOD) == year &
           coicop %in% qwe) 

#The tendency 
mean(data2$values)
#this table summarises inflation for each country-category pair:number of observations ,mean,median,standard deviation,min and max
desc_table <- data2 %>%
  group_by(geo, coicop) %>%
  summarise(
    n = n(),
    mean = mean(values, na.rm = TRUE),
    median = median(values, na.rm = TRUE),
    sd = sd(values, na.rm = TRUE),
    min = min(values, na.rm = TRUE),
    max = max(values, na.rm = TRUE)
  )
desc_table

CP00_2023 <- data2 %>%
  filter(coicop == "CP00", geo %in% c("PL", "DE"))
t_test_result <- t.test(values ~ geo, data = CP00_2023)
t_test_result

#The variables 
sd(data2$values)
range(data2$values)

#mean, sd, mediana of inf rate in EU (4 countries)         
data_infl <- data %>%
  filter(coicop == "CP00")
mean(CP00_2023$values) 
sd(CP00_2023$values)