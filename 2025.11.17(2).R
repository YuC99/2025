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

CP00_all <- data2 %>%
  filter(coicop == "CP00")
anova_model <- aov(values ~ geo ,data = CP00_all)
summary(anova_model)

ggplot(data2, aes(x = geo, y = values, fill = geo)) +
  geom_boxplot() +
  facet_wrap(~ coicop) +
  labs(title = "HICP inflation in 2023 by country and category", x = "Country", y = "Monthly inflation rate") +
  theme_minimal()

ggplot(data2, aes(x = TIME_PERIOD, y = values, colour = geo)) +
  geom_line() +
  facet_wrap(~ coicop) +
  labs(title = "Monthly HICP inflation in 2023", x = "Month", y = "Inflation rate") +
  theme_minimal()
