library(ggplot2)
library(eurostat)
library(lubridate)
library(dplyr)
library(tidyr)
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

des <- data2 %>%
  group_by(geo) %>%
  summarise(
    n = n(),
    mean = mean(values, na.rm = TRUE),
    median = median(values, na.rm = TRUE),
    min = min(values, na.rm = TRUE),
    max = max(values, na.rm = TRUE),
    sd = sd(values, na.rm = TRUE),
    cv = sd/mean
  )
summary(des)

wide <- data2 %>%
  select(TIME_PERIOD,geo,values) %>%
  pivot_wider(names_from = geo, values_from = values)
wide

cor <- wide %>%
  select(-TIME_PERIOD) %>%
  cor(use = "pairwise.complete.obs")
cor

ggplot(data2, aes(x = TIME_PERIOD, y = values, color = geo, group = geo)) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    title = "Monthly inflation in 7 EU countries, 2023",
    x = "Month",
    y = "Inflation rate (y-o-y, %)",
    color = "Country"
  ) +
  theme_minimal()


ggplot(data2, aes(x = geo, y = values)) +
  geom_boxplot() +
  labs(
    title = "Distribution of monthly inflation in 2023",
    x = "Country",
    y = "Inflation rate (y-o-y, %)"
  ) +
  theme_minimal()

cor2 <- as.data.frame(cor) %>%
  mutate(country1 = rownames(cor)) %>%
  pivot_longer(
    cols = -country1,
    names_to = "country2",
    values_to = "corr"
  )

ggplot(cor2, aes(x = country1, y = country2, fill = corr)) +
  geom_tile() +
  scale_fill_gradient2(
    limits = c(-1, 1),
    midpoint = 0
  ) +
  labs(
    title = "Correlation of monthly inflation between countries (2023)",
    x = "Country",
    y = "Country",
    fill = "Correlation"
  ) +
  theme_minimal()

ci <- data2 %>%
  group_by(geo) %>%
  summarise(
    n    = n(),
    mean = mean(values, na.rm = TRUE),
    sd   = sd(values, na.rm = TRUE)
  ) %>%
  mutate(
    se        = sd / sqrt(n),
    t_crit    = qt(0.975, df = n - 1),      
    ci_lower  = mean - t_crit * se,
    ci_upper  = mean + t_crit * se
  )
ci

two_ctr <- c("PL", "DE")
data_two <- data2 %>%
  filter(geo %in% two_ctr)
t_test <- t.test(
  values ~ geo,
  data = data_two,
  var.equal = FALSE   
)
t_test

