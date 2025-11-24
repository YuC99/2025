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
#The mean inflation across countries ranges from 3.43% to 12.14%.
#So, inflation levels were very different among the 7 countries.
#Median values go from 3.3% to 10.7%.Median and mean are similar, which suggests no extreme skew in the data.
#It confirms the difference in inflation levels between countries.
#The minimum monthly inflation ranges from –1.0% to 7.6%.A value of –1% means one country had a month of slightly negative inflation (deflation).
#The maximum inflation goes up to 19.1%, which is very high.
#Standard deviation ranges from 1.15 to 4.04.
#Lower SD means inflation is stable.Higher SD means inflation is more unstable or volatile.
#CV ranges from 0.20 to 0.78.The higher the CV, the more relative volatility compared to its average inflation.
#A CV of 0.20 = very stable inflation.A CV of 0.78 = highly unstable inflation.

wide <- data2 %>%
  select(TIME_PERIOD,geo,values) %>%
  pivot_wider(names_from = geo, values_from = values)
wide
cor <- wide %>%
  select(-TIME_PERIOD) %>%
  cor(use = "pairwise.complete.obs")
cor
#Inflation in Central and Western Europe (Germany, Italy, France, Czech Republic, Poland, Netherlands) moves together, showing strong similarity.
#Spain is the outlier, with a very different inflation pattern and much lower inflation levels.

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
#CZ begins the year at almost 20%.
#PL also starts very high, around 16%.
#Both countries show a clear downward trend during the year.
#Italy starts above 10%,
#but inflation falls sharply to around 0–2% at the end of the year.
#Germany (DE) and France (FR) are stable
#Both countries stay in the range of 4–8%.
#Their inflation lines are smooth and show a moderate, steady decline.
#Spain remains around 1–6% throughout the year.
#It is consistently the lowest inflation country among the group.
#NL shows both high inflation early in the year and negative inflation later.
#Its inflation drops below 0% around September–October.

ggplot(data2, aes(x = geo, y = values)) +
  geom_boxplot() +
  labs(
    title = "Distribution of monthly inflation in 2023",
    x = "Country",
    y = "Inflation rate (y-o-y, %)"
  ) +
  theme_minimal()
#CZ has one of the highest inflation levels (high median).
#The box is tall large variation across months.
#Upper whisker reaches close to 20%.
#Poland also has a high median inflation similar to CZ.
#Wide range from about 7% to 15%.
#Indicates high and unstable inflation.
#Italy (IT)median is in the middle range.
#Whiskers are long inflation changed a lot.
#Italy has strong volatility.
#Germany’s median inflation is moderate.
#The box is narrower more stable inflation than CZ/PL/IT.
#Less spread between months.
#France has a similar pattern to Germany.
#Moderate inflation and limited variation.
#One of the more stable countries.
#Netherlands (NL)very wide box and whiskers high volatility.
#Inflation ranges from negative values to higher mid-year values.
#Shows the most unstable pattern among all countries.
#Spain has the lowest inflation levels overall.
#Very small box inflation is stable and low.
#Only one small outlier.

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
#This heatmap shows how similar the inflation movements were between the seven EU countries in 2023.
#Darker blue means a strong positive correlation (the countries’ inflation moved in the same direction).
#Lighter areas mean a weaker relationship.

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
#Czech Republic (CZ)Mean inflation: 12.1%95% CI: 9.57% to 14.7%
#CZ has the highest average inflation and a wide interval, meaning inflation was high and quite variable.
#Poland (PL)Mean: 11.0%95% CI: 8.49% to 13.5%
#Poland also shows very high inflation with large variability.
#Italy (IT)Mean: 6.02%95% CI: 3.83% to 8.20%
#The interval is wide, showing high volatility, even though the average level is moderate.
#Germany (DE)Mean: 6.11%95% CI: 4.64% to 7.57%
#Germany’s CI is narrower, meaning inflation was more stable.
#France (FR)Mean: 5.68% 95% CI: 4.95% to 6.41%
#France has one of the smallest intervals, indicating very stable inflation.
#Netherlands (NL)Mean: 4.22%95% CI: 2.11% to 6.33%
#The interval is relatively wide because NL had large monthly fluctuations, including negative inflation.
#Spain (ES)Mean: 3.43% (lowest)95% CI: 2.59% to 4.28%
#Spain has both the lowest inflation and one of the narrowest CIs, showing very stable and low inflation.

two_ctr <- c("PL", "DE")
data_two <- data2 %>%
  filter(geo %in% two_ctr)
t_test <- t.test(
  values ~ geo,
  data = data_two,
  var.equal = FALSE   
)
t_test
#Germany had a significantly lower average inflation rate than Poland in 2023 (p = 0.001651).
