# Packages ----------------------------------------------------------------
library(readxl)
library(tidyverse)
library(dplyr)
library(tidyr)
library(zoo)
library(readr)
library(stringr)
library(ggplot2)
library(lubridate)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)
library(forecast)
library(bsts)
library(scales)
library(purrr)
library(systemfit)
library(grid)
library(car)
library(seasonal)
library(tempdisagg)
library(writexl)
library(openxlsx)
# Delta Prices--------------------------------------------
cpi_quartal <- read_excel("HSG BA/R/Daten/USA/CPI.xlsx") %>%
  mutate(
    Index = as.double(Index),
    Quarter = as.Date(Quarter)
  ) %>%
  arrange(Quarter) %>%
  mutate(
    log_index = log(Index),
    delta_p = 400 * (log_index - lag(log_index))
  )

cpi_quartal_filter <- cpi_quartal %>% 
  filter(lubridate::year(Quarter) >= 2010)

ggplot(cpi_quartal_filter, aes(x = Quarter)) +
  geom_line(aes(y = delta_p, color = "CPI"), size = 0.7) +
  labs(
    title = "Figure 1: Quarter-on-quarter annualised inflation, CPI (delta_p)",
    x = "",
    y = "Percent",
    color = "",
    caption = "Source: Bernanke & Blanchard (2023), and own computation"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0, size = 9)
  )

delta_p <- cpi_quartal
# Delta Wage  ------------------------------------------------------------
lohn_quartal <- read_excel("HSG BA/R/Daten/USA/Wages.xlsx") %>%
  mutate(
    wages = as.double(wages),
    Quarter = as.Date(Quarter)
  ) %>%
  arrange(Quarter) %>%
  mutate(
    log_wages = log(wages),
    delta_w = 400 * (log_wages - lag(log_wages))
  )

lohn_quartal_filter <- lohn_quartal %>%
  filter(lubridate::year(Quarter) >= 2020)

ggplot(lohn_quartal_filter, aes(x = Quarter, y = delta_w)) +
  geom_line(color = "steelblue", size = 1) +
  labs(
    title = "Figure 2: q-o-q annualised wage Inflation, ECI (delta_w)",
    x = "Zeit", y = "Percent",
    caption = "Source: Bernanke & Blanchard (2023), and own computation"
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 3)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  theme_minimal()

delta_w <- lohn_quartal
# CPI Energy  -------------------------------------------------------
cpi_energy <- read_excel("HSG BA/R/Daten/USA/CPI_energy.xlsx") %>%
  mutate(
    CPIENGSL = as.double(Index),
    Quarter = as.Date(Quarter)
  )

cpi_energy_filter <- cpi_energy %>%
  filter(lubridate::year(Quarter) >= 2020)

ggplot(cpi_energy_filter, aes(x = Quarter, y = CPIENGSL)) +
  geom_line(color = "darkgreen", size = 1) +
  labs(
    title = "CPI Energy",
    x = "Time", y = "Index",
    caption = "Source: Bernanke & Blanchard (2023)"
  ) +
  theme_minimal()

# CPI Food ----------------------------------------------------------
cpi_food <- read_excel("HSG BA/R/Daten/USA/CPI_food.xlsx") %>%
  mutate(
    CPIUFDSL = as.double(index),
    Quarter = as.Date(Quarter)
  )

cpi_food_filter <- cpi_food %>%
  filter(lubridate::year(Quarter) >= 2020)

ggplot(cpi_food_filter, aes(x = Quarter, y = CPIUFDSL)) +
  geom_line(color = "darkblue", size = 1) +
  labs(
    title = "CPI Food",
    x = "Time", y = "Index",
    caption = "Source: Bernanke & Blanchard (2023)"
  ) +
  theme_minimal()
# RPE (Delta Wage GDP) ----------------------------------------------------
RPE_Data <- delta_w %>%
  select(Quarter, wages) %>%
  left_join(cpi_energy %>% select(Quarter, CPIENGSL), by = "Quarter") %>%
  mutate(
    rpe = as.double(CPIENGSL / wages),
    log_rpe = log(rpe),
    RPE = 400 * (log_rpe - lag(log_rpe))
  ) %>%
  drop_na()

RPE_filter <- RPE_Data %>% 
  filter(lubridate::year(Quarter) >= 2020)
RPE_filter$Quarter <- as.yearqtr(RPE_filter$Quarter)
RPE_filter$Quarter_qtr <- as.yearqtr(RPE_filter$Quarter)

ggplot(RPE_filter, aes(x = Quarter_qtr)) +
  geom_line(aes(y = RPE, color = "Delta RPE"), size = 0.9) +
  labs(
    title = "Figure 3.1: q-o-q annualised growth rate ratio Energy prices to delta_w",
    x = "", y = "", color = "",
    caption = "Notes: Bernanke & Blanchard (2023), and Own computation"
  ) +
  scale_color_manual(values = c("Delta RPE" = "red")) +
  scale_x_yearqtr(format = "%Y Q%q", n = 6) +  
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0, size = 6),
    plot.title = element_text(face = "bold", size = 10)
  )
  

# RPF (Delta Wage GDP) ----------------------------------------------------
RPF_Data <- delta_w %>%
  select(Quarter, wages) %>%
  left_join(cpi_food %>% select(Quarter, CPIUFDSL), by = "Quarter") %>%
  mutate(
    rpf = as.double(CPIUFDSL / wages),
    log_rpf = log(rpf),
    RPF = 400 * (log_rpf - lag(log_rpf))
  ) %>%
  drop_na()

RPF_filter <- RPF_Data %>% 
  filter(lubridate::year(Quarter) >= 2020)
RPF_filter$Quarter_qtr <- as.yearqtr(RPF_filter$Quarter)

ggplot(RPF_filter, aes(x = Quarter_qtr)) +
  geom_line(aes(y = RPF, color = "Delta RPF"), size = 0.9) +
  labs(
    title = "Figure 3.2: QoQ annualised growth rate ratio Food prices to delta_w",
    x = "", y = "", color = "",
    caption = "Notes: Bernanke & Blanchard (2023), and Own computation"
  ) +
  scale_color_manual(values = c("Delta RPF" = "darkgreen")) +
  scale_x_yearqtr(format = "%Y Q%q", n = 6) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0, size = 6),
    plot.title = element_text(face = "bold", size = 10)
  )


# Shortages (Google Trends) -------------------------------------------
shortages <- read_excel("HSG BA/R/Daten/USA/Shortage.xlsx")
shortages$Quarter_qtr <- as.yearqtr(shortages$Quarter)

ggplot(shortages, aes(x = Quarter_qtr, y = shortage)) +
  geom_line(color = "darkorange", size = 1) +
  labs(
    title = "Shortages Index Over Time (Google-Trend Data)",
    x = "", y = "Shortages Index",
    caption = "Source: Bernanke & Blanchard (2023)"
  ) +
  scale_x_yearqtr(format = "%Y Q%q", n = 6) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 13) +
  theme(
    plot.caption = element_text(hjust = 0, size = 6),
    plot.title = element_text(face = "bold", size = 10)
  )

# VU ----------------------------------------------------------------------
VU <- read_excel("HSG BA/R/Daten/USA/VU.xlsx")

VU$Quarter_qtr <- as.yearqtr(VU$Quarter)
ref_value <- VU %>%
  filter(Quarter == as.Date("2019-01-01")) %>%
  pull(VU)
VU_filter <- VU %>%
  filter(Quarter >= as.Date("2019-01-01")) %>%
  mutate(
    VU_indexed = VU / ref_value
  )

ggplot(VU_filter, aes(x = Quarter_qtr, y = VU_indexed)) +
  geom_line(color = "steelblue", size = 1) +
  labs(
    title = "Index V/U-Ratio (2019 Q1, Base = 1)",
    x = "", y = "Index (2019 Q1 = 1)",
    caption = "Quelle: Bernanke & Blanchard (2023), and Own computation"
  ) +
  scale_x_yearqtr(format = "%Y Q%q", n = 6) +
  scale_y_continuous(limits = c(0, 2), breaks = seq(0, 2, by = 1)) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 13) +
  theme(
    plot.caption = element_text(hjust = 0, size = 6),
    plot.title = element_text(face = "bold", size = 10)
  )

# Productivity  -----------------------------------------------------------
productivity <- read_excel("HSG BA/R/Daten/USA/mapty.xlsx")

productivity <- productivity %>%
  mutate(
    Index = as.double(Index),
    Quarter = as.Date(Quarter),
    gpty = 400 * (log(Index) - log(lag(Index))),
    mapty = 0.125 * rowSums(sapply(0:7, function(l) lag(gpty, l)))
  ) %>%
  drop_na()
productivity$Quarter_qtr <- as.yearqtr(productivity$Quarter)

productivity_filter <- productivity %>% 
filter(lubridate::year(Quarter) >= 2019)

ggplot(productivity_filter, aes(x = Quarter_qtr, y =mapty)) +
  geom_line(color = "darkgreen", size = 1) +
  labs(
    title = "Produtivity Growth (Moving Average)",
    x = "", y = "Percent",
    caption = "Quelle: Bernanke & Blanchard (2023), and Own computation"
  ) +
  scale_x_yearqtr(format = "%Y Q%q", n = 6) +
  theme_minimal(base_size = 13) +
  theme(
    plot.caption = element_text(hjust = 0, size = 6),
    plot.title = element_text(face = "bold", size = 10)
  )

# Inflation Expectations (short term) -------------------------------------
cf1_exp <- read_excel("HSG BA/R/Daten/USA/cf1_exp.xlsx")
cf1_exp$Quarter <- as.Date(as.character(cf1_exp$Quarter))


cf1_exp_filter <- cf1_exp %>% 
  filter(year(Quarter) >= 2020)


ggplot(cf1_exp_filter, aes(x = Quarter, y = Expectations)) +
  geom_line(color = "royalblue", size = 1) +
  labs(
    title = "Short-term Inflation Expectations (CF1)",
    x = "", y = "in %",
    caption = "Quelle: Bernanke & Blanchard (2023)"
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 13) +
  theme(
    plot.caption = element_text(hjust = 0, size = 6),
    plot.title = element_text(face = "bold", size = 10)
  )

exp_short <- cf1_exp %>% 
  rename(CF1 = Expectations)
# Inflation Expectations (long term) --------------------------------------
cf10_exp <- read_excel("HSG BA/R/Daten/USA/cf10_exp.xlsx")
cf10_exp$Quarter <- as.Date(as.character(cf10_exp$Quarter))

cf10_exp_filter <- cf10_exp %>% 
  filter(year(Quarter) >= 2020)

ggplot(cf10_exp_filter, aes(x = Quarter, y = cf10_inf)) +
  geom_line(color = "firebrick", size = 1) +
  labs(
    title = "long-term Inflation expectations (CF10)",
    x = "", y = "in %",
    caption = "Quelle: Bernanke & Blanchard (2023)"
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 13) +
  theme(
    plot.caption = element_text(hjust = 0, size = 6),
    plot.title = element_text(face = "bold", size = 10)
  )

long_term <- cf10_exp %>% 
  rename(CF5 = cf10_inf)

# Catch-up Effect ---------------------------------------------------------
catchup_data <- cpi_quartal %>%
  left_join(exp_short, by = "Quarter") %>%
  mutate(
    expected_lagged = lag(CF1, 4),
    delta_p_avg = rollmean(delta_p, k = 4, align = "right", fill = NA),
    CU = delta_p_avg - expected_lagged  
  )

CU <- catchup_data


catchup_data$Quarter <- as.Date(catchup_data$Quarter)
ggplot(catchup_data %>% filter(Quarter >= as.Date("1990-01-01")),
       aes(x = Quarter, y = CU)) +
  geom_line(color = "firebrick", size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "Catch-up effect ",
    x = "Quartal",
    y = "Catch-up in %-Points",
    caption = "Bernanke & Blanchard (2023), and Own computation"
  ) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  theme_minimal(base_size = 14) +
  theme(
    plot.caption = element_text(hjust = 0, size = 8),
    plot.title = element_text(face = "bold", size = 13)
  )
# Sample Size  ------------------------------------------------------------

start_date <- as.Date("1989-01-01")
end_date <- as.Date("2023-01-01")

delta_w <- delta_w %>% filter(Quarter >= start_date & Quarter <= end_date)
delta_p <- delta_p %>% filter(Quarter >= start_date & Quarter <= end_date)
RPE_Data <- RPE_Data %>% filter(Quarter >= start_date & Quarter <= end_date)
RPF_Data <- RPF_Data %>% filter(Quarter >= start_date & Quarter <= end_date)
shortages <- shortages %>% filter(Quarter >= start_date & Quarter <= end_date)
VU <- VU %>% filter(Quarter >= start_date & Quarter <= end_date)
productivity <- productivity %>% filter(Quarter >= start_date & Quarter <= end_date)
long_term <- long_term %>% filter(Quarter >= start_date & Quarter <= end_date)
exp_short <- exp_short %>% filter(Quarter >= start_date & Quarter <= end_date)
CU <- CU %>% filter(Quarter >= start_date & Quarter <= end_date)

  


# Wage Equation Data-----------------------------------------------------------
start_date <- as.Date("1989-01-01")
end_date <- as.Date("2019-12-31")  

Wage_equation <- delta_w %>%
  select(Quarter, delta_w) %>%
  left_join(VU %>% select(Quarter, VU), by = "Quarter") %>%
  left_join(CU %>% select(Quarter, CU), by = "Quarter") %>%
  left_join(exp_short %>% select(Quarter, CF1), by = "Quarter") %>%
  left_join(productivity %>% select(Quarter, mapty), by = "Quarter") %>%
  filter(Quarter >= start_date & Quarter <= end_date)

# Price Equation Data -----------------------------------------------------
start_date <- as.Date("1989-01-01")
end_date <- as.Date("2023-01-01")  

price_equation <- delta_p %>%
  select(Quarter, delta_p) %>%
  left_join(delta_w %>% select(Quarter, delta_w), by = "Quarter") %>%
  left_join(RPE_Data %>% select(Quarter, RPE), by = "Quarter") %>%
  left_join(RPF_Data %>% select(Quarter, RPF), by = "Quarter") %>%
  left_join(shortages %>% select(Quarter, shortage), by = "Quarter") %>%
  left_join(productivity %>% select(Quarter, mapty), by = "Quarter") %>%
  filter(Quarter >= start_date & Quarter <= end_date)
# CF1 Equation Data -------------------------------------------------------
start_date <- as.Date("1989-01-01")
end_date <- as.Date("2019-10-01")

cf1_equation <- exp_short %>%
  select(Quarter, CF1) %>%
  left_join(long_term %>% select(Quarter, CF5), by = "Quarter") %>%
  left_join(delta_p %>% select(Quarter, delta_p), by = "Quarter") %>%
  filter(Quarter >= start_date & Quarter <= end_date)

# CF5 Equation Data -------------------------------------------------------
start_date <- as.Date("1989-01-01")
end_date <- as.Date("2019-10-01")  

cf5_equation <- long_term %>%
  select(Quarter, CF5) %>%
  left_join(delta_p %>% select(Quarter, delta_p), by = "Quarter") %>%
  filter(Quarter >= start_date & Quarter <= end_date)
# Wage Equation Regression -----------------
wage_equation_data <- Wage_equation %>%
  select(Quarter, delta_w, CF1, CU, VU, mapty) %>% 
  arrange(Quarter) %>%
  mutate(
    delta_w_lag1 = lag(delta_w, 1),
    delta_w_lag2 = lag(delta_w, 2),
    delta_w_lag3 = lag(delta_w, 3),
    delta_w_lag4 = lag(delta_w, 4),
    
    CF1_lag1 = lag(CF1, 1),
    CF1_lag2 = lag(CF1, 2),
    CF1_lag3 = lag(CF1, 3),
    CF1_lag4 = lag(CF1, 4),
    
    CU_lag1 = lag(CU, 1),
    CU_lag2 = lag(CU, 2),
    CU_lag3 = lag(CU, 3),
    CU_lag4 = lag(CU, 4),
    
    VU_lag1 = lag(VU, 1),
    VU_lag2 = lag(VU, 2),
    VU_lag3 = lag(VU, 3),
    VU_lag4 = lag(VU, 4),
    
    mapty_lag1 = lag(mapty, 1)
  ) %>%
  drop_na()

# Regressionsformel
formel_eq <- delta_w ~ delta_w_lag1 + delta_w_lag2 + delta_w_lag3 + delta_w_lag4 +
  CF1_lag1 + CF1_lag2 + CF1_lag3 + CF1_lag4 +
  CU_lag1 + CU_lag2 + CU_lag3 + CU_lag4 +
  VU_lag1 + VU_lag2 + VU_lag3 + VU_lag4 +
  mapty_lag1 

# Restriktion: Summe der wage_delta_w_lag[1:4] + wage_CF1_lag[1:4] = 1
restriktion <- c(
  "wage_delta_w_lag1 + wage_delta_w_lag2 + wage_delta_w_lag3 + wage_delta_w_lag4 + 
   wage_CF1_lag1 + wage_CF1_lag2 + wage_CF1_lag3 + wage_CF1_lag4 = 1"
)

# Modell mit Restriktion schätzen
wage_model <- systemfit(
  formula = list(wage = formel_eq),
  method = "OLS",
  data = wage_equation_data,
  restrict.matrix = restriktion
)

# Ausgabe
summary(wage_model)

#p_sum
linearHypothesis(wage_model, "wage_delta_w_lag1 + wage_delta_w_lag2 + wage_delta_w_lag3 + wage_delta_w_lag4 = 0")
linearHypothesis(wage_model, "wage_CF1_lag1 + wage_CF1_lag2 + wage_CF1_lag3 + wage_CF1_lag4 = 0")
linearHypothesis(wage_model, "wage_CU_lag1 + wage_CU_lag2 + wage_CU_lag3 + wage_CU_lag4 = 0")
linearHypothesis(wage_model, "wage_VU_lag1 + wage_VU_lag2 + wage_VU_lag3 + wage_VU_lag4 = 0")

#p_join
linearHypothesis(
  wage_model,
  c(
    "wage_delta_w_lag1 = 0",
    "wage_delta_w_lag2 = 0",
    "wage_delta_w_lag3 = 0",
    "wage_delta_w_lag4 = 0"
  )
)

linearHypothesis(
  wage_model,
  c(
    "wage_CF1_lag1 = 0",
    "wage_CF1_lag2 = 0",
    "wage_CF1_lag3 = 0",
    "wage_CF1_lag4 = 0"
  )
)

linearHypothesis(
  wage_model,
  c(
    "wage_CU_lag1 = 0",
    "wage_CU_lag2 = 0",
    "wage_CU_lag3 = 0",
    "wage_CU_lag4 = 0"
  )
)

linearHypothesis(
  wage_model,
  c(
    "wage_VU_lag1 = 0",
    "wage_VU_lag2 = 0",
    "wage_VU_lag3 = 0",
    "wage_VU_lag4 = 0"
  )
)

# Koeffizienten extrahieren
coefs_wage <- coef(wage_model)


# Summe der Lags pro Block berechnen
sum_delta_w <- sum(coefs_wage[c(
  "wage_delta_w_lag1", "wage_delta_w_lag2", 
  "wage_delta_w_lag3", "wage_delta_w_lag4"
)])

sum_CF1 <- sum(coefs_wage[c(
  "wage_CF1_lag1", "wage_CF1_lag2", 
  "wage_CF1_lag3", "wage_CF1_lag4"
)])

sum_CU <- sum(coefs_wage[c(
  "wage_CU_lag1", "wage_CU_lag2", 
  "wage_CU_lag3", "wage_CU_lag4"
)])

sum_VU <- sum(coefs_wage[c(
  "wage_VU_lag1", "wage_VU_lag2", 
  "wage_VU_lag3", "wage_VU_lag4"
)])

# mapty hat nur 1 Lag
mapty <- coefs_wage["wage_mapty_lag1"]

# Ausgabe
cat("∑ Δwₜ Lags:     ", round(sum_delta_w, 4), "\n")
cat("∑ CF1 Lags (Erwartungen):     ", round(sum_CF1, 4), "\n")
cat("∑ CU Lags (Catch-up):         ", round(sum_CU, 4), "\n")
cat("∑ VU Lags (Tightness):        ", round(sum_VU, 4), "\n")
 cat("maptyₜ₋₁ (Produktivität):      ", round(mapty, 4), "\n)")

# Price Equation Regression ----------------------------------
price_equation_data <- price_equation %>%
  select(Quarter, delta_p, delta_w, shortage, RPE, RPF, mapty) %>%
  arrange(Quarter) %>%
  mutate(
    
    delta_p_lag1 = lag(delta_p, 1),
    delta_p_lag2 = lag(delta_p, 2),
    delta_p_lag3 = lag(delta_p, 3),
    delta_p_lag4 = lag(delta_p, 4),
    
    delta_w_lag0 = delta_w,
    delta_w_lag1 = lag(delta_w, 1),
    delta_w_lag2 = lag(delta_w, 2),
    delta_w_lag3 = lag(delta_w, 3),
    delta_w_lag4 = lag(delta_w, 4),
    
    shortage_lag0 = shortage,
    shortage_lag1 = lag(shortage, 1),
    shortage_lag2 = lag(shortage, 2),
    shortage_lag3 = lag(shortage, 3),
    shortage_lag4 = lag(shortage, 4),
    
    RPE_lag0 = RPE,
    RPE_lag1 = lag(RPE, 1),
    RPE_lag2 = lag(RPE, 2),
    RPE_lag3 = lag(RPE, 3),
    RPE_lag4 = lag(RPE, 4),
    
    RPF_lag0 = RPF,
    RPF_lag1 = lag(RPF, 1),
    RPF_lag2 = lag(RPF, 2),
    RPF_lag3 = lag(RPF, 3),
    RPF_lag4 = lag(RPF, 4),
    
    mapty_lag1 = lag(mapty, 1)
  ) %>%
  drop_na()

formel_price <- delta_p ~ 
  delta_p_lag1 + delta_p_lag2 + delta_p_lag3 + delta_p_lag4 +
  delta_w_lag0 + delta_w_lag1 + delta_w_lag2 + delta_w_lag3 + delta_w_lag4 +
  shortage_lag0 + shortage_lag1 + shortage_lag2 + shortage_lag3 + shortage_lag4 +
  RPE_lag0 + RPE_lag1 + RPE_lag2 + RPE_lag3 + RPE_lag4 +
  RPF_lag0 + RPF_lag1 + RPF_lag2 + RPF_lag3 + RPF_lag4 +
  mapty_lag1

restriktion_price <- c(
  "price_delta_p_lag1 + price_delta_p_lag2 + price_delta_p_lag3 + price_delta_p_lag4 + 
   price_delta_w_lag0 + price_delta_w_lag1 + price_delta_w_lag2 + price_delta_w_lag3 + price_delta_w_lag4 = 1"
)

price_model <- systemfit(
  formula = list(price = formel_price),
  method = "SUR",
  data = price_equation_data,
  restrict.matrix = restriktion_price
)

summary(price_model)

# Koeffizienten extrahieren
coefs_price <- coef(price_model)

# Summen berechnen
sum_delta_p <- sum(coefs_price[c(
  "price_delta_p_lag1", "price_delta_p_lag2",
  "price_delta_p_lag3", "price_delta_p_lag4"
)])

sum_delta_w <- sum(coefs_price[c(
  "price_delta_w_lag0", "price_delta_w_lag1", 
  "price_delta_w_lag2", "price_delta_w_lag3", 
  "price_delta_w_lag4"
)])

sum_shortage <- sum(coefs_price[c(
  "price_shortage_lag0", "price_shortage_lag1", 
  "price_shortage_lag2", "price_shortage_lag3", 
  "price_shortage_lag4"
)])

sum_RPE <- sum(coefs_price[c(
  "price_RPE_lag0", "price_RPE_lag1", 
  "price_RPE_lag2", "price_RPE_lag3", 
  "price_RPE_lag4"
)])

sum_RPF <- sum(coefs_price[c(
  "price_RPF_lag0", "price_RPF_lag1", 
  "price_RPF_lag2", "price_RPF_lag3", 
  "price_RPF_lag4"
)])

mapty <- coefs_price["price_mapty_lag1"]

# Ausgabe
cat("∑ Δpₜ Lags:                  ", round(sum_delta_p, 4), "\n")
cat("∑ Δwₜ Lags:                  ", round(sum_delta_w, 4), "\n")
cat("∑ Shortage Lags:            ", round(sum_shortage, 4), "\n")
cat("∑ RPE Lags:             ", round(sum_RPE, 4), "\n")
cat("∑ RPF Lags:             ", round(sum_RPF, 4), "\n")
cat("maptyₜ₋₁ (Produktivität):     ", round(mapty, 4), "\n")

#p_sum
linearHypothesis(price_model, "price_delta_w_lag0+price_delta_w_lag1 + price_delta_w_lag2 + price_delta_w_lag3 + price_delta_w_lag4 = 0")
linearHypothesis(price_model, "price_delta_p_lag1 + price_delta_p_lag2 + price_delta_p_lag3 + price_delta_p_lag4 = 0")
linearHypothesis(price_model, "price_shortage_lag0 + price_shortage_lag1 + price_shortage_lag2 + price_shortage_lag3 + price_shortage_lag4 = 0")
linearHypothesis(price_model, "price_RPE_lag0 + price_RPE_lag1 + price_RPE_lag2 + price_RPE_lag3 + price_RPE_lag4 = 0")
linearHypothesis(price_model, "price_RPF_lag0 + price_RPF_lag1 + price_RPF_lag2 + price_RPF_lag3 + price_RPF_lag4 = 0")

#p_joint
linearHypothesis(price_model, c(
  "price_delta_w_lag0 = 0",
  "price_delta_w_lag1 = 0",
  "price_delta_w_lag2 = 0",
  "price_delta_w_lag3 = 0",
  "price_delta_w_lag4 = 0"
))

linearHypothesis(price_model, c(
  "price_delta_p_lag1 = 0",
  "price_delta_p_lag2 = 0",
  "price_delta_p_lag3 = 0",
  "price_delta_p_lag4 = 0"
))

linearHypothesis(price_model, c(
  "price_shortage_lag0 = 0",
  "price_shortage_lag1 = 0",
  "price_shortage_lag2 = 0",
  "price_shortage_lag3 = 0",
  "price_shortage_lag4 = 0"
))

linearHypothesis(price_model, c(
  "price_RPE_lag0 = 0",
  "price_RPE_lag1 = 0",
  "price_RPE_lag2 = 0",
  "price_RPE_lag3 = 0",
  "price_RPE_lag4 = 0"
))

linearHypothesis(price_model, c(
  "price_RPF_lag0 = 0",
  "price_RPF_lag1 = 0",
  "price_RPF_lag2 = 0",
  "price_RPF_lag3 = 0",
  "price_RPF_lag4 = 0"
))



# (Short-term) Inflation Equation Regression --------------------------
Short_Infl_data <- cf1_equation %>%
  select(Quarter, CF1, CF5, delta_p) %>%
  arrange(Quarter) %>%
  mutate(
    
    CF1_lag1 = lag(CF1, 1),
    CF1_lag2 = lag(CF1, 2),
    CF1_lag3 = lag(CF1, 3),
    CF1_lag4 = lag(CF1, 4),
    
    CF5_lag0 = CF5,
    CF5_lag1 = lag(CF5, 1),
    CF5_lag2 = lag(CF5, 2),
    CF5_lag3 = lag(CF5, 3),
    CF5_lag4 = lag(CF5, 4),
    
    delta_p_lag0 = delta_p,
    delta_p_lag1 = lag(delta_p, 1),
    delta_p_lag2 = lag(delta_p, 2),
    delta_p_lag3 = lag(delta_p, 3),
    delta_p_lag4 = lag(delta_p, 4)
  ) %>%
  drop_na()

formel_cf1 <- CF1 ~ 
  CF1_lag1 + CF1_lag2 + CF1_lag3 + CF1_lag4 +
  CF5_lag0 + CF5_lag1 + CF5_lag2 + CF5_lag3 + CF5_lag4 +
  delta_p_lag0 + delta_p_lag1 + delta_p_lag2 + delta_p_lag3 + delta_p_lag4 - 1

restriktion_cf1 <- c(
  "cf1_CF1_lag1 + cf1_CF1_lag2 + cf1_CF1_lag3 + cf1_CF1_lag4 + 
   cf1_CF5_lag0 + cf1_CF5_lag1 + cf1_CF5_lag2 + cf1_CF5_lag3 + cf1_CF5_lag4 +
   cf1_delta_p_lag0 + cf1_delta_p_lag1 + cf1_delta_p_lag2 + cf1_delta_p_lag3 + cf1_delta_p_lag4 = 1"
)

cf1_model <- systemfit(
  formula = list(cf1 = formel_cf1),
  method = "OLS",
  data = Short_Infl_data,
  restrict.matrix = restriktion_cf1
)

summary(cf1_model)

coefs_cf1 <- coef(cf1_model)

sum_CF1 <- sum(coefs_cf1[c(
  "cf1_CF1_lag1", "cf1_CF1_lag2", "cf1_CF1_lag3", "cf1_CF1_lag4"
)])

sum_CF5 <- sum(coefs_cf1[c(
  "cf1_CF5_lag0", "cf1_CF5_lag1", "cf1_CF5_lag2", "cf1_CF5_lag3", "cf1_CF5_lag4"
)])

sum_deltap <- sum(coefs_cf1[c(
  "cf1_delta_p_lag0", "cf1_delta_p_lag1", "cf1_delta_p_lag2", "cf1_delta_p_lag3", "cf1_delta_p_lag4"
)])

# Ausgabe
cat("∑ CF1 Lags (1–4):                  ", round(sum_CF1, 4), "\n")
cat("∑ CF5 Lags (0–4):                  ", round(sum_CF5, 4), "\n")
cat("∑ delta_p Lags (0–4):              ", round(sum_deltap, 4), "\n")

#p_sum
linearHypothesis(cf1_model, "cf1_CF1_lag1 + cf1_CF1_lag2 + cf1_CF1_lag3 + cf1_CF1_lag4 = 0")

linearHypothesis(cf1_model, "cf1_CF5_lag0 + cf1_CF5_lag1 + cf1_CF5_lag2 + cf1_CF5_lag3 + cf1_CF5_lag4 = 0")

linearHypothesis(cf1_model, "cf1_delta_p_lag0 + cf1_delta_p_lag1 + cf1_delta_p_lag2 + cf1_delta_p_lag3 + cf1_delta_p_lag4 = 0")

#p_joint

linearHypothesis(cf1_model, c(
  "cf1_CF1_lag1 = 0",
  "cf1_CF1_lag2 = 0",
  "cf1_CF1_lag3 = 0",
  "cf1_CF1_lag4 = 0"
))

linearHypothesis(cf1_model, c(
  "cf1_CF5_lag0 = 0",
  "cf1_CF5_lag1 = 0",
  "cf1_CF5_lag2 = 0",
  "cf1_CF5_lag3 = 0",
  "cf1_CF5_lag4 = 0"
))

linearHypothesis(cf1_model, c(
  "cf1_delta_p_lag0 = 0",
  "cf1_delta_p_lag1 = 0",
  "cf1_delta_p_lag2 = 0",
  "cf1_delta_p_lag3 = 0",
  "cf1_delta_p_lag4 = 0"
))


# (Long-term) Inflation Equation Regression -------------------------------
Long_Infl_data <- cf5_equation %>%
  select(Quarter, CF5, delta_p) %>%
  arrange(Quarter) %>%
  mutate(
    
    CF5_lag1 = lag(CF5, 1),
    CF5_lag2 = lag(CF5, 2),
    CF5_lag3 = lag(CF5, 3),
    CF5_lag4 = lag(CF5, 4),
    
    delta_p_lag0 = delta_p,
    delta_p_lag1 = lag(delta_p, 1),
    delta_p_lag2 = lag(delta_p, 2),
    delta_p_lag3 = lag(delta_p, 3),
    delta_p_lag4 = lag(delta_p, 4),
  ) %>%
  filter(Quarter <= as.Date("2023-07-31")) %>%
  drop_na()

formel_cf5 <- CF5 ~ 
  CF5_lag1 + CF5_lag2 + CF5_lag3 + CF5_lag4 +
  delta_p_lag0 + delta_p_lag1 + delta_p_lag2 + delta_p_lag3 + delta_p_lag4  - 1  

restriktion_cf5 <- c(
  "cf5_CF5_lag1 + cf5_CF5_lag2 + cf5_CF5_lag3 + cf5_CF5_lag4 + 
   cf5_delta_p_lag0 + cf5_delta_p_lag1 + cf5_delta_p_lag2 + 
   cf5_delta_p_lag3 + cf5_delta_p_lag4  = 1"
)

cf5_model <- systemfit(
  formula = list(cf5 = formel_cf5),
  method = "OLS",
  data = Long_Infl_data,
  restrict.matrix = restriktion_cf5
)

summary(cf5_model)

coefs_cf5 <- coef(cf5_model)

# Summen berechnen
sum_CF5_lags <- sum(coefs_cf5[c(
  "cf5_CF5_lag1", "cf5_CF5_lag2", "cf5_CF5_lag3", "cf5_CF5_lag4"
)])

sum_delta_p_lags <- sum(coefs_cf5[c(
  "cf5_delta_p_lag0", "cf5_delta_p_lag1", "cf5_delta_p_lag2",
  "cf5_delta_p_lag3", "cf5_delta_p_lag4"
)])

# Ausgabe
cat("∑ CF5 Lags (1–4):                 ", round(sum_CF5_lags, 4), "\n")
cat("∑ delta_p Lags (0–4):             ", round(sum_delta_p_lags, 4), "\n")

#p_sum
linearHypothesis(cf5_model, "cf5_CF5_lag1 + cf5_CF5_lag2 + cf5_CF5_lag3 + cf5_CF5_lag4 = 0")

linearHypothesis(cf5_model, "cf5_delta_p_lag0 + cf5_delta_p_lag1 + cf5_delta_p_lag2 + cf5_delta_p_lag3 + cf5_delta_p_lag4 = 0")

#p_joint
linearHypothesis(cf5_model, c(
  "cf5_CF5_lag1 = 0",
  "cf5_CF5_lag2 = 0",
  "cf5_CF5_lag3 = 0",
  "cf5_CF5_lag4 = 0"
))

linearHypothesis(cf5_model, c(
  "cf5_delta_p_lag0 = 0",
  "cf5_delta_p_lag1 = 0",
  "cf5_delta_p_lag2 = 0",
  "cf5_delta_p_lag3 = 0",
  "cf5_delta_p_lag4 = 0"
))




# Wage Equation Estimated vs. Actual Plot  --------------------------------

Wage_equation_estimated_values <- delta_w %>%
  select(Quarter, delta_w) %>%
  left_join(exp_short %>% select(Quarter, CF1), by = "Quarter") %>%
  left_join(VU %>% select(Quarter, VU), by = "Quarter") %>%
  left_join(CU %>% select(Quarter, CU), by = "Quarter") %>%
  left_join(productivity %>% select(Quarter, mapty), by = "Quarter") %>%
  arrange(Quarter)

Wage_equation_estimated_values <- Wage_equation_estimated_values %>%
  mutate(
    delta_w_lag1 = lag(delta_w, 1),
    delta_w_lag2 = lag(delta_w, 2),
    delta_w_lag3 = lag(delta_w, 3),
    delta_w_lag4 = lag(delta_w, 4),
    
    CF1_lag1 = lag(CF1, 1),
    CF1_lag2 = lag(CF1, 2),
    CF1_lag3 = lag(CF1, 3),
    CF1_lag4 = lag(CF1, 4),
    
    VU_lag1 = lag(VU, 1),
    VU_lag2 = lag(VU, 2),
    VU_lag3 = lag(VU, 3),
    VU_lag4 = lag(VU, 4),
    
    CU_lag1 = lag(CU, 1),
    CU_lag2 = lag(CU, 2),
    CU_lag3 = lag(CU, 3),
    CU_lag4 = lag(CU, 4)
  ) %>%
  filter(Quarter >= as.Date("2018-01-01") & Quarter <= as.Date("2023-12-31"))

Wage_equation_estimated_values <- Wage_equation_estimated_values %>%
  mutate(
    delta_w_sim =
      coefs_wage["wage_(Intercept)"] +
      
      coefs_wage["wage_delta_w_lag1"] * delta_w_lag1 +
      coefs_wage["wage_delta_w_lag2"] * delta_w_lag2 +
      coefs_wage["wage_delta_w_lag3"] * delta_w_lag3 +
      coefs_wage["wage_delta_w_lag4"] * delta_w_lag4 +
      
      coefs_wage["wage_CF1_lag1"] * CF1_lag1 +
      coefs_wage["wage_CF1_lag2"] * CF1_lag2 +
      coefs_wage["wage_CF1_lag3"] * CF1_lag3 +
      coefs_wage["wage_CF1_lag4"] * CF1_lag4 +
      
      coefs_wage["wage_CU_lag1"] * CU_lag1 +
      coefs_wage["wage_CU_lag2"] * CU_lag2 +
      coefs_wage["wage_CU_lag3"] * CU_lag3 +
      coefs_wage["wage_CU_lag4"] * CU_lag4 +
      
      coefs_wage["wage_VU_lag1"] * VU_lag1 +
      coefs_wage["wage_VU_lag2"] * VU_lag2 +
      coefs_wage["wage_VU_lag3"] * VU_lag3 +
      coefs_wage["wage_VU_lag4"] * VU_lag4 
    
  )

plot_data <- Wage_equation_estimated_values %>%
  select(Quarter, delta_w, delta_w_sim) %>%
  filter(Quarter >= as.Date("2020-01-01")) %>%
  pivot_longer(cols = c(delta_w, delta_w_sim),
               names_to = "Variable", values_to = "Value")

ggplot(plot_data, aes(x = Quarter, y = Value, color = Variable)) +
  geom_line(size = 1) +
  labs(
    title = "realised vs. simulated delta_w; Q1 2020 - Q1 2023",
    x = "", y = " in % ",
    color = "Variable",
    caption = "Quelle: Bernanke & Blanchard (2023), and Own computation"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Price Equation Estimated vs. Actual Plot --------------------------------
price_equation_estimation <- price_equation_data %>%
  mutate(
    delta_p_sim =
      coefs_price["price_(Intercept)"] +
      
      coefs_price["price_delta_p_lag1"] * delta_p_lag1 +
      coefs_price["price_delta_p_lag2"] * delta_p_lag2 +
      coefs_price["price_delta_p_lag3"] * delta_p_lag3 +
      coefs_price["price_delta_p_lag4"] * delta_p_lag4 +
      
      coefs_price["price_delta_w_lag0"] * delta_w_lag0 +
      coefs_price["price_delta_w_lag1"] * delta_w_lag1 +
      coefs_price["price_delta_w_lag2"] * delta_w_lag2 +
      coefs_price["price_delta_w_lag3"] * delta_w_lag3 +
      coefs_price["price_delta_w_lag4"] * delta_w_lag4 +
      
      coefs_price["price_RPE_lag0"] * RPE_lag0 +
      coefs_price["price_RPE_lag1"] * RPE_lag1 +
      coefs_price["price_RPE_lag2"] * RPE_lag2 +
      coefs_price["price_RPE_lag3"] * RPE_lag3 +
      coefs_price["price_RPE_lag4"] * RPE_lag4 +
      
      coefs_price["price_RPF_lag0"] * RPF_lag0 +
      coefs_price["price_RPF_lag1"] * RPF_lag1 +
      coefs_price["price_RPF_lag2"] * RPF_lag2 +
      coefs_price["price_RPF_lag3"] * RPF_lag3 +
      coefs_price["price_RPF_lag4"] * RPF_lag4 +
      
      coefs_price["price_shortage_lag0"] * shortage_lag0 +
      coefs_price["price_shortage_lag1"] * shortage_lag1 +
      coefs_price["price_shortage_lag2"] * shortage_lag2 +
      coefs_price["price_shortage_lag3"] * shortage_lag3 +
      coefs_price["price_shortage_lag4"] * shortage_lag4 +
      
      coefs_price["price_mapty_lag1"] * mapty_lag1
  )

plot_data <- price_equation_estimation %>%
  filter(Quarter >= as.Date("2020-01-01")) %>%
  select(Quarter, delta_p, delta_p_sim) %>%
  pivot_longer(cols = c(delta_p, delta_p_sim), names_to = "Variable", values_to = "Value")

ggplot(plot_data, aes(x = Quarter, y = Value, color = Variable, linetype = Variable)) +
  geom_line(size = 1.2) +
  labs(
    title = "realised vs. simulated delta_p; Q1 2020 - Q1 2023",
    x = "Quartal",
    y = "in %",
    color = "Variable",
    linetype = "Variable"
  ) +
  scale_color_manual(values = c("delta_p" = "black", "delta_p_sim" = "red")) +
  scale_linetype_manual(values = c("delta_p" = "solid", "delta_p_sim" = "dashed")) +
  theme_minimal(base_size = 14)

# (Short term) Estimated vs. Actual Plot ----------------------------------
Short_Infl_estimation <- exp_short %>%
  select(Quarter, CF1) %>%
  left_join(long_term %>% select(Quarter, CF5), by = "Quarter") %>%
  left_join(delta_p %>% select(Quarter, delta_p), by = "Quarter")

Short_Infl_estimation <- Short_Infl_estimation %>%
  select(Quarter, CF1, CF5, delta_p) %>%
  arrange(Quarter) %>%
  mutate(
    
    CF1_lag1 = lag(CF1, 1),
    CF1_lag2 = lag(CF1, 2),
    CF1_lag3 = lag(CF1, 3),
    CF1_lag4 = lag(CF1, 4),
    
    CF5_lag0 = CF5,
    CF5_lag1 = lag(CF5, 1),
    CF5_lag2 = lag(CF5, 2),
    CF5_lag3 = lag(CF5, 3),
    CF5_lag4 = lag(CF5, 4),
    
    delta_p_lag0 = delta_p,
    delta_p_lag1 = lag(delta_p, 1),
    delta_p_lag2 = lag(delta_p, 2),
    delta_p_lag3 = lag(delta_p, 3),
    delta_p_lag4 = lag(delta_p, 4)
  ) 

Short_Infl_estimation <- Short_Infl_estimation%>%
  mutate(
    short_infl_sim =
  
      coefs_cf1["cf1_CF1_lag1"] * CF1_lag1 +
      coefs_cf1["cf1_CF1_lag2"] * CF1_lag2 +
      coefs_cf1["cf1_CF1_lag3"] * CF1_lag3 +
      coefs_cf1["cf1_CF1_lag4"] * CF1_lag4 +
      
      coefs_cf1["cf1_CF5_lag0"] * CF5_lag0 +
      coefs_cf1["cf1_CF5_lag1"] * CF5_lag1 +
      coefs_cf1["cf1_CF5_lag2"] * CF5_lag2 +
      coefs_cf1["cf1_CF5_lag3"] * CF5_lag3 +
      coefs_cf1["cf1_CF5_lag4"] * CF5_lag4 +
      
      coefs_cf1["cf1_delta_p_lag0"] * delta_p_lag0 +
      coefs_cf1["cf1_delta_p_lag1"] * delta_p_lag1 +
      coefs_cf1["cf1_delta_p_lag2"] * delta_p_lag2 +
      coefs_cf1["cf1_delta_p_lag3"] * delta_p_lag3 +
      coefs_cf1["cf1_delta_p_lag4"] * delta_p_lag4
  )

plot_short <- Short_Infl_estimation %>%
  filter(Quarter >= as.Date("2020-01-01")) %>%
  select(Quarter, CF1, short_infl_sim) %>%
  pivot_longer(cols = c(CF1, short_infl_sim), names_to = "Variable", values_to = "Value")

ggplot(plot_short, aes(x = Quarter, y = Value, color = Variable, linetype = Variable)) +
  geom_line(size = 1.2) +
  labs(
    title = "realised vs. simulated CF1, Q1 2020 - Q1 2023",
    x = "Time",
    y = "in %",
    color = "Variable",
    linetype = "Variable"
  ) +
  scale_color_manual(values = c("CF1" = "black", "short_infl_sim" = "blue")) +
  scale_linetype_manual(values = c("CF1" = "solid", "short_infl_sim" = "dashed")) +
  theme_minimal(base_size = 14)
# (Long term) Estimated vs. Actual Plot -----------------------------------
Long_Infl_estimation <- long_term %>%
  select(Quarter, CF5) %>%
  left_join(delta_p %>% select(Quarter, delta_p), by = "Quarter")

Long_Infl_estimation <- Long_Infl_estimation %>%
  select(Quarter, CF5, delta_p) %>%
  arrange(Quarter) %>%
  mutate(
    
    CF5_lag1 = lag(CF5, 1),
    CF5_lag2 = lag(CF5, 2),
    CF5_lag3 = lag(CF5, 3),
    CF5_lag4 = lag(CF5, 4),
    
    delta_p_lag0 = delta_p,
    delta_p_lag1 = lag(delta_p, 1),
    delta_p_lag2 = lag(delta_p, 2),
    delta_p_lag3 = lag(delta_p, 3),
    delta_p_lag4 = lag(delta_p, 4),
  ) %>%
  drop_na()

Long_Infl_estimation <- Long_Infl_estimation %>%
  mutate(
    long_infl_sim =
      
      coefs_cf5["cf5_CF5_lag1"] * CF5_lag1 +
      coefs_cf5["cf5_CF5_lag2"] * CF5_lag2 +
      coefs_cf5["cf5_CF5_lag3"] * CF5_lag3 +
      coefs_cf5["cf5_CF5_lag4"] * CF5_lag4 +
      
      coefs_cf5["cf5_delta_p_lag0"] * delta_p_lag0 +
      coefs_cf5["cf5_delta_p_lag1"] * delta_p_lag1 +
      coefs_cf5["cf5_delta_p_lag2"] * delta_p_lag2 +
      coefs_cf5["cf5_delta_p_lag3"] * delta_p_lag3 +
      coefs_cf5["cf5_delta_p_lag4"] * delta_p_lag4
  )

plot_long <- Long_Infl_estimation %>%
  filter(Quarter >= as.Date("2020-01-01")) %>%
  select(Quarter, CF5, long_infl_sim) %>%
  pivot_longer(cols = c(CF5, long_infl_sim), names_to = "Variable", values_to = "Value")

ggplot(plot_long, aes(x = Quarter, y = Value, color = Variable, linetype = Variable)) +
  geom_line(size = 1.2) +
  labs(
    title = "realised vs. simulated CF10, Q1 2020 - Q1 2023",
    x = "Time",
    y = "in %",
    color = "Variable",
    linetype = "Variable"
  ) +
  scale_color_manual(values = c("CF5" = "black", "long_infl_sim" = "darkgreen")) +
  scale_linetype_manual(values = c("CF5" = "solid", "long_infl_sim" = "dashed")) +
  theme_minimal(base_size = 14)


# Shock (standard Deviation)  ---------------------------------------------
Wage_equation_shock <- delta_w %>%
  select(Quarter, delta_w) %>%
  left_join(exp_short %>% select(Quarter, CF1), by = "Quarter") %>%
  left_join(VU %>% select(Quarter, VU), by = "Quarter") %>%
  left_join(CU %>% select(Quarter, CU), by = "Quarter") %>%
  left_join(productivity %>% select(Quarter, mapty), by = "Quarter") %>%
  filter(Quarter >= as.Date("2020-01-01") & Quarter <= as.Date("2023-01-01")) %>%
  drop_na() %>%
  arrange(Quarter)


price_equation_shock <- CU %>%
  select(Quarter, delta_p) %>%
  left_join(delta_w %>% select(Quarter, delta_w), by = "Quarter") %>%
  left_join(RPE_Data %>% select(Quarter, RPE), by = "Quarter") %>%
  left_join(RPF_Data %>% select(Quarter, RPF), by = "Quarter") %>%
  left_join(shortages %>% select(Quarter, shortage), by = "Quarter") %>%
  left_join(productivity %>% select(Quarter, mapty), by = "Quarter") %>%
  filter(Quarter >= as.Date("2020-01-01") & Quarter <= as.Date("2023-01-01")) %>%
  drop_na() %>%
  arrange(Quarter)

# Wage Equation
std_devs_wages <- Wage_equation_shock %>%
  select(-Quarter) %>%
  summarise(across(everything(), ~ sd(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "standard deviation")

# Price Equation
std_devs_price <- price_equation_shock %>%
  select(-Quarter) %>%
  summarise(across(everything(), ~ sd(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "standard deviation")

std_combined_clean <- bind_rows(std_devs_wages, std_devs_price) %>%
  distinct(Variable, .keep_all = TRUE) %>%  # doppelte Variablen vermeiden
  drop_na()

# Coefficients ------------------------------------------------------------
#wage_equation
wage_coeff_wage_delta_w_lag1 <- coefs_wage["wage_delta_w_lag1"]
wage_coeff_wage_delta_w_lag2 <- coefs_wage["wage_delta_w_lag2"]
wage_coeff_wage_delta_w_lag3 <- coefs_wage["wage_delta_w_lag3"]
wage_coeff_wage_delta_w_lag4 <- coefs_wage["wage_delta_w_lag4"]

wage_coeff_CF1_lag1 <- coefs_wage["wage_CF1_lag1"]
wage_coeff_CF1_lag2 <- coefs_wage["wage_CF1_lag2"]
wage_coeff_CF1_lag3 <- coefs_wage["wage_CF1_lag3"]
wage_coeff_CF1_lag4 <- coefs_wage["wage_CF1_lag4"]

wage_coeff_CU_lag1 <- coefs_wage["wage_CU_lag1"]
wage_coeff_CU_lag2 <- coefs_wage["wage_CU_lag2"]
wage_coeff_CU_lag3 <- coefs_wage["wage_CU_lag3"]
wage_coeff_CU_lag4 <- coefs_wage["wage_CU_lag4"]

wage_coeff_VU_lag1 <- coefs_wage["wage_VU_lag1"]
wage_coeff_VU_lag2 <- coefs_wage["wage_VU_lag2"]
wage_coeff_VU_lag3 <- coefs_wage["wage_VU_lag3"]
wage_coeff_VU_lag4 <- coefs_wage["wage_VU_lag4"]

wage_coeff_mapty_lag1_wage <- coefs_wage["wage_mapty_lag1"]

wage_coeff_intercept <- coefs_wage["wage_(Intercept)"]

#price_equation

price_coeff_delta_p_lag1 <- coefs_price["price_delta_p_lag1"]
price_coeff_delta_p_lag2 <- coefs_price["price_delta_p_lag2"]
price_coeff_delta_p_lag3 <- coefs_price["price_delta_p_lag3"]
price_coeff_delta_p_lag4 <- coefs_price["price_delta_p_lag4"]

price_coeff_delta_w_lag0 <- coefs_price["price_delta_w_lag0"]
price_coeff_delta_w_lag1 <- coefs_price["price_delta_w_lag1"]
price_coeff_delta_w_lag2 <- coefs_price["price_delta_w_lag2"]
price_coeff_delta_w_lag3 <- coefs_price["price_delta_w_lag3"]
price_coeff_delta_w_lag4 <- coefs_price["price_delta_w_lag4"]

price_coeff_shortage_0 <- coefs_price["price_shortage_lag0"]
price_coeff_shortage_1 <- coefs_price["price_shortage_lag1"]
price_coeff_shortage_2 <- coefs_price["price_shortage_lag2"]
price_coeff_shortage_3 <- coefs_price["price_shortage_lag3"]
price_coeff_shortage_4 <- coefs_price["price_shortage_lag4"]

price_coeff_RPE_0 <- coefs_price["price_RPE_lag0"]
price_coeff_RPE_1 <- coefs_price["price_RPE_lag1"]
price_coeff_RPE_2 <- coefs_price["price_RPE_lag2"]
price_coeff_RPE_3 <- coefs_price["price_RPE_lag3"]
price_coeff_RPE_4 <- coefs_price["price_RPE_lag4"]

price_coeff_RPF_0 <- coefs_price["price_RPF_lag0"]
price_coeff_RPF_1 <- coefs_price["price_RPF_lag1"]
price_coeff_RPF_2 <- coefs_price["price_RPF_lag2"]
price_coeff_RPF_3 <- coefs_price["price_RPF_lag3"]
price_coeff_RPF_4 <- coefs_price["price_RPF_lag4"]

price_coeff_mapty_lag1 <- coefs_price["price_mapty_lag1"]

price_coeff_intercept <- coefs_price["price_(Intercept)"]

#short term expectations

CF1_coeff_cf1_lag1 <- coefs_cf1["cf1_CF1_lag1"]
CF1_coeff_cf1_lag2 <- coefs_cf1["cf1_CF1_lag2"]
CF1_coeff_cf1_lag3 <- coefs_cf1["cf1_CF1_lag3"]
CF1_coeff_cf1_lag4 <- coefs_cf1["cf1_CF1_lag4"]

CF1_coeff_CF5_0 <- coefs_cf1["cf1_CF5_lag0"]
CF1_coeff_CF5_1 <- coefs_cf1["cf1_CF5_lag1"]
CF1_coeff_CF5_2 <- coefs_cf1["cf1_CF5_lag2"]
CF1_coeff_CF5_3 <- coefs_cf1["cf1_CF5_lag3"]
CF1_coeff_CF5_4 <- coefs_cf1["cf1_CF5_lag4"]

CF1_coeff_deltap_0 <- coefs_cf1["cf1_delta_p_lag0"]
CF1_coeff_deltap_1 <- coefs_cf1["cf1_delta_p_lag1"]
CF1_coeff_deltap_2 <- coefs_cf1["cf1_delta_p_lag2"]
CF1_coeff_deltap_3 <- coefs_cf1["cf1_delta_p_lag3"]
CF1_coeff_deltap_4 <- coefs_cf1["cf1_delta_p_lag4"]

# long term expectations

CF5_coeff_cf5_lag1 <- coefs_cf5["cf5_CF5_lag1"]
CF5_coeff_cf5_lag2 <- coefs_cf5["cf5_CF5_lag2"]
CF5_coeff_cf5_lag3 <- coefs_cf5["cf5_CF5_lag3"]
CF5_coeff_cf5_lag4 <- coefs_cf5["cf5_CF5_lag4"]

CF5_coeff_deltap_0 <- coefs_cf5["cf5_delta_p_lag0"]
CF5_coeff_deltap_1 <- coefs_cf5["cf5_delta_p_lag1"]
CF5_coeff_deltap_2 <- coefs_cf5["cf5_delta_p_lag2"]
CF5_coeff_deltap_3 <- coefs_cf5["cf5_delta_p_lag3"]
CF5_coeff_deltap_4 <- coefs_cf5["cf5_delta_p_lag4"]

# IRF Zero ----------------------------------------------------------------


Period <- 1:40

vars <- c(
  "w", "w_sim", "p", "p_sim", 
  "cf1", "cf1_sim", "cf5", "cf5_sim", 
  "vu", "CU", "CU_sim", 
  "mapty", "RPE", "RPF", "shortage"
)

IRF_zero <- data.frame(Period = Period)
for (var in vars) {
  IRF_zero[[var]] <- 0
}

# IRF shocks std ----------------------------------------------------------
sd_RPE <- std_combined_clean$`standard deviation`[std_combined_clean$Variable == "RPE"]
sd_RPF <- std_combined_clean$`standard deviation`[std_combined_clean$Variable == "RPF"]
sd_shortage <- std_combined_clean$`standard deviation`[std_combined_clean$Variable == "shortage"]

IRF_RPE <- IRF_zero
IRF_RPF <- IRF_zero
IRF_shortage <- IRF_zero

IRF_RPE$RPE[IRF_RPE$Period == 5] <- sd_RPE
IRF_RPF$RPF[IRF_RPF$Period == 5] <- sd_RPF
IRF_shortage$shortage[IRF_shortage$Period == 5] <- sd_shortage

# IRF RPE Shock  ----------------------------------------------------------
for (t in 5:40) {
  # --- Wage Equation (w_sim) ---
  IRF_RPE$w_sim[t] <-
    wage_coeff_wage_delta_w_lag1 * IRF_RPE$w_sim[t-1] +
    wage_coeff_wage_delta_w_lag2 * IRF_RPE$w_sim[t-2] +
    wage_coeff_wage_delta_w_lag3 * IRF_RPE$w_sim[t-3] +
    wage_coeff_wage_delta_w_lag4 * IRF_RPE$w_sim[t-4] +
    wage_coeff_CF1_lag1 * IRF_RPE$cf1_sim[t-1] +
    wage_coeff_CF1_lag2 * IRF_RPE$cf1_sim[t-2] +
    wage_coeff_CF1_lag3 * IRF_RPE$cf1_sim[t-3] +
    wage_coeff_CF1_lag4 * IRF_RPE$cf1_sim[t-4] +
    wage_coeff_CU_lag1 * IRF_RPE$CU_sim[t-1] +
    wage_coeff_CU_lag2 * IRF_RPE$CU_sim[t-2] +
    wage_coeff_CU_lag3 * IRF_RPE$CU_sim[t-3] +
    wage_coeff_CU_lag4 * IRF_RPE$CU_sim[t-4] +
    wage_coeff_VU_lag1 * IRF_RPE$vu[t-1] +
    wage_coeff_VU_lag2 * IRF_RPE$vu[t-2] +
    wage_coeff_VU_lag3 * IRF_RPE$vu[t-3] +
    wage_coeff_VU_lag4 * IRF_RPE$vu[t-4] +
    wage_coeff_mapty_lag1_wage * IRF_RPE$mapty[t-1]
  
  # --- Price Equation (p_sim) ---
  IRF_RPE$p_sim[t] <-
    price_coeff_delta_p_lag1 * IRF_RPE$p_sim[t-1] +
    price_coeff_delta_p_lag2 * IRF_RPE$p_sim[t-2] +
    price_coeff_delta_p_lag3 * IRF_RPE$p_sim[t-3] +
    price_coeff_delta_p_lag4 * IRF_RPE$p_sim[t-4] +
    price_coeff_delta_w_lag0 * IRF_RPE$w_sim[t] +
    price_coeff_delta_w_lag1 * IRF_RPE$w_sim[t-1] +
    price_coeff_delta_w_lag2 * IRF_RPE$w_sim[t-2] +
    price_coeff_delta_w_lag3 * IRF_RPE$w_sim[t-3] +
    price_coeff_delta_w_lag4 * IRF_RPE$w_sim[t-4] +
    price_coeff_RPE_0 * IRF_RPE$RPE[t] +
    price_coeff_RPE_1 * IRF_RPE$RPE[t-1] +
    price_coeff_RPE_2 * IRF_RPE$RPE[t-2] +
    price_coeff_RPE_3 * IRF_RPE$RPE[t-3] +
    price_coeff_RPE_4 * IRF_RPE$RPE[t-4] +
    price_coeff_RPF_0 * IRF_RPE$RPF[t] +
    price_coeff_RPF_1 * IRF_RPE$RPF[t-1] +
    price_coeff_RPF_2 * IRF_RPE$RPF[t-2] +
    price_coeff_RPF_3 * IRF_RPE$RPF[t-3] +
    price_coeff_RPF_4 * IRF_RPE$RPF[t-4] +
    price_coeff_shortage_0 * IRF_RPE$shortage[t] +
    price_coeff_shortage_1 * IRF_RPE$shortage[t-1] +
    price_coeff_shortage_2 * IRF_RPE$shortage[t-2] +
    price_coeff_shortage_3 * IRF_RPE$shortage[t-3] +
    price_coeff_shortage_4 * IRF_RPE$shortage[t-4] +
    price_coeff_mapty_lag1 * IRF_RPE$mapty[t-1]
  
  # --- Long-run Expectations (cf5_sim) ---
  IRF_RPE$cf5_sim[t] <-
    CF5_coeff_cf5_lag1 * IRF_RPE$cf5_sim[t-1] +
    CF5_coeff_cf5_lag2 * IRF_RPE$cf5_sim[t-2] +
    CF5_coeff_cf5_lag3 * IRF_RPE$cf5_sim[t-3] +
    CF5_coeff_cf5_lag4 * IRF_RPE$cf5_sim[t-4] +
    CF5_coeff_deltap_0 * IRF_RPE$p_sim[t] +
    CF5_coeff_deltap_1 * IRF_RPE$p_sim[t-1] +
    CF5_coeff_deltap_2 * IRF_RPE$p_sim[t-2] +
    CF5_coeff_deltap_3 * IRF_RPE$p_sim[t-3] +
    CF5_coeff_deltap_4 * IRF_RPE$p_sim[t-4]
  
  # --- Short-run Expectations (cf1_sim) ---
  IRF_RPE$cf1_sim[t] <-
    CF1_coeff_cf1_lag1 * IRF_RPE$cf1_sim[t-1] +
    CF1_coeff_cf1_lag2 * IRF_RPE$cf1_sim[t-2] +
    CF1_coeff_cf1_lag3 * IRF_RPE$cf1_sim[t-3] +
    CF1_coeff_cf1_lag4 * IRF_RPE$cf1_sim[t-4] +
    CF1_coeff_CF5_0 * IRF_RPE$cf5_sim[t] +
    CF1_coeff_CF5_1 * IRF_RPE$cf5_sim[t-1] +
    CF1_coeff_CF5_2 * IRF_RPE$cf5_sim[t-2] +
    CF1_coeff_CF5_3 * IRF_RPE$cf5_sim[t-3] +
    CF1_coeff_CF5_4 * IRF_RPE$cf5_sim[t-4] +
    CF1_coeff_deltap_0 * IRF_RPE$p_sim[t] +
    CF1_coeff_deltap_1 * IRF_RPE$p_sim[t-1] +
    CF1_coeff_deltap_2 * IRF_RPE$p_sim[t-2] +
    CF1_coeff_deltap_3 * IRF_RPE$p_sim[t-3] +
    CF1_coeff_deltap_4 * IRF_RPE$p_sim[t-4]
  
  # --- Catch-up-Term (CU_sim) ---
  IRF_RPE$CU_sim[t] <- 0.25 * sum(IRF_RPE$p_sim[(t-3):t]) - IRF_RPE$cf1_sim[t-4]
}

ggplot(IRF_RPE, aes(x = Period, y = p_sim)) +
  geom_line(color = "blue", size = 1.2) +
  geom_point(color = "blue", size = 2) +
  labs(title = "Evolution of delta_p (Inflation) after a rel. Energy price shock",
       x = "Periode", y = "in %") +
  theme_minimal()

# IRF RPF Shock -----------------------------------------------------------
for (t in 5:40) {
  # --- Wage Equation (w_sim) ---
  IRF_RPF$w_sim[t] <-
    wage_coeff_wage_delta_w_lag1 * IRF_RPF$w_sim[t-1] +
    wage_coeff_wage_delta_w_lag2 * IRF_RPF$w_sim[t-2] +
    wage_coeff_wage_delta_w_lag3 * IRF_RPF$w_sim[t-3] +
    wage_coeff_wage_delta_w_lag4 * IRF_RPF$w_sim[t-4] +
    wage_coeff_CF1_lag1 * IRF_RPF$cf1_sim[t-1] +
    wage_coeff_CF1_lag2 * IRF_RPF$cf1_sim[t-2] +
    wage_coeff_CF1_lag3 * IRF_RPF$cf1_sim[t-3] +
    wage_coeff_CF1_lag4 * IRF_RPF$cf1_sim[t-4] +
    wage_coeff_CU_lag1 * IRF_RPF$CU_sim[t-1] +
    wage_coeff_CU_lag2 * IRF_RPF$CU_sim[t-2] +
    wage_coeff_CU_lag3 * IRF_RPF$CU_sim[t-3] +
    wage_coeff_CU_lag4 * IRF_RPF$CU_sim[t-4] +
    wage_coeff_VU_lag1 * IRF_RPF$vu[t-1] +
    wage_coeff_VU_lag2 * IRF_RPF$vu[t-2] +
    wage_coeff_VU_lag3 * IRF_RPF$vu[t-3] +
    wage_coeff_VU_lag4 * IRF_RPF$vu[t-4] +
    wage_coeff_mapty_lag1_wage * IRF_RPF$mapty[t-1]
  
  # --- Price Equation (p_sim) ---
  IRF_RPF$p_sim[t] <-
    price_coeff_delta_p_lag1 * IRF_RPF$p_sim[t-1] +
    price_coeff_delta_p_lag2 * IRF_RPF$p_sim[t-2] +
    price_coeff_delta_p_lag3 * IRF_RPF$p_sim[t-3] +
    price_coeff_delta_p_lag4 * IRF_RPF$p_sim[t-4] +
    price_coeff_delta_w_lag0 * IRF_RPF$w_sim[t] +
    price_coeff_delta_w_lag1 * IRF_RPF$w_sim[t-1] +
    price_coeff_delta_w_lag2 * IRF_RPF$w_sim[t-2] +
    price_coeff_delta_w_lag3 * IRF_RPF$w_sim[t-3] +
    price_coeff_delta_w_lag4 * IRF_RPF$w_sim[t-4] +
    price_coeff_RPE_0 * IRF_RPF$RPE[t] +
    price_coeff_RPE_1 * IRF_RPF$RPE[t-1] +
    price_coeff_RPE_2 * IRF_RPF$RPE[t-2] +
    price_coeff_RPE_3 * IRF_RPF$RPE[t-3] +
    price_coeff_RPE_4 * IRF_RPF$RPE[t-4] +
    price_coeff_RPF_0 * IRF_RPF$RPF[t] +
    price_coeff_RPF_1 * IRF_RPF$RPF[t-1] +
    price_coeff_RPF_2 * IRF_RPF$RPF[t-2] +
    price_coeff_RPF_3 * IRF_RPF$RPF[t-3] +
    price_coeff_RPF_4 * IRF_RPF$RPF[t-4] +
    price_coeff_shortage_0 * IRF_RPF$shortage[t] +
    price_coeff_shortage_1 * IRF_RPF$shortage[t-1] +
    price_coeff_shortage_2 * IRF_RPF$shortage[t-2] +
    price_coeff_shortage_3 * IRF_RPF$shortage[t-3] +
    price_coeff_shortage_4 * IRF_RPF$shortage[t-4] +
    price_coeff_mapty_lag1 * IRF_RPF$mapty[t-1]
  
  # --- Long-run Expectations (cf5_sim) ---
  IRF_RPF$cf5_sim[t] <-
    CF5_coeff_cf5_lag1 * IRF_RPF$cf5_sim[t-1] +
    CF5_coeff_cf5_lag2 * IRF_RPF$cf5_sim[t-2] +
    CF5_coeff_cf5_lag3 * IRF_RPF$cf5_sim[t-3] +
    CF5_coeff_cf5_lag4 * IRF_RPF$cf5_sim[t-4] +
    CF5_coeff_deltap_0 * IRF_RPF$p_sim[t] +
    CF5_coeff_deltap_1 * IRF_RPF$p_sim[t-1] +
    CF5_coeff_deltap_2 * IRF_RPF$p_sim[t-2] +
    CF5_coeff_deltap_3 * IRF_RPF$p_sim[t-3] +
    CF5_coeff_deltap_4 * IRF_RPF$p_sim[t-4]
  
  # --- Short-run Expectations (cf1_sim) ---
  IRF_RPF$cf1_sim[t] <-
    CF1_coeff_cf1_lag1 * IRF_RPF$cf1_sim[t-1] +
    CF1_coeff_cf1_lag2 * IRF_RPF$cf1_sim[t-2] +
    CF1_coeff_cf1_lag3 * IRF_RPF$cf1_sim[t-3] +
    CF1_coeff_cf1_lag4 * IRF_RPF$cf1_sim[t-4] +
    CF1_coeff_CF5_0 * IRF_RPF$cf5_sim[t] +
    CF1_coeff_CF5_1 * IRF_RPF$cf5_sim[t-1] +
    CF1_coeff_CF5_2 * IRF_RPF$cf5_sim[t-2] +
    CF1_coeff_CF5_3 * IRF_RPF$cf5_sim[t-3] +
    CF1_coeff_CF5_4 * IRF_RPF$cf5_sim[t-4] +
    CF1_coeff_deltap_0 * IRF_RPF$p_sim[t] +
    CF1_coeff_deltap_1 * IRF_RPF$p_sim[t-1] +
    CF1_coeff_deltap_2 * IRF_RPF$p_sim[t-2] +
    CF1_coeff_deltap_3 * IRF_RPF$p_sim[t-3] +
    CF1_coeff_deltap_4 * IRF_RPF$p_sim[t-4]
  
  # --- Catch-up-Term (CU_sim) ---
  IRF_RPF$CU_sim[t] <- 0.25 * sum(IRF_RPF$p_sim[(t-3):t]) - IRF_RPF$cf1_sim[t-4]
}


ggplot(IRF_RPF, aes(x = Period, y = p_sim)) +
  geom_line(color = "blue", size = 1.2) +
  geom_point(color = "blue", size = 2) +
  labs(title = "Evolution of delta_p (Inflation) after a rel. Food price shock)",
       x = "Periode", y = "in %") +
  theme_minimal()

# IRF Shortage Shock -----------------------------------------------------
for (t in 5:40) {
  # --- Wage Equation (w_sim) ---
  IRF_shortage$w_sim[t] <-
    wage_coeff_wage_delta_w_lag1 * IRF_shortage$w_sim[t-1] +
    wage_coeff_wage_delta_w_lag2 * IRF_shortage$w_sim[t-2] +
    wage_coeff_wage_delta_w_lag3 * IRF_shortage$w_sim[t-3] +
    wage_coeff_wage_delta_w_lag4 * IRF_shortage$w_sim[t-4] +
    wage_coeff_CF1_lag1 * IRF_shortage$cf1_sim[t-1] +
    wage_coeff_CF1_lag2 * IRF_shortage$cf1_sim[t-2] +
    wage_coeff_CF1_lag3 * IRF_shortage$cf1_sim[t-3] +
    wage_coeff_CF1_lag4 * IRF_shortage$cf1_sim[t-4] +
    wage_coeff_CU_lag1 * IRF_shortage$CU_sim[t-1] +
    wage_coeff_CU_lag2 * IRF_shortage$CU_sim[t-2] +
    wage_coeff_CU_lag3 * IRF_shortage$CU_sim[t-3] +
    wage_coeff_CU_lag4 * IRF_shortage$CU_sim[t-4] +
    wage_coeff_VU_lag1 * IRF_shortage$vu[t-1] +
    wage_coeff_VU_lag2 * IRF_shortage$vu[t-2] +
    wage_coeff_VU_lag3 * IRF_shortage$vu[t-3] +
    wage_coeff_VU_lag4 * IRF_shortage$vu[t-4] +
    wage_coeff_mapty_lag1_wage * IRF_shortage$mapty[t-1]
  
  # --- Price Equation (p_sim) ---
  IRF_shortage$p_sim[t] <-
    price_coeff_delta_p_lag1 * IRF_shortage$p_sim[t-1] +
    price_coeff_delta_p_lag2 * IRF_shortage$p_sim[t-2] +
    price_coeff_delta_p_lag3 * IRF_shortage$p_sim[t-3] +
    price_coeff_delta_p_lag4 * IRF_shortage$p_sim[t-4] +
    price_coeff_delta_w_lag0 * IRF_shortage$w_sim[t] +
    price_coeff_delta_w_lag1 * IRF_shortage$w_sim[t-1] +
    price_coeff_delta_w_lag2 * IRF_shortage$w_sim[t-2] +
    price_coeff_delta_w_lag3 * IRF_shortage$w_sim[t-3] +
    price_coeff_delta_w_lag4 * IRF_shortage$w_sim[t-4] +
    price_coeff_RPE_0 * IRF_shortage$RPE[t] +
    price_coeff_RPE_1 * IRF_shortage$RPE[t-1] +
    price_coeff_RPE_2 * IRF_shortage$RPE[t-2] +
    price_coeff_RPE_3 * IRF_shortage$RPE[t-3] +
    price_coeff_RPE_4 * IRF_shortage$RPE[t-4] +
    price_coeff_RPF_0 * IRF_shortage$RPF[t] +
    price_coeff_RPF_1 * IRF_shortage$RPF[t-1] +
    price_coeff_RPF_2 * IRF_shortage$RPF[t-2] +
    price_coeff_RPF_3 * IRF_shortage$RPF[t-3] +
    price_coeff_RPF_4 * IRF_shortage$RPF[t-4] +
    price_coeff_shortage_0 * IRF_shortage$shortage[t] +
    price_coeff_shortage_1 * IRF_shortage$shortage[t-1] +
    price_coeff_shortage_2 * IRF_shortage$shortage[t-2] +
    price_coeff_shortage_3 * IRF_shortage$shortage[t-3] +
    price_coeff_shortage_4 * IRF_shortage$shortage[t-4] +
    price_coeff_mapty_lag1 * IRF_shortage$mapty[t-1]
  
  # --- Long-run Expectations (cf5_sim) ---
  IRF_shortage$cf5_sim[t] <-
    CF5_coeff_cf5_lag1 * IRF_shortage$cf5_sim[t-1] +
    CF5_coeff_cf5_lag2 * IRF_shortage$cf5_sim[t-2] +
    CF5_coeff_cf5_lag3 * IRF_shortage$cf5_sim[t-3] +
    CF5_coeff_cf5_lag4 * IRF_shortage$cf5_sim[t-4] +
    CF5_coeff_deltap_0 * IRF_shortage$p_sim[t] +
    CF5_coeff_deltap_1 * IRF_shortage$p_sim[t-1] +
    CF5_coeff_deltap_2 * IRF_shortage$p_sim[t-2] +
    CF5_coeff_deltap_3 * IRF_shortage$p_sim[t-3] +
    CF5_coeff_deltap_4 * IRF_shortage$p_sim[t-4]
  
  # --- Short-run Expectations (cf1_sim) ---
  IRF_shortage$cf1_sim[t] <-
    CF1_coeff_cf1_lag1 * IRF_shortage$cf1_sim[t-1] +
    CF1_coeff_cf1_lag2 * IRF_shortage$cf1_sim[t-2] +
    CF1_coeff_cf1_lag3 * IRF_shortage$cf1_sim[t-3] +
    CF1_coeff_cf1_lag4 * IRF_shortage$cf1_sim[t-4] +
    CF1_coeff_CF5_0 * IRF_shortage$cf5_sim[t] +
    CF1_coeff_CF5_1 * IRF_shortage$cf5_sim[t-1] +
    CF1_coeff_CF5_2 * IRF_shortage$cf5_sim[t-2] +
    CF1_coeff_CF5_3 * IRF_shortage$cf5_sim[t-3] +
    CF1_coeff_CF5_4 * IRF_shortage$cf5_sim[t-4] +
    CF1_coeff_deltap_0 * IRF_shortage$p_sim[t] +
    CF1_coeff_deltap_1 * IRF_shortage$p_sim[t-1] +
    CF1_coeff_deltap_2 * IRF_shortage$p_sim[t-2] +
    CF1_coeff_deltap_3 * IRF_shortage$p_sim[t-3] +
    CF1_coeff_deltap_4 * IRF_shortage$p_sim[t-4]
  
  # --- Catch-up-Term (CU_sim) ---
  IRF_shortage$CU_sim[t] <- 0.25 * sum(IRF_shortage$p_sim[(t-3):t]) - IRF_shortage$cf1_sim[t-4]
}



ggplot(IRF_shortage, aes(x = Period, y = p_sim)) +
  geom_line(color = "blue", size = 1.2) +
  geom_point(color = "blue", size = 2) +
  labs(title = "Evolution of delta_p (Inflation) after a shortage shock",
       x = "Periode", y = "p_sim") +
  theme_minimal()

# Decomposition no shocks removed  ----------------------------------------

cf5_equation <- long_term %>%
  select(Quarter, CF5) %>%
  left_join(delta_p %>% select(Quarter, delta_p), by = "Quarter")


quarter_dates <- seq(as.Date("2017-10-01"), as.Date("2023-01-01"), by = "quarter")

decomposition_normal <- data.frame(
  Quarter      = quarter_dates,
  gw_sim_orig  = 0, gp_sim_orig  = 0,
  cf1_sim_orig = 0, cf3_sim_orig = 0,
  rpe          = 0, rpf          = 0,
  vu           = 0, shortage     = 0,
  mapty        = 0, cu           = 0
)

for(x in list(
  list(df = VU,            col = "VU",          target = "vu"),
  list(df = productivity,  col = "mapty",       target = "mapty"),
  list(df = RPE_Data,      col = "RPE",         target = "rpe"),
  list(df = RPF_Data,      col = "RPF",         target = "rpf"),
  list(df = shortages,     col = "shortage", target = "shortage")
)) {
  decomposition_normal <- merge(
    decomposition_normal, x$df[, c("Quarter", x$col)],
    by = "Quarter", all.x = TRUE
  )
  decomposition_normal[[x$target]] <- decomposition_normal[[x$col]]
  decomposition_normal[[x$col]] <- NULL
}

quartale_short <- seq(as.Date("2017-10-01"), as.Date("2019-07-01"), by = "quarter")
rows_short <- which(decomposition_normal$Quarter %in% quartale_short)

subset_wage  <- subset(Wage_equation, Quarter %in% quartale_short)
subset_price <- subset(price_equation,  Quarter %in% quartale_short)
subset_cf5   <- subset(cf5_equation,             Quarter %in% quartale_short)

decomposition_normal$gw_sim_orig[rows_short]  <- subset_wage$delta_w
decomposition_normal$cf1_sim_orig[rows_short] <- subset_wage$CF1
decomposition_normal$cu[rows_short]           <- subset_wage$CU
decomposition_normal$gp_sim_orig[rows_short]  <- subset_price$delta_p
decomposition_normal$cf3_sim_orig[rows_short] <- subset_cf5$CF5

if ("mapty.x" %in% names(decomposition_normal)) decomposition_normal$mapty.x <- NULL
if ("mapty.y" %in% names(decomposition_normal)) names(decomposition_normal)[names(decomposition_normal) == "mapty.y"] <- "mapty"

if ("shortage.x" %in% names(decomposition_normal)) decomposition_normal$shortage.x <- NULL
if ("shortage.y" %in% names(decomposition_normal)) names(decomposition_normal)[names(decomposition_normal) == "shortage.y"] <- "shortage"
decomposition_normal <- decomposition_normal %>% select(-mapty)

start_quarter <- as.Date("2019-10-01")
end_quarter   <- as.Date("2023-01-01")
row_start <- which(decomposition_normal$Quarter == start_quarter)
row_end   <- which(decomposition_normal$Quarter == end_quarter)

for (row_t in row_start:row_end) {
  decomposition_normal$gw_sim_orig[row_t] <- wage_coeff_intercept +
    wage_coeff_wage_delta_w_lag1 * decomposition_normal$gw_sim_orig[row_t - 1] +
    wage_coeff_wage_delta_w_lag2 * decomposition_normal$gw_sim_orig[row_t - 2] +
    wage_coeff_wage_delta_w_lag3 * decomposition_normal$gw_sim_orig[row_t - 3] +
    wage_coeff_wage_delta_w_lag4 * decomposition_normal$gw_sim_orig[row_t - 4] +
    
    wage_coeff_CF1_lag1 * decomposition_normal$cf1_sim_orig[row_t - 1] +
    wage_coeff_CF1_lag2 * decomposition_normal$cf1_sim_orig[row_t - 2] +
    wage_coeff_CF1_lag3 * decomposition_normal$cf1_sim_orig[row_t - 3] +
    wage_coeff_CF1_lag4 * decomposition_normal$cf1_sim_orig[row_t - 4] +
    
    wage_coeff_CU_lag1 * decomposition_normal$cu[row_t - 1] +
    wage_coeff_CU_lag2 * decomposition_normal$cu[row_t - 2] +
    wage_coeff_CU_lag3 * decomposition_normal$cu[row_t - 3] +
    wage_coeff_CU_lag4 * decomposition_normal$cu[row_t - 4] +
    
    wage_coeff_VU_lag1 * decomposition_normal$vu[row_t - 1] +
    wage_coeff_VU_lag2 * decomposition_normal$vu[row_t - 2] +
    wage_coeff_VU_lag3 * decomposition_normal$vu[row_t - 3] +
    wage_coeff_VU_lag4 * decomposition_normal$vu[row_t - 4] 
    
  decomposition_normal$gp_sim_orig[row_t] <- price_coeff_intercept +
    price_coeff_delta_p_lag1 * decomposition_normal$gp_sim_orig[row_t-1] +
    price_coeff_delta_p_lag2 * decomposition_normal$gp_sim_orig[row_t-2] +
    price_coeff_delta_p_lag3 * decomposition_normal$gp_sim_orig[row_t-3] +
    price_coeff_delta_p_lag4 * decomposition_normal$gp_sim_orig[row_t-4] +
    price_coeff_delta_w_lag0 * decomposition_normal$gw_sim_orig[row_t] +
    price_coeff_delta_w_lag1 * decomposition_normal$gw_sim_orig[row_t-1] +
    price_coeff_delta_w_lag2 * decomposition_normal$gw_sim_orig[row_t-2] +
    price_coeff_delta_w_lag3 * decomposition_normal$gw_sim_orig[row_t-3] +
    price_coeff_delta_w_lag4 * decomposition_normal$gw_sim_orig[row_t-4] +
    price_coeff_RPE_0 * decomposition_normal$rpe[row_t] +
    price_coeff_RPE_1 * decomposition_normal$rpe[row_t-1] +
    price_coeff_RPE_2 * decomposition_normal$rpe[row_t-2] +
    price_coeff_RPE_3 * decomposition_normal$rpe[row_t-3] +
    price_coeff_RPE_4 * decomposition_normal$rpe[row_t-4] +
    price_coeff_RPF_0 * decomposition_normal$rpf[row_t] +
    price_coeff_RPF_1 * decomposition_normal$rpf[row_t-1] +
    price_coeff_RPF_2 * decomposition_normal$rpf[row_t-2] +
    price_coeff_RPF_3 * decomposition_normal$rpf[row_t-3] +
    price_coeff_RPF_4 * decomposition_normal$rpf[row_t-4] +
    price_coeff_shortage_0 * decomposition_normal$shortage[row_t] +
    price_coeff_shortage_1 * decomposition_normal$shortage[row_t-1] +
    price_coeff_shortage_2 * decomposition_normal$shortage[row_t-2] +
    price_coeff_shortage_3 * decomposition_normal$shortage[row_t-3] +
    price_coeff_shortage_4 * decomposition_normal$shortage[row_t-4] 
  
  decomposition_normal$cu[row_t] <-
    mean(decomposition_normal$gp_sim_orig[(row_t-1):(row_t-4)]) -
    decomposition_normal$cf1_sim_orig[row_t-4]
  
  decomposition_normal$cf3_sim_orig[row_t] <-
    CF5_coeff_cf5_lag1 * decomposition_normal$cf3_sim_orig[row_t-1] +
    CF5_coeff_cf5_lag2 * decomposition_normal$cf3_sim_orig[row_t-2] +
    CF5_coeff_cf5_lag3 * decomposition_normal$cf3_sim_orig[row_t-3] +
    CF5_coeff_cf5_lag4 * decomposition_normal$cf3_sim_orig[row_t-4] +
    CF5_coeff_deltap_0 * decomposition_normal$gp_sim_orig[row_t] +
    CF5_coeff_deltap_1 * decomposition_normal$gp_sim_orig[row_t-1] +
    CF5_coeff_deltap_2 * decomposition_normal$gp_sim_orig[row_t-2] +
    CF5_coeff_deltap_3 * decomposition_normal$gp_sim_orig[row_t-3] +
    CF5_coeff_deltap_4 * decomposition_normal$gp_sim_orig[row_t-4]
  
  decomposition_normal$cf1_sim_orig[row_t] <-
    CF1_coeff_cf1_lag1 * decomposition_normal$cf1_sim_orig[row_t-1] +
    CF1_coeff_cf1_lag2 * decomposition_normal$cf1_sim_orig[row_t-2] +
    CF1_coeff_cf1_lag3 * decomposition_normal$cf1_sim_orig[row_t-3] +
    CF1_coeff_cf1_lag4 * decomposition_normal$cf1_sim_orig[row_t-4] +
    CF1_coeff_CF5_0 * decomposition_normal$cf3_sim_orig[row_t] +
    CF1_coeff_CF5_1 * decomposition_normal$cf3_sim_orig[row_t-1] +
    CF1_coeff_CF5_2 * decomposition_normal$cf3_sim_orig[row_t-2] +
    CF1_coeff_CF5_3 * decomposition_normal$cf3_sim_orig[row_t-3] +
    CF1_coeff_CF5_4 * decomposition_normal$cf3_sim_orig[row_t-4] +
    CF1_coeff_deltap_0 * decomposition_normal$gp_sim_orig[row_t] +
    CF1_coeff_deltap_1 * decomposition_normal$gp_sim_orig[row_t-1] +
    CF1_coeff_deltap_2 * decomposition_normal$gp_sim_orig[row_t-2] +
    CF1_coeff_deltap_3 * decomposition_normal$gp_sim_orig[row_t-3] +
    CF1_coeff_deltap_4 * decomposition_normal$gp_sim_orig[row_t-4]
}

# Decomposition RPE Removed -----------------------------------------------
cf5_equation <- long_term %>%
  select(Quarter, CF5) %>%
  left_join(delta_p %>% select(Quarter, delta_p), by = "Quarter")

quarter_dates <- seq(as.Date("2017-10-01"), as.Date("2023-01-01"), by = "quarter")

decomposition_rpe <- data.frame(
  Quarter      = quarter_dates,
  gw_sim_rpe  = 0, gp_sim_rpe  = 0,
  cf1_sim_rpe = 0, cf3_sim_rpe = 0,
  rpe          = 0, rpf          = 0,
  vu           = 0, shortage     = 0,
  mapty        = 0, cu           = 0
)

for(x in list(
  list(df = VU,            col = "VU",          target = "vu"),
  list(df = productivity,  col = "mapty",       target = "mapty"),
  list(df = RPE_Data,      col = "RPE",         target = "rpe"),
  list(df = RPF_Data,      col = "RPF",         target = "rpf"),
  list(df = shortages,     col = "shortage",    target = "shortage")
)) {
  decomposition_rpe <- merge(
    decomposition_rpe, x$df[, c("Quarter", x$col)],
    by = "Quarter", all.x = TRUE
  )
  decomposition_rpe[[x$target]] <- decomposition_rpe[[x$col]]
  decomposition_rpe[[x$col]] <- NULL
}

quartale_short <- seq(as.Date("2017-10-01"), as.Date("2019-07-01"), by = "quarter")
rows_short <- which(decomposition_rpe$Quarter %in% quartale_short)

subset_wage  <- subset(Wage_equation, Quarter %in% quartale_short)
subset_price <- subset(price_equation,  Quarter %in% quartale_short)
subset_cf5   <- subset(cf5_equation,             Quarter %in% quartale_short)

decomposition_rpe$gw_sim_rpe[rows_short]  <- subset_wage$delta_w
decomposition_rpe$cf1_sim_rpe[rows_short] <- subset_wage$CF1
decomposition_rpe$cu[rows_short]           <- subset_wage$CU
decomposition_rpe$gp_sim_rpe[rows_short]  <- subset_price$delta_p
decomposition_rpe$cf3_sim_rpe[rows_short] <- subset_cf5$CF5

if ("mapty.x" %in% names(decomposition_rpe)) decomposition_rpe$mapty.x <- NULL
if ("mapty.y" %in% names(decomposition_rpe)) names(decomposition_rpe)[names(decomposition_rpe) == "mapty.y"] <- "mapty"

if ("shortage.x" %in% names(decomposition_rpe)) decomposition_rpe$shortage.x <- NULL
if ("shortage.y" %in% names(decomposition_rpe)) names(decomposition_rpe)[names(decomposition_rpe) == "shortage.y"] <- "shortage"

decomposition_rpe <- decomposition_rpe %>% select(-mapty)
decomposition_rpe <- decomposition_rpe %>%
  mutate(rpe = if_else(Quarter >= as.Date("2019-10-01"), 0, rpe))

start_quarter <- as.Date("2019-10-01")
end_quarter   <- as.Date("2023-01-01")
row_start <- which(decomposition_rpe$Quarter == start_quarter)
row_end   <- which(decomposition_rpe$Quarter == end_quarter)

for (row_t in row_start:row_end) {
  decomposition_rpe$gw_sim_rpe[row_t] <- wage_coeff_intercept +
    wage_coeff_wage_delta_w_lag1 * decomposition_rpe$gw_sim_rpe[row_t - 1] +
    wage_coeff_wage_delta_w_lag2 * decomposition_rpe$gw_sim_rpe[row_t - 2] +
    wage_coeff_wage_delta_w_lag3 * decomposition_rpe$gw_sim_rpe[row_t - 3] +
    wage_coeff_wage_delta_w_lag4 * decomposition_rpe$gw_sim_rpe[row_t - 4] +
    
    wage_coeff_CF1_lag1 * decomposition_rpe$cf1_sim_rpe[row_t - 1] +
    wage_coeff_CF1_lag2 * decomposition_rpe$cf1_sim_rpe[row_t - 2] +
    wage_coeff_CF1_lag3 * decomposition_rpe$cf1_sim_rpe[row_t - 3] +
    wage_coeff_CF1_lag4 * decomposition_rpe$cf1_sim_rpe[row_t - 4] +
    
    wage_coeff_CU_lag1 * decomposition_rpe$cu[row_t - 1] +
    wage_coeff_CU_lag2 * decomposition_rpe$cu[row_t - 2] +
    wage_coeff_CU_lag3 * decomposition_rpe$cu[row_t - 3] +
    wage_coeff_CU_lag4 * decomposition_rpe$cu[row_t - 4] +
    
    wage_coeff_VU_lag1 * decomposition_rpe$vu[row_t - 1] +
    wage_coeff_VU_lag2 * decomposition_rpe$vu[row_t - 2] +
    wage_coeff_VU_lag3 * decomposition_rpe$vu[row_t - 3] +
    wage_coeff_VU_lag4 * decomposition_rpe$vu[row_t - 4]
  
  decomposition_rpe$gp_sim_rpe[row_t] <- price_coeff_intercept +
    price_coeff_delta_p_lag1 * decomposition_rpe$gp_sim_rpe[row_t - 1] +
    price_coeff_delta_p_lag2 * decomposition_rpe$gp_sim_rpe[row_t - 2] +
    price_coeff_delta_p_lag3 * decomposition_rpe$gp_sim_rpe[row_t - 3] +
    price_coeff_delta_p_lag4 * decomposition_rpe$gp_sim_rpe[row_t - 4] +
    price_coeff_delta_w_lag0 * decomposition_rpe$gw_sim_rpe[row_t] +
    price_coeff_delta_w_lag1 * decomposition_rpe$gw_sim_rpe[row_t - 1] +
    price_coeff_delta_w_lag2 * decomposition_rpe$gw_sim_rpe[row_t - 2] +
    price_coeff_delta_w_lag3 * decomposition_rpe$gw_sim_rpe[row_t - 3] +
    price_coeff_delta_w_lag4 * decomposition_rpe$gw_sim_rpe[row_t - 4] +
    price_coeff_RPE_0 * decomposition_rpe$rpe[row_t] +
    price_coeff_RPE_1 * decomposition_rpe$rpe[row_t - 1] +
    price_coeff_RPE_2 * decomposition_rpe$rpe[row_t - 2] +
    price_coeff_RPE_3 * decomposition_rpe$rpe[row_t - 3] +
    price_coeff_RPE_4 * decomposition_rpe$rpe[row_t - 4] +
    price_coeff_RPF_0 * decomposition_rpe$rpf[row_t] +
    price_coeff_RPF_1 * decomposition_rpe$rpf[row_t - 1] +
    price_coeff_RPF_2 * decomposition_rpe$rpf[row_t - 2] +
    price_coeff_RPF_3 * decomposition_rpe$rpf[row_t - 3] +
    price_coeff_RPF_4 * decomposition_rpe$rpf[row_t - 4] +
    price_coeff_shortage_0 * decomposition_rpe$shortage[row_t] +
    price_coeff_shortage_1 * decomposition_rpe$shortage[row_t - 1] +
    price_coeff_shortage_2 * decomposition_rpe$shortage[row_t - 2] +
    price_coeff_shortage_3 * decomposition_rpe$shortage[row_t - 3] +
    price_coeff_shortage_4 * decomposition_rpe$shortage[row_t - 4]
  

  decomposition_rpe$cu[row_t] <-
    mean(decomposition_rpe$gp_sim_rpe[(row_t-1):(row_t-4)]) -
    decomposition_rpe$cf1_sim_rpe[row_t-4]
  

  decomposition_rpe$cf3_sim_rpe[row_t] <-
    CF5_coeff_cf5_lag1 * decomposition_rpe$cf3_sim_rpe[row_t - 1] +
    CF5_coeff_cf5_lag2 * decomposition_rpe$cf3_sim_rpe[row_t - 2] +
    CF5_coeff_cf5_lag3 * decomposition_rpe$cf3_sim_rpe[row_t - 3] +
    CF5_coeff_cf5_lag4 * decomposition_rpe$cf3_sim_rpe[row_t - 4] +
    CF5_coeff_deltap_0 * decomposition_rpe$gp_sim_rpe[row_t] +
    CF5_coeff_deltap_1 * decomposition_rpe$gp_sim_rpe[row_t - 1] +
    CF5_coeff_deltap_2 * decomposition_rpe$gp_sim_rpe[row_t - 2] +
    CF5_coeff_deltap_3 * decomposition_rpe$gp_sim_rpe[row_t - 3] +
    CF5_coeff_deltap_4 * decomposition_rpe$gp_sim_rpe[row_t - 4]
  

  decomposition_rpe$cf1_sim_rpe[row_t] <-
    CF1_coeff_cf1_lag1 * decomposition_rpe$cf1_sim_rpe[row_t - 1] +
    CF1_coeff_cf1_lag2 * decomposition_rpe$cf1_sim_rpe[row_t - 2] +
    CF1_coeff_cf1_lag3 * decomposition_rpe$cf1_sim_rpe[row_t - 3] +
    CF1_coeff_cf1_lag4 * decomposition_rpe$cf1_sim_rpe[row_t - 4] +
    CF1_coeff_CF5_0 * decomposition_rpe$cf3_sim_rpe[row_t] +
    CF1_coeff_CF5_1 * decomposition_rpe$cf3_sim_rpe[row_t - 1] +
    CF1_coeff_CF5_2 * decomposition_rpe$cf3_sim_rpe[row_t - 2] +
    CF1_coeff_CF5_3 * decomposition_rpe$cf3_sim_rpe[row_t - 3] +
    CF1_coeff_CF5_4 * decomposition_rpe$cf3_sim_rpe[row_t - 4] +
    CF1_coeff_deltap_0 * decomposition_rpe$gp_sim_rpe[row_t] +
    CF1_coeff_deltap_1 * decomposition_rpe$gp_sim_rpe[row_t - 1] +
    CF1_coeff_deltap_2 * decomposition_rpe$gp_sim_rpe[row_t - 2] +
    CF1_coeff_deltap_3 * decomposition_rpe$gp_sim_rpe[row_t - 3] +
    CF1_coeff_deltap_4 * decomposition_rpe$gp_sim_rpe[row_t - 4]
}

# Decomposition RPF Removed -----------------------------------------------
cf5_equation <- long_term %>%
  select(Quarter, CF5) %>%
  left_join(delta_p %>% select(Quarter, delta_p), by = "Quarter")

quarter_dates <- seq(as.Date("2017-10-01"), as.Date("2023-01-01"), by = "quarter")

decomposition_rpf <- data.frame(
  Quarter      = quarter_dates,
  gw_sim_rpf  = 0, gp_sim_rpf  = 0,
  cf1_sim_rpf = 0, cf3_sim_rpf = 0,
  rpe          = 0, rpf          = 0,
  vu           = 0, shortage     = 0,
  mapty        = 0, cu           = 0
)

for(x in list(
  list(df = VU,            col = "VU",          target = "vu"),
  list(df = productivity,  col = "mapty",       target = "mapty"),
  list(df = RPE_Data,      col = "RPE",         target = "rpe"),
  list(df = RPF_Data,      col = "RPF",         target = "rpf"),
  list(df = shortages,     col = "shortage",    target = "shortage")
)) {
  decomposition_rpf <- merge(
    decomposition_rpf, x$df[, c("Quarter", x$col)],
    by = "Quarter", all.x = TRUE
  )
  decomposition_rpf[[x$target]] <- decomposition_rpf[[x$col]]
  decomposition_rpf[[x$col]] <- NULL
}

quartale_short <- seq(as.Date("2017-10-01"), as.Date("2019-07-01"), by = "quarter")
rows_short <- which(decomposition_rpf$Quarter %in% quartale_short)

subset_wage  <- subset(Wage_equation, Quarter %in% quartale_short)
subset_price <- subset(price_equation,  Quarter %in% quartale_short)
subset_cf5   <- subset(cf5_equation,             Quarter %in% quartale_short)

decomposition_rpf$gw_sim_rpf[rows_short]  <- subset_wage$delta_w
decomposition_rpf$cf1_sim_rpf[rows_short] <- subset_wage$CF1
decomposition_rpf$cu[rows_short]          <- subset_wage$CU
decomposition_rpf$gp_sim_rpf[rows_short]  <- subset_price$delta_p
decomposition_rpf$cf3_sim_rpf[rows_short] <- subset_cf5$CF5

if ("mapty.x" %in% names(decomposition_rpf)) decomposition_rpf$mapty.x <- NULL
if ("mapty.y" %in% names(decomposition_rpf)) names(decomposition_rpf)[names(decomposition_rpf) == "mapty.y"] <- "mapty"

if ("shortage.x" %in% names(decomposition_rpf)) decomposition_rpf$shortage.x <- NULL
if ("shortage.y" %in% names(decomposition_rpf)) names(decomposition_rpf)[names(decomposition_rpf) == "shortage.y"] <- "shortage"

decomposition_rpf <- decomposition_rpf %>% select(-mapty)
decomposition_rpf <- decomposition_rpf %>%
  mutate(rpf = if_else(Quarter >= as.Date("2019-10-01"), 0, rpf))

start_quarter <- as.Date("2019-10-01")
end_quarter   <- as.Date("2023-01-01")
row_start <- which(decomposition_rpf$Quarter == start_quarter)
row_end   <- which(decomposition_rpf$Quarter == end_quarter)

for (row_t in row_start:row_end) {

  decomposition_rpf$gw_sim_rpf[row_t] <- wage_coeff_intercept +
    wage_coeff_wage_delta_w_lag1 * decomposition_rpf$gw_sim_rpf[row_t - 1] +
    wage_coeff_wage_delta_w_lag2 * decomposition_rpf$gw_sim_rpf[row_t - 2] +
    wage_coeff_wage_delta_w_lag3 * decomposition_rpf$gw_sim_rpf[row_t - 3] +
    wage_coeff_wage_delta_w_lag4 * decomposition_rpf$gw_sim_rpf[row_t - 4] +
    
    wage_coeff_CF1_lag1 * decomposition_rpf$cf1_sim_rpf[row_t - 1] +
    wage_coeff_CF1_lag2 * decomposition_rpf$cf1_sim_rpf[row_t - 2] +
    wage_coeff_CF1_lag3 * decomposition_rpf$cf1_sim_rpf[row_t - 3] +
    wage_coeff_CF1_lag4 * decomposition_rpf$cf1_sim_rpf[row_t - 4] +
    
    wage_coeff_CU_lag1 * decomposition_rpf$cu[row_t - 1] +
    wage_coeff_CU_lag2 * decomposition_rpf$cu[row_t - 2] +
    wage_coeff_CU_lag3 * decomposition_rpf$cu[row_t - 3] +
    wage_coeff_CU_lag4 * decomposition_rpf$cu[row_t - 4] +
    
    wage_coeff_VU_lag1 * decomposition_rpf$vu[row_t - 1] +
    wage_coeff_VU_lag2 * decomposition_rpf$vu[row_t - 2] +
    wage_coeff_VU_lag3 * decomposition_rpf$vu[row_t - 3] +
    wage_coeff_VU_lag4 * decomposition_rpf$vu[row_t - 4]
  
  decomposition_rpf$gp_sim_rpf[row_t] <- price_coeff_intercept +
    price_coeff_delta_p_lag1 * decomposition_rpf$gp_sim_rpf[row_t - 1] +
    price_coeff_delta_p_lag2 * decomposition_rpf$gp_sim_rpf[row_t - 2] +
    price_coeff_delta_p_lag3 * decomposition_rpf$gp_sim_rpf[row_t - 3] +
    price_coeff_delta_p_lag4 * decomposition_rpf$gp_sim_rpf[row_t - 4] +
    price_coeff_delta_w_lag0 * decomposition_rpf$gw_sim_rpf[row_t] +
    price_coeff_delta_w_lag1 * decomposition_rpf$gw_sim_rpf[row_t - 1] +
    price_coeff_delta_w_lag2 * decomposition_rpf$gw_sim_rpf[row_t - 2] +
    price_coeff_delta_w_lag3 * decomposition_rpf$gw_sim_rpf[row_t - 3] +
    price_coeff_delta_w_lag4 * decomposition_rpf$gw_sim_rpf[row_t - 4] +
    price_coeff_RPE_0 * decomposition_rpf$rpe[row_t] +
    price_coeff_RPE_1 * decomposition_rpf$rpe[row_t - 1] +
    price_coeff_RPE_2 * decomposition_rpf$rpe[row_t - 2] +
    price_coeff_RPE_3 * decomposition_rpf$rpe[row_t - 3] +
    price_coeff_RPE_4 * decomposition_rpf$rpe[row_t - 4] +
    price_coeff_RPF_0 * decomposition_rpf$rpf[row_t] +
    price_coeff_RPF_1 * decomposition_rpf$rpf[row_t - 1] +
    price_coeff_RPF_2 * decomposition_rpf$rpf[row_t - 2] +
    price_coeff_RPF_3 * decomposition_rpf$rpf[row_t - 3] +
    price_coeff_RPF_4 * decomposition_rpf$rpf[row_t - 4] +
    price_coeff_shortage_0 * decomposition_rpf$shortage[row_t] +
    price_coeff_shortage_1 * decomposition_rpf$shortage[row_t - 1] +
    price_coeff_shortage_2 * decomposition_rpf$shortage[row_t - 2] +
    price_coeff_shortage_3 * decomposition_rpf$shortage[row_t - 3] +
    price_coeff_shortage_4 * decomposition_rpf$shortage[row_t - 4]
  
  decomposition_rpf$cu[row_t] <-
    mean(decomposition_rpf$gp_sim_rpf[(row_t-1):(row_t-4)]) -
    decomposition_rpf$cf1_sim_rpf[row_t-4]
  
  decomposition_rpf$cf3_sim_rpf[row_t] <-
    CF5_coeff_cf5_lag1 * decomposition_rpf$cf3_sim_rpf[row_t - 1] +
    CF5_coeff_cf5_lag2 * decomposition_rpf$cf3_sim_rpf[row_t - 2] +
    CF5_coeff_cf5_lag3 * decomposition_rpf$cf3_sim_rpf[row_t - 3] +
    CF5_coeff_cf5_lag4 * decomposition_rpf$cf3_sim_rpf[row_t - 4] +
    CF5_coeff_deltap_0 * decomposition_rpf$gp_sim_rpf[row_t] +
    CF5_coeff_deltap_1 * decomposition_rpf$gp_sim_rpf[row_t - 1] +
    CF5_coeff_deltap_2 * decomposition_rpf$gp_sim_rpf[row_t - 2] +
    CF5_coeff_deltap_3 * decomposition_rpf$gp_sim_rpf[row_t - 3] +
    CF5_coeff_deltap_4 * decomposition_rpf$gp_sim_rpf[row_t - 4]
  
  decomposition_rpf$cf1_sim_rpf[row_t] <-
    CF1_coeff_cf1_lag1 * decomposition_rpf$cf1_sim_rpf[row_t - 1] +
    CF1_coeff_cf1_lag2 * decomposition_rpf$cf1_sim_rpf[row_t - 2] +
    CF1_coeff_cf1_lag3 * decomposition_rpf$cf1_sim_rpf[row_t - 3] +
    CF1_coeff_cf1_lag4 * decomposition_rpf$cf1_sim_rpf[row_t - 4] +
    CF1_coeff_CF5_0 * decomposition_rpf$cf3_sim_rpf[row_t] +
    CF1_coeff_CF5_1 * decomposition_rpf$cf3_sim_rpf[row_t - 1] +
    CF1_coeff_CF5_2 * decomposition_rpf$cf3_sim_rpf[row_t - 2] +
    CF1_coeff_CF5_3 * decomposition_rpf$cf3_sim_rpf[row_t - 3] +
    CF1_coeff_CF5_4 * decomposition_rpf$cf3_sim_rpf[row_t - 4] +
    CF1_coeff_deltap_0 * decomposition_rpf$gp_sim_rpf[row_t] +
    CF1_coeff_deltap_1 * decomposition_rpf$gp_sim_rpf[row_t - 1] +
    CF1_coeff_deltap_2 * decomposition_rpf$gp_sim_rpf[row_t - 2] +
    CF1_coeff_deltap_3 * decomposition_rpf$gp_sim_rpf[row_t - 3] +
    CF1_coeff_deltap_4 * decomposition_rpf$gp_sim_rpf[row_t - 4]
} 
# Decomposition shortage Removed --------------------------------------------------
cf5_equation <- long_term %>%
  select(Quarter, CF5) %>%
  left_join(delta_p %>% select(Quarter, delta_p), by = "Quarter")

quarter_dates <- seq(as.Date("2017-10-01"), as.Date("2023-01-01"), by = "quarter")

decomposition_shortage <- data.frame(
  Quarter      = quarter_dates,
  gw_sim_shortage  = 0, gp_sim_shortage  = 0,
  cf1_sim_shortage = 0, cf3_sim_shortage = 0,
  rpe              = 0, rpf              = 0,
  vu               = 0, shortage         = 0,
  mapty            = 0, cu               = 0
)

for(x in list(
  list(df = VU,            col = "VU",          target = "vu"),
  list(df = productivity,  col = "mapty",       target = "mapty"),
  list(df = RPE_Data,      col = "RPE",         target = "rpe"),
  list(df = RPF_Data,      col = "RPF",         target = "rpf"),
  list(df = shortages,     col = "shortage",    target = "shortage")
)) {
  decomposition_shortage <- merge(
    decomposition_shortage, x$df[, c("Quarter", x$col)],
    by = "Quarter", all.x = TRUE
  )
  decomposition_shortage[[x$target]] <- decomposition_shortage[[x$col]]
  decomposition_shortage[[x$col]] <- NULL
}

quartale_short <- seq(as.Date("2017-10-01"), as.Date("2019-07-01"), by = "quarter")
rows_short <- which(decomposition_shortage$Quarter %in% quartale_short)

subset_wage  <- subset(Wage_equation, Quarter %in% quartale_short)
subset_price <- subset(price_equation,  Quarter %in% quartale_short)
subset_cf5   <- subset(cf5_equation,             Quarter %in% quartale_short)

decomposition_shortage$gw_sim_shortage[rows_short]  <- subset_wage$delta_w
decomposition_shortage$cf1_sim_shortage[rows_short] <- subset_wage$CF1
decomposition_shortage$cu[rows_short]               <- subset_wage$CU
decomposition_shortage$gp_sim_shortage[rows_short]  <- subset_price$delta_p
decomposition_shortage$cf3_sim_shortage[rows_short] <- subset_cf5$CF5

if ("mapty.x" %in% names(decomposition_shortage)) decomposition_shortage$mapty.x <- NULL
if ("mapty.y" %in% names(decomposition_shortage)) names(decomposition_shortage)[names(decomposition_shortage) == "mapty.y"] <- "mapty"

if ("shortage.x" %in% names(decomposition_shortage)) decomposition_shortage$shortage.x <- NULL
if ("shortage.y" %in% names(decomposition_shortage)) names(decomposition_shortage)[names(decomposition_shortage) == "shortage.y"] <- "shortage"

decomposition_shortage <- decomposition_shortage %>% select(-mapty)
decomposition_shortage <- decomposition_shortage %>%
  mutate(shortage = if_else(Quarter >= as.Date("2019-10-01"), 5, shortage))

start_quarter <- as.Date("2019-10-01")
end_quarter   <- as.Date("2023-01-01")
row_start <- which(decomposition_shortage$Quarter == start_quarter)
row_end   <- which(decomposition_shortage$Quarter == end_quarter)

for (row_t in row_start:row_end) {

  decomposition_shortage$gw_sim_shortage[row_t] <- wage_coeff_intercept +
    wage_coeff_wage_delta_w_lag1 * decomposition_shortage$gw_sim_shortage[row_t - 1] +
    wage_coeff_wage_delta_w_lag2 * decomposition_shortage$gw_sim_shortage[row_t - 2] +
    wage_coeff_wage_delta_w_lag3 * decomposition_shortage$gw_sim_shortage[row_t - 3] +
    wage_coeff_wage_delta_w_lag4 * decomposition_shortage$gw_sim_shortage[row_t - 4] +
    
    wage_coeff_CF1_lag1 * decomposition_shortage$cf1_sim_shortage[row_t - 1] +
    wage_coeff_CF1_lag2 * decomposition_shortage$cf1_sim_shortage[row_t - 2] +
    wage_coeff_CF1_lag3 * decomposition_shortage$cf1_sim_shortage[row_t - 3] +
    wage_coeff_CF1_lag4 * decomposition_shortage$cf1_sim_shortage[row_t - 4] +
    
    wage_coeff_CU_lag1 * decomposition_shortage$cu[row_t - 1] +
    wage_coeff_CU_lag2 * decomposition_shortage$cu[row_t - 2] +
    wage_coeff_CU_lag3 * decomposition_shortage$cu[row_t - 3] +
    wage_coeff_CU_lag4 * decomposition_shortage$cu[row_t - 4] +
    
    wage_coeff_VU_lag1 * decomposition_shortage$vu[row_t - 1] +
    wage_coeff_VU_lag2 * decomposition_shortage$vu[row_t - 2] +
    wage_coeff_VU_lag3 * decomposition_shortage$vu[row_t - 3] +
    wage_coeff_VU_lag4 * decomposition_shortage$vu[row_t - 4]
  
  decomposition_shortage$gp_sim_shortage[row_t] <- price_coeff_intercept +
    price_coeff_delta_p_lag1 * decomposition_shortage$gp_sim_shortage[row_t - 1] +
    price_coeff_delta_p_lag2 * decomposition_shortage$gp_sim_shortage[row_t - 2] +
    price_coeff_delta_p_lag3 * decomposition_shortage$gp_sim_shortage[row_t - 3] +
    price_coeff_delta_p_lag4 * decomposition_shortage$gp_sim_shortage[row_t - 4] +
    price_coeff_delta_w_lag0 * decomposition_shortage$gw_sim_shortage[row_t] +
    price_coeff_delta_w_lag1 * decomposition_shortage$gw_sim_shortage[row_t - 1] +
    price_coeff_delta_w_lag2 * decomposition_shortage$gw_sim_shortage[row_t - 2] +
    price_coeff_delta_w_lag3 * decomposition_shortage$gw_sim_shortage[row_t - 3] +
    price_coeff_delta_w_lag4 * decomposition_shortage$gw_sim_shortage[row_t - 4] +
    price_coeff_RPE_0 * decomposition_shortage$rpe[row_t] +
    price_coeff_RPE_1 * decomposition_shortage$rpe[row_t - 1] +
    price_coeff_RPE_2 * decomposition_shortage$rpe[row_t - 2] +
    price_coeff_RPE_3 * decomposition_shortage$rpe[row_t - 3] +
    price_coeff_RPE_4 * decomposition_shortage$rpe[row_t - 4] +
    price_coeff_RPF_0 * decomposition_shortage$rpf[row_t] +
    price_coeff_RPF_1 * decomposition_shortage$rpf[row_t - 1] +
    price_coeff_RPF_2 * decomposition_shortage$rpf[row_t - 2] +
    price_coeff_RPF_3 * decomposition_shortage$rpf[row_t - 3] +
    price_coeff_RPF_4 * decomposition_shortage$rpf[row_t - 4] +
    price_coeff_shortage_0 * decomposition_shortage$shortage[row_t] +
    price_coeff_shortage_1 * decomposition_shortage$shortage[row_t - 1] +
    price_coeff_shortage_2 * decomposition_shortage$shortage[row_t - 2] +
    price_coeff_shortage_3 * decomposition_shortage$shortage[row_t - 3] +
    price_coeff_shortage_4 * decomposition_shortage$shortage[row_t - 4]
  
  decomposition_shortage$cu[row_t] <-
    mean(decomposition_shortage$gp_sim_shortage[(row_t-1):(row_t-4)]) -
    decomposition_shortage$cf1_sim_shortage[row_t-4]
  
  decomposition_shortage$cf3_sim_shortage[row_t] <-
    CF5_coeff_cf5_lag1 * decomposition_shortage$cf3_sim_shortage[row_t - 1] +
    CF5_coeff_cf5_lag2 * decomposition_shortage$cf3_sim_shortage[row_t - 2] +
    CF5_coeff_cf5_lag3 * decomposition_shortage$cf3_sim_shortage[row_t - 3] +
    CF5_coeff_cf5_lag4 * decomposition_shortage$cf3_sim_shortage[row_t - 4] +
    CF5_coeff_deltap_0 * decomposition_shortage$gp_sim_shortage[row_t] +
    CF5_coeff_deltap_1 * decomposition_shortage$gp_sim_shortage[row_t - 1] +
    CF5_coeff_deltap_2 * decomposition_shortage$gp_sim_shortage[row_t - 2] +
    CF5_coeff_deltap_3 * decomposition_shortage$gp_sim_shortage[row_t - 3] +
    CF5_coeff_deltap_4 * decomposition_shortage$gp_sim_shortage[row_t - 4]
  
  decomposition_shortage$cf1_sim_shortage[row_t] <-
    CF1_coeff_cf1_lag1 * decomposition_shortage$cf1_sim_shortage[row_t - 1] +
    CF1_coeff_cf1_lag2 * decomposition_shortage$cf1_sim_shortage[row_t - 2] +
    CF1_coeff_cf1_lag3 * decomposition_shortage$cf1_sim_shortage[row_t - 3] +
    CF1_coeff_cf1_lag4 * decomposition_shortage$cf1_sim_shortage[row_t - 4] +
    CF1_coeff_CF5_0 * decomposition_shortage$cf3_sim_shortage[row_t] +
    CF1_coeff_CF5_1 * decomposition_shortage$cf3_sim_shortage[row_t - 1] +
    CF1_coeff_CF5_2 * decomposition_shortage$cf3_sim_shortage[row_t - 2] +
    CF1_coeff_CF5_3 * decomposition_shortage$cf3_sim_shortage[row_t - 3] +
    CF1_coeff_CF5_4 * decomposition_shortage$cf3_sim_shortage[row_t - 4] +
    CF1_coeff_deltap_0 * decomposition_shortage$gp_sim_shortage[row_t] +
    CF1_coeff_deltap_1 * decomposition_shortage$gp_sim_shortage[row_t - 1] +
    CF1_coeff_deltap_2 * decomposition_shortage$gp_sim_shortage[row_t - 2] +
    CF1_coeff_deltap_3 * decomposition_shortage$gp_sim_shortage[row_t - 3] +
    CF1_coeff_deltap_4 * decomposition_shortage$gp_sim_shortage[row_t - 4]
}

# Decomposition VU Removed --------------------------------------------------------
cf5_equation <- long_term %>%
  select(Quarter, CF5) %>%
  left_join(delta_p %>% select(Quarter, delta_p), by = "Quarter")

quarter_dates <- seq(as.Date("2017-10-01"), as.Date("2023-01-01"), by = "quarter")

decomposition_vu <- data.frame(
  Quarter      = quarter_dates,
  gw_sim_vu  = 0, gp_sim_vu  = 0,
  cf1_sim_vu = 0, cf3_sim_vu = 0,
  rpe          = 0, rpf          = 0,
  vu           = 0, shortage     = 0,
  mapty        = 0, cu           = 0
)

for(x in list(
  list(df = VU,            col = "VU",          target = "vu"),
  list(df = productivity,  col = "mapty",       target = "mapty"),
  list(df = RPE_Data,      col = "RPE",         target = "rpe"),
  list(df = RPF_Data,      col = "RPF",         target = "rpf"),
  list(df = shortages,     col = "shortage",    target = "shortage")
)) {
  decomposition_vu <- merge(
    decomposition_vu, x$df[, c("Quarter", x$col)],
    by = "Quarter", all.x = TRUE
  )
  decomposition_vu[[x$target]] <- decomposition_vu[[x$col]]
  decomposition_vu[[x$col]] <- NULL
}

quartale_short <- seq(as.Date("2017-10-01"), as.Date("2019-07-01"), by = "quarter")
rows_short <- which(decomposition_vu$Quarter %in% quartale_short)

subset_wage  <- subset(Wage_equation, Quarter %in% quartale_short)
subset_price <- subset(price_equation,  Quarter %in% quartale_short)
subset_cf5   <- subset(cf5_equation,             Quarter %in% quartale_short)

decomposition_vu$gw_sim_vu[rows_short]  <- subset_wage$delta_w
decomposition_vu$cf1_sim_vu[rows_short] <- subset_wage$CF1
decomposition_vu$cu[rows_short]         <- subset_wage$CU
decomposition_vu$gp_sim_vu[rows_short]  <- subset_price$delta_p
decomposition_vu$cf3_sim_vu[rows_short] <- subset_cf5$CF5

if ("mapty.x" %in% names(decomposition_vu)) decomposition_vu$mapty.x <- NULL
if ("mapty.y" %in% names(decomposition_vu)) names(decomposition_vu)[names(decomposition_vu) == "mapty.y"] <- "mapty"

if ("shortage.x" %in% names(decomposition_vu)) decomposition_vu$shortage.x <- NULL
if ("shortage.y" %in% names(decomposition_vu)) names(decomposition_vu)[names(decomposition_vu) == "shortage.y"] <- "shortage"

decomposition_vu <- decomposition_vu %>% select(-mapty)

vu_shock_value <- VU %>% filter(Quarter == as.Date("2019-10-01")) %>% pull(VU)
decomposition_vu <- decomposition_vu %>%
  mutate(vu = if_else(Quarter >= as.Date("2019-10-01"), vu_shock_value, vu))



start_quarter <- as.Date("2019-10-01")
end_quarter   <- as.Date("2023-01-01")
row_start <- which(decomposition_vu$Quarter == start_quarter)
row_end   <- which(decomposition_vu$Quarter == end_quarter)

for (row_t in row_start:row_end) {
  
  decomposition_vu$gw_sim_vu[row_t] <- wage_coeff_intercept +
    wage_coeff_wage_delta_w_lag1 * decomposition_vu$gw_sim_vu[row_t - 1] +
    wage_coeff_wage_delta_w_lag2 * decomposition_vu$gw_sim_vu[row_t - 2] +
    wage_coeff_wage_delta_w_lag3 * decomposition_vu$gw_sim_vu[row_t - 3] +
    wage_coeff_wage_delta_w_lag4 * decomposition_vu$gw_sim_vu[row_t - 4] +
    
    wage_coeff_CF1_lag1 * decomposition_vu$cf1_sim_vu[row_t - 1] +
    wage_coeff_CF1_lag2 * decomposition_vu$cf1_sim_vu[row_t - 2] +
    wage_coeff_CF1_lag3 * decomposition_vu$cf1_sim_vu[row_t - 3] +
    wage_coeff_CF1_lag4 * decomposition_vu$cf1_sim_vu[row_t - 4] +
    
    wage_coeff_CU_lag1 * decomposition_vu$cu[row_t - 1] +
    wage_coeff_CU_lag2 * decomposition_vu$cu[row_t - 2] +
    wage_coeff_CU_lag3 * decomposition_vu$cu[row_t - 3] +
    wage_coeff_CU_lag4 * decomposition_vu$cu[row_t - 4] +
    
    wage_coeff_VU_lag1 * decomposition_vu$vu[row_t - 1] +
    wage_coeff_VU_lag2 * decomposition_vu$vu[row_t - 2] +
    wage_coeff_VU_lag3 * decomposition_vu$vu[row_t - 3] +
    wage_coeff_VU_lag4 * decomposition_vu$vu[row_t - 4]
  
  decomposition_vu$gp_sim_vu[row_t] <- price_coeff_intercept +
    price_coeff_delta_p_lag1 * decomposition_vu$gp_sim_vu[row_t - 1] +
    price_coeff_delta_p_lag2 * decomposition_vu$gp_sim_vu[row_t - 2] +
    price_coeff_delta_p_lag3 * decomposition_vu$gp_sim_vu[row_t - 3] +
    price_coeff_delta_p_lag4 * decomposition_vu$gp_sim_vu[row_t - 4] +
    price_coeff_delta_w_lag0 * decomposition_vu$gw_sim_vu[row_t] +
    price_coeff_delta_w_lag1 * decomposition_vu$gw_sim_vu[row_t - 1] +
    price_coeff_delta_w_lag2 * decomposition_vu$gw_sim_vu[row_t - 2] +
    price_coeff_delta_w_lag3 * decomposition_vu$gw_sim_vu[row_t - 3] +
    price_coeff_delta_w_lag4 * decomposition_vu$gw_sim_vu[row_t - 4] +
    price_coeff_RPE_0 * decomposition_vu$rpe[row_t] +
    price_coeff_RPE_1 * decomposition_vu$rpe[row_t - 1] +
    price_coeff_RPE_2 * decomposition_vu$rpe[row_t - 2] +
    price_coeff_RPE_3 * decomposition_vu$rpe[row_t - 3] +
    price_coeff_RPE_4 * decomposition_vu$rpe[row_t - 4] +
    price_coeff_RPF_0 * decomposition_vu$rpf[row_t] +
    price_coeff_RPF_1 * decomposition_vu$rpf[row_t - 1] +
    price_coeff_RPF_2 * decomposition_vu$rpf[row_t - 2] +
    price_coeff_RPF_3 * decomposition_vu$rpf[row_t - 3] +
    price_coeff_RPF_4 * decomposition_vu$rpf[row_t - 4] +
    price_coeff_shortage_0 * decomposition_vu$shortage[row_t] +
    price_coeff_shortage_1 * decomposition_vu$shortage[row_t - 1] +
    price_coeff_shortage_2 * decomposition_vu$shortage[row_t - 2] +
    price_coeff_shortage_3 * decomposition_vu$shortage[row_t - 3] +
    price_coeff_shortage_4 * decomposition_vu$shortage[row_t - 4]
  
  decomposition_vu$cu[row_t] <-
    mean(decomposition_vu$gp_sim_vu[(row_t-1):(row_t-4)]) -
    decomposition_vu$cf1_sim_vu[row_t-4]
  
  decomposition_vu$cf3_sim_vu[row_t] <-
    CF5_coeff_cf5_lag1 * decomposition_vu$cf3_sim_vu[row_t - 1] +
    CF5_coeff_cf5_lag2 * decomposition_vu$cf3_sim_vu[row_t - 2] +
    CF5_coeff_cf5_lag3 * decomposition_vu$cf3_sim_vu[row_t - 3] +
    CF5_coeff_cf5_lag4 * decomposition_vu$cf3_sim_vu[row_t - 4] +
    CF5_coeff_deltap_0 * decomposition_vu$gp_sim_vu[row_t] +
    CF5_coeff_deltap_1 * decomposition_vu$gp_sim_vu[row_t - 1] +
    CF5_coeff_deltap_2 * decomposition_vu$gp_sim_vu[row_t - 2] +
    CF5_coeff_deltap_3 * decomposition_vu$gp_sim_vu[row_t - 3] +
    CF5_coeff_deltap_4 * decomposition_vu$gp_sim_vu[row_t - 4]
  
  decomposition_vu$cf1_sim_vu[row_t] <-
    CF1_coeff_cf1_lag1 * decomposition_vu$cf1_sim_vu[row_t - 1] +
    CF1_coeff_cf1_lag2 * decomposition_vu$cf1_sim_vu[row_t - 2] +
    CF1_coeff_cf1_lag3 * decomposition_vu$cf1_sim_vu[row_t - 3] +
    CF1_coeff_cf1_lag4 * decomposition_vu$cf1_sim_vu[row_t - 4] +
    CF1_coeff_CF5_0 * decomposition_vu$cf3_sim_vu[row_t] +
    CF1_coeff_CF5_1 * decomposition_vu$cf3_sim_vu[row_t - 1] +
    CF1_coeff_CF5_2 * decomposition_vu$cf3_sim_vu[row_t - 2] +
    CF1_coeff_CF5_3 * decomposition_vu$cf3_sim_vu[row_t - 3] +
    CF1_coeff_CF5_4 * decomposition_vu$cf3_sim_vu[row_t - 4] +
    CF1_coeff_deltap_0 * decomposition_vu$gp_sim_vu[row_t] +
    CF1_coeff_deltap_1 * decomposition_vu$gp_sim_vu[row_t - 1] +
    CF1_coeff_deltap_2 * decomposition_vu$gp_sim_vu[row_t - 2] +
    CF1_coeff_deltap_3 * decomposition_vu$gp_sim_vu[row_t - 3] +
    CF1_coeff_deltap_4 * decomposition_vu$gp_sim_vu[row_t - 4]
}

# Contribution ------------------------------------------------------------

composition <- data.frame(
  Quarter = decomposition_normal$Quarter,
  gw_sim_orig     = decomposition_normal$gw_sim_orig,
  gw_sim_rpe      = decomposition_rpe$gw_sim_rpe,
  gw_sim_rpf      = decomposition_rpf$gw_sim_rpf,
  gw_sim_shortage = decomposition_shortage$gw_sim_shortage,
  gw_sim_vu       = decomposition_vu$gw_sim_vu,
  gp_sim_orig     = decomposition_normal$gp_sim_orig,
  gp_sim_rpe      = decomposition_rpe$gp_sim_rpe,
  gp_sim_rpf      = decomposition_rpf$gp_sim_rpf,
  gp_sim_shortage = decomposition_shortage$gp_sim_shortage,
  gp_sim_vu       = decomposition_vu$gp_sim_vu
)

for (type in c("rpe","rpf","shortage","vu")) {
  composition[[paste0("gw_contr_", type)]] <- composition$gw_sim_orig - composition[[paste0("gw_sim_", type)]]
  composition[[paste0("gp_contr_", type)]] <- composition$gp_sim_orig - composition[[paste0("gp_sim_", type)]]
}

composition <- composition[, c(
  "Quarter",
  "gw_sim_orig",      "gw_sim_rpe",      "gw_contr_rpe",
  "gw_sim_rpf",       "gw_contr_rpf",
  "gw_sim_shortage",  "gw_contr_shortage",
  "gw_sim_vu",        "gw_contr_vu",
  "gp_sim_orig",      "gp_sim_rpe",      "gp_contr_rpe",
  "gp_sim_rpf",       "gp_contr_rpf",
  "gp_sim_shortage",  "gp_contr_shortage",
  "gp_sim_vu",        "gp_contr_vu"
)]

composition$sum_contr_gw        <- rowSums(composition[, grep("^gw_contr_", names(composition))])
composition$sum_contr_gp        <- rowSums(composition[, grep("^gp_contr_", names(composition))])
composition$initial_conditions_gw <- composition$gw_sim_orig - composition$sum_contr_gw
composition$initial_conditions_gp <- composition$gp_sim_orig - composition$sum_contr_gp

composition$Quarter <- as.Date(composition$Quarter)
composition$actual_gp <- price_equation$delta_p[match(composition$Quarter, as.Date(price_equation$Quarter))]
composition$actual_gw <- delta_w$delta_w[match(composition$Quarter, as.Date(delta_w$Quarter))]

composition <- composition %>%
  filter(Quarter >= as.Date("2019-10-01"))
# Decomposition Price Inflation -------------------------------------------

composition$Quarter <- as.Date(composition$Quarter)
x_labels_fmt <- format(as.yearqtr(composition$Quarter), "Q%q %Y")

composition_long <- composition %>%
  select(
    Quarter,
    initial_conditions_gp,
    gp_contr_vu,
    gp_contr_rpe,
    gp_contr_rpf,
    gp_contr_shortage
  ) %>%
  pivot_longer(
    cols = -Quarter,
    names_to = "Component",
    values_to = "Contribution"
  ) %>%
  mutate(
    Component = factor(
      Component,
      levels = c("initial_conditions_gp", "gp_contr_vu", "gp_contr_rpe", "gp_contr_rpf", "gp_contr_shortage"),
      labels = c("Initial conditions", "v/u", "Energy Prices", "Food Prices", "Shortages")
    )
  )

gp_plot <- ggplot() +
  geom_col(
    data = composition_long,
    aes(x = Quarter, y = Contribution, fill = Component),
    position = position_stack(reverse = TRUE),
    width = 80
  ) +
  geom_line(
    data = composition,
    aes(x = Quarter, y = actual_gp),
    color = "black", size = 1
  ) +
  scale_fill_manual(
    values = c("gray40", "darkred", "steelblue", "darkblue", "gold"),
    name = NULL
  ) +
  scale_x_date(
    breaks = composition$Quarter,
    labels = x_labels_fmt,
    expand = c(0.01, 0.01)
  ) +
  scale_y_continuous(
    name = "Percent",
    breaks = seq(-4, 10, by = 2),
    limits = c(-4, 10)
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 9),           
    legend.key.size = unit(0.6, "lines"),           
    legend.spacing.x = unit(0.4, "cm"),              
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(10, 20, 10, 10)
  )

print(gp_plot)


# Decomposition Wage Inflation --------------------------------------------

composition$Quarter <- as.Date(composition$Quarter)
x_labels_fmt <- format(as.yearqtr(composition$Quarter), "Q%q %Y")

composition_long_wage <- composition %>%
  select(
    Quarter,
    initial_conditions_gw,
    gw_contr_vu,
    gw_contr_rpe,
    gw_contr_rpf,
    gw_contr_shortage
  ) %>%
  pivot_longer(
    cols = -Quarter,
    names_to = "Component",
    values_to = "Contribution"
  ) %>%
  mutate(
    Component = factor(
      Component,
      levels = c("initial_conditions_gw", "gw_contr_vu", "gw_contr_rpe", "gw_contr_rpf", "gw_contr_shortage"),
      labels = c("Initial conditions", "v/u", "Energy Prices", "Food Prices", "Shortages")
    )
  )

gw_plot <- ggplot() +
  geom_col(
    data = composition_long_wage,
    aes(x = Quarter, y = Contribution, fill = Component),
    position = position_stack(reverse = TRUE),
    width = 80
  ) +
  geom_line(
    data = composition,
    aes(x = Quarter, y = actual_gw),
    color = "black", size = 1
  ) +
  scale_fill_manual(
    values = c("gray40", "darkred", "steelblue", "darkblue", "gold"),
    name = NULL
  ) +
  scale_x_date(
    breaks = composition$Quarter,
    labels = x_labels_fmt,
    expand = c(0.01, 0.01)
  ) +
  scale_y_continuous(
    name = "Percent",
    breaks = seq(-4, 6.5, by = 2),
    limits = c(-4, 6.5)
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 9),
    legend.key.size = unit(0.6, "lines"),
    legend.spacing.x = unit(0.4, "cm"),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(10, 20, 10, 10)
  )

print(gw_plot)


