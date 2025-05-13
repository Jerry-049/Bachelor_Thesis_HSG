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

cpi_quartal <- read_excel("HSG BA/R/Daten/CH/CPI_CH_OECD.xlsx") %>%
  mutate(
    Index = as.double(Index),
    Quarter = as.Date(Quarter)
  ) %>%
  arrange(Quarter) %>%
  mutate(
    log_index = log(Index),
    delta_p =  100*(log_index - lag(log_index,4))
  )

cpi_quartal_filter <- cpi_quartal %>% 
  filter(Quarter >= as.Date("2010-01-01") & Quarter <= as.Date("2023-01-01"))



corona_period <- cpi_quartal_filter %>%
  filter(Quarter >= as.Date("2020-01-01") & Quarter <= as.Date("2023-01-01"))

min_val_corona <- min(corona_period$delta_p, na.rm = TRUE)
max_val_corona <- max(corona_period$delta_p, na.rm = TRUE)

ggplot(cpi_quartal_filter, aes(x = Quarter)) +
  geom_line(aes(y = delta_p), color = "black", linewidth = 0.7) +
  annotate("rect",
           xmin = as.Date("2020-01-01"), xmax = as.Date("2023-01-01"),
           ymin = -Inf, ymax = Inf,
           alpha = 0.2, fill = "red") +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
  geom_hline(yintercept = min_val_corona, linetype = "dashed", color = "darkgray") +
  geom_hline(yintercept = max_val_corona, linetype = "dashed", color = "darkgray") +
  labs(
    x = NULL,
    y = "Percent",
    caption = "Source: OECD and own calculations. Deseasonalized Data. \nNote: Red shading indicates the COVID-19 pandemic period."
  ) +
  theme_minimal(base_family = "serif") +
  theme(
    panel.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(color = "black", size = 10),
    axis.ticks = element_line(color = "black"),
    plot.margin = margin(10, 10, 10, 10),
    legend.position = "none",
    plot.caption = element_text(hjust = 0, size = 9, margin = margin(t = 10))
  ) +
  scale_y_continuous(limits = c(-2, 4), breaks = seq(-2, 4, 1)) +
  scale_x_date(
    breaks = seq(from = min(cpi_quartal_filter$Quarter),
                 to = max(cpi_quartal_filter$Quarter),
                 by = "5 years"),
    labels = date_format("%Y")
  )


delta_p <- cpi_quartal
# Delta Wage  ------------------------------------------------------------
lohn_quartal <- read_excel("HSG BA/R/Daten/CH/Wages_constructed_CH.xlsx") %>%
  mutate(
    wages = as.double(wages),
    Quarter = as.Date(Quarter)
  ) %>%
  arrange(Quarter) %>%
  mutate(
    log_wages = log(wages),
    delta_w = 100* (log_wages - lag(log_wages,4))
  )

lohn_quartal_filter <- lohn_quartal %>%
  filter(Quarter >= as.Date("2010-10-01") & Quarter <= as.Date("2023-01-01"))

corona_period_lohn <- lohn_quartal_filter %>%
  filter(Quarter >= as.Date("2020-01-01") & Quarter <= as.Date("2023-01-01"))

min_val_lohn <- min(corona_period_lohn$delta_w, na.rm = TRUE)
max_val_lohn <- max(corona_period_lohn$delta_w, na.rm = TRUE)

ggplot(lohn_quartal_filter, aes(x = Quarter)) +
  geom_line(aes(y = delta_w), color = "steelblue", linewidth = 0.7) +
  annotate("rect",
           xmin = as.Date("2020-01-01"), xmax = as.Date("2023-01-01"),
           ymin = -Inf, ymax = Inf,
           alpha = 0.2, fill = "red") +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
  geom_hline(yintercept = min_val_lohn, linetype = "dashed", color = "darkgray") +
  geom_hline(yintercept = max_val_lohn, linetype = "dashed", color = "darkgray") +
  labs(
    x = NULL,
    y = "Percent",
    caption = "Source: SECO, BFS and own calculations. \nNote: Red shading indicates the COVID-19 pandemic period."
  ) +
  theme_minimal(base_family = "serif") +
  theme(
    panel.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(color = "black", size = 10),
    axis.ticks = element_line(color = "black"),
    plot.margin = margin(10, 10, 10, 10),
    legend.position = "none",
    plot.caption = element_text(hjust = 0, size = 9, margin = margin(t = 10))
  ) +
  scale_y_continuous(limits = c(-2, 4), breaks = seq(-2, 4, 1)) +
  scale_x_date(
    breaks = seq(from = min(lohn_quartal_filter$Quarter),
                 to = max(lohn_quartal_filter$Quarter),
                 by = "5 years"),
    labels = date_format("%Y")
  )

delta_w <- lohn_quartal
# CPI Energy  -------------------------------------------------------
cpi_energy <- read_excel("HSG BA/R/Daten/CH/CPI_Energy_CH_OECD.xlsx") %>%
  mutate(
    CPIENGSL = as.double(Index),
    Quarter = as.Date(Quarter)
  )
# CPI Food ----------------------------------------------------------
cpi_food <- read_excel("HSG BA/R/Daten/CH/CPI_Food_CH_OECD.xlsx")%>%
  mutate(
    CPIUFDSL = as.double(index),
    Quarter = as.Date(Quarter)
  )
# Graph CPI Energy and Food -----------------------------------------------
cpi_energy_filter <- read_excel("HSG BA/R/Daten/CH/CPI_Energy_CH_OECD.xlsx") %>%
  mutate(CPIENGSL = as.double(Index), Quarter = as.Date(Quarter)) %>%
  filter(Quarter >= as.Date("2010-01-01") & Quarter <= as.Date("2023-01-01")) %>%
  select(Quarter, CPIENGSL)

cpi_food_filter <- read_excel("HSG BA/R/Daten/CH/CPI_Food_CH_OECD.xlsx") %>%
  mutate(CPIUFDSL = as.double(index), Quarter = as.Date(Quarter)) %>%
  filter(Quarter >= as.Date("2010-01-01") & Quarter <= as.Date("2023-01-01")) %>%
  select(Quarter, CPIUFDSL)

cpi_combined <- full_join(cpi_energy_filter, cpi_food_filter, by = "Quarter") %>%
  pivot_longer(cols = c(CPIENGSL, CPIUFDSL),
               names_to = "Category", values_to = "Index")

corona_period <- cpi_combined %>%
  filter(Quarter >= as.Date("2020-01-01") & Quarter <= as.Date("2023-01-01"))

min_val <- min(corona_period$Index, na.rm = TRUE)
max_val <- max(corona_period$Index, na.rm = TRUE)

ggplot(cpi_combined, aes(x = Quarter, y = Index, color = Category)) +
  geom_line(linewidth = 0.9) +
  annotate("rect",
           xmin = as.Date("2020-01-01"), xmax = as.Date("2023-01-01"),
           ymin = -Inf, ymax = Inf,
           alpha = 0.2, fill = "red") +
  geom_hline(yintercept = min_val, linetype = "dashed", color = "darkgray") +
  geom_hline(yintercept = max_val, linetype = "dashed", color = "darkgray") +
  scale_color_manual(
    values = c("CPIENGSL" = "purple", "CPIUFDSL" = "orange"),
    labels = c("CPIENGSL" = "CPI Energy", "CPIUFDSL" = "CPI Food")
  ) +
  scale_y_continuous(name = "Index Level") +
  scale_x_date(
    breaks = seq(from = as.Date("2010-01-01"),
                 to = as.Date("2023-01-01"),
                 by = "2 years"),
    labels = date_format("%Y")
  ) +
  labs(
    x = NULL,
    color = "",
    caption = "Source: OECD and own calculations. \nNote: Red shading indicates the COVID-19 pandemic period."
  ) +
  theme_minimal(base_family = "serif") +
  theme(
    panel.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(color = "black", size = 10),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.ticks = element_line(color = "black"),
    plot.margin = margin(10, 10, 10, 10),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0, size = 9, margin = margin(t = 10))
  )
# RPE (Delta Wage ) ----------------------------------------------------
RPE_Data <- delta_w %>%
  select(Quarter, wages) %>%
  left_join(cpi_energy %>% select(Quarter, CPIENGSL), by = "Quarter") %>%
  mutate(
    rpe = as.double(CPIENGSL / wages),
    log_rpe = log(rpe),
    RPE = 100 * (log_rpe - lag(log_rpe,4))
  ) %>%
  drop_na()

RPE_filter <- RPE_Data %>% 
  filter(as.yearqtr(Quarter) >= as.yearqtr("2010 Q1") & 
           as.yearqtr(Quarter) <= as.yearqtr("2023 Q1"))
RPE_filter$Quarter <- as.yearqtr(RPE_filter$Quarter)
RPE_filter$Quarter_qtr <- as.yearqtr(RPE_filter$Quarter)

ggplot(RPE_filter, aes(x = Quarter_qtr)) +
  geom_line(aes(y = RPE, color = "Delta RPE"), size = 0.9) +
  labs(
    x = "", y = "", color = "",
    caption = "Notes: OECD and Own computation"
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
  

# RPF (Delta Wage) ----------------------------------------------------
RPF_Data <- delta_w %>%
  select(Quarter, wages) %>%
  left_join(cpi_food %>% select(Quarter, CPIUFDSL), by = "Quarter") %>%
  mutate(
    rpf = as.double(CPIUFDSL / wages),
    log_rpf = log(rpf),
    RPF = 100 * (log_rpf - lag(log_rpf,4))
  ) %>%
  drop_na()

RPF_filter <- RPF_Data %>% 
  filter(as.yearqtr(Quarter) >= as.yearqtr("2010 Q1") & 
           as.yearqtr(Quarter) <= as.yearqtr("2023 Q1"))
RPF_filter$Quarter_qtr <- as.yearqtr(RPF_filter$Quarter)

ggplot(RPF_filter, aes(x = Quarter_qtr)) +
  geom_line(aes(y = RPF, color = "Delta RPF"), size = 0.9) +
  labs(
    title = "",
    x = "", y = "", color = "",
    caption = "Notes: OECD and Own computation"
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


# Graph RPE and RPF -------------------------------------------------------
RPE_RPF_combined <- full_join(
  RPE_filter %>% select(Quarter_qtr, RPE),
  RPF_filter %>% select(Quarter_qtr, RPF),
  by = "Quarter_qtr"
) %>%
  pivot_longer(cols = c(RPE, RPF), names_to = "Variable", values_to = "Value")

corona_period <- RPE_RPF_combined %>%
  filter(Quarter_qtr >= as.yearqtr("2020 Q1") & Quarter_qtr <= as.yearqtr("2023 Q1"))

min_val <- min(corona_period$Value, na.rm = TRUE)
max_val <- max(corona_period$Value, na.rm = TRUE)

ggplot(RPE_RPF_combined, aes(x = Quarter_qtr, y = Value, color = Variable)) +
  geom_line(linewidth = 0.9) +
  annotate("rect",
           xmin = as.yearqtr("2020 Q1"), xmax = as.yearqtr("2023 Q1"),
           ymin = -Inf, ymax = Inf,
           alpha = 0.2, fill = "red") +
  geom_hline(yintercept = min_val, linetype = "dashed", color = "darkgray") +
  geom_hline(yintercept = max_val, linetype = "dashed", color = "darkgray") +
  scale_color_manual(
    values = c("RPE" = "red", "RPF" = "darkgreen"),
    labels = c("RPE" = "Δ RPE", "RPF" = "Δ RPF")
  ) +
  scale_y_continuous(name = "Percent") +
  scale_x_yearqtr(format = "%Y Q%q", n = 6) +
  labs(
    x = NULL,
    color = "",
    caption = "Source: SECO and own calculations. \nNote: Red shading indicates the COVID-19 pandemic period."
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = "serif", base_size = 13) +
  theme(
    panel.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(color = "black", size = 10),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.ticks = element_line(color = "black"),
    plot.margin = margin(10, 10, 10, 10),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0, size = 9, margin = margin(t = 10))
  )


# Shortages (Google Trends) -------------------------------------------
shortages <- read_excel("HSG BA/R/Daten/CH/Google_Trend_Shortages_CH.xlsx")
shortages$Quarter_qtr <- as.yearqtr(shortages$Quarter)

ggplot(shortages, aes(x = Quarter_qtr, y = shortage_std)) +
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
VU <- read_excel("HSG BA/R/Daten/CH/Labour_market_CH.xlsx")

VU$Quarter_qtr <- as.yearqtr(VU$Quarter)
ref_value <- VU %>%
  filter(Quarter == as.Date("2019-01-01")) %>%
  pull(VU)
VU_filter <- VU %>%
  filter(Quarter_qtr >= as.yearqtr("2010 Q1") & Quarter_qtr <= as.yearqtr("2023 Q1")) %>%
  mutate(VU_indexed = VU / ref_value)

corona_period_vu <- VU_filter %>%
  filter(Quarter_qtr >= as.yearqtr("2019 Q1") & Quarter_qtr <= as.yearqtr("2023 Q1"))

min_val_vu <- min(corona_period_vu$VU_indexed, na.rm = TRUE)
max_val_vu <- max(corona_period_vu$VU_indexed, na.rm = TRUE)

ggplot(VU_filter, aes(x = Quarter_qtr, y = VU_indexed)) +
  geom_line(color = "lightblue", linewidth = 0.9) +
  annotate("rect",
           xmin = as.yearqtr("2020 Q1"), xmax = as.yearqtr("2023 Q1"),
           ymin = -Inf, ymax = Inf,
           alpha = 0.2, fill = "red") +
  geom_hline(yintercept = min_val_vu, linetype = "dashed", color = "darkgray") +
  geom_hline(yintercept = max_val_vu, linetype = "dashed", color = "darkgray") +
  labs(
    x = NULL, y = "Index (2019 Q1 = 1)",
    caption = "Source: SECO, SNB and Own computation. \nNote: Red shading indicates the COVID-19 pandemic period."
  ) +
  scale_x_yearqtr(format = "%Y Q%q", n = 6) +
  scale_y_continuous(limits = c(0, 3), breaks = seq(0, 3, by = 0.5)) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = "serif", base_size = 13) +
  theme(
    panel.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(color = "black", size = 10),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.ticks = element_line(color = "black"),
    plot.margin = margin(10, 10, 10, 10),
    legend.position = "none",
    plot.caption = element_text(hjust = 0, size = 9, margin = margin(t = 10))
  )


# Productivity  -----------------------------------------------------------
productivity <- read_excel("HSG BA/R/Daten/CH/Productivity_season_CH.xlsx")

productivity <- productivity %>%
  mutate(
    Index = as.double(Index),
    Quarter = as.Date(Quarter),
    gpty = 100 * (log(Index) - log(lag(Index,4))),
    mapty = 0.125 * rowSums(sapply(0:7, function(l) lag(gpty, l)))
  ) %>%
  drop_na()
productivity$Quarter_qtr <- as.yearqtr(productivity$Quarter)

productivity_filter <- productivity %>% 
  filter(Quarter >= as.Date("2010-01-01") & Quarter <= as.Date("2023-01-01"))

corona_period_productivity <- productivity_filter %>%
  filter(Quarter_qtr >= as.yearqtr("2020 Q1") & Quarter_qtr <= as.yearqtr("2023 Q1"))

min_val_productivity <- min(corona_period_productivity$mapty, na.rm = TRUE)
max_val_productivity <- max(corona_period_productivity$mapty, na.rm = TRUE)

ggplot(productivity_filter, aes(x = Quarter_qtr, y = mapty)) +
  geom_line(color = "darkgreen", linewidth = 0.9) +
  annotate("rect",
           xmin = as.yearqtr("2020 Q1"), xmax = as.yearqtr("2023 Q1"),
           ymin = -Inf, ymax = Inf,
           alpha = 0.2, fill = "red") +
  geom_hline(yintercept = min_val_productivity, linetype = "dashed", color = "darkgray") +
  geom_hline(yintercept = max_val_productivity, linetype = "dashed", color = "darkgray") +
  labs(
    x = NULL, y = "Percent",
    caption = "Source: EUROSTAT and own computation. \nNote: Red shading indicates the COVID-19 pandemic. "
  ) +
  scale_x_yearqtr(
    limits = c(as.yearqtr("2010 Q1"), max(productivity_filter$Quarter_qtr)),
    format = "%Y Q%q",
    n = 6
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = "serif", base_size = 13) +
  theme(
    panel.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(color = "black", size = 10),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.ticks = element_line(color = "black"),
    plot.margin = margin(10, 10, 10, 10),
    legend.position = "none",
    plot.caption = element_text(hjust = 0, size = 9, margin = margin(t = 10))
  )

# Inflation Expectations (short term) -------------------------------------
cf1_exp <- read_excel("HSG BA/R/Daten/CH/cf1_exp_CH.xlsx")
cf1_exp$Quarter <- as.Date(as.character(cf1_exp$Quarter))


cf1_exp_filter <- cf1_exp %>% 
  filter(Quarter >= as.Date("2010-01-01") & Quarter <= as.Date("2023-01-01"))

corona_period_cf1 <- cf1_exp_filter %>%
  filter(Quarter >= as.Date("2020-01-01") & Quarter <= as.Date("2023-01-01"))

min_val_cf1 <- min(corona_period_cf1$Expectations, na.rm = TRUE)
max_val_cf1 <- max(corona_period_cf1$Expectations, na.rm = TRUE)

ggplot(cf1_exp_filter, aes(x = Quarter, y = Expectations)) +
  geom_line(color = "purple4", linewidth = 0.9) +
  annotate("rect",
           xmin = as.Date("2020-01-01"), xmax = as.Date("2023-01-01"),
           ymin = -Inf, ymax = Inf,
           alpha = 0.2, fill = "red") +
  geom_hline(yintercept = min_val_cf1, linetype = "dashed", color = "darkgray") +
  geom_hline(yintercept = max_val_cf1, linetype = "dashed", color = "darkgray") +
  labs(
    x = NULL, y = "Percent",
    caption = "Source: SECO, SNB and own computation. \nNote: Red shading indicates the COVID-19 pandemic period."
  ) +
  scale_x_date(
    limits = c(as.Date("2010-01-01"), max(cf1_exp_filter$Quarter)),
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = "serif", base_size = 13) +
  theme(
    panel.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(color = "black", size = 10),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.ticks = element_line(color = "black"),
    plot.margin = margin(10, 10, 10, 10),
    legend.position = "none",
    plot.caption = element_text(hjust = 0, size = 9, margin = margin(t = 10))
  )

exp_short <- cf1_exp %>% 
  rename(CF1 = Expectations)

# Inflation Expectations (long term) --------------------------------------
cf10_exp <- read_excel("HSG BA/R/Daten/CH/cf5_exp_CH.xlsx")
cf10_exp$Quarter <- as.Date(as.character(cf10_exp$Quarter))

cf10_exp_filter <- cf10_exp %>% 
  filter(Quarter >= as.Date("2010-01-01") & Quarter <= as.Date("2023-01-01"))

corona_period_cf10 <- cf10_exp_filter %>%
  filter(Quarter >= as.Date("2020-01-01") & Quarter <= as.Date("2023-01-01"))

min_val_cf10 <- min(corona_period_cf10$cf10_inf, na.rm = TRUE)
max_val_cf10 <- max(corona_period_cf10$cf10_inf, na.rm = TRUE)

ggplot(cf10_exp_filter, aes(x = Quarter, y = cf10_inf)) +
  geom_line(color = "cadetblue", linewidth = 0.9) +
  annotate("rect",
           xmin = as.Date("2020-01-01"), xmax = as.Date("2023-01-01"),
           ymin = -Inf, ymax = Inf,
           alpha = 0.2, fill = "red") +
  geom_hline(yintercept = min_val_cf10, linetype = "dashed", color = "darkgray") +
  geom_hline(yintercept = max_val_cf10, linetype = "dashed", color = "darkgray") +
  labs(
    x = NULL, y = "Percent",
    caption = "Source: ECB, SNB and own computation.\nNote: Red shading indicates the COVID-19 pandemic period."
  ) +
  scale_x_date(
    limits = c(as.Date("2010-01-01"), max(cf10_exp_filter$Quarter)),
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = "serif", base_size = 13) +
  theme(
    panel.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(color = "black", size = 10),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.ticks = element_line(color = "black"),
    plot.margin = margin(10, 10, 10, 10),
    legend.position = "none",
    plot.caption = element_text(hjust = 0, size = 9, margin = margin(t = 10))
  )

  long_term <- cf10_exp %>% 
  rename(CF5 = cf10_inf)

# Graph both expectations -------------------------------------------------

  cf1_cf10_combined <- full_join(
    cf1_exp_filter %>% select(Quarter, CF1 = Expectations),
    cf10_exp_filter %>% select(Quarter, CF10 = cf10_inf),
    by = "Quarter"
  ) %>%
    pivot_longer(cols = c(CF1, CF10), names_to = "Horizon", values_to = "Value")
  
  corona_period_cf <- cf1_cf10_combined %>%
    filter(Quarter >= as.Date("2020-01-01") & Quarter <= as.Date("2023-01-01"))
  
  min_val_cf <- min(corona_period_cf$Value, na.rm = TRUE)
  max_val_cf <- max(corona_period_cf$Value, na.rm = TRUE)
  
  ggplot(cf1_cf10_combined, aes(x = Quarter, y = Value, color = Horizon)) +
    geom_line(linewidth = 0.9) +
    annotate("rect",
             xmin = as.Date("2020-01-01"), xmax = as.Date("2023-01-01"),
             ymin = -Inf, ymax = Inf,
             alpha = 0.2, fill = "red") +
    geom_hline(yintercept = min_val_cf, linetype = "dashed", color = "darkgray") +
    geom_hline(yintercept = max_val_cf, linetype = "dashed", color = "darkgray") +
    scale_color_manual(
      values = c("CF1" = "purple4", "CF10" = "cadetblue"),
      labels = c("CF1" = "Short-term (CF1)", "CF10" = "Long-term (CF5)")
    ) +
    scale_x_date(
      limits = c(as.Date("2010-01-01"), max(cf1_cf10_combined$Quarter)),
      date_breaks = "1 year",
      date_labels = "%Y"
    ) +
    labs(
      x = NULL, y = "in %",
      color = "",
      caption = "Source: ECB, SNB and own computation.\nNote: Red shading indicates the COVID-19 pandemic period."
    ) +
    coord_cartesian(clip = "off") +
    theme_minimal(base_family = "serif", base_size = 13) +
    theme(
      panel.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
      panel.grid = element_blank(),
      axis.line = element_line(color = "black"),
      axis.text = element_text(color = "black", size = 10),
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.ticks = element_line(color = "black"),
      plot.margin = margin(10, 10, 10, 10),
      legend.position = "bottom",
      plot.caption = element_text(hjust = 0, size = 9, margin = margin(t = 10))
    )
  
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
  
  catchup_filter <- catchup_data %>%
    filter(Quarter >= as.Date("2010-10-01") & Quarter <= as.Date("2023-01-01"))
  
  corona_period_cu <- catchup_filter %>%
    filter(Quarter >= as.Date("2020-01-01") & Quarter <= as.Date("2023-01-01"))
  
  min_val_cu <- min(corona_period_cu$CU, na.rm = TRUE)
  max_val_cu <- max(corona_period_cu$CU, na.rm = TRUE)
  
  ggplot(catchup_filter, aes(x = Quarter, y = CU)) +
    geom_line(color = "darkgoldenrod", linewidth = 1.2) +
    annotate("rect",
             xmin = as.Date("2020-01-01"), xmax = as.Date("2023-01-01"),
             ymin = -Inf, ymax = Inf,
             alpha = 0.2, fill = "red") +
    geom_hline(yintercept = min_val_cu, linetype = "dashed", color = "darkgray") +
    geom_hline(yintercept = max_val_cu, linetype = "dashed", color = "darkgray") +
    labs(
      x = NULL,
      y = "Percent",
      caption = "Source: own computation.\nRed shading indicates the COVID-19 pandemic period."
    ) +
    scale_x_date(
      limits = c(as.Date("2010-01-01"), as.Date("2023-01-01")),
      date_breaks = "1 year",
      date_labels = "%Y"
    ) +
    coord_cartesian(clip = "off") +
    theme_minimal(base_family = "serif", base_size = 14) +
    theme(
      panel.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
      panel.grid = element_blank(),
      axis.line = element_line(color = "black"),
      axis.text = element_text(color = "black", size = 10),
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.ticks = element_line(color = "black"),
      plot.margin = margin(10, 10, 10, 10),
      legend.position = "none",
      plot.caption = element_text(hjust = 0, size = 9, margin = margin(t = 10)),
      plot.title = element_text(face = "bold", size = 13)
    )
  
# Sample Size  ------------------------------------------------------------
start_date <- as.Date("1999-01-01")
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
start_date <- as.Date("1999-01-01")
end_date <- as.Date("2023-01-01")  

Wage_equation <- delta_w %>%
  select(Quarter, delta_w) %>%
  left_join(VU %>% select(Quarter, VU), by = "Quarter") %>%
  left_join(CU %>% select(Quarter, CU), by = "Quarter") %>%
  left_join(exp_short %>% select(Quarter, CF1), by = "Quarter") %>%
  left_join(productivity %>% select(Quarter, mapty), by = "Quarter") %>%
  filter(Quarter >= start_date & Quarter <= end_date)

# Price Equation Data -----------------------------------------------------
start_date <- as.Date("1999-01-01")
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
start_date <- as.Date("1999-01-01")
end_date <- as.Date("2023-01-01")

cf1_equation <- exp_short %>%
  select(Quarter, CF1) %>%
  left_join(long_term %>% select(Quarter, CF5), by = "Quarter") %>%
  left_join(delta_p %>% select(Quarter, delta_p), by = "Quarter") %>%
  filter(Quarter >= start_date & Quarter <= end_date)

# CF5 Equation Data -------------------------------------------------------
start_date <- as.Date("1999-01-01")
end_date <- as.Date("2023-01-01")  

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

wage_equation_data <- wage_equation_data %>%
  mutate(
    dummy_q2_2020 = if_else(Quarter == ymd("2020-04-01"), 1, 0),
    dummy_q3_2020 = if_else(Quarter == ymd("2020-07-01"), 1, 0)
  )


  # Regressionsformel
formel_eq <- delta_w ~ delta_w_lag1 + delta_w_lag2 + delta_w_lag3 + delta_w_lag4 +
  CF1_lag1 + CF1_lag2 + CF1_lag3 + CF1_lag4 +
  CU_lag1 + CU_lag2 + CU_lag3 + CU_lag4 +
  VU_lag1 + VU_lag2 + VU_lag3 + VU_lag4 +
  mapty_lag1 + dummy_q2_2020 + dummy_q3_2020

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
linearHypothesis(wage_model, "wage_dummy_q2_2020 + wage_dummy_q3_2020 = 0")

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


linearHypothesis(wage_model, c("wage_dummy_q2_2020 = 0", "wage_dummy_q3_2020 = 0"))

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

sum_dummies <- coefs_wage["wage_dummy_q2_2020"] + coefs_wage["wage_dummy_q3_2020"]

# Ausgabe
cat("∑ Δwₜ Lags:     ", round(sum_delta_w, 4), "\n")
cat("∑ CF1 Lags (Erwartungen):     ", round(sum_CF1, 4), "\n")
cat("∑ CU Lags (Catch-up):         ", round(sum_CU, 4), "\n")
cat("∑ VU Lags (Tightness):        ", round(sum_VU, 4), "\n")
cat("maptyₜ₋₁ (Produktivität):      ", round(mapty, 4), "\n)")
cat("∑ Dummy Q2 2020 + Q3 2020:    ", round(sum_dummies, 4), "\n")

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
  arrange(Quarter) %>%
  mutate(
    dummy_q2_2020 = if_else(Quarter == as.Date("2020-04-01"), 1, 0),
    dummy_q3_2020 = if_else(Quarter == as.Date("2020-07-01"), 1, 0)
  )


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
      coefs_wage["wage_VU_lag4"] * VU_lag4 +
      
      coefs_wage["wage_mapty_lag1"] * mapty+
      
      coefs_wage["wage_dummy_q2_2020"] * dummy_q2_2020 + 
      coefs_wage["wage_dummy_q3_2020"] * dummy_q3_2020
    
  )

plot_data <- Wage_equation_estimated_values %>%
  select(Quarter, delta_w, delta_w_sim) %>%
  filter(Quarter >= as.Date("2019-07-01")) %>%
  pivot_longer(cols = c(delta_w, delta_w_sim),
               names_to = "Variable", values_to = "Value")

plot_data$Quarter_qtr <- as.yearqtr(plot_data$Quarter)

ggplot(plot_data, aes(x = Quarter_qtr, y = Value, color = Variable, linetype = Variable)) +
  geom_line(linewidth = 1) +
  scale_linetype_manual(
    values = c("delta_w" = "solid", "delta_w_sim" = "dashed"),
    labels = c("delta_w" = "Realised", "delta_w_sim" = "Simulated")
  ) +
  scale_color_manual(
    values = c("delta_w" = "royalblue", "delta_w_sim" = "tomato"),
    labels = c("delta_w" = "Realised", "delta_w_sim" = "Simulated")
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray") +  # Horizontale gestrichelte Linie bei 0
  labs(
    x = NULL, y = "Percent",
    color = "", linetype = "",
    caption = "Source: Own computation."
  ) +
  scale_x_yearqtr(
    format = "%Y Q%q",
    limits = c(as.yearqtr("2019 Q3"), as.yearqtr("2023 Q1")),
    n = 6
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = "serif", base_size = 13) +
  theme(
    panel.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(color = "black", size = 10),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.ticks = element_line(color = "black"),
    plot.margin = margin(10, 10, 10, 10),
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.caption = element_text(hjust = 0, size = 9, margin = margin(t = 10)),
    plot.title = element_text(face = "bold", size = 12)
  )


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
  filter(Quarter >= as.Date("2019-01-01")) %>%
  select(Quarter, delta_p, delta_p_sim) %>%
  pivot_longer(cols = c(delta_p, delta_p_sim), names_to = "Variable", values_to = "Value")

plot_data$Quarter_qtr <- as.yearqtr(plot_data$Quarter)

# Plot erstellen
ggplot(plot_data, aes(x = Quarter_qtr, y = Value, color = Variable, linetype = Variable)) +
  geom_line(linewidth = 1.2) +
  scale_linetype_manual(
    values = c("delta_p" = "solid", "delta_p_sim" = "dashed"),
    labels = c("delta_p" = "Realised", "delta_p_sim" = "Simulated")
  ) +
  scale_color_manual(
    values = c("delta_p" = "darkgreen", "delta_p_sim" = "black"),
    labels = c("delta_p" = "Realised", "delta_p_sim" = "Simulated")
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray") +  # Horizontale gestrichelte Linie bei 0
  labs(
    x = NULL,
    y = "Percent",
    color = "", linetype = "",
    caption = "Source: Own computation."
  ) +
  scale_x_yearqtr(
    format = "%Y Q%q",
    limits = c(as.yearqtr("2019 Q1"), as.yearqtr("2023 Q1")),
    n = 6
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = "serif", base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(color = "black", size = 10),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.ticks = element_line(color = "black"),
    plot.margin = margin(10, 10, 10, 10),
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.caption = element_text(hjust = 0, size = 9, margin = margin(t = 10)),
    plot.title = element_text(face = "bold", size = 12)
  )
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
  filter(Quarter >= as.Date("2019-01-01")) %>%
  select(Quarter, CF1, short_infl_sim) %>%
  pivot_longer(cols = c(CF1, short_infl_sim), names_to = "Variable", values_to = "Value")

plot_short$Quarter_qtr <- as.yearqtr(plot_short$Quarter)

ggplot(plot_short, aes(x = Quarter_qtr, y = Value, color = Variable, linetype = Variable)) +
  geom_line(linewidth = 1.2) +
  scale_linetype_manual(
    values = c("CF1" = "solid", "short_infl_sim" = "dashed"),
    labels = c("CF1" = "Realised", "short_infl_sim" = "Simulated")
  ) +
  scale_color_manual(
    values = c("CF1" = "darkcyan", "short_infl_sim" = "orangered4"),
    labels = c("CF1" = "Realised", "short_infl_sim" = "Simulated")
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray") +  # Horizontale gestrichelte Linie bei 0
  labs(
    x = NULL,
    y = "Percent",
    color = "", linetype = "",
    caption = "Source: Own computation."
  ) +
  scale_x_yearqtr(
    format = "%Y Q%q",
    limits = c(as.yearqtr("2019 Q1"), as.yearqtr("2023 Q1")),
    n = 6
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = "serif", base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(color = "black", size = 10),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.ticks = element_line(color = "black"),
    plot.margin = margin(10, 10, 10, 10),
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.caption = element_text(hjust = 0, size = 9, margin = margin(t = 10)),
    plot.title = element_text(face = "bold", size = 12)
  )
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
  filter(Quarter >= as.Date("2019-01-01")) %>%
  select(Quarter, CF5, long_infl_sim) %>%
  pivot_longer(cols = c(CF5, long_infl_sim), names_to = "Variable", values_to = "Value")

plot_long$Quarter_qtr <- as.yearqtr(plot_long$Quarter)

ggplot(plot_long, aes(x = Quarter_qtr, y = Value, color = Variable, linetype = Variable)) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = 0, linewidth = 0.4, color = "gray40") +  
  scale_linetype_manual(
    values = c("CF5" = "solid", "long_infl_sim" = "dashed"),
    labels = c("CF5" = "Realised", "long_infl_sim" = "Simulated")
  ) +
  scale_color_manual(
    values = c("CF5" = "deeppink4", "long_infl_sim" = "darkslateblue"),
    labels = c("CF5" = "Realised", "long_infl_sim" = "Simulated")
  ) +
  labs(
    x = NULL,
    y = "Percent",
    color = "", linetype = "",
    caption = "Source: Own computation."
  ) +
  scale_x_yearqtr(
    format = "%Y Q%q",
    limits = c(as.yearqtr("2019 Q1"), as.yearqtr("2023 Q1")),
    n = 6
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = "serif", base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(color = "black", size = 10),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.ticks = element_line(color = "black"),
    plot.margin = margin(10, 10, 10, 10),
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.caption = element_text(hjust = 0, size = 9, margin = margin(t = 10)),
    plot.title = element_text(face = "bold", size = 12)
  )

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

wage_coeff_dummy_q2 <- coefs_wage["wage_dummy_q2_2020"]
wage_coeff_dummy_q3 <- coefs_wage["wage_dummy_q3_2020"]

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
sd_mapty <- std_combined_clean$`standard deviation`[std_combined_clean$Variable == "mapty"]

IRF_RPE <- IRF_zero
IRF_RPF <- IRF_zero
IRF_shortage <- IRF_zero
IRF_mapty    <- IRF_zero

IRF_RPE$RPE[IRF_RPE$Period == 5] <- sd_RPE
IRF_RPF$RPF[IRF_RPF$Period == 5] <- sd_RPF
IRF_shortage$shortage[IRF_shortage$Period == 5] <- sd_shortage
IRF_mapty$mapty[IRF_mapty$Period == 5]       <- sd_mapty

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


IRF_RPE_filtered <- IRF_RPE %>%
  filter(Period >= 5 & Period <= 20) %>%
  mutate(Period_plot = row_number())

ggplot(IRF_RPE_filtered, aes(x = Period_plot, y = p_sim)) +
  geom_line(color = "blue", linewidth = 1.2, lineend = "round", linejoin = "round") +
  geom_point(color = "blue", size = 2) +
  scale_x_continuous(breaks = 1:16, labels = 1:16) +
  labs(
    x = "Period",
    y = "Percent",
    caption = "Source: Own computation."
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = "serif", base_size = 13) +
  theme(
    panel.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(color = "black", size = 10),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.ticks = element_line(color = "black"),
    plot.margin = margin(10, 10, 10, 10),
    plot.caption = element_text(hjust = 0, size = 9, margin = margin(t = 10)),
    plot.title = element_text(face = "bold", size = 12)
  )

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



IRF_RPF_filtered <- IRF_RPF %>%
  filter(Period >= 5 & Period <= 20) %>%
  mutate(Period_plot = row_number())

ggplot(IRF_RPF_filtered, aes(x = Period_plot, y = p_sim)) +
  geom_line(color = "darkorange", linewidth = 1.2, lineend = "round", linejoin = "round") +
  geom_point(color = "darkorange", size = 2) +
  scale_x_continuous(breaks = 1:16, labels = 1:16) +
  labs(
    x = "Period",
    y = "Percent",
    caption = "Source: Own computation."
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = "serif", base_size = 13) +
  theme(
    panel.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(color = "black", size = 10),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.ticks = element_line(color = "black"),
    plot.margin = margin(10, 10, 10, 10),
    legend.position = "none",
    plot.caption = element_text(hjust = 0, size = 9, margin = margin(t = 10))
  )

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



IRF_shortage_filtered <- IRF_shortage %>%
  filter(Period >= 5 & Period <= 20) %>%
  mutate(Period_plot = row_number())

ggplot(IRF_shortage_filtered, aes(x = Period_plot, y = p_sim)) +
  geom_line(color = "mediumvioletred", linewidth = 1.2, lineend = "round", linejoin = "round") +
  geom_point(color = "mediumvioletred", size = 2) +
  scale_x_continuous(breaks = 1:16, labels = 1:16) +
  labs(
    x = "Period",
    y = "Percent",
    caption = "Source: Own computation."
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = "serif", base_size = 13) +
  theme(
    panel.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(color = "black", size = 10),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.ticks = element_line(color = "black"),
    plot.margin = margin(10, 10, 10, 10),
    legend.position = "none",
    plot.caption = element_text(hjust = 0, size = 9, margin = margin(t = 10))
  )
# IRF mapty Shock ---------------------------------------------------------
for (t in 5:40) {
  # --- Wage Equation (w_sim) ---
  IRF_mapty$w_sim[t] <-
    wage_coeff_wage_delta_w_lag1 * IRF_mapty$w_sim[t-1] +
    wage_coeff_wage_delta_w_lag2 * IRF_mapty$w_sim[t-2] +
    wage_coeff_wage_delta_w_lag3 * IRF_mapty$w_sim[t-3] +
    wage_coeff_wage_delta_w_lag4 * IRF_mapty$w_sim[t-4] +
    wage_coeff_CF1_lag1 * IRF_mapty$cf1_sim[t-1] +
    wage_coeff_CF1_lag2 * IRF_mapty$cf1_sim[t-2] +
    wage_coeff_CF1_lag3 * IRF_mapty$cf1_sim[t-3] +
    wage_coeff_CF1_lag4 * IRF_mapty$cf1_sim[t-4] +
    wage_coeff_CU_lag1 * IRF_mapty$CU_sim[t-1] +
    wage_coeff_CU_lag2 * IRF_mapty$CU_sim[t-2] +
    wage_coeff_CU_lag3 * IRF_mapty$CU_sim[t-3] +
    wage_coeff_CU_lag4 * IRF_mapty$CU_sim[t-4] +
    wage_coeff_VU_lag1 * IRF_mapty$vu[t-1] +
    wage_coeff_VU_lag2 * IRF_mapty$vu[t-2] +
    wage_coeff_VU_lag3 * IRF_mapty$vu[t-3] +
    wage_coeff_VU_lag4 * IRF_mapty$vu[t-4] +
    wage_coeff_mapty_lag1_wage * IRF_mapty$mapty[t-1]
  
  # --- Price Equation (p_sim) ---
  IRF_mapty$p_sim[t] <-
    price_coeff_delta_p_lag1 * IRF_mapty$p_sim[t-1] +
    price_coeff_delta_p_lag2 * IRF_mapty$p_sim[t-2] +
    price_coeff_delta_p_lag3 * IRF_mapty$p_sim[t-3] +
    price_coeff_delta_p_lag4 * IRF_mapty$p_sim[t-4] +
    price_coeff_delta_w_lag0 * IRF_mapty$w_sim[t] +
    price_coeff_delta_w_lag1 * IRF_mapty$w_sim[t-1] +
    price_coeff_delta_w_lag2 * IRF_mapty$w_sim[t-2] +
    price_coeff_delta_w_lag3 * IRF_mapty$w_sim[t-3] +
    price_coeff_delta_w_lag4 * IRF_mapty$w_sim[t-4] +
    price_coeff_RPE_0 * IRF_mapty$RPE[t] +
    price_coeff_RPE_1 * IRF_mapty$RPE[t-1] +
    price_coeff_RPE_2 * IRF_mapty$RPE[t-2] +
    price_coeff_RPE_3 * IRF_mapty$RPE[t-3] +
    price_coeff_RPE_4 * IRF_mapty$RPE[t-4] +
    price_coeff_RPF_0 * IRF_mapty$RPF[t] +
    price_coeff_RPF_1 * IRF_mapty$RPF[t-1] +
    price_coeff_RPF_2 * IRF_mapty$RPF[t-2] +
    price_coeff_RPF_3 * IRF_mapty$RPF[t-3] +
    price_coeff_RPF_4 * IRF_mapty$RPF[t-4] +
    price_coeff_shortage_0 * IRF_mapty$shortage[t] +
    price_coeff_shortage_1 * IRF_mapty$shortage[t-1] +
    price_coeff_shortage_2 * IRF_mapty$shortage[t-2] +
    price_coeff_shortage_3 * IRF_mapty$shortage[t-3] +
    price_coeff_shortage_4 * IRF_mapty$shortage[t-4] +
    price_coeff_mapty_lag1 * IRF_mapty$mapty[t-1]
  
  # --- Long-term Expectations ---
  IRF_mapty$cf5_sim[t] <-
    CF5_coeff_cf5_lag1 * IRF_mapty$cf5_sim[t-1] +
    CF5_coeff_cf5_lag2 * IRF_mapty$cf5_sim[t-2] +
    CF5_coeff_cf5_lag3 * IRF_mapty$cf5_sim[t-3] +
    CF5_coeff_cf5_lag4 * IRF_mapty$cf5_sim[t-4] +
    CF5_coeff_deltap_0 * IRF_mapty$p_sim[t] +
    CF5_coeff_deltap_1 * IRF_mapty$p_sim[t-1] +
    CF5_coeff_deltap_2 * IRF_mapty$p_sim[t-2] +
    CF5_coeff_deltap_3 * IRF_mapty$p_sim[t-3] +
    CF5_coeff_deltap_4 * IRF_mapty$p_sim[t-4]
  
  # --- Short-term Expectations ---
  IRF_mapty$cf1_sim[t] <-
    CF1_coeff_cf1_lag1 * IRF_mapty$cf1_sim[t-1] +
    CF1_coeff_cf1_lag2 * IRF_mapty$cf1_sim[t-2] +
    CF1_coeff_cf1_lag3 * IRF_mapty$cf1_sim[t-3] +
    CF1_coeff_cf1_lag4 * IRF_mapty$cf1_sim[t-4] +
    CF1_coeff_CF5_0 * IRF_mapty$cf5_sim[t] +
    CF1_coeff_CF5_1 * IRF_mapty$cf5_sim[t-1] +
    CF1_coeff_CF5_2 * IRF_mapty$cf5_sim[t-2] +
    CF1_coeff_CF5_3 * IRF_mapty$cf5_sim[t-3] +
    CF1_coeff_CF5_4 * IRF_mapty$cf5_sim[t-4] +
    CF1_coeff_deltap_0 * IRF_mapty$p_sim[t] +
    CF1_coeff_deltap_1 * IRF_mapty$p_sim[t-1] +
    CF1_coeff_deltap_2 * IRF_mapty$p_sim[t-2] +
    CF1_coeff_deltap_3 * IRF_mapty$p_sim[t-3] +
    CF1_coeff_deltap_4 * IRF_mapty$p_sim[t-4]
  
  # --- Catch-up-Term (CU_sim) ---
  IRF_mapty$CU_sim[t] <- 0.25 * sum(IRF_mapty$p_sim[(t-3):t]) - IRF_mapty$cf1_sim[t-4]
}
IRF_mapty_filtered <- IRF_mapty %>%
  filter(Period >= 5 & Period <= 20) %>%
  mutate(Period_plot = row_number())

ggplot(IRF_mapty_filtered, aes(x = Period_plot, y = p_sim)) +
  geom_line(color = "seagreen4", linewidth = 1.2, lineend = "round", linejoin = "round") +
  geom_point(color = "seagreen4", size = 2) +
  scale_x_continuous(breaks = 1:16, labels = 1:16) +
  labs(
    x = "Period",
    y = "Percent",
    caption = "Source: Own computation."
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = "serif", base_size = 13) +
  theme(
    panel.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(color = "black", size = 10),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.ticks = element_line(color = "black"),
    plot.margin = margin(10, 10, 10, 10),
    plot.caption = element_text(hjust = 0, size = 9, margin = margin(t = 10))
  )


# IRF Graph Price Inflation ---------------------------------------------------------------
IRF_RPE_filtered <- IRF_RPE %>%
  filter(Period >= 5 & Period <= 20) %>%
  mutate(Period_plot = row_number(), Shock = "Rel. energy prices")

IRF_RPF_filtered <- IRF_RPF %>%
  filter(Period >= 5 & Period <= 20) %>%
  mutate(Period_plot = row_number(), Shock = "Rel. food prices")

IRF_shortage_filtered <- IRF_shortage %>%
  filter(Period >= 5 & Period <= 20) %>%
  mutate(Period_plot = row_number(), Shock = "Shortages")

IRF_mapty_filtered <- IRF_mapty %>%
  filter(Period >= 5 & Period <= 20) %>%
  mutate(Period_plot = row_number(), Shock = "Productivity")

IRF_combined <- bind_rows(
  IRF_RPE_filtered,
  IRF_RPF_filtered,
  IRF_shortage_filtered,
  IRF_mapty_filtered
)

ggplot(IRF_combined, aes(x = Period_plot, y = p_sim, color = Shock)) +
  geom_line(linewidth = 1.2, lineend = "round") +
  geom_point(size = 2) +
  annotate("segment",
           x = 1, xend = 16,
           y = 0, yend = 0,
           linetype = "dashed", color = "gray40") +
  scale_x_continuous(breaks = 1:16) +
  scale_y_continuous(
    limits = c(-0.3, 1.2),
    breaks = c(-0.2,0.0,0.5, 1.0)
  ) +
  scale_color_manual(values = c(
    "Rel. energy prices" = "blue",
    "Rel. food prices" = "darkorange",
    "Shortages" = "mediumvioletred",
    "Productivity" = "seagreen4"
  )) +
  labs(
    x = "Period",
    y = "Percent",
    caption = "Source: Own computation."
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = "serif", base_size = 13) +
  theme(
    panel.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(color = "black", size = 10),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.ticks = element_line(color = "black"),
    plot.margin = margin(10, 10, 10, 10),
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.caption = element_text(hjust = 0, size = 9, margin = margin(t = 10))
  )


# IRF Graph Wage Inflation ------------------------------------------------
IRF_RPE_filtered <- IRF_RPE %>%
  filter(Period >= 5 & Period <= 20) %>%
  mutate(Period_plot = row_number(), Shock = "Rel. energy prices")

IRF_RPF_filtered <- IRF_RPF %>%
  filter(Period >= 5 & Period <= 20) %>%
  mutate(Period_plot = row_number(), Shock = "Rel. food prices")

IRF_shortage_filtered <- IRF_shortage %>%
  filter(Period >= 5 & Period <= 20) %>%
  mutate(Period_plot = row_number(), Shock = "Shortages")

IRF_mapty_filtered <- IRF_mapty %>%
  filter(Period >= 5 & Period <= 20) %>%
  mutate(Period_plot = row_number(), Shock = "Productivity")

IRF_combined <- bind_rows(
  IRF_RPE_filtered,
  IRF_RPF_filtered,
  IRF_shortage_filtered,
  IRF_mapty_filtered
)

ggplot(IRF_combined, aes(x = Period_plot, y = w_sim, color = Shock)) +
  geom_line(linewidth = 1.2, lineend = "round") +
  geom_point(size = 2) +
  annotate("segment",
           x = 1, xend = 16,
           y = 0, yend = 0,
           linetype = "dashed", color = "gray40") +
  scale_x_continuous(breaks = 1:16) +
  scale_y_continuous(limits = c(-0.3, 0.5)) +
  scale_color_manual(values = c(
    "Rel. energy prices" = "blue",
    "Rel. food prices" = "darkorange",
    "Shortages" = "mediumvioletred",
    "Productivity" = "seagreen4"
  )) +
  labs(
    x = NULL,
    y = "Percent",
    caption = "Source: Own computation."
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = "serif", base_size = 13) +
  theme(
    panel.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(color = "black", size = 10),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.ticks = element_line(color = "black"),
    plot.margin = margin(10, 10, 10, 10),
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.caption = element_text(hjust = 0, size = 9, margin = margin(t = 10))
  )

# IRF Graph short-term Expectations ---------------------------------------
IRF_RPE_filtered <- IRF_RPE %>%
  filter(Period >= 5 & Period <= 20) %>%
  mutate(Period_plot = row_number(), Shock = "Rel. energy prices")

IRF_RPF_filtered <- IRF_RPF %>%
  filter(Period >= 5 & Period <= 20) %>%
  mutate(Period_plot = row_number(), Shock = "Rel. food prices")

IRF_shortage_filtered <- IRF_shortage %>%
  filter(Period >= 5 & Period <= 20) %>%
  mutate(Period_plot = row_number(), Shock = "Shortages")

IRF_mapty_filtered <- IRF_mapty %>%
  filter(Period >= 5 & Period <= 20) %>%
  mutate(Period_plot = row_number(), Shock = "Productivity")

IRF_combined <- bind_rows(
  IRF_RPE_filtered,
  IRF_RPF_filtered,
  IRF_shortage_filtered,
  IRF_mapty_filtered
)

ggplot(IRF_combined, aes(x = Period_plot, y = cf1_sim, color = Shock)) +
  geom_line(linewidth = 1.2, lineend = "round") +
  geom_point(size = 2) +
  annotate("segment",
           x = 1, xend = 16,
           y = 0, yend = 0,
           linetype = "dashed", color = "gray40") +
  scale_x_continuous(breaks = 1:16) +
  scale_y_continuous(
    limits = c(-0.2, 0.7),
    breaks = seq(-0.2, 0.7, by = 0.2)
  ) +
  scale_color_manual(values = c(
    "Rel. energy prices" = "blue",
    "Rel. food prices" = "darkorange",
    "Shortages" = "mediumvioletred",
    "Productivity" = "seagreen4"
  )) +
  labs(
    x = NULL,
    y = "Percent",
    caption = "Source: Own computation."
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = "serif", base_size = 13) +
  theme(
    panel.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(color = "black", size = 10),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.ticks = element_line(color = "black"),
    plot.margin = margin(10, 10, 10, 10),
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.caption = element_text(hjust = 0, size = 9, margin = margin(t = 10))
  )


# IRF Graph long-term Expectations ----------------------------------------
IRF_RPE_filtered <- IRF_RPE %>%
  filter(Period >= 5 & Period <= 20) %>%
  mutate(Period_plot = row_number(), Shock = "Rel. energy prices")

IRF_RPF_filtered <- IRF_RPF %>%
  filter(Period >= 5 & Period <= 20) %>%
  mutate(Period_plot = row_number(), Shock = "Rel. food prices")

IRF_shortage_filtered <- IRF_shortage %>%
  filter(Period >= 5 & Period <= 20) %>%
  mutate(Period_plot = row_number(), Shock = "Shortages")

IRF_mapty_filtered <- IRF_mapty %>%
  filter(Period >= 5 & Period <= 20) %>%
  mutate(Period_plot = row_number(), Shock = "Productivity")

IRF_combined <- bind_rows(
  IRF_RPE_filtered,
  IRF_RPF_filtered,
  IRF_shortage_filtered,
  IRF_mapty_filtered
)

ggplot(IRF_combined, aes(x = Period_plot, y = cf5_sim, color = Shock)) +
  geom_line(linewidth = 1.2, lineend = "round") +
  geom_point(size = 2) +
  annotate("segment",
           x = 1, xend = 16,
           y = 0, yend = 0,
           linetype = "dashed", color = "gray40") +
  scale_x_continuous(breaks = 1:16) +
  scale_y_continuous(
    limits = c(-0.02, 0.04),
    breaks = seq(-0.02, 0.04, by = 0.02)
  ) +
  scale_color_manual(values = c(
    "Rel. energy prices" = "blue",
    "Rel. food prices" = "darkorange",
    "Shortages" = "mediumvioletred",
    "Productivity" = "seagreen4"
  )) +
  labs(
    x = NULL,
    y = "Percent",
    caption = "Source: Own computation."
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = "serif", base_size = 13) +
  theme(
    panel.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(color = "black", size = 10),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.ticks = element_line(color = "black"),
    plot.margin = margin(10, 10, 10, 10),
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.caption = element_text(hjust = 0, size = 9, margin = margin(t = 10))
  )


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
  mapty        = 0, cu           = 0,
  dummy_q2_2020  = ifelse(quarter_dates == as.Date("2020-04-01"), 1, 0),
  dummy_q3_2020  = ifelse(quarter_dates == as.Date("2020-07-01"), 1, 0)
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
    wage_coeff_VU_lag4 * decomposition_normal$vu[row_t - 4] +
    
    wage_coeff_mapty_lag1_wage * decomposition_normal$mapty[row_t - 1] +
    
    wage_coeff_dummy_q2 * decomposition_normal$dummy_q2_2020[row_t] +
    wage_coeff_dummy_q3 * decomposition_normal$dummy_q3_2020[row_t]
    
    
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
    price_coeff_shortage_4 * decomposition_normal$shortage[row_t-4] +
    
    price_coeff_mapty_lag1 * decomposition_normal$mapty[row_t-1]
  
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
  mapty        = 0, cu           = 0,
  dummy_q2_2020  = ifelse(quarter_dates == as.Date("2020-04-01"), 1, 0),
  dummy_q3_2020  = ifelse(quarter_dates == as.Date("2020-07-01"), 1, 0)
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
    wage_coeff_VU_lag4 * decomposition_rpe$vu[row_t - 4]+ 
    
    wage_coeff_mapty_lag1_wage * decomposition_rpe$mapty[row_t - 1] +
    
    wage_coeff_dummy_q2 * decomposition_rpe$dummy_q2_2020[row_t] +
    wage_coeff_dummy_q3 * decomposition_rpe$dummy_q3_2020[row_t]
  
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
    price_coeff_shortage_4 * decomposition_rpe$shortage[row_t - 4] +
    
    price_coeff_mapty_lag1 * decomposition_rpe$mapty[row_t-1]
  

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
  mapty        = 0, cu           = 0,
  dummy_q2_2020  = ifelse(quarter_dates == as.Date("2020-04-01"), 1, 0),
  dummy_q3_2020  = ifelse(quarter_dates == as.Date("2020-07-01"), 1, 0)
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
    wage_coeff_VU_lag4 * decomposition_rpf$vu[row_t - 4] +
    
    wage_coeff_mapty_lag1_wage * decomposition_rpf$mapty[row_t - 1] +
    
    wage_coeff_dummy_q2 * decomposition_rpf$dummy_q2_2020[row_t] +
    wage_coeff_dummy_q3 * decomposition_rpf$dummy_q3_2020[row_t]
  
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
    price_coeff_shortage_4 * decomposition_rpf$shortage[row_t - 4] +
    
    price_coeff_mapty_lag1 * decomposition_rpf$mapty[row_t-1]
  
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
  mapty            = 0, cu               = 0,
  dummy_q2_2020  = ifelse(quarter_dates == as.Date("2020-04-01"), 1, 0),
  dummy_q3_2020  = ifelse(quarter_dates == as.Date("2020-07-01"), 1, 0)
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

decomposition_shortage <- decomposition_shortage %>%
  mutate(shortage = if_else(Quarter >= as.Date("2019-10-01"), 0, shortage))


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
    wage_coeff_VU_lag4 * decomposition_shortage$vu[row_t - 4] +
    
    wage_coeff_mapty_lag1_wage * decomposition_shortage$mapty[row_t - 1] +
    
    wage_coeff_dummy_q2 * decomposition_shortage$dummy_q2_2020[row_t] +
    wage_coeff_dummy_q3 * decomposition_shortage$dummy_q3_2020[row_t]

  
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
    price_coeff_shortage_4 * decomposition_shortage$shortage[row_t - 4] +
    
    price_coeff_mapty_lag1 * decomposition_shortage$mapty[row_t-1]
    
  
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
  mapty        = 0, cu           = 0,
  dummy_q2_2020  = ifelse(quarter_dates == as.Date("2020-04-01"), 1, 0),
  dummy_q3_2020  = ifelse(quarter_dates == as.Date("2020-07-01"), 1, 0)
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
    wage_coeff_VU_lag4 * decomposition_vu$vu[row_t - 4] +
    
    wage_coeff_mapty_lag1_wage * decomposition_vu$mapty[row_t - 1] +
    
    wage_coeff_dummy_q2 * decomposition_vu$dummy_q2_2020[row_t] +
    wage_coeff_dummy_q3 * decomposition_vu$dummy_q3_2020[row_t]
  
  
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
    price_coeff_shortage_4 * decomposition_vu$shortage[row_t - 4] +
    
    price_coeff_mapty_lag1 * decomposition_shortage$mapty[row_t-1]
  
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

# Decomposition mapty Removed ---------------------------------------------
cf5_equation <- long_term %>%
  select(Quarter, CF5) %>%
  left_join(delta_p %>% select(Quarter, delta_p), by = "Quarter")

quarter_dates <- seq(as.Date("2017-10-01"), as.Date("2023-01-01"), by = "quarter")

decomposition_mapty <- data.frame(
  Quarter          = quarter_dates,
  gw_sim_mapty     = 0, gp_sim_mapty     = 0,
  cf1_sim_mapty    = 0, cf3_sim_mapty    = 0,
  rpe              = 0, rpf              = 0,
  vu               = 0, shortage         = 0,
  mapty            = 0, cu               = 0,
  dummy_q2_2020    = ifelse(quarter_dates == as.Date("2020-04-01"), 1, 0),
  dummy_q3_2020    = ifelse(quarter_dates == as.Date("2020-07-01"), 1, 0)
)

for(x in list(
  list(df = VU,            col = "VU",          target = "vu"),
  list(df = productivity,  col = "mapty",       target = "mapty"),
  list(df = RPE_Data,      col = "RPE",         target = "rpe"),
  list(df = RPF_Data,      col = "RPF",         target = "rpf"),
  list(df = shortages,     col = "shortage",    target = "shortage")
)) {
  decomposition_mapty <- merge(
    decomposition_mapty, x$df[, c("Quarter", x$col)],
    by = "Quarter", all.x = TRUE
  )
  decomposition_mapty[[x$target]] <- decomposition_mapty[[x$col]]
  decomposition_mapty[[x$col]] <- NULL
}

quartale_short <- seq(as.Date("2017-10-01"), as.Date("2019-07-01"), by = "quarter")
rows_short <- which(decomposition_mapty$Quarter %in% quartale_short)

subset_wage  <- subset(Wage_equation, Quarter %in% quartale_short)
subset_price <- subset(price_equation,  Quarter %in% quartale_short)
subset_cf5   <- subset(cf5_equation,             Quarter %in% quartale_short)

decomposition_mapty$gw_sim_mapty[rows_short]  <- subset_wage$delta_w
decomposition_mapty$cf1_sim_mapty[rows_short] <- subset_wage$CF1
decomposition_mapty$cu[rows_short]            <- subset_wage$CU
decomposition_mapty$gp_sim_mapty[rows_short]  <- subset_price$delta_p
decomposition_mapty$cf3_sim_mapty[rows_short] <- subset_cf5$CF5

if ("mapty.x" %in% names(decomposition_mapty)) decomposition_mapty$mapty.x <- NULL
if ("mapty.y" %in% names(decomposition_mapty)) names(decomposition_mapty)[names(decomposition_mapty) == "mapty.y"] <- "mapty"
if ("shortage.x" %in% names(decomposition_mapty)) decomposition_mapty$shortage.x <- NULL
if ("shortage.y" %in% names(decomposition_mapty)) names(decomposition_mapty)[names(decomposition_mapty) == "shortage.y"] <- "shortage"

avg_mapty <- decomposition_mapty %>%
  filter(Quarter < as.Date("2019-10-01")) %>%
  summarise(mean_mapty = mean(mapty, na.rm = TRUE)) %>%
  pull(mean_mapty)

decomposition_mapty <- decomposition_mapty %>%
  mutate(mapty = if_else(Quarter >= as.Date("2019-10-01"), avg_mapty, mapty))

start_quarter <- as.Date("2019-10-01")
end_quarter   <- as.Date("2023-01-01")
row_start <- which(decomposition_mapty$Quarter == start_quarter)
row_end   <- which(decomposition_mapty$Quarter == end_quarter)

for (row_t in row_start:row_end) {
  decomposition_mapty$gw_sim_mapty[row_t] <- wage_coeff_intercept +
    wage_coeff_wage_delta_w_lag1 * decomposition_mapty$gw_sim_mapty[row_t - 1] +
    wage_coeff_wage_delta_w_lag2 * decomposition_mapty$gw_sim_mapty[row_t - 2] +
    wage_coeff_wage_delta_w_lag3 * decomposition_mapty$gw_sim_mapty[row_t - 3] +
    wage_coeff_wage_delta_w_lag4 * decomposition_mapty$gw_sim_mapty[row_t - 4] +
    wage_coeff_CF1_lag1 * decomposition_mapty$cf1_sim_mapty[row_t - 1] +
    wage_coeff_CF1_lag2 * decomposition_mapty$cf1_sim_mapty[row_t - 2] +
    wage_coeff_CF1_lag3 * decomposition_mapty$cf1_sim_mapty[row_t - 3] +
    wage_coeff_CF1_lag4 * decomposition_mapty$cf1_sim_mapty[row_t - 4] +
    wage_coeff_CU_lag1 * decomposition_mapty$cu[row_t - 1] +
    wage_coeff_CU_lag2 * decomposition_mapty$cu[row_t - 2] +
    wage_coeff_CU_lag3 * decomposition_mapty$cu[row_t - 3] +
    wage_coeff_CU_lag4 * decomposition_mapty$cu[row_t - 4] +
    wage_coeff_VU_lag1 * decomposition_mapty$vu[row_t - 1] +
    wage_coeff_VU_lag2 * decomposition_mapty$vu[row_t - 2] +
    wage_coeff_VU_lag3 * decomposition_mapty$vu[row_t - 3] +
    wage_coeff_VU_lag4 * decomposition_mapty$vu[row_t - 4] +
    wage_coeff_mapty_lag1_wage * decomposition_mapty$mapty[row_t - 1] +
    wage_coeff_dummy_q2 * decomposition_mapty$dummy_q2_2020[row_t] +
    wage_coeff_dummy_q3 * decomposition_mapty$dummy_q3_2020[row_t]
  
  decomposition_mapty$gp_sim_mapty[row_t] <- price_coeff_intercept +
    price_coeff_delta_p_lag1 * decomposition_mapty$gp_sim_mapty[row_t - 1] +
    price_coeff_delta_p_lag2 * decomposition_mapty$gp_sim_mapty[row_t - 2] +
    price_coeff_delta_p_lag3 * decomposition_mapty$gp_sim_mapty[row_t - 3] +
    price_coeff_delta_p_lag4 * decomposition_mapty$gp_sim_mapty[row_t - 4] +
    price_coeff_delta_w_lag0 * decomposition_mapty$gw_sim_mapty[row_t] +
    price_coeff_delta_w_lag1 * decomposition_mapty$gw_sim_mapty[row_t - 1] +
    price_coeff_delta_w_lag2 * decomposition_mapty$gw_sim_mapty[row_t - 2] +
    price_coeff_delta_w_lag3 * decomposition_mapty$gw_sim_mapty[row_t - 3] +
    price_coeff_delta_w_lag4 * decomposition_mapty$gw_sim_mapty[row_t - 4] +
    price_coeff_RPE_0 * decomposition_mapty$rpe[row_t] +
    price_coeff_RPE_1 * decomposition_mapty$rpe[row_t - 1] +
    price_coeff_RPE_2 * decomposition_mapty$rpe[row_t - 2] +
    price_coeff_RPE_3 * decomposition_mapty$rpe[row_t - 3] +
    price_coeff_RPE_4 * decomposition_mapty$rpe[row_t - 4] +
    price_coeff_RPF_0 * decomposition_mapty$rpf[row_t] +
    price_coeff_RPF_1 * decomposition_mapty$rpf[row_t - 1] +
    price_coeff_RPF_2 * decomposition_mapty$rpf[row_t - 2] +
    price_coeff_RPF_3 * decomposition_mapty$rpf[row_t - 3] +
    price_coeff_RPF_4 * decomposition_mapty$rpf[row_t - 4] +
    price_coeff_shortage_0 * decomposition_mapty$shortage[row_t] +
    price_coeff_shortage_1 * decomposition_mapty$shortage[row_t - 1] +
    price_coeff_shortage_2 * decomposition_mapty$shortage[row_t - 2] +
    price_coeff_shortage_3 * decomposition_mapty$shortage[row_t - 3] +
    price_coeff_shortage_4 * decomposition_mapty$shortage[row_t - 4] +
    price_coeff_mapty_lag1 * decomposition_mapty$mapty[row_t - 1]
  
  decomposition_mapty$cu[row_t] <-
    mean(decomposition_mapty$gp_sim_mapty[(row_t-1):(row_t-4)]) -
    decomposition_mapty$cf1_sim_mapty[row_t-4]
  
  decomposition_mapty$cf3_sim_mapty[row_t] <-
    CF5_coeff_cf5_lag1 * decomposition_mapty$cf3_sim_mapty[row_t - 1] +
    CF5_coeff_cf5_lag2 * decomposition_mapty$cf3_sim_mapty[row_t - 2] +
    CF5_coeff_cf5_lag3 * decomposition_mapty$cf3_sim_mapty[row_t - 3] +
    CF5_coeff_cf5_lag4 * decomposition_mapty$cf3_sim_mapty[row_t - 4] +
    CF5_coeff_deltap_0 * decomposition_mapty$gp_sim_mapty[row_t] +
    CF5_coeff_deltap_1 * decomposition_mapty$gp_sim_mapty[row_t - 1] +
    CF5_coeff_deltap_2 * decomposition_mapty$gp_sim_mapty[row_t - 2] +
    CF5_coeff_deltap_3 * decomposition_mapty$gp_sim_mapty[row_t - 3] +
    CF5_coeff_deltap_4 * decomposition_mapty$gp_sim_mapty[row_t - 4]
  
  decomposition_mapty$cf1_sim_mapty[row_t] <-
    CF1_coeff_cf1_lag1 * decomposition_mapty$cf1_sim_mapty[row_t - 1] +
    CF1_coeff_cf1_lag2 * decomposition_mapty$cf1_sim_mapty[row_t - 2] +
    CF1_coeff_cf1_lag3 * decomposition_mapty$cf1_sim_mapty[row_t - 3] +
    CF1_coeff_cf1_lag4 * decomposition_mapty$cf1_sim_mapty[row_t - 4] +
    CF1_coeff_CF5_0 * decomposition_mapty$cf3_sim_mapty[row_t] +
    CF1_coeff_CF5_1 * decomposition_mapty$cf3_sim_mapty[row_t - 1] +
    CF1_coeff_CF5_2 * decomposition_mapty$cf3_sim_mapty[row_t - 2] +
    CF1_coeff_CF5_3 * decomposition_mapty$cf3_sim_mapty[row_t - 3] +
    CF1_coeff_CF5_4 * decomposition_mapty$cf3_sim_mapty[row_t - 4] +
    CF1_coeff_deltap_0 * decomposition_mapty$gp_sim_mapty[row_t] +
    CF1_coeff_deltap_1 * decomposition_mapty$gp_sim_mapty[row_t - 1] +
    CF1_coeff_deltap_2 * decomposition_mapty$gp_sim_mapty[row_t - 2] +
    CF1_coeff_deltap_3 * decomposition_mapty$gp_sim_mapty[row_t - 3] +
    CF1_coeff_deltap_4 * decomposition_mapty$gp_sim_mapty[row_t - 4]
}

# Decomposition dummy removed ---------------------------------------------
cf5_equation <- long_term %>%
  select(Quarter, CF5) %>%
  left_join(delta_p %>% select(Quarter, delta_p), by = "Quarter")

quarter_dates <- seq(as.Date("2017-10-01"), as.Date("2023-01-01"), by = "quarter")

decomposition_dummy <- data.frame(
  Quarter          = quarter_dates,
  gw_sim_dummy     = 0, gp_sim_dummy     = 0,
  cf1_sim_dummy    = 0, cf3_sim_dummy    = 0,
  rpe              = 0, rpf              = 0,
  vu               = 0, shortage         = 0,
  mapty            = 0, cu               = 0,
  dummy_q2_2020    = 0,
  dummy_q3_2020    = 0
)

for(x in list(
  list(df = VU,            col = "VU",          target = "vu"),
  list(df = productivity,  col = "mapty",       target = "mapty"),
  list(df = RPE_Data,      col = "RPE",         target = "rpe"),
  list(df = RPF_Data,      col = "RPF",         target = "rpf"),
  list(df = shortages,     col = "shortage",    target = "shortage")
)) {
  decomposition_dummy <- merge(
    decomposition_dummy, x$df[, c("Quarter", x$col)],
    by = "Quarter", all.x = TRUE
  )
  decomposition_dummy[[x$target]] <- decomposition_dummy[[x$col]]
  decomposition_dummy[[x$col]] <- NULL
}

quartale_short <- seq(as.Date("2017-10-01"), as.Date("2019-07-01"), by = "quarter")
rows_short <- which(decomposition_dummy$Quarter %in% quartale_short)

subset_wage  <- subset(Wage_equation, Quarter %in% quartale_short)
subset_price <- subset(price_equation,  Quarter %in% quartale_short)
subset_cf5   <- subset(cf5_equation,             Quarter %in% quartale_short)

decomposition_dummy$gw_sim_dummy[rows_short]  <- subset_wage$delta_w
decomposition_dummy$cf1_sim_dummy[rows_short] <- subset_wage$CF1
decomposition_dummy$cu[rows_short]            <- subset_wage$CU
decomposition_dummy$gp_sim_dummy[rows_short]  <- subset_price$delta_p
decomposition_dummy$cf3_sim_dummy[rows_short] <- subset_cf5$CF5

if ("mapty.x" %in% names(decomposition_dummy)) decomposition_dummy$mapty.x <- NULL
if ("mapty.y" %in% names(decomposition_dummy)) names(decomposition_dummy)[names(decomposition_dummy) == "mapty.y"] <- "mapty"
if ("shortage.x" %in% names(decomposition_dummy)) decomposition_dummy$shortage.x <- NULL
if ("shortage.y" %in% names(decomposition_dummy)) names(decomposition_dummy)[names(decomposition_dummy) == "shortage.y"] <- "shortage"

start_quarter <- as.Date("2019-10-01")
end_quarter   <- as.Date("2023-01-01")
row_start <- which(decomposition_dummy$Quarter == start_quarter)
row_end   <- which(decomposition_dummy$Quarter == end_quarter)

for (row_t in row_start:row_end) {
  decomposition_dummy$gw_sim_dummy[row_t] <- wage_coeff_intercept +
    wage_coeff_wage_delta_w_lag1 * decomposition_dummy$gw_sim_dummy[row_t - 1] +
    wage_coeff_wage_delta_w_lag2 * decomposition_dummy$gw_sim_dummy[row_t - 2] +
    wage_coeff_wage_delta_w_lag3 * decomposition_dummy$gw_sim_dummy[row_t - 3] +
    wage_coeff_wage_delta_w_lag4 * decomposition_dummy$gw_sim_dummy[row_t - 4] +
    wage_coeff_CF1_lag1 * decomposition_dummy$cf1_sim_dummy[row_t - 1] +
    wage_coeff_CF1_lag2 * decomposition_dummy$cf1_sim_dummy[row_t - 2] +
    wage_coeff_CF1_lag3 * decomposition_dummy$cf1_sim_dummy[row_t - 3] +
    wage_coeff_CF1_lag4 * decomposition_dummy$cf1_sim_dummy[row_t - 4] +
    wage_coeff_CU_lag1 * decomposition_dummy$cu[row_t - 1] +
    wage_coeff_CU_lag2 * decomposition_dummy$cu[row_t - 2] +
    wage_coeff_CU_lag3 * decomposition_dummy$cu[row_t - 3] +
    wage_coeff_CU_lag4 * decomposition_dummy$cu[row_t - 4] +
    wage_coeff_VU_lag1 * decomposition_dummy$vu[row_t - 1] +
    wage_coeff_VU_lag2 * decomposition_dummy$vu[row_t - 2] +
    wage_coeff_VU_lag3 * decomposition_dummy$vu[row_t - 3] +
    wage_coeff_VU_lag4 * decomposition_dummy$vu[row_t - 4] +
    wage_coeff_mapty_lag1_wage * decomposition_dummy$mapty[row_t - 1] +
    wage_coeff_dummy_q2 * 0 +
    wage_coeff_dummy_q3 * 0
  
  decomposition_dummy$gp_sim_dummy[row_t] <- price_coeff_intercept +
    price_coeff_delta_p_lag1 * decomposition_dummy$gp_sim_dummy[row_t - 1] +
    price_coeff_delta_p_lag2 * decomposition_dummy$gp_sim_dummy[row_t - 2] +
    price_coeff_delta_p_lag3 * decomposition_dummy$gp_sim_dummy[row_t - 3] +
    price_coeff_delta_p_lag4 * decomposition_dummy$gp_sim_dummy[row_t - 4] +
    price_coeff_delta_w_lag0 * decomposition_dummy$gw_sim_dummy[row_t] +
    price_coeff_delta_w_lag1 * decomposition_dummy$gw_sim_dummy[row_t - 1] +
    price_coeff_delta_w_lag2 * decomposition_dummy$gw_sim_dummy[row_t - 2] +
    price_coeff_delta_w_lag3 * decomposition_dummy$gw_sim_dummy[row_t - 3] +
    price_coeff_delta_w_lag4 * decomposition_dummy$gw_sim_dummy[row_t - 4] +
    price_coeff_RPE_0 * decomposition_dummy$rpe[row_t] +
    price_coeff_RPE_1 * decomposition_dummy$rpe[row_t - 1] +
    price_coeff_RPE_2 * decomposition_dummy$rpe[row_t - 2] +
    price_coeff_RPE_3 * decomposition_dummy$rpe[row_t - 3] +
    price_coeff_RPE_4 * decomposition_dummy$rpe[row_t - 4] +
    price_coeff_RPF_0 * decomposition_dummy$rpf[row_t] +
    price_coeff_RPF_1 * decomposition_dummy$rpf[row_t - 1] +
    price_coeff_RPF_2 * decomposition_dummy$rpf[row_t - 2] +
    price_coeff_RPF_3 * decomposition_dummy$rpf[row_t - 3] +
    price_coeff_RPF_4 * decomposition_dummy$rpf[row_t - 4] +
    price_coeff_shortage_0 * decomposition_dummy$shortage[row_t] +
    price_coeff_shortage_1 * decomposition_dummy$shortage[row_t - 1] +
    price_coeff_shortage_2 * decomposition_dummy$shortage[row_t - 2] +
    price_coeff_shortage_3 * decomposition_dummy$shortage[row_t - 3] +
    price_coeff_shortage_4 * decomposition_dummy$shortage[row_t - 4] +
    price_coeff_mapty_lag1 * decomposition_dummy$mapty[row_t - 1]
  
  decomposition_dummy$cu[row_t] <-
    mean(decomposition_dummy$gp_sim_dummy[(row_t-1):(row_t-4)]) -
    decomposition_dummy$cf1_sim_dummy[row_t-4]
  
  decomposition_dummy$cf3_sim_dummy[row_t] <-
    CF5_coeff_cf5_lag1 * decomposition_dummy$cf3_sim_dummy[row_t - 1] +
    CF5_coeff_cf5_lag2 * decomposition_dummy$cf3_sim_dummy[row_t - 2] +
    CF5_coeff_cf5_lag3 * decomposition_dummy$cf3_sim_dummy[row_t - 3] +
    CF5_coeff_cf5_lag4 * decomposition_dummy$cf3_sim_dummy[row_t - 4] +
    CF5_coeff_deltap_0 * decomposition_dummy$gp_sim_dummy[row_t] +
    CF5_coeff_deltap_1 * decomposition_dummy$gp_sim_dummy[row_t - 1] +
    CF5_coeff_deltap_2 * decomposition_dummy$gp_sim_dummy[row_t - 2] +
    CF5_coeff_deltap_3 * decomposition_dummy$gp_sim_dummy[row_t - 3] +
    CF5_coeff_deltap_4 * decomposition_dummy$gp_sim_dummy[row_t - 4]
  
  decomposition_dummy$cf1_sim_dummy[row_t] <-
    CF1_coeff_cf1_lag1 * decomposition_dummy$cf1_sim_dummy[row_t - 1] +
    CF1_coeff_cf1_lag2 * decomposition_dummy$cf1_sim_dummy[row_t - 2] +
    CF1_coeff_cf1_lag3 * decomposition_dummy$cf1_sim_dummy[row_t - 3] +
    CF1_coeff_cf1_lag4 * decomposition_dummy$cf1_sim_dummy[row_t - 4] +
    CF1_coeff_CF5_0 * decomposition_dummy$cf3_sim_dummy[row_t] +
    CF1_coeff_CF5_1 * decomposition_dummy$cf3_sim_dummy[row_t - 1] +
    CF1_coeff_CF5_2 * decomposition_dummy$cf3_sim_dummy[row_t - 2] +
    CF1_coeff_CF5_3 * decomposition_dummy$cf3_sim_dummy[row_t - 3] +
    CF1_coeff_CF5_4 * decomposition_dummy$cf3_sim_dummy[row_t - 4] +
    CF1_coeff_deltap_0 * decomposition_dummy$gp_sim_dummy[row_t] +
    CF1_coeff_deltap_1 * decomposition_dummy$gp_sim_dummy[row_t - 1] +
    CF1_coeff_deltap_2 * decomposition_dummy$gp_sim_dummy[row_t - 2] +
    CF1_coeff_deltap_3 * decomposition_dummy$gp_sim_dummy[row_t - 3] +
    CF1_coeff_deltap_4 * decomposition_dummy$gp_sim_dummy[row_t - 4]
}



# Contribution ------------------------------------------------------------
composition <- data.frame(
  Quarter           = decomposition_normal$Quarter,
  gw_sim_orig       = decomposition_normal$gw_sim_orig,
  gw_sim_rpe        = decomposition_rpe$gw_sim_rpe,
  gw_sim_rpf        = decomposition_rpf$gw_sim_rpf,
  gw_sim_shortage   = decomposition_shortage$gw_sim_shortage,
  gw_sim_vu         = decomposition_vu$gw_sim_vu,
  gw_sim_mapty      = decomposition_mapty$gw_sim_mapty,
  gw_sim_dummy      = decomposition_dummy$gw_sim_dummy,
  gp_sim_orig       = decomposition_normal$gp_sim_orig,
  gp_sim_rpe        = decomposition_rpe$gp_sim_rpe,
  gp_sim_rpf        = decomposition_rpf$gp_sim_rpf,
  gp_sim_shortage   = decomposition_shortage$gp_sim_shortage,
  gp_sim_vu         = decomposition_vu$gp_sim_vu,
  gp_sim_mapty      = decomposition_mapty$gp_sim_mapty,
  gp_sim_dummy      = decomposition_dummy$gp_sim_dummy,
  cf1_sim_orig      = decomposition_normal$cf1_sim_orig,
  cf1_sim_rpe       = decomposition_rpe$cf1_sim_rpe,
  cf1_sim_rpf       = decomposition_rpf$cf1_sim_rpf,
  cf1_sim_shortage  = decomposition_shortage$cf1_sim_shortage,
  cf1_sim_vu        = decomposition_vu$cf1_sim_vu,
  cf1_sim_mapty     = decomposition_mapty$cf1_sim_mapty,
  cf1_sim_dummy     = decomposition_dummy$cf1_sim_dummy,
  cf3_sim_orig      = decomposition_normal$cf3_sim_orig,
  cf3_sim_rpe       = decomposition_rpe$cf3_sim_rpe,
  cf3_sim_rpf       = decomposition_rpf$cf3_sim_rpf,
  cf3_sim_shortage  = decomposition_shortage$cf3_sim_shortage,
  cf3_sim_vu        = decomposition_vu$cf3_sim_vu,
  cf3_sim_mapty     = decomposition_mapty$cf3_sim_mapty,
  cf3_sim_dummy     = decomposition_dummy$cf3_sim_dummy
)


for (type in c("rpe", "rpf", "shortage", "vu", "mapty", "dummy")) {
  composition[[paste0("gw_contr_", type)]] <- composition$gw_sim_orig - composition[[paste0("gw_sim_", type)]]
  composition[[paste0("gp_contr_", type)]] <- composition$gp_sim_orig - composition[[paste0("gp_sim_", type)]]
}

for (type in c("rpe", "rpf", "shortage", "vu", "mapty", "dummy")) {
  composition[[paste0("cf1_contr_", type)]] <- composition$cf1_sim_orig - composition[[paste0("cf1_sim_", type)]]
  composition[[paste0("cf3_contr_", type)]] <- composition$cf3_sim_orig - composition[[paste0("cf3_sim_", type)]]
}


composition <- composition[, c(
  "Quarter",
  "gw_sim_orig",      "gw_sim_rpe",      "gw_contr_rpe",
  "gw_sim_rpf",       "gw_contr_rpf",
  "gw_sim_shortage",  "gw_contr_shortage",
  "gw_sim_vu",        "gw_contr_vu",
  "gw_sim_mapty",     "gw_contr_mapty",
  "gw_sim_dummy",     "gw_contr_dummy",
  "cf1_sim_orig",     "cf1_contr_rpe",
  "cf1_sim_rpe",      "cf1_contr_rpe",
  "cf1_sim_rpf",      "cf1_contr_rpf",
  "cf1_sim_shortage", "cf1_contr_shortage",
  "cf1_sim_vu",       "cf1_contr_vu",
  "cf1_sim_mapty",    "cf1_contr_mapty",
  "cf1_sim_dummy",    "cf1_contr_dummy",
  "cf3_sim_orig",     "cf3_contr_rpe",
  "cf3_sim_rpe",      "cf3_contr_rpe",
  "cf3_sim_rpf",      "cf3_contr_rpf",
  "cf3_sim_shortage", "cf3_contr_shortage",
  "cf3_sim_vu",       "cf3_contr_vu",
  "cf3_sim_mapty",    "cf3_contr_mapty",
  "cf3_sim_dummy",    "cf3_contr_dummy",
  "gp_sim_orig",      "gp_sim_rpe",      "gp_contr_rpe",
  "gp_sim_rpf",       "gp_contr_rpf",
  "gp_sim_shortage",  "gp_contr_shortage",
  "gp_sim_vu",        "gp_contr_vu",
  "gp_sim_mapty",     "gp_contr_mapty",
  "gp_sim_dummy",     "gp_contr_dummy"
)]


composition$sum_contr_gw <- rowSums(composition[, grep("^gw_contr_", names(composition))])
composition$sum_contr_gp <- rowSums(composition[, grep("^gp_contr_", names(composition))])
composition$sum_contr_cf1 <- rowSums(composition[, grep("^cf1_contr_", names(composition))])
composition$sum_contr_cf3 <- rowSums(composition[, grep("^cf3_contr_", names(composition))])

composition$initial_conditions_gw <- composition$gw_sim_orig - composition$sum_contr_gw
composition$initial_conditions_gp <- composition$gp_sim_orig - composition$sum_contr_gp
composition$initial_conditions_cf1 <- composition$cf1_sim_orig - composition$sum_contr_cf1
composition$initial_conditions_cf3 <- composition$cf3_sim_orig - composition$sum_contr_cf3

composition$Quarter <- as.Date(composition$Quarter)
composition$actual_gp <- price_equation$delta_p[match(composition$Quarter, as.Date(price_equation$Quarter))]
composition$actual_gw <- delta_w$delta_w[match(composition$Quarter, as.Date(delta_w$Quarter))]
composition$actual_cf1 <- exp_short$CF1[match(composition$Quarter, as.Date(exp_short$Quarter))]
composition$actual_cf3 <- long_term$CF5[match(composition$Quarter, as.Date(long_term$Quarter))]
composition <- composition %>%
  filter(Quarter >= as.Date("2019-10-01"))

# Decomposition Price Inflation -------------------------------------------
composition_long <- composition %>%
  select(
    Quarter,
    initial_conditions_gp,
    gp_contr_vu,
    gp_contr_rpe,
    gp_contr_rpf,
    gp_contr_shortage,
    gp_contr_mapty,
    gp_contr_dummy
  ) %>%
  pivot_longer(
    cols = -Quarter,
    names_to = "Component",
    values_to = "Contribution"
  ) %>%
  mutate(
    Component = factor(
      Component,
      levels = c(
        "initial_conditions_gp",
        "gp_contr_vu",
        "gp_contr_rpe",
        "gp_contr_rpf",
        "gp_contr_shortage",
        "gp_contr_mapty",
        "gp_contr_dummy"
      ),
      labels = c(
        "Initial conditions",
        "Labour market",
        "Energy Prices",
        "Food Prices",
        "Shortages",
        "Productivity",
        "Lockdown"
      )
    )
  )

composition$Quarter <- as.Date(composition$Quarter)
x_labels_fmt <- format(as.yearqtr(composition$Quarter), "Q%q %Y")

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
    color = "black", linewidth = 1
  ) +
  scale_fill_manual(
    values = c(
      "Initial conditions" = "gray40",
      "Labour market" = "darkred",
      "Energy Prices" = "steelblue",
      "Food Prices" = "darkblue",
      "Shortages" = "gold",
      "Productivity" = "deeppink",
      "Lockdown" = "forestgreen"
    ),
    name = NULL
  ) +
  scale_x_date(
    breaks = composition$Quarter,
    labels = x_labels_fmt,
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    name = "Percent",
    breaks = seq(-3, 4.5, by = 1),
    limits = c(-3, 4.5)
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = "serif", base_size = 13) +
  theme(
    panel.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 10),
    axis.text.y = element_text(color = "black", size = 10),
    legend.position = "bottom",
    legend.text = element_text(size = 9),
    legend.key.size = unit(0.6, "lines"),
    legend.spacing.x = unit(0.4, "cm"),
    plot.caption = element_text(hjust = 0, size = 9, margin = margin(t = 10)),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  labs(
    y = "Percent",
    caption = "Source: Own computation. Note: The black solid line shows the actual observed Price Inflation."
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
    gw_contr_shortage,
    gw_contr_mapty,
    gw_contr_dummy
  ) %>%
  pivot_longer(
    cols = -Quarter,
    names_to = "Component",
    values_to = "Contribution"
  ) %>%
  mutate(
    Component = factor(
      Component,
      levels = c(
        "initial_conditions_gw",
        "gw_contr_vu",
        "gw_contr_rpe",
        "gw_contr_rpf",
        "gw_contr_shortage",
        "gw_contr_mapty",
        "gw_contr_dummy"
      ),
      labels = c(
        "Initial conditions",
        "Labour market",
        "Energy Prices",
        "Food Prices",
        "Shortages",
        "Productivity",
        "Lockdown"
      )
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
    color = "black", size = 1.1
  ) +
  scale_fill_manual(
    values = c(
      "Initial conditions" = "gray40",
      "Labour market" = "darkred",
      "Energy Prices" = "steelblue",
      "Food Prices" = "darkblue",
      "Shortages" = "gold",
      "Productivity" = "deeppink",
      "Lockdown" = "forestgreen"
    ),
    name = NULL
  ) +
  scale_x_date(
    breaks = composition$Quarter,
    labels = x_labels_fmt,
    expand = c(0.01, 0.01)
  ) +
  scale_y_continuous(
    name = "Percent",
    breaks = seq(-4, 3, by = 1),
    limits = c(-4, 3)
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = "serif", base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 9),
    legend.key.size = unit(0.6, "lines"),
    legend.spacing.x = unit(0.4, "cm"),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.ticks = element_line(color = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    plot.margin = margin(10, 20, 10, 10)
  ) +
  labs(
    y = "Percent",
    caption = "Source: Own computation. Note: The black solid line shows the actual observed Wage Inflation."
  )

print(gw_plot)


# Decomposition short-term expectations -----------------------------------
composition$Quarter <- as.Date(composition$Quarter)
x_labels_fmt <- format(as.yearqtr(composition$Quarter), "Q%q %Y")

composition_long_cf1 <- composition %>%
  select(
    Quarter,
    initial_conditions_cf1,
    cf1_contr_rpe,
    cf1_contr_rpf,
    cf1_contr_shortage,
    cf1_contr_vu,
    cf1_contr_mapty,
    cf1_contr_dummy
  ) %>%
  pivot_longer(
    cols = -Quarter,
    names_to = "Component",
    values_to = "Contribution"
  ) %>%
  mutate(
    Component = factor(
      Component,
      levels = c(
        "initial_conditions_cf1",
        "cf1_contr_rpe",
        "cf1_contr_rpf",
        "cf1_contr_shortage",
        "cf1_contr_vu",
        "cf1_contr_mapty",
        "cf1_contr_dummy"
      ),
      labels = c(
        "Initial conditions",
        "Energy Prices",
        "Food Prices",
        "Shortages",
        "v/u",
        "Productivity",
        "Lockdown"
      )
    )
  )

cf1_plot <- ggplot() +
  geom_col(
    data = composition_long_cf1 %>%
      mutate(Component = fct_recode(Component, "Labour market" = "v/u")),
    aes(x = Quarter, y = Contribution, fill = Component),
    position = position_stack(reverse = TRUE),
    width = 80
  ) +
  geom_line(
    data = composition,
    aes(x = Quarter, y = actual_cf1),
    color = "black", size = 1.1
  ) +
  scale_fill_manual(
    values = c(
      "Initial conditions" = "gray40",
      "Energy Prices" = "steelblue",
      "Food Prices" = "darkblue",
      "Shortages" = "gold",
      "Labour market" = "darkred",
      "Productivity" = "deeppink",
      "Lockdown" = "forestgreen"
    ),
    name = NULL
  ) +
  scale_x_date(
    breaks = composition$Quarter,
    labels = x_labels_fmt,
    expand = c(0.01, 0.01)
  ) +
  scale_y_continuous(
    name = "Percent",
    breaks = seq(-1, 4, by = 1),  # Adjusted to include 0
    limits = c(-1.5, 4)
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = "serif", base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 9),
    legend.key.size = unit(0.6, "lines"),
    legend.spacing.x = unit(0.4, "cm"),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.ticks = element_line(color = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    plot.margin = margin(10, 20, 10, 10)
  ) +
  labs(
    y = "Percent",
    caption = "Source: Own computation. Note: The black solid line shows the actual short-term expectations."
  )

print(cf1_plot)


# Decomposition long-term expectations ------------------------------------
composition$Quarter <- as.Date(composition$Quarter)
x_labels_fmt <- format(as.yearqtr(composition$Quarter), "Q%q %Y")

composition_long_cf3 <- composition %>%
  select(
    Quarter,
    initial_conditions_cf3,
    cf3_contr_rpe,
    cf3_contr_rpf,
    cf3_contr_shortage,
    cf3_contr_vu,
    cf3_contr_mapty,
    cf3_contr_dummy
  ) %>%
  pivot_longer(
    cols = -Quarter,
    names_to = "Component",
    values_to = "Contribution"
  ) %>%
  mutate(
    Component = factor(
      Component,
      levels = c(
        "initial_conditions_cf3",
        "cf3_contr_rpe",
        "cf3_contr_rpf",
        "cf3_contr_shortage",
        "cf3_contr_vu",
        "cf3_contr_mapty",
        "cf3_contr_dummy"
      ),
      labels = c(
        "Initial conditions",
        "Energy Prices",
        "Food Prices",
        "Shortages",
        "v/u",
        "Productivity",
        "Lockdown"
      )
    )
  )

cf3_plot <- ggplot() +
  geom_col(
    data = composition_long_cf3 %>%
      mutate(Component = fct_recode(Component, "Labour market" = "v/u")),
    aes(x = Quarter, y = Contribution, fill = Component),
    position = position_stack(reverse = TRUE),
    width = 80
  ) +
  geom_line(
    data = composition,
    aes(x = Quarter, y = actual_cf3),
    color = "black", size = 1.1
  ) +
  scale_fill_manual(
    values = c(
      "Initial conditions" = "gray40",
      "Energy Prices" = "steelblue",
      "Food Prices" = "darkblue",
      "Shortages" = "gold",
      "Labour market" = "darkred",
      "Productivity" = "deeppink",
      "Lockdown" = "forestgreen"
    ),
    name = NULL
  ) +
  scale_x_date(
    breaks = composition$Quarter,
    labels = x_labels_fmt,
    expand = c(0.01, 0.01)
  ) +
  scale_y_continuous(
    name = "Percent",
    breaks = seq(-0.5, 2, by = 0.5),
    limits = c(-0.5, 2)
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = "serif", base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 9),
    legend.key.size = unit(0.6, "lines"),
    legend.spacing.x = unit(0.4, "cm"),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.ticks = element_line(color = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    plot.margin = margin(10, 20, 10, 10)
  ) +
  labs(
    y = "Percent",
    caption = "Source: Own computation. Note: The black solid lines show the actual long-term expectations."
  )

print(cf3_plot)




