# CPI -Total, Energy, Food- Data CH----------------------------------------------------------------
CPI_all <- read_excel("HSG BA/R/Daten/CPI_test.xlsx")
CPI_all <- as.data.frame(t(CPI_all)) %>% rownames_to_column("Quarter")
colnames(CPI_all) <- c("Quarter", "CPI", "CPI_Food", "CPI_Energy")
CPI_all <- CPI_all %>% mutate(across(c(CPI, CPI_Food, CPI_Energy), as.numeric)) %>% slice(-1) %>% 
  na.omit()
rownames(CPI_all) <- NULL

CPI_all <- CPI_all %>%
  mutate(
    Quarter = str_replace_all(Quarter, "-", ""),
    Quarter = paste0(substr(Quarter, 1, 4), "-", 
                     case_when(
                       str_detect(Quarter, "Q1") ~ "01-01",
                       str_detect(Quarter, "Q2") ~ "04-01",
                       str_detect(Quarter, "Q3") ~ "07-01",
                       str_detect(Quarter, "Q4") ~ "10-01"
                     )),
    Quarter = ymd(Quarter)
  ) %>%
  arrange(Quarter)

start_year <- year(min(CPI_all$Quarter))
start_quarter <- quarter(min(CPI_all$Quarter))

ts_CPI <- ts(CPI_all$CPI, start = c(start_year, start_quarter), frequency = 4)
ts_CPI_Food <- ts(CPI_all$CPI_Food, start = c(start_year, start_quarter), frequency = 4)
ts_CPI_Energy <- ts(CPI_all$CPI_Energy, start = c(start_year, start_quarter), frequency = 4)

CPI_all$CPI_sa <- final(seas(ts_CPI))
CPI_all$CPI_Food_sa <- final(seas(ts_CPI_Food))
CPI_all$CPI_Energy_sa <- final(seas(ts_CPI_Energy))

CPI_total_CH <- CPI_all %>%
  select(Quarter, CPI_sa)%>%
  rename(Index = CPI_sa)

CPI_food_CH <- CPI_all %>%
  select(Quarter, CPI_Food_sa) %>%
  rename(index = CPI_Food_sa)

CPI_energy_CH <- CPI_all %>%
  select(Quarter, CPI_Energy_sa) %>%
  rename(Index = CPI_Energy_sa)

write_xlsx(CPI_total_CH, "CPI_CH_OECD.xlsx")
write_xlsx(CPI_food_CH, "CPI_Food_CH_OECD.xlsx")
write_xlsx(CPI_energy_CH, "CPI_Energy_CH_OECD.xlsx")
# Wages  ------------------------------------------------------------------
Wage_index <- read_excel("HSG BA/R/Daten/je-d-03.04.03.00.04(3).xlsx")
GDP <- read_csv("HSG BA/R/Daten/CPMNACSCAB1GQCH.csv")

wage_ts <- ts(Wage_index$Index, start = min(Wage_index$Quarter), frequency = 1)

GDP <- GDP %>%
  rename(Date = observation_date, GDP = CPMNACSCAB1GQCH) %>%
  mutate(Date = as.Date(Date))

gdp_ts <- ts(GDP$GDP, start = c(year(min(GDP$Date)), quarter(min(GDP$Date))), frequency = 4)

model_cl <- td(lohn_ts ~ gdp_ts, method = "chow-lin-maxlog")

wages_quarterly <- predict(model_cl)

wages_quarterly_df <- data.frame(
  Quarter = as.Date(as.yearqtr(time(wages_quarterly))),
  wages = as.numeric(wages_quarterly)
)

write_xlsx(wages_quarterly_df, "Wages_constructed_CH.xlsx")




# Shortages  --------------------------------------------------------------
shortages <- read_csv("HSG BA/R/Daten/multiTimeline(2).csv", skip = 2, col_names = "raw")[-1, ] %>%
  rename(Quarter = raw, Value = X2) %>%
  mutate(
    Quarter = as.Date(paste0(Quarter, "-01")),
    Value = as.numeric(Value)
  ) %>%
  bind_rows(
    data.frame(
      Quarter = seq(from = as.Date("1990-01-01"), to = as.Date("2003-12-01"), by = "month"),
      Value = 0
    )
  ) %>%
  arrange(Quarter) %>%
  mutate(Quarter = as.yearqtr(Quarter)) %>%
  group_by(Quarter) %>%
  summarise(shortage = mean(Value, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    Quarter = as.Date(Quarter),
    shortage_std = as.numeric(scale(shortage))
  )

write_xlsx(shortages, "Google_Trend_Shortages_CH.xlsx")

# Labour Market Tightness -------------------------------------------------
unemployed_quarter <- read_excel("~/HSG BA/R/Daten/2.1 Arbeitslosenquoten.xlsx") %>%
  slice(2:4) %>%                              
  select(-1) %>%                              
  t() %>%
  as_tibble(.name_repair = "minimal") %>%
  setNames(c("Quarter", "Metric", "Value")) %>%
  mutate(
    Quarter = dmy(paste0("01 ", Quarter)),
    Value = as.numeric(Value)
  ) %>%
  pivot_wider(names_from = Metric, values_from = Value) %>%
  rename(
    `Unemployment rate` = Arbeitslosenquote,
    `Unemployed people` = `Registrierte Arbeitslose`
  ) %>%
  slice(-374) %>%                                
  mutate(Quarter = floor_date(Quarter, unit = "quarter")) %>%
  group_by(Quarter) %>%
  summarise(
    `Unemployed people (quarterly)` = mean(`Unemployed people`, na.rm = TRUE),
    `Unemployment rate (quarterly)` = mean(`Unemployment rate`, na.rm = TRUE),
    .groups = "drop"
  )

vacancies_quarterly <- read_excel("~/HSG BA/R/Daten/Vacancies_SNB.xlsx") %>%
  mutate(Date = as.Date(paste0(Date, "-01"))) %>%
  mutate(Quarter = as.Date(as.yearqtr(Date))) %>%
  group_by(Quarter) %>%
  summarise(Vacancies = mean(Value, na.rm = TRUE), .groups = "drop")

VU <- vacancies_quarterly %>%
  rename(`Vacancies (SNB)` = Vacancies) %>%
  left_join(
    unemployed_quarter %>% select(Quarter, `Unemployed people (quarterly)`),
    by = "Quarter"
  ) %>%
  mutate(
    LMT = `Vacancies (SNB)` / `Unemployed people (quarterly)`
  ) %>%
  rename(VU = LMT)


write.xlsx(VU, file = "Labour_market_CH.xlsx", sheetName = "VU", overwrite = TRUE)



# Productivity ------------------------------------------------------------
productivity <- read_excel("HSG BA/R/Daten/CH/Productivity_CH.xlsx") %>%
  arrange(Quarter) %>%
  na.omit()

start_year <- year(min(productivity$Quarter))
start_quarter <- quarter(min(productivity$Quarter))

ts_index <- ts(productivity$Index, start = c(start_year, start_quarter), frequency = 4)
productivity$Index <- final(seas(ts_index))

write_xlsx(productivity, "Productivity_season_CH.xlsx")


# (short-term) Infl. Expectations -----------------------------------------

seco_clean <- read_excel("~/HSG BA/R/Daten/Konsumentenstimmung.xlsx", skip = 7)[1:7] %>%
  setNames(c("Year", "Quarter", "Konsumenten_Index", "Vergangene_Wirtschaft", 
             "Erwartete_Wirtschaft", "Vergangene_Preise", "expected_inflation")) %>%
  mutate(
    Quarter = as.integer(Quarter),
    Year = as.integer(Year),
    Month = case_when(Quarter == 1 ~ 1, Quarter == 2 ~ 4,
                      Quarter == 3 ~ 7, Quarter == 4 ~ 10),
    Date = make_date(Year, Month, 1),
    expected_inflation = as.numeric(expected_inflation)
  ) %>%
  filter(!is.na(expected_inflation)) %>%
  select(Date, expected_inflation)

seco_clean <- seco_clean %>%
  filter(!is.na(Date))


ggplot(seco_clean, aes(x = Date, y = expected_inflation)) +
  geom_line() +
  labs(
    title = "Expected Inflation (12 Months) – SECO Consumer Sentiments Survey",
    x = "Date",
    y = "Index (SECO-Sentiments)",
    caption = "Source: SECO, eigene Darstellung"
  ) +
  theme_minimal(base_size = 14)

inf_exp_pct <- read_excel("~/HSG BA/R/Daten/inflation_expectations.xlsx") %>%
  transmute(
    Quarter = as.Date(Quarter),
    expected_inflation_pct = `expected Inflation (short term)`
  )

seco_index <- seco_clean %>%
  transmute(
    Quarter = as.Date(Date),
    expected_inflation_index = expected_inflation
  )

model <- lm(expected_inflation_pct ~ expected_inflation_index,
            data = inner_join(seco_index, inf_exp_pct, by = "Quarter"))

summary(model)

seco_predicted <- seco_index %>%
  mutate(expected_inflation_predicted = predict(model, newdata = .))

plot_data <- left_join(seco_predicted, inf_exp_pct, by = "Quarter") %>%
  mutate(Type = if_else(is.na(expected_inflation_pct), "Predicted", "Observed"))

ggplot(plot_data, aes(x = Quarter, y = coalesce(expected_inflation_pct, expected_inflation_predicted), color = Type)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("Observed" = "steelblue", "Predicted" = "firebrick")) +
  labs(
    title = "Kurzfristige Inflationserwartungen – Beobachtet vs. Geschätzt (SECO)",
    x = "Quartal", y = "Inflationserwartung (%)",
    color = "Datenquelle",
    caption = "SECO & BFS, eigene Berechnung"
  ) +
  theme_minimal(base_size = 14)

plot_data %>%
  filter(Quarter >= as.Date("2011-10-01")) %>%
  pivot_longer(c(expected_inflation_predicted, expected_inflation_pct),
               names_to = "Variable", values_to = "Inflationserwartung") %>%
  ggplot(aes(x = Quarter, y = Inflationserwartung, color = Variable)) +
  geom_line(size = 1.2) +
  scale_color_manual(
    values = c("expected_inflation_predicted" = "firebrick",
               "expected_inflation_pct" = "steelblue"),
    labels = c("estimated (with SECO Data)", "actual")
  ) +
  labs(
    x = "Time", y = "in (%)",
    color = "Datenquelle",
    caption = "SECO & BFS, Own Computation"
  ) +
  theme_minimal(base_size = 14)

exp_short <- plot_data %>%
  transmute(
    Quarter,
    CF1 = if_else(Quarter <= as.Date("2011-07-01"),
                  expected_inflation_predicted,
                  expected_inflation_pct)
  )


write_xlsx(exp_short, "cf1_exp_CH.xlsx")

# (long-term) Infl. Expectations ------------------------------------------
long_term <- read_excel("~/HSG BA/R/Daten/inflation_expectations.xlsx") %>%
  transmute(
    Quarter = ymd(Quarter),
    long_term_exp = as.numeric(`expected Inflation (long term)`)
  ) %>%
  slice(-(1:36))    

long_term_ECB <- read_excel("HSG BA/R/Daten/long_term_inflation_ECB.xlsx") %>%
  mutate(Quarter = ymd(Quarter))


long_term <- long_term %>%
  left_join(long_term_ECB, by = "Quarter") %>%
  mutate(
    difference = Expectations - long_term_exp,
    exp_est = Expectations - mean(difference, na.rm = TRUE)
  ) %>%
  mutate(across(where(is.numeric), ~ round(., 1))) %>%
  mutate(
    long_term_exp = if_else(Quarter <= ymd("2011-10-01"), exp_est, long_term_exp)
  ) %>% 
  mutate(CF5 = long_term_exp)

write.xlsx(long_term, file = "cf5_exp_CH.xlsx", sheetName = "CF5", overwrite = TRUE)



# KOF Covid Index ---------------------------------------------------------

KOF_index_plus <- read_csv("HSG BA/R/Daten/kof_plus_data_export_2025-05-10_21_45_52.csv")

KOF_index_plus <- KOF_index_plus[, 1:2]
colnames(KOF_index_plus) <- c("Quarter", "Index")

KOF_index_plus <- KOF_index_plus %>%
  mutate(Quarter = as.yearqtr(as.Date(Quarter))) %>%
  group_by(Quarter) %>%
  summarise(Index = mean(Index, na.rm = TRUE)) %>%
  ungroup()



KOF_index_plus <- KOF_index_plus %>%
  mutate(Quarter = as.Date(as.yearqtr(Quarter), frac = 0))

missing_quarters <- seq(as.Date("1990-01-01"), as.Date("2019-10-01"), by = "quarter")
missing_data <- tibble(
  Quarter = missing_quarters,
  Index = 0
)

KOF_index_plus <- bind_rows(missing_data, KOF_index_plus) %>%
  arrange(Quarter)                      

write_xlsx(KOF_index_plus, path = "KOF_index_CH.xlsx")






# (short-term) model ------------------------------------------------------

plot_data %>%
  filter(Quarter >= as.Date("2011-10-01")) %>%
  pivot_longer(c(expected_inflation_predicted, expected_inflation_pct),
               names_to = "Variable", values_to = "Inflationserwartung") %>%
  ggplot(aes(x = Quarter, y = Inflationserwartung, color = Variable)) +
  geom_line(size = 1.2) +
  scale_color_manual(
    values = c("expected_inflation_predicted" = "firebrick",
               "expected_inflation_pct" = "steelblue"),
    labels = c("Estimated", "realised")
  ) +
  labs(
    x = NULL, 
    y = "Percent",
    color = NULL,
    caption = "Source: SECO, SNB and own computation."
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(color = "black", size = 12),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.caption = element_text(hjust = 0, size = 10),
    plot.margin = margin(15, 15, 15, 15),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = "black", linewidth = 0.5)
  )


