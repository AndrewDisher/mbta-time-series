#
# Author: Andrew Disher
# Date: 2024/01/14
#

# ----------------------------------
# ----- Libraries and Packages -----
# ----------------------------------

box::use(
  dplyr[`%>%`, case_when, group_by, filter, mutate, select, summarize, ungroup],
  forecast[...],
  ggplot2[...],
  lubridate[day, hour, month, year],
  MASS[fitdistr]
)

# ---------------------------------------------------
# ----- Box Module Imports for Custom Functions -----
# ---------------------------------------------------

box::use(
  functions/plotting[time_series]
)

# -------------------------------------------
# ----- Import the Ferry Ridership Data -----
# -------------------------------------------

ferry_ridership <- read.csv("data/ferry_ridership.csv")

# -------------------------------------
# ----- Preliminary Data Cleaning -----
# -------------------------------------

# See if any NAs exist in pax_on variable
ferry_ridership[is.na(ferry_ridership$pax_on),]

# Create column variables for date, year, month
ferry_ridership <- ferry_ridership %>% 
  mutate(actual_departure = as.POSIXct(actual_departure),
         year = year(actual_departure),
         month = month(actual_departure),
         day = day(actual_departure),
         hour = hour(actual_departure))

# ------------------
# --- Daily Data ---
# ------------------

# Aggregate data by day
daily_ridership <- ferry_ridership %>% 
  group_by(year, month, day) %>% 
  summarize(boardings = sum(pax_on, na.rm = TRUE),
            avg_boardings = mean(pax_on, na.rm = TRUE)) %>% 
  mutate(avg_boardings = case_when(is.na(avg_boardings) ~ 0,
                                   TRUE ~ avg_boardings)) %>% 
  ungroup() %>% 
  mutate(new_date = paste0(year, "-", month, "-", day) %>% 
           as.Date(format = "%Y-%m-%d"))

# There is a large gap in available data
daily_ridership[496, "new_date"]
daily_ridership[497, "new_date"]

# We'll probably just cut the data set starting at 2020/06/22, and use data from there onward.
# NOTE: The gap in service was due to efforts to slow the spread of COVID-19.

post_covid_daily_ridership <- daily_ridership %>% 
  filter(new_date > as.Date("2020-03-16"))

# --------------------
# --- Monthly Data ---
# --------------------

# Aggregate data by month
monthly_ridership <- ferry_ridership %>% 
  group_by(year, month) %>% 
  summarize(boardings = sum(pax_on, na.rm = TRUE),
            avg_boardings = mean(pax_on, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(new_date = paste0(year, "-", month, "-01") %>% 
           as.Date(format = "%Y-%m-%d"))

monthly_ridership <- monthly_ridership %>% 
  na.omit()

# -------------------------------------------
# ------ Examine Daily Data Time Series -----
# -------------------------------------------

# Plot of time series for daily boardings
time_series$plotTS(data = post_covid_daily_ridership,
                   date_var = "new_date",
                   y_var = "boardings",
                   y = "Total Boardings",
                   title = "Daily Ferry Boardings")

# Distribution of boardings variable
time_series$plotHist(data_vec = post_covid_daily_ridership$boardings,
                     title = "Distribution of Boardings Variable")

# ACF and PACF of daily data
Acf(post_covid_daily_ridership$boardings)
Pacf(post_covid_daily_ridership$boardings)

# --------------------------------------------
# ----- Examine Monthly Data Time Series -----
# --------------------------------------------

# Plot of time series for monthly boardings
time_series$plotTS(data = monthly_ridership,
                   date_var = "new_date",
                   y_var = "boardings",
                   y = "Total Boardings",
                   title = "Monthly Ferry Boardings")

# ACF and PACF of monthly data
Acf(monthly_ridership$boardings)
Pacf(monthly_ridership$boardings)

# --------------------------------------------------------
# ----- Examine Average Boardings for the Daily Data -----
# --------------------------------------------------------

# Plot of time series for daily boardings
time_series$plotTS(data = post_covid_daily_ridership,
                   date_var = "new_date",
                   y_var = "avg_boardings",
                   y = "Average Boardings",
                   title = "Average Daily Ferry Boardings")

# Distribution of boardings variable
time_series$plotHist(data_vec = post_covid_daily_ridership$avg_boardings,
                     title = "Distribution of Boardings Variable")

# ACF and PACF of daily data
Acf(post_covid_daily_ridership$avg_boardings)
Pacf(post_covid_daily_ridership$avg_boardings)

# -------------------------------------------------------
# ----- Creating a Preliminary Model For Daily Data -----
# -------------------------------------------------------

# ARIMA(0, 1, 0) x (0, 1, 0)[7]
daily_model_NULL <- Arima(post_covid_daily_ridership$avg_boardings,
                       order = c(0, 1, 0),
                       seasonal = list(order = c(0, 1, 0),
                                       period = 7))

summary(daily_model_NULL)

# sigma^2 = 40.08:  log likelihood = -3437.38
# AIC=6876.76   AICc=6876.77   BIC=6881.72
# 
# Training set error measures:
#   ME    RMSE      MAE  MPE MAPE      MASE       ACF1
# Training set -0.003751381 6.30692 4.079634 -Inf  Inf 0.9603061 -0.4058708

# --- Examine the residual diagnostic plots of the ARIMA model ---

# Residual ACF and PACF
Acf(daily_model_NULL$residuals)
Pacf(daily_model_NULL$residuals)

# Normal QQ Plot of Residuals
time_series$plotQQ(data_vec = daily_model_NULL$residuals,
                   title = "Normal QQ Plot of Model Residuals")

# Histogram of the Residuals
time_series$plotHist(data_vec = daily_model_NULL$residuals,
                     title = "Histogram of Residuals")

# Ordered Residual Plot
time_series$plotTS(data = data.frame(residuals = daily_model_NULL$residuals,
                                     time_step = 1:length(daily_model_NULL$residuals)),
                   date_var = "time_step",
                   y_var = "residuals",
                   y = "Model Residuals",
                   title = "Ordered Residual Plot") +
  geom_hline(yintercept = 0, 
             linetype = "dashed",
             color = "red")

# -------------------------------------------------
# ----- Further Specifications of Arima Model -----
# -------------------------------------------------

# -------------------------------------
# --- ARIMA(0, 1, 0) x (0, 1, 1)[7] ---
# -------------------------------------
daily_model_010_011 <- Arima(post_covid_daily_ridership$avg_boardings,
                       order = c(0, 1, 0),
                       seasonal = list(order = c(0, 1, 1),
                                       period = 7))

summary(daily_model_010_011)

# sigma^2 = 26.82:  log likelihood = -3228.14
# AIC=6460.28   AICc=6460.29   BIC=6470.2
# 
# Training set error measures:
#   ME     RMSE      MAE  MPE MAPE      MASE       ACF1
# Training set -0.005034311 5.156501 3.397052 -Inf  Inf 0.7996329 -0.3750174

# --- Examine the residual diagnostic plots of the ARIMA model ---

# Residual ACF and PACF
Acf(daily_model_010_011$residuals)
Pacf(daily_model_010_011$residuals)

# Normal QQ Plot of Residuals
time_series$plotQQ(data_vec = daily_model_010_011$residuals,
                   title = "Normal QQ Plot of Model Residuals")

# Histogram of the Residuals
time_series$plotHist(data_vec = daily_model_010_011$residuals,
                     title = "Histogram of Residuals")

# Unit Circle 
autoplot(daily_model_010_011)

# Ordered Residual Plot
time_series$plotTS(data = data.frame(residuals = daily_model_010_011$residuals,
                                     time_step = 1:length(daily_model_010_011$residuals)),
                   date_var = "time_step",
                   y_var = "residuals",
                   y = "Model Residuals",
                   title = "Ordered Residual Plot") +
  geom_hline(yintercept = 0, 
             linetype = "dashed",
             color = "red")

# -------------------------------------
# --- ARIMA(0, 1, 2) x (0, 1, 1)[7] ---
# -------------------------------------
daily_model_012_011 <- Arima(post_covid_daily_ridership$avg_boardings,
                             order = c(0, 1, 2),
                             seasonal = list(order = c(0, 1, 1),
                                             period = 7))

summary(daily_model_012_011)

# sigma^2 = 17.22:  log likelihood = -2995.24
# AIC=5998.48   AICc=5998.51   BIC=6018.31
# 
# Training set error measures:
#   ME     RMSE      MAE  MPE MAPE      MASE          ACF1
# Training set -0.0225195 4.128265 2.763419 -Inf  Inf 0.6504818 -0.0002914175

# --- Examine the residual diagnostic plots of the ARIMA model ---

# Residual ACF and PACF
Acf(daily_model_012_011$residuals)
Pacf(daily_model_012_011$residuals)

# Normal QQ Plot of Residuals
time_series$plotQQ(data_vec = daily_model_012_011$residuals,
                   title = "Normal QQ Plot of Model Residuals")

# Histogram of the Residuals
time_series$plotHist(data_vec = daily_model_012_011$residuals,
                     title = "Histogram of Residuals")

# Unit Circle 
autoplot(daily_model_012_011)

# Ordered Residual Plot
time_series$plotTS(data = data.frame(residuals = daily_model_012_011$residuals,
                                     time_step = 1:length(daily_model_012_011$residuals)),
                   date_var = "time_step",
                   y_var = "residuals",
                   y = "Model Residuals",
                   title = "Ordered Residual Plot") +
  geom_hline(yintercept = 0, 
             linetype = "dashed",
             color = "red")


# --------------------------------------------
# ----- Find t-distribution of Residuals -----
# --------------------------------------------

# Empirically estimate the residual t-distribution parameters
dist_params <- as.list(fitdistr(daily_model_012_011$residuals, "t")$estimate)
dist_params

# T-distributed QQ Plot of Residuals
time_series$plotQQ(data_vec = daily_model_012_011$residuals,
                   title = "T-Distribution QQ Plot of Model Residuals",
                   dist_func = stats::qt,
                   dparams = dist_params["df"])

# NOTE: The residual distribution reflects a t-distribution instead of normal. 
#       This is not an issue, however we need to bootstrap our prediction intervals
#       because of this. 

# -----------------------
# ----- Forecasting -----
# -----------------------

# Generate forecasts and intervals using bootstrapping
forecasts <- forecast(daily_model_012_011, bootstrap = TRUE)

# Create a suitable data frame containing the forecasts and prediction intervals
forecast_df <- data.frame(point = as.numeric(forecasts$mean),
                          low_80 = as.numeric(forecasts$lower[, 1]),
                          low_95 = as.numeric(forecasts$lower[, 2]),
                          high_80 = as.numeric(forecasts$upper[, 1]),
                          high_95 = as.numeric(forecasts$upper[, 2]),
                          date = seq.Date(from = as.Date("2023-09-01"),
                                          length.out = 14,
                                          by = 1))

# Plot subset of historical data and compare with forecasts
ggplot() +
  geom_line(data = post_covid_daily_ridership[1040:1061,],
            mapping = aes(x = new_date, y = avg_boardings),
            color = "#007aff") + 
  geom_ribbon(data = forecast_df,
              mapping = aes(ymin = low_95, ymax = high_95, x = date),
              fill = "#ffe4bf") +
  geom_ribbon(data = forecast_df,
              mapping = aes(ymin = low_80, ymax = high_80, x = date),
              fill = "#ffaf3f") +
  geom_line(data = forecast_df, 
            mapping = aes(x = date, y = point),
            color = "#ED8B00") + 
  theme_minimal() +
  labs(x = "Date",
       y = "Average Boardings",
       title = "Historical Average Boardings and Forecasts")







