#
# Author: Andrew Disher
# Date: 2024/01/16
#

# ----------------------------------
# ----- Libraries and Packages -----
# ----------------------------------

box::use(
  dplyr[`%>%`, arrange, case_when, group_by, filter, mutate, pull, row_number, select,
        summarize, ungroup],
  fable[...],
  fabletools[...],
  feasts[ACF, guerrero, PACF, STL],
  generics[augment, glance],
  ggplot2[...],
  gridExtra[...],
  imputeTS[ggplot_na_distribution, ggplot_na_imputations, na_interpolation],
  lubridate[day, hour, month, year],
  MASS[fitdistr],
  tidyr[everything, pivot_longer, pivot_wider],
  tsibble[...],
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
         hour = hour(actual_departure)) %>% 
  mutate(new_date = paste0(year, "-", month, "-", day) %>% 
           as.Date(format = "%Y-%m-%d"),
         DOW = weekdays(new_date)) %>% 
  filter(!(DOW %in% c("Saturday", "Sunday")))

# There are some days where NAs exist, since data was not available. 

# Bad weather, storm with high wind speeds (50-70 mph)
ferry_ridership %>% 
  filter(new_date == as.Date("2022-01-17")) %>% 
  head()

ferry_ridership %>% 
  filter(new_date == as.Date("2022-11-24")) %>% 
  head()

# Another Nor'Easter, pouring rain in boston with coastal flooding. 
ferry_ridership %>% 
  filter(new_date == as.Date("2023-03-14")) %>% 
  head()

# There is also one day where no people boarded the ferry, which is very unusual. 
# It turns out there was a powerful Nor'Easter Winter snow storm on this day, with snowfall
# totals greater than the sum of ALL snowfall in the previous winter season. There were widespread
# power outages as well. 
ferry_ridership %>% 
  filter(new_date == as.Date("2020-12-17")) %>% 
  head()

# Therefore, we'll interpolate these values, after we have aggregated by day. 

# ------------------
# --- Daily Data ---
# ------------------

# Aggregate data by day
daily_ridership <- ferry_ridership %>% 
  group_by(year, month, day, new_date) %>% 
  summarize(boardings = sum(pax_on, na.rm = TRUE),
            avg_boardings = mean(pax_on, na.rm = TRUE)) %>% 
  # mutate(avg_boardings = case_when(is.na(avg_boardings) ~ 0,
  #                                  TRUE ~ avg_boardings)) %>% 
  ungroup()

# There is a large gap in available data
daily_ridership[352, "new_date"]
daily_ridership[353, "new_date"]

# We'll probably just cut the data set starting at 2020/06/22, and use data from there onward.
# NOTE: The gap in service was due to efforts to slow the spread of COVID-19.

post_covid_daily_ridership <- daily_ridership %>% 
  filter(new_date > as.Date("2020-03-16")) %>% 
  mutate(day_index = row_number()) %>% 
  as_tsibble(index = day_index) 

# ------------------------------
# ----- Data Interpolation -----
# ------------------------------

# Compile vector of problem dates
problem_dates <- as.Date(c("2020-12-17", "2022-01-17", "2022-11-24", "2023-03-14"))

# See the daily values rows
post_covid_daily_ridership %>% 
  filter(new_date %in% problem_dates)

# Replace the average boardings values with NAs
post_covid_daily_ridership <- post_covid_daily_ridership %>% 
  mutate(avg_boardings = case_when(is.nan(avg_boardings) ~ NA,
                                   new_date == as.Date("2020-12-17") ~ NA,
                                   TRUE ~ avg_boardings))

# Visualize the missing values time series
ggplot_na_distribution(post_covid_daily_ridership$avg_boardings)

# Store interpolation results and visualize them
interp <- na_interpolation(post_covid_daily_ridership$avg_boardings, option = "spline", method = "natural")
ggplot_na_imputations(post_covid_daily_ridership$avg_boardings, interp)

# Results look good, replace the vector with the newly interpolated vector
post_covid_daily_ridership <- post_covid_daily_ridership %>% 
  mutate(avg_boardings = interp)

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
post_covid_daily_ridership %>% 
  ACF(boardings) %>% 
  autoplot() +
  theme_minimal()
post_covid_daily_ridership %>% 
  PACF(boardings) %>% 
  autoplot() +
  theme_minimal()

# --------------------------------------------------------
# ----- Examine Average Boardings for the Daily Data -----
# --------------------------------------------------------

# Plot of time series for average daily boardings
time_series$plotTS(data = post_covid_daily_ridership,
                   date_var = "new_date",
                   y_var = "avg_boardings",
                   y = "Average Boardings",
                   title = "Average Daily Ferry Boardings")

# Distribution of average boardings variable
time_series$plotHist(data_vec = post_covid_daily_ridership$avg_boardings,
                     title = "Distribution of Average Boardings Variable")

# ACF and PACF of daily data
post_covid_daily_ridership %>% 
  ACF(avg_boardings) %>% 
  autoplot() +
  theme_minimal()
post_covid_daily_ridership %>% 
  PACF(avg_boardings) %>% 
  autoplot() +
  theme_minimal()

# --------------------------------------------------------------------
# ----- Examine Average Transformed Boardings for the Daily Data -----
# --------------------------------------------------------------------

# Obtain optimal lambda for a Box Cox transformation of avg_boardings
lambda <- post_covid_daily_ridership %>% 
  features(avg_boardings, features = guerrero) %>% 
  pull(lambda_guerrero) 

lambda # 0.2731198

# Create a transformed variable using obtained lambda
post_covid_daily_ridership <- post_covid_daily_ridership %>% 
  mutate(avg_boardings_trans = box_cox(avg_boardings + 1, lambda))

# Plot of time series for transformed average daily boardings
time_series$plotTS(data = post_covid_daily_ridership,
                   date_var = "new_date",
                   y_var = "avg_boardings_trans",
                   y = "Average Boardings",
                   title = "Average Daily Ferry Boardings")

# Distribution of transformed average boardings variable
time_series$plotHist(data_vec = post_covid_daily_ridership$avg_boardings_trans,
                     title = "Distribution of Transformed Average Boardings Variable")

# ACF and PACF of daily data
post_covid_daily_ridership %>% 
  ACF(avg_boardings_trans) %>% 
  autoplot() +
  theme_minimal()
post_covid_daily_ridership %>% 
  PACF(avg_boardings_trans) %>% 
  autoplot() +
  theme_minimal()

# -------------------------------------------------------
# ----- Creating a Preliminary Model For Daily Data -----
# -------------------------------------------------------

# Fit a variety of models
daily_models <- post_covid_daily_ridership %>%
  model(
    seasonal_diff = ARIMA(box_cox(avg_boardings, lambda) ~ pdq(0, 0, 0) + PDQ(0, 1, 0, period = 5)),
    arima_000_011 = ARIMA(box_cox(avg_boardings, lambda) ~ pdq(0, 0, 0) + PDQ(0, 1, 1, period = 5)),
    arima_100_011 = ARIMA(box_cox(avg_boardings, lambda) ~ pdq(1, 0, 0) + PDQ(0, 1, 1, period = 5)),
    nonseasonal_diff = ARIMA(box_cox(avg_boardings, lambda) ~ pdq(0, 1, 0) + PDQ(0, 0, 0, period = 5)),
    double_diff = ARIMA(box_cox(avg_boardings, lambda) ~ pdq(0, 1, 0) + PDQ(0, 1, 0, period = 5)),
    arima_010_011 = ARIMA(box_cox(avg_boardings, lambda) ~ pdq(0, 1, 0) + PDQ(0, 1, 1, period = 5)),
    arima_011_011 = ARIMA(box_cox(avg_boardings, lambda) ~ pdq(0, 1, 1) + PDQ(0, 1, 1, period = 5)),
    arima_012_011 = ARIMA(box_cox(avg_boardings, lambda) ~ pdq(0, 1, 2) + PDQ(0, 1, 1, period = 5)),
    arima_111_011 = ARIMA(box_cox(avg_boardings, lambda) ~ pdq(1, 1, 1) + PDQ(0, 1, 1, period = 5)),
    auto_fit = ARIMA(box_cox(avg_boardings, lambda)),
  )

daily_models %>% 
  pivot_longer(everything(), 
               names_to = "Model", 
               values_to = "Orders")

glance(daily_models) %>% 
  arrange(AICc) %>% 
  select(.model:BIC)

# Store important information, like residuals, in a new data frame
model_info <- augment(daily_models)

# -------------------------------------------
# --- Residual Diagnostics for Null Model ---
# -------------------------------------------
model_info %>% 
  filter(.model == "seasonal_diff") %>% 
  as.data.frame() %>% 
  select(.resid) %>%
  unlist() %>% 
  unname() %>% 
  time_series$model_diagnostics()

# ----------------------------------------------
# --- Residual Diagnostics for arima_000_011 ---
# ----------------------------------------------
model_info %>% 
  filter(.model == "arima_000_011") %>% 
  as.data.frame() %>% 
  select(.resid) %>%
  unlist() %>% 
  unname() %>% 
  time_series$model_diagnostics()

# ----------------------------------------------
# --- Residual Diagnostics for arima_100_011 ---
# ----------------------------------------------
model_info %>% 
  filter(.model == "arima_100_011") %>% 
  as.data.frame() %>% 
  select(.resid) %>%
  unlist() %>% 
  unname() %>% 
  time_series$model_diagnostics()

# -------------------------------------------------
# --- Residual Diagnostics for nonseasonal_diff ---
# -------------------------------------------------
model_info %>% 
  filter(.model == "nonseasonal_diff") %>% 
  as.data.frame() %>% 
  select(.resid) %>%
  unlist() %>% 
  unname() %>% 
  time_series$model_diagnostics()

# --------------------------------------------
# --- Residual Diagnostics for double_diff ---
# --------------------------------------------
model_info %>% 
  filter(.model == "double_diff") %>% 
  as.data.frame() %>% 
  select(.resid) %>%
  unlist() %>% 
  unname() %>% 
  time_series$model_diagnostics()

# ----------------------------------------------
# --- Residual Diagnostics for arima_010_011 ---
# ----------------------------------------------
model_info %>% 
  filter(.model == "arima_010_011") %>% 
  as.data.frame() %>% 
  select(.resid) %>%
  unlist() %>% 
  unname() %>% 
  time_series$model_diagnostics()

# ----------------------------------------------
# --- Residual Diagnostics for arima_011_011 ---
# ----------------------------------------------
model_info %>% 
  filter(.model == "arima_011_011") %>% 
  as.data.frame() %>% 
  select(.resid) %>%
  unlist() %>% 
  unname() %>% 
  time_series$model_diagnostics()

# ----------------------------------------------
# --- Residual Diagnostics for arima_012_011 ---
# ----------------------------------------------
model_info %>% 
  filter(.model == "arima_012_011") %>% 
  as.data.frame() %>% 
  select(.resid) %>%
  unlist() %>% 
  unname() %>% 
  time_series$model_diagnostics()

# ----------------------------------------------
# --- Residual Diagnostics for arima_111_011 ---
# ----------------------------------------------
model_info %>% 
  filter(.model == "arima_111_011") %>% 
  as.data.frame() %>% 
  select(.resid) %>%
  unlist() %>% 
  unname() %>% 
  time_series$model_diagnostics()

# --------------------------------------------
# ----- Find t-distribution of Residuals -----
# --------------------------------------------

arima_012_011_resid <- model_info %>% 
  filter(.model == "arima_012_011") %>% 
  as.data.frame() %>% 
  select(.resid) %>%
  unlist() %>% 
  unname() 

# Empirically estimate the residual t-distribution parameters
dist_params <- as.list(fitdistr(arima_012_011_resid, "t")$estimate)
dist_params

# T-distributed QQ Plot of Residuals
time_series$plotQQ(data_vec = arima_012_011_resid,
                   title = "T-Distribution QQ Plot of Model Residuals",
                   dist_func = stats::qt,
                   dparams = dist_params["df"])

# NOTE: The residual distribution reflects a t-distribution instead of normal. 
#       This is not an issue, however we need to bootstrap our prediction intervals
#       because of this. 

# -----------------------------------------------
# ----- Cross Validation of Proposed Models -----
# -----------------------------------------------

# Create blocked subsets of the data (training sets contains 100 observations each)
cv_data <- post_covid_daily_ridership %>% 
  slide_tsibble(.size = 100, .step = 1) %>% 
  filter(!(.id %in% max(.id):(max(.id) - 13)))
  
# Create a models using each of these training sets to obtain 
cv_results <- cv_data %>% 
  model(
    seasonal_diff = ARIMA(box_cox(avg_boardings, lambda) ~ pdq(0, 0, 0) + PDQ(0, 1, 0, period = 5)),
    arima_000_011 = ARIMA(box_cox(avg_boardings, lambda) ~ pdq(0, 0, 0) + PDQ(0, 1, 1, period = 5)),
    arima_100_011 = ARIMA(box_cox(avg_boardings, lambda) ~ pdq(1, 0, 0) + PDQ(0, 1, 1, period = 5)),
    nonseasonal_diff = ARIMA(box_cox(avg_boardings, lambda) ~ pdq(0, 1, 0) + PDQ(0, 0, 0, period = 5)),
    double_diff = ARIMA(box_cox(avg_boardings, lambda) ~ pdq(0, 1, 0) + PDQ(0, 1, 0, period = 5)),
    arima_010_011 = ARIMA(box_cox(avg_boardings, lambda) ~ pdq(0, 1, 0) + PDQ(0, 1, 1, period = 5)),
    arima_011_011 = ARIMA(box_cox(avg_boardings, lambda) ~ pdq(0, 1, 1) + PDQ(0, 1, 1, period = 5)),
    arima_012_011 = ARIMA(box_cox(avg_boardings, lambda) ~ pdq(0, 1, 2) + PDQ(0, 1, 1, period = 5)),
    arima_111_011 = ARIMA(box_cox(avg_boardings, lambda) ~ pdq(1, 1, 1) + PDQ(0, 1, 1, period = 5))
    ) %>% 
  forecast(h = 14) %>% 
  accuracy(post_covid_daily_ridership) %>% 
  arrange(RMSE)

cv_results

# Compare to models tested on just the last block of data 
cv_data %>% 
  filter(.id == max(.id)) %>% 
  model(
    seasonal_diff = ARIMA(avg_boardings_trans ~ pdq(0, 0, 0) + PDQ(0, 1, 0, period = 5)),
    arima_000_011 = ARIMA(avg_boardings_trans ~ pdq(0, 0, 0) + PDQ(0, 1, 1, period = 5)),
    arima_100_011 = ARIMA(avg_boardings_trans ~ pdq(1, 0, 0) + PDQ(0, 1, 1, period = 5)),
    nonseasonal_diff = ARIMA(avg_boardings_trans ~ pdq(0, 1, 0) + PDQ(0, 0, 0, period = 5)),
    double_diff = ARIMA(avg_boardings_trans ~ pdq(0, 1, 0) + PDQ(0, 1, 0, period = 5)),
    arima_010_011 = ARIMA(avg_boardings_trans ~ pdq(0, 1, 0) + PDQ(0, 1, 1, period = 5)),
    arima_011_011 = ARIMA(avg_boardings_trans ~ pdq(0, 1, 1) + PDQ(0, 1, 1, period = 5)),
    arima_012_011 = ARIMA(avg_boardings_trans ~ pdq(0, 1, 2) + PDQ(0, 1, 1, period = 5)),
    arima_111_011 = ARIMA(avg_boardings_trans ~ pdq(1, 1, 1) + PDQ(0, 1, 1, period = 5))
  ) %>% 
  forecast(h = 14) %>% 
  accuracy(post_covid_daily_ridership) %>% 
  arrange(RMSE)

# NOTE: Our blocked cross-validation reveals that ARIMA(0, 1, 2) x (0, 1, 1) performs 
#       better than all other models by a large margin, meaning that it generalizes 
#       better to new data. However, using only last block cross-validation suggests
#       that ARIMA(1, 0, 0) x (0, 1, 1) wins out. This is the advantage of blocked cross
#       validation, since the this model clearly had a lot of remaining
#       serial autocorrelation left in its residuals, and is not a good model to employ. 
# 
#       Therefore, we'll decide to use ARIMA(0, 1, 2) x (0, 1, 1) as our final model. 

# -----------------------------
# ----- Model Forecasting -----
# -----------------------------

# Produce train test split for all data
train_data <- post_covid_daily_ridership[1:(nrow(post_covid_daily_ridership) - 14), ]
test_data <- post_covid_daily_ridership[(nrow(post_covid_daily_ridership) - 13):nrow(post_covid_daily_ridership), ]

# Train the models on all previous data, and produce forecasts
model_forecasts <- train_data %>% 
  model(
    arima_012_011 = ARIMA(box_cox(avg_boardings, lambda) ~ pdq(1, 1, 1) + PDQ(0, 1, 1, period = 5))
  ) %>% 
  forecast(h = 14, bootstrap = TRUE) 

# Extract information about the confidence intervals
intervals <- model_forecasts %>% 
  hilo()

# Construct a suitable data frame for ggplot2 to plot
fc_plot_data <- data.frame(date = test_data$new_date,
                           actual = test_data$avg_boardings,
                           point_forecast = intervals$.mean,
                           low_80 = intervals$`80%`$lower,
                           high_80 = intervals$`80%`$upper,
                           low_95 = intervals$`95%`$lower,
                           high_95 = intervals$`95%`$upper)

# Construct the ggplot graph
ggplot() +
  geom_line(data = train_data[(nrow(train_data) - 56):nrow(train_data),], 
            mapping = aes(x = new_date, y = avg_boardings),
            color = "#007aff") +
  geom_ribbon(data = fc_plot_data,
              mapping = aes(ymin = low_95, ymax = high_95, x = date),
              fill = "#ffe4bf") +
  geom_ribbon(data = fc_plot_data,
              mapping = aes(ymin = low_80, ymax = high_80, x = date),
              fill = "#ffaf3f") +
  geom_line(data = fc_plot_data, 
            mapping = aes(x = date, y = point_forecast),
            color = "#ED8B00") +
  geom_line(data = fc_plot_data, 
            mapping = aes(x = date, y = actual),
            color = "green") +
  theme_minimal() +
  labs(x = "Date",
       y = "Average Boardings",
       title = "ARIMA Forecasts of Average Ferry Boardings")

# -------------------------------------
# -------------------------------------
# ----- Multiple Seasonality: STL -----
# -------------------------------------
# -------------------------------------

# ACF and PACF of daily data, extended to see yearly seasonality
post_covid_daily_ridership %>% 
  ACF(avg_boardings_trans, lag_max = 300) %>% 
  autoplot() +
  theme_minimal()

# Decomposition of series
post_covid_daily_ridership %>% 
  model(
    STL(box_cox(avg_boardings, lambda) ~ season(5) + season(5*52),
        robust = TRUE)
  ) %>% 
  components() %>% 
  autoplot() 

# Storing the various components of the series
components <- post_covid_daily_ridership %>% 
  model(
    STL(box_cox(avg_boardings, lambda) ~ season(5) + season(5*52),
        robust = TRUE)
  ) %>% 
  components()

# Visualizing the trend
components %>% 
  as_tsibble() %>% 
  autoplot(`box_cox(avg_boardings, lambda)`, color = "gray") +
  geom_line(aes(y = trend), color = "#007aff") +
  theme_minimal() +
  labs(y = "Transformed Average Ferry Boardings",
       x = "Time Step")

# --------------------------------------------
# --- Fitting a Model and Cross Validating ---
# --------------------------------------------

# Model Specs from STL+ETS decomposition
my_dcmp_spec <- decomposition_model(
  STL(box_cox(avg_boardings, lambda) ~ season(5) + season(5*52),
      robust = TRUE),
  ETS(season_adjust ~ season("N"))
)

# Blocked Cross-validation
# Create a models using each of these training sets to obtain 
cv_ETS_results <- cv_data %>% 
  model(my_dcmp_spec) %>% 
  forecast(h = 14) %>% 
  accuracy(post_covid_daily_ridership) 

cv_ETS_results

# ---------------------------------------------
# ----- Cross-Validation with New Subsets -----
# ---------------------------------------------

# Subset so each training set contains about 1 year of observations, to adequately capture yearly seasonality. 
cv_yearly_data <- post_covid_daily_ridership %>% 
  slide_tsibble(.size = 260, .step = 1) %>% 
  filter(!(.id %in% max(.id):(max(.id) - 13)))

cv_ETS_results <- cv_yearly_data %>% 
  model(my_dcmp_spec) %>% 
  forecast(h = 14) %>% 
  accuracy(post_covid_daily_ridership) 

cv_ETS_results


# ------------------------------------------
# ----- Forecasting with STL/ETS Model -----
# ------------------------------------------

# Fitting the model to the whole set of training data
STL_ETS_model <- train_data %>% 
  model(my_dcmp_spec)

STL_ETS_model %>% 
  report()

# Retrieve model information
ETS_model_info <- augment(STL_ETS_model) %>% 
  as.data.frame()

# Examine some residual diagnostics
ETS_model_info %>% 
  select(.resid) %>%
  unlist() %>% 
  unname() %>% 
  time_series$model_diagnostics()

# Generate forecasts
STL_ETS_forecasts <- STL_ETS_model %>% 
  forecast(h = 14) 


# Produce a data frame suitable for plotting
# Extract information about the confidence intervals
STL_ETS_intervals <- STL_ETS_forecasts %>% 
  hilo()

# Construct a suitable data frame for ggplot2 to plot
STL_ETS_plot_data <- data.frame(date = test_data$new_date,
                           actual = test_data$avg_boardings,
                           point_forecast = STL_ETS_intervals$.mean,
                           low_80 = STL_ETS_intervals$`80%`$lower,
                           high_80 = STL_ETS_intervals$`80%`$upper,
                           low_95 = STL_ETS_intervals$`95%`$lower,
                           high_95 = STL_ETS_intervals$`95%`$upper)

# Construct the ggplot graph
ggplot() +
  geom_line(data = train_data[(nrow(train_data) - 56):nrow(train_data),], 
            mapping = aes(x = new_date, y = avg_boardings),
            color = "#007aff") +
  geom_ribbon(data = STL_ETS_plot_data,
              mapping = aes(ymin = low_95, ymax = high_95, x = date),
              fill = "#ffe4bf") +
  geom_ribbon(data = STL_ETS_plot_data,
              mapping = aes(ymin = low_80, ymax = high_80, x = date),
              fill = "#ffaf3f") +
  geom_line(data = STL_ETS_plot_data, 
            mapping = aes(x = date, y = point_forecast),
            color = "#ED8B00") +
  geom_line(data = STL_ETS_plot_data, 
            mapping = aes(x = date, y = actual),
            color = "green") +
  theme_minimal() +
  labs(x = "Date",
       y = "Average Boardings",
       title = "STL/ETS Forecasts of Average Ferry Boardings")


# -------------------------------------------------------------
# -------------------------------------------------------------
# ----- Multiple Seasonality: Dynamic Harmonic Regression -----
# -------------------------------------------------------------
# -------------------------------------------------------------

# --------------------------------------------
# --- Fitting a Model and Cross Validating ---
# --------------------------------------------
cv_dnr_results <- cv_yearly_data %>% 
  model(
    dnr_5 = ARIMA(box_cox(avg_boardings, lambda) ~ PDQ(0, 0, 0) + pdq(d = 0) +
                  fourier(period = 5, K = 2) + fourier(period = 5*52, K = 5)),
    dnr_6 = ARIMA(box_cox(avg_boardings, lambda) ~ PDQ(0, 0, 0) + pdq(d = 0) +
                  fourier(period = 5, K = 2) + fourier(period = 5*52, K = 6)),
    dnr_7 = ARIMA(box_cox(avg_boardings, lambda) ~ PDQ(0, 0, 0) + pdq(d = 0) +
                  fourier(period = 5, K = 2) + fourier(period = 5*52, K = 7)),
    dnr_8 = ARIMA(box_cox(avg_boardings, lambda) ~ PDQ(0, 0, 0) + pdq(d = 0) +
                    fourier(period = 5, K = 2) + fourier(period = 5*52, K = 8)),
    dnr_9 = ARIMA(box_cox(avg_boardings, lambda) ~ PDQ(0, 0, 0) + pdq(d = 0) +
                    fourier(period = 5, K = 2) + fourier(period = 5*52, K = 9)),
    dnr_10 = ARIMA(box_cox(avg_boardings, lambda) ~ PDQ(0, 0, 0) + pdq(d = 0) +
                    fourier(period = 5, K = 2) + fourier(period = 5*52, K = 10)),
    dnr_11 = ARIMA(box_cox(avg_boardings, lambda) ~ PDQ(0, 0, 0) + pdq(d = 0) +
                    fourier(period = 5, K = 2) + fourier(period = 5*52, K = 11)),
    dnr_12 = ARIMA(box_cox(avg_boardings, lambda) ~ PDQ(0, 0, 0) + pdq(d = 0) +
                    fourier(period = 5, K = 2) + fourier(period = 5*52, K = 12)),
    dnr_13 = ARIMA(box_cox(avg_boardings, lambda) ~ PDQ(0, 0, 0) + pdq(d = 0) +
                    fourier(period = 5, K = 2) + fourier(period = 5*52, K = 13)),
    dnr_14 = ARIMA(box_cox(avg_boardings, lambda) ~ PDQ(0, 0, 0) + pdq(d = 0) +
                    fourier(period = 5, K = 2) + fourier(period = 5*52, K = 14)),
    dnr_15 = ARIMA(box_cox(avg_boardings, lambda) ~ PDQ(0, 0, 0) + pdq(d = 0) +
                    fourier(period = 5, K = 2) + fourier(period = 5*52, K = 15))
    
  ) %>% 
  forecast(h = 14) %>% 
  accuracy(post_covid_daily_ridership) 

cv_dnr_results

# --------------------------------------
# ----- Forecasting with DNR Model -----
# --------------------------------------
dnr_model <- train_data %>% 
  model(
    dnr_1 = ARIMA(box_cox(avg_boardings, lambda) ~ PDQ(0, 0, 0)  +
                  fourier(period = 5, K = 2) + fourier(period = 5*52, K = 5)),
    dnr_2 = ARIMA(box_cox(avg_boardings, lambda) ~ PDQ(0, 0, 0)  +
                  fourier(period = 5, K = 2) + fourier(period = 5*52, K = 8)),
    dnr_3 = ARIMA(box_cox(avg_boardings, lambda) ~ PDQ(0, 0, 0)  +
                  fourier(period = 5, K = 2) + fourier(period = 5*52, K = 10)),
    dnr_4 = ARIMA(box_cox(avg_boardings, lambda) ~ PDQ(0, 0, 0)  +
                    fourier(period = 5, K = 2) + fourier(period = 5*52, K = 15))
  ) 

glance(dnr_model) %>% 
  arrange(AICc) %>% 
  select(.model:BIC)

dnr_model[[4]][[1]] %>% 
  report()

# Retrieve model information
dnr_model_info <- augment(dnr_model)

# Examine some residual diagnostics
dnr_model_info %>% 
  filter(.model == "dnr_4") %>% 
  as.data.frame() %>% 
  select(.resid) %>%
  unlist() %>% 
  unname() %>% 
  time_series$model_diagnostics()

# Generate forecasts
dnr_forecasts <- dnr_model[[4]][[1]] %>% 
  forecast(h = 14) 

# Produce a data frame suitable for plotting
# Extract information about the confidence intervals
dnr_intervals <- dnr_forecasts %>% 
  hilo()

# Construct a suitable data frame for ggplot2 to plot
dnr_plot_data <- data.frame(date = test_data$new_date,
                            actual = test_data$avg_boardings,
                            point_forecast = dnr_intervals$.mean,
                            low_80 = dnr_intervals$`80%`$lower,
                            high_80 = dnr_intervals$`80%`$upper,
                            low_95 = dnr_intervals$`95%`$lower,
                            high_95 = dnr_intervals$`95%`$upper)

# Construct the ggplot graph
ggplot() +
  geom_line(data = train_data[(nrow(train_data) - 56):nrow(train_data),], 
            mapping = aes(x = new_date, y = avg_boardings),
            color = "#007aff") +
  geom_ribbon(data = dnr_plot_data,
              mapping = aes(ymin = low_95, ymax = high_95, x = date),
              fill = "#ffe4bf") +
  geom_ribbon(data = dnr_plot_data,
              mapping = aes(ymin = low_80, ymax = high_80, x = date),
              fill = "#ffaf3f") +
  geom_line(data = dnr_plot_data, 
            mapping = aes(x = date, y = point_forecast),
            color = "#ED8B00") +
  geom_line(data = dnr_plot_data, 
            mapping = aes(x = date, y = actual),
            color = "green") +
  theme_minimal() +
  labs(x = "Date",
       y = "Average Boardings",
       title = "Dynamic Harmonic Regression Forecasts of Average Ferry Boardings")

# ------------------------------------
# ----- Bringing it all Together -----
# ------------------------------------

# Construct the ggplot graph of all point forecasts
ggplot() +
  geom_line(data = train_data[(nrow(train_data) - 56):nrow(train_data),], 
            mapping = aes(x = new_date, y = avg_boardings),
            color = "#007aff") +
  geom_line(data = dnr_plot_data, 
            mapping = aes(x = date, y = point_forecast),
            color = "#ED8B00") +
  geom_line(data = STL_ETS_plot_data, 
            mapping = aes(x = date, y = point_forecast),
            color = "#003DA5") +
  geom_line(data = fc_plot_data, 
            mapping = aes(x = date, y = point_forecast),
            color = "#DA291C") +
  geom_line(data = dnr_plot_data, 
            mapping = aes(x = date, y = actual),
            color = "green") +
  theme_minimal() +
  labs(x = "Date",
       y = "Average Boardings",
       title = "Dynamic Harmonic Regression Forecasts of Average Ferry Boardings")

# --- Combing forecasts ---

# Train all models
all_models <- train_data %>% 
  model(
    arima = ARIMA(box_cox(avg_boardings, lambda) ~ pdq(1, 1, 1) + PDQ(0, 1, 1, period = 5)),
    STL_ECS = my_dcmp_spec,
    dnr = ARIMA(box_cox(avg_boardings, lambda) ~ PDQ(0, 0, 0)  +
                  fourier(period = 5, K = 2) + fourier(period = 5*52, K = 15))
  ) %>% 
  mutate(combination = (arima + STL_ECS + dnr) / 3)

# Produce forecasts
all_forecasts <- all_models %>% 
  forecast(h = 20)

# Retrieve model information
all_intervals <- all_forecasts %>%
  # filter(.model == "combination") %>% 
  hilo()

# Construct a suitable data frame for ggplot2 to plot
all_plot_data <- data.frame(.model = all_intervals$.model,
                            date = test_data$new_date,
                            actual = test_data$avg_boardings,
                            point_forecast = all_intervals$.mean,
                            low_80 = all_intervals$`80%`$lower,
                            high_80 = all_intervals$`80%`$upper,
                            low_95 = all_intervals$`95%`$lower,
                            high_95 = all_intervals$`95%`$upper)

# Construct the ggplot graph of all point forecasts
ggplot() +
  geom_line(data = train_data[(nrow(train_data) - 56):nrow(train_data),], 
            mapping = aes(x = new_date, y = avg_boardings),
            color = "#007aff") +
  geom_line(data = dnr_plot_data, 
            mapping = aes(x = date, y = point_forecast),
            color = "#ED8B00") +
  geom_line(data = STL_ETS_plot_data, 
            mapping = aes(x = date, y = point_forecast),
            color = "#003DA5") +
  geom_line(data = fc_plot_data, 
            mapping = aes(x = date, y = point_forecast),
            color = "#DA291C") +
  geom_line(data = dnr_plot_data, 
            mapping = aes(x = date, y = actual),
            color = "green") +
  geom_line(data = all_plot_data, 
            mapping = aes(x = date, y = point_forecast),
            color = "#1a7e2e") +
  theme_minimal() +
  labs(x = "Date",
       y = "Average Boardings",
       title = "Forecast Comparisons of Average Ferry Boardings")

# -----------------------------------------
# ----- Combining Error Distributions -----
# -----------------------------------------

future_forecasts <- all_models %>% 
  generate(h = 20, times = 1000) %>% 
  as_tsibble() %>% 
  group_by(.model) %>% 
  summarize(
    dist = distributional::dist_sample(list(.sim))
  ) %>% 
  ungroup() %>% 
  as_fable(index = day_index, key = .model,
           distribution = dist, response = "avg_boardings")

future_forecasts |>
  filter(.model == "combination") |>
  autoplot(post_covid_daily_ridership[(nrow(post_covid_daily_ridership) - 56):nrow(post_covid_daily_ridership),]) +
  labs(y = "Average Boardings",
       title = "Forecast Combination of Average Ferry Boardings")





