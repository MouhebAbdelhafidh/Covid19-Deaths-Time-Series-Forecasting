data <- read.csv("C:/Users/asus/Desktop/Covid19-Deaths-Time-Series-Forecasting/COVID-19_Data.csv", header = TRUE, stringsAsFactors = FALSE)
data$Date <- as.Date(data$Date, format = "%m/%d/%Y")

# Order by Date(ascending)
data <- data[order(data$Date), ]
covid19_daths <- data$DeathsRateTotal

# Checking the dataset structure
head(covid19_daths)
str(covid19_daths)
summary(covid19_daths)

# Creating the TS
covid19_daths.ts<- ts(covid19_daths, start = c(2020, 3), end = c(2024, 5), frequency = 12)
class(covid19_daths.ts)

# Checking missing values and outliers
sum(is.na(covid19_daths.ts))
boxplot(covid19_daths.ts~cycle(covid19_daths.ts))
# No missing values and no Outliers

################### Graphical and statistical analysis ###############

plot(covid19_daths.ts, 
     main = "Covid19 Deaths",
     ylab = "Deaths",
     xlab = "Time",
     col = "blue", 
     lwd = 2)

plot(decompose(covid19_daths.ts))
# Existance of trend, saisonality and residual composants

#Colleniarity
lag.plot(covid19_daths.ts, lag = 12) 
acf(covid19_daths.ts)
# Existance of a high auto-collinearity and a Tendance, no saisonality

# Additive Model vs Multiplicative Model
additive.m <- decompose(covid19_daths.ts, "additive")
plot(additive.m)
multiplicative.m <- decompose(covid19_daths.ts, "multiplicative")
plot(multiplicative.m)
# Additive model is better because it presents less random composant


################### Time series adjustment ###############

time_index <- 1:length(covid19_daths.ts)

# Trend adjustment
# Linear model
model.linear <- lm(covid19_daths.ts~ time_index)
summary(model.linear)
# Individual significativity: Both observations are significatives(P_value<0.05) => H1 accepted
# Global significativity: Adjusted R-squared:  0.9688 => Good model => H1 accepted

# Polynomial model
model.polynomial <- lm(covid19_daths.ts ~ poly(time_index, degree = 2, raw = TRUE))
summary(model.polynomial)
# Individual significativity: New variable is not significative (P_value=0.661) => H0 accepted
# We stick with the linear model for Trend (Higher Adjusted R-squared)

# Seasonality adjustment 
n <- length(covid19_daths.ts)            
time_index <- 1:n  

# Handle zeros/negatives before log transform
if(any(covid19_daths.ts <= 0)) {
  covid19_daths.ts[covid19_daths.ts <= 0] <- 0.5  # Replace with small positive value
}

MC <- matrix(0, n, 6)  
MS <- matrix(0, n, 6)  
for(i in 1:6) {
  freq <- 12/i  
  MC[,i] <- cos(2*pi*time_index/freq)
  MS[,i] <- sin(2*pi*time_index/freq)
}

model.seasonal <- lm(log(covid19_daths.ts) ~ time_index + 
                    MC[,1] + MC[,2] + MC[,3] + MC[,4] +  
                    MS[,1] + MS[,2] + MS[,4] + MS[,5])

summary(model.seasonal)
# All harmonic terms are not significative

# Simplified model keeping only significant terms
simple_model.seasonal <- lm(log(covid19_daths.ts) ~ time_index)

# Compare models
AIC(simple_model.seasonal , model.seasonal)
# simple_model.seasonal AIC=128.6248 < model.seasonal AIC=140.8635 => simple model preferred
acf(resid(simple_model.seasonal))
# There is missing information

# Stationarity check
library(tseries)
library(forecast)
# Augmented Dickey-Fuller Test(ADF)
adf_test <- adf.test(covid19_daths.ts)
adf_test
# TS not stationnaire

# Regular + Seasonal différenciation 
combined_diff <- diff(diff(covid19_daths.ts), lag = 12)
# ADF Test
adf.test(combined_diff)
# p-value = 0.6699 => No stationarity
# Bow-pierce test
Box.test(diff(covid19_daths.ts, lag=12))
# p-value = 1.005e-06 => H1 accepted : Not a White noise
# => We need to modielise the Residuals composant


# 1. Prepare the data 
log_ts <- log(covid19_daths.ts + 0.5)  
time_index <- 1:length(log_ts)

# 2. Remove the linear trend 
trend <- 0.0852 * time_index - 3.0462  # Coefficients from linear model
detrended <- log_ts - trend

# 3. Fit SARIMA to detrended residuals
# Auto-select best SARIMA model
sarima_model <- auto.arima(detrended,
                          seasonal = TRUE,
                          stepwise = FALSE,
                          approximation = FALSE,
                          trace = TRUE)


summary(sarima_model)
checkresiduals(sarima_model)

# Combined model object for forecasting
final_model <- list(
  trend_model = list(
    intercept = -3.0462,
    slope = 0.0852
  ),
  sarima_model = sarima_model
)

class(final_model) <- "covid_forecast_model"

# 1. Model Selection
# Best Model: ARIMA(0,1,0) with drift
# No AR or MA terms needed (p = 0, q = 0) → Residuals follow a random walk with drift.
# 1st-order differencing (d=1) was sufficient for stationarity.
# Drift term included → Small but significant trend in residuals.
# 2. Trend & Residual Behavior
# Original trend dominates: Your log-linear trend (log(y) = -3.0462 + 0.0852*time) explains most variation (high R²).
# Detrended residuals show:
# Negative drift (-0.0701) → Slight downward trend in residuals after removing the main trend.
# No seasonality detected → No SAR/SMA terms were needed.
# 3. Residual Diagnostics (White Noise Check)
# Ljung-Box test (p = 0.9978) → Residuals show no autocorrelation (good fit).
# ACF/PACF of residuals → No significant spikes (supports white noise).
# Low AIC/BIC (-80.97, -77.14) → Model is parsimonious and well-fitted.
