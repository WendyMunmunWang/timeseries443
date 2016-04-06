# -----------------------------------------------------------------------------
# Function declarations
# -----------------------------------------------------------------------------
# new comment
install_new_packages <- function(packages) {
  #' Installs each package in parameter packages if not already installed
  #' 
  #' @param packages A character vector containing the names of packages
  
  # decide which packages are needed
  to_install <- !(packages %in% installed.packages()[, "Package"])
  new_packages <- packages[to_install]
  
  # do installation if needed
  if(length(new_packages) > 0) {
    install.packages(new_packages)
  } else {
    # let user know the status
    message("No new packages need to be installed!")
  }
}

# -----------------------------------------------------------------------------
# Set up local R environment
# -----------------------------------------------------------------------------

packages <- c("astsa", "dplyr", "FSA", "rugarch", "HoltWinters", "forecast")
install_new_packages(packages)
library(FSA)
library(astsa)
library(zoo)
library(HoltWinters)
library(forecast)

# -----------------------------------------------------------------------------
# Read data
# -----------------------------------------------------------------------------

# Uncomment the correct directory for your computer (or add it!):
dir <- "~/Documents/time_series_443/PROJECT/SP500janmar.csv" # Geoff
#dir <- "~/Documents/STAT443/Stat443_project/timeseries443/SP500janmar.csv" # Wendy

spdata <- read.csv(dir, na.strings=".")
spdata$DATE <- as.Date(spdata$DATE, format="%y-%m-%d")
impute_avg <- function(i, df) {
  df[i] <- (df[i-1] + df[i+1]) / 2
  df
}
spdata.full <- spdata
for (i in which(!complete.cases(spdata.full))) {
  spdata.full$VALUE <- impute_avg(i, spdata.full$VALUE)
}
# drop additional cases where two values are missing
#spdata.full <- spdata.full[complete.cases(spdata.full),]
sp.ts <- ts(spdata.full$VALUE)

# -----------------------------------------------------------------------------
# Explore data and candidate SARIMA models
# -----------------------------------------------------------------------------

# View(spdata)
# sp.ts <- ts(rev(spdata$VALUE))
ts.plot(sp.ts, main = "S&P 500: 64 Observations", ylab="Index Value", 
        xlab="Time (business days since 01/04/2016)")
acf(sp.ts, lag.max=40)
pacf(sp.ts)

# -----------------------------------------------------------------------------
# Evaluate whether frequency-domain analysis in time series is appropriate
# -----------------------------------------------------------------------------
spectrum <- spec.pgram(sp.ts, log="no", 
                       main="S&P 500: 64 Observations\nRaw Periodogram")
1 / spectrum$freq[which.max(spectrum$spec)]
# next peak occurs at frequency index 4
1 / spectrum$freq[4]
# Conclusion: frequency domain analysis will not be effective here

# -----------------------------------------------------------------------------
# Model One: SARIMA models for data with first difference
# -----------------------------------------------------------------------------

#Take the first difference
sp.ts_1diff <- diff(sp.ts, 1)
ts.plot(sp.ts_1diff, main="First Difference of S&P 500 Series, X(t)", 
        xlab="Time (days since 01/04/2016)", ylab="X(t) - X(t+1)")
sp.ts_2diff <- diff(sp.ts_1diff, 1)
ts.plot(sp.ts_2diff, main="First Difference of X(t), Y(t)", 
        xlab="Time (days since 01/04/2016)",
        ylab="Y(t) = X(t) - 2X(t-1) + X(t+2)")
par(mfrow=c(1,2)) # plot side by side
acf(sp.ts_2diff, main = "ACF for Y(t)")
pacf(sp.ts_2diff, main = "PACF for Y(t)")

#ARIMA Model Validation
p2d2q1P0D0Q0<-arima(sp.ts, order=c(2, 2, 1), seasonal = list(order=c(0, 0, 0)))
tsdiag(p2d2q1P0D0Q0, lag.max=15)
AIC(p2d2q1P0D0Q0)
mean(sum((p2d2q1P0D0Q0$residuals)^2))  # MSE of the model
predp2d2q1P0D0Q0 <- forecast(p2d2q1P0D0Q0, h=5)

# -----------------------------------------------------------------------------
# Exponential smoothing for baseline comparison
# -----------------------------------------------------------------------------

expSmooth <-HoltWinters(sp.ts,alpha=NULL, beta=FALSE, gamma=FALSE)
plot(hw1)
expSmooth.pred <- forecast(hw1, h = 5)

# -----------------------------------------------------------------------------
# Model Two: SARIMA models for data after taking forward ratio, applying 
#            log function, then add 100.
# -----------------------------------------------------------------------------

#Lag constant
days <- 1

#Take the forward ratio, applying log function, then add 100
sp.ts.ratio <- lagratio(sp.ts, lag = days, recursion = 1, direction = "forward")
sp.ts.ratio.log.100 <- log(sp.ts.ratio) + 100

ts.plot(sp.ts.ratio.log.100, main="W(t), Log Forward Ratio of S&P 500 Serie, X(t), plus 100", 
        xlab="Time (days since 01/04/2016)", ylab="W(t) = log ( X(t+1) / X(t) ) + 100")

par(mfrow=c(1,2)) # plot side by side
acf(sp.ts.ratio.log.100, main = "ACF for W(t)")
pacf(sp.ts.ratio.log.100, main = "PACF for W(t)")

par(mfrow=c(1,1)) 
spec.pgram(sp.ts.ratio.log.100, log="no", main = "W(t) Raw Periodogram")

#ARIMA Model Validation
p1d0q1P0D0Q0<-arima(sp.ts.ratio.log.100, order=c(1, 0, 1), seasonal = list(order=c(0, 0, 0)))
p1d0q1P0D0Q0
tsdiag(p1d0q1P0D0Q0, gof.lag = 40)
AIC(p1d0q1P0D0Q0)
mean(sum((p1d0q1P0D0Q0$residuals)^2))  # MSE of the model

predp1d0q1P0D0Q0 <- forecast(p1d0q1P0D0Q0, h=5)
pred.w <- predp1d0q1P0D0Q0$mean
pred.x.1 <- (exp(100.01869 - 100)) * 2035.94 
pred.x.2 <- exp(pred.w[1] - 100) * pred.x.1
pred.x.3 <- exp(pred.w[2] - 100) * pred.x.2
pred.x.4 <- exp(pred.w[3] - 100) * pred.x.3
pred.x.5 <- exp(pred.w[4] - 100) * pred.x.4
pred.x <- c(pred.x.1, pred.x.2, pred.x.3, pred.x.4, pred.x.5)

# Ratio stuff
# take data weekly, every friday
# e.g. this friday is x, next onis y, lpog ratio

# -----------------------------------------------------------------------------
# Model comparisons
# -----------------------------------------------------------------------------

actual<-c(2037.05, 2055.01, 2063.95, 2059.74, 2072.78)
MSE_baseline <- mean((expSmooth.pred$mean-actual)^2)
MSE_model1 <- mean((predp2d2q1P0D0Q0$mean - actual)^2)
# for lag ratio comparison
actual.ratio <- lagratio(actual, lag = days, recursion = 1, direction = "forward")
MSE_model2 <- mean((pred.x - actual)^2)
