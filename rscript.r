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

packages <- c("astsa", "dplyr", "FSA", "rugarch", "HoltWinters")
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

spdata <- read.csv(dir, na.strings="NA")
impute_avg <- function(i, df) {
  df[i] <- (df[i-1] + df[i+1]) / 2
  df
}
spdata.full <- spdata
for (i in which(!complete.cases(spdata.full))) {
  spdata.full$VALUE <- impute_avg(i, spdata.full$VALUE)
}
# drop additional cases where two values are missing
spdata.full <- spdata.full[complete.cases(spdata.full),]
sp.ts <- ts(spdata.full$VALUE)

# -----------------------------------------------------------------------------
# Explore data and candidate SARIMA models
# -----------------------------------------------------------------------------

# View(spdata)
sp.ts <- ts(rev(spdata$VALUE))
ts.plot(sp.ts, main = "S&P 500: 64 Observations", ylab="Closing Price", 
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
spec.pgram(sp.ts_2diff, log="no")

#ARIMA Model Validation
p2d2q1P0D0Q0<-arima(sp.ts, order=c(2, 2, 1), seasonal = list(order=c(0, 0, 0)))
tsdiag(p2d2q1P0D0Q0, lag.max=15)
AIC(p2d2q1P0D0Q0)
mean(sum(p2d2q1P0D0Q0$residuals))  # MSE of the model
predp2d2q1P0D0Q0 <- forecast(p2d2q1P0D0Q0, h=5)

# -----------------------------------------------------------------------------
# Holt Winter
# -----------------------------------------------------------------------------
# Geoff notes to self: parameters are fit using sum of squares
#                not the best criteria: try a grid of values and CV?

# N <- 5
# K <- 5
# n <- length(sp.ts)
# 
# # parameter grid
# n_alpha <- 100
# n_beta <- 100
# alpha <- seq(0,1,length.out = n_alpha)
# beta <- seq(0,1,length.out = n_beta)
# 
# # reform data frame and intermediates
# mspe <- matrix(nrow = n_alpha, ncol = n_beta)
# indices <- 1:n %% K + 1
# 
# # shuffle indices for IID assumption
# set.seed(kSeed) # set seed for reproducibility
# indices <- sample(indices, size = length(indices), replace=FALSE)
# 
# for (j in 1:N) {
#   # hold responses for this run of K-fold CV
#   pred <- rep(n, 0)
#   for (i in 1:K) {
#     testIvec <- indices == i
#     trainIvec <- !testIvec
#     # model matrix includes response variable
#     XTrain <- X[trainIvec, ]
#     XTest <- X[testIvec, ]
#     
#     
#   }
#   
#   # zero out any NaN cases from BoxCox models
#   mspe[j] <- mean((pred - y)^2)
# }
# 
# }
# 
# for (a in alpha) {
#   for (b in beta) {
#     
#     
#   }
# }
# 
# for (j in 1:N) {
#   # hold responses for this run of K-fold CV
#   pred <- rep(n, 0)
#   for (i in 1:K) {
#     testIvec <- indices == i
#     trainIvec <- !testIvec
#     # model matrix includes response variable
#     XTrain <- X[trainIvec, ]
#     XTest <- X[testIvec, ]
#   }
#   
#   # zero out any NaN cases from BoxCox models
#   mspe[j] <- mean((pred - y)^2)
# }

hw1 <-HoltWinters(sp.ts,alpha=.3, beta=NULL, gamma=FALSE)
plot(hw1)
forecast(hw1, h =5)

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
tsdiag(p1d0q1P0D0Q0, gof.lag = 40)
AIC(p1d0q1P0D0Q0)
mean(sum(p1d0q1P0D0Q0$residuals))  # MSE of the model

# Ratio stuff
# take data weekly, every friday
# e.g. this friday is x, next onis y, lpog ratio


# -----------------------------------------------------------------------------
# Model Three: GARCH
# -----------------------------------------------------------------------------
# needs more data...
# p3d0q0P0D0Q0$residuals
# library(rugarch)
# library(fGarch)
# s2 <- (sp.ts^2)
# plot(s2)
# acf(s2, lag.max = 1000)
# # experiment with GARCH model using parameters from best SARIMA model
# # financial markets are modelled better by t-distribution, use "std"
# specs<-ugarchspec(
#   variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
#   mean.model = list(armaOrder = c(3, 0), include.mean = TRUE),
#   distribution.model = "std"
# )
# #
# N <- length(sp.ts)
# # TODO: what is the definition of a return? Why multiply by 100 and take ratio of successive days?
# sp.ts.returns=100*(log(sp.ts[2:N])-log(sp.ts[1:(N-1)]))
# modelfit = ugarchfit(spec = specs, data = sp.ts.returns)
# modelfit
# plot(modelfit)

# -----------------------------------------------------------------------------
# Model comparisons
# -----------------------------------------------------------------------------

actual<-c(2037.05, 2055.01, 2063.95, 2059.74, 2072.78)
MSE_model1 <- mean((predp2d2q1P0D0Q0$mean - actual)^2)
