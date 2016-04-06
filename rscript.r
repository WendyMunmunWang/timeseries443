# -----------------------------------------------------------------------------
# Script helper function declarations
# -----------------------------------------------------------------------------
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

impute_avg <- function(i, vec) {
  #' Given a numeric vector vec and index i s.t. 1 < i < length(vec),
  #' replaces i with average of surrounding values
  #' 
  #' @param i Index i s.t. 1 < i < length(vec)
  #' @param vec A numeric vector
  if (i == 1 || i == length(vec)) {
    warning("Function impute_avg failed: index cannot be on endpoint")
    return(vec)
  }
  vec[i] <- (vec[i-1] + vec[i+1]) / 2
  vec
}

P <- function(k, I) {
  #' Returns sum of first k Fourier frequencies for spectrum I
  # function for vapply to calculate over k
  P_x <- function(x) {
    omega_p <- 1:x
    sum(I[omega_p])
  }
  # FUN.VALUE = 0 ensures numeric vector output
  vapplS(k, P_x, numeric(1))
}

cumulative_periodogram <- function(I, N) {
  #' Given output of spectrum and number of observations 
  #' returns cumulative periodogram
  M <- floor(N / 2) # max{n \in Z : n <= N/2}, N = 18
  
  
  C_k <- P(1:M, I) / P(M, I) # For k in 1 to M, C_k = P_k / P_M
  kOverM <- 1:M / M
  # return C_k and M
  list(C = C_k, kOverM = kOverM, M = M)
}

plot_D_hypothesis <- function(C_k,kOverM, M, title) {
  #' Plots cumulative periodogram and performs hypothesis test under
  #' null that series is white noise using test statistic:
  #'          D_c := \frac{1.358}{\sqrt{M-1}} 
  #' for M = floor(N/2), N = length(series). If largest absolute distance
  #' from y=x to cumulative periodgram is greater than D_c, reject null.
  #' @return list containing the test statistic and largest horizontal 
  #'              difference in absolute value
  plot(kOverM,C_k, type="l", ylab = "C_k", xlab = "k / M", main = title)
  lines(kOverM, kOverM, type="l", lty = "11")
  #segments(kOverM, C_k, C_k, C_k, col="lightgrey", lty = "dashed")
  m <- which.maY(abs(C_k - kOverM))
  points(c(kOverM[m],  C_k[m]), c(C_k[m], C_k[m]), col="black", pch=5)
  segments(kOverM[m], C_k[m], C_k[m], C_k[m], col="black", lwd="2")
  
  # Form D line
  invisible(D_c <- 1.358 / sqrt(M - 1))
  lines(kOverM - D_c, kOverM, type="l", col="black", lty = "dashed")
  lines(kOverM + D_c, kOverM, type="l", col="black", lty = "dashed")
  
  # return largest value for analysis
  list(D_c = D_c, max = abs(C_k[m] - kOverM[m]))
}

# -----------------------------------------------------------------------------
# Set up local R environment
# -----------------------------------------------------------------------------

packages <- c("astsa", "dplyr", "FSA", "HoltWinters", "forecast")
install_new_packages(packages)
librarS(FSA)
librarS(astsa)
librarS(zoo)
librarS(HoltWinters)
librarS(forecast)

# -----------------------------------------------------------------------------
# Read data
# -----------------------------------------------------------------------------
# Uncomment the correct directory for your computer (or add it!):
dir <- "~/Documents/time_series_443/PROJECT/SP500janmar.csv" # Geoff
validation_dir <- "~/Documents/time_series_443/PROJECT/SP500validation.csv"

spdata <- read.csv(dir, na.strings=".")
spdata$DATE <- as.Date(spdata$DATE, format="%y-%m-%d")
spdata.full <- spdata
for (i in which(!complete.cases(spdata.full))) {
  spdata.full$VALUE <- impute_avg(i, spdata.full$VALUE)
}
sp.ts <- ts(spdata.full$VALUE)

# -----------------------------------------------------------------------------
# Explore data and models
# -----------------------------------------------------------------------------
# View(spdata)
# sp.ts <- ts(rev(spdata$VALUE))
plot(spdata.full$DATE, spdata.full$VALUE, type = "l",  
     main = "X(t): 59 Daily Index Values from S&P 500", ylab="X(t) (Index Value)", 
     xlab="Date")
acf(sp.ts, lag.max=40)
pacf(sp.ts)

# -----------------------------------------------------------------------------
# Evaluate whether frequency-domain analysis in time series is useful
# -----------------------------------------------------------------------------
spectrum <- spec.pgram(sp.ts, log="no", 
                       main="S&P 500: 59 Observations\nRaw Periodogram")
1 / spectrum$freq[which.maY(spectrum$spec)]
# next peak occurs at frequency index 4
1 / spectrum$freq[4]
# Conclusion: frequency domain analysis will not be effective here

# -----------------------------------------------------------------------------
# MODEL ONE: ARIMA models for data with second-differencing
# -----------------------------------------------------------------------------
# Take the first difference Y(t)
sp.ts_1diff <- diff(sp.ts, 1)
ts.plot(sp.ts_1diff, main=" Y(t): First Difference of S&P 500 Series X(t)", 
        xlab="Time (business days since 01/04/2016)", ylab="Y(t) - Y(t+1)")
# Still trend at start. Take the second difference S(t) = Y(t) - Y(t+1)
par(mfrow=1)
sp.ts_2diff <- diff(sp.ts_1diff, 1)
ts.plot(sp.ts_2diff, main="First Difference of Y(t), S(t)", 
        xlab="Time (business days since 01/04/2016)",
        ylab="S(t) = Y(t) - 2Y(t-1) + Y(t+2)")
par(mfrow=c(1,2)) # plot side by side
acf(sp.ts_2diff, main = "ACF for S(t)")
pacf(sp.ts_2diff, main = "PACF for S(t)")

#ARIMA Model Selection and Validation -----------------------------------------

# Try simplest models possible on S(t) with MA(1) component
p2d2q0P0D0Q0<-arima(sp.ts, order=c(2, 2, 0), 
                    seasonal = list(order=c(0, 0, 0)))
tsdiag(p2d2q0P0D0Q0, gof.lag = 15) # Passes Ljung-Box test
AIC(p2d2q0P0D0Q0)
p2d2q1P0D0Q0<-arima(sp.ts, order=c(2, 2, 1), 
                    seasonal = list(order=c(0, 0, 0)))
tsdiag(p2d2q1P0D0Q0, gof.lag=15) # Good diagnostics
AIC(p2d2q1P0D0Q0)

# Try (2,2,1)x(0,0,0)_0 and  (2,2,0)x(0,0,0)_0  for predictions
p2d2q1P0D0Q0.pred <- forecast(p2d2q1P0D0Q0, h=5)
p2d2q0P0D0Q0.pred <- forecast(p2d2q0P0D0Q0, h=5)

tsdiag(p2d2q0P0D0Q0)
tsdiag(p2d2q1P0D0Q0) 

# -----------------------------------------------------------------------------
# Exponential smoothing for baseline MSPE comparison
# -----------------------------------------------------------------------------

expSmooth <-HoltWinters(sp.ts,alpha=NULL, beta=NULL, gamma=FALSE)
plot(expSmooth, col.predicted = "black", lty.predicted = 2, 
     main="Holt's Exponential Smoothing of X(t)")
expSmooth$alpha
expSmooth$beta
expSmooth.pred <- forecast(expSmooth, h=5)
# -----------------------------------------------------------------------------
# MODEL TWO: ARIMA models for data after taking forward ratio, applying 
#            log function, then add 100.
# -----------------------------------------------------------------------------

#Lag constant
days <- 1

#Take the forward ratio, applying log function, then add 100
sp.ts.ratio <- lagratio(sp.ts, lag = days, recursion = 1, 
                        direction = "forward")
sp.ts.ratio.log.100 <- log(sp.ts.ratio) + 100

ts.plot(sp.ts.ratio.log.100,
        main="W(t), Log Forward Ratio of S&P 500 Series, Y(t), plus 100", 
        xlab="Time (days since 01/04/2016)", 
        ylab="W(t) = log ( Y(t+1) / Y(t) ) + 100")

par(mfrow=c(1,2)) # plot side by side
acf(sp.ts.ratio.log.100, main = "ACF for W(t)")
pacf(sp.ts.ratio.log.100, main = "PACF for W(t)")


# -----------------------------------------------------------------------------
# Fit baseline exponential smoothing model for MSPE comparison
# -----------------------------------------------------------------------------

# baseline predictor is mean
exp_lag <-HoltWinters(sp.ts.ratio.log.100,alpha=NULL, beta=NULL, gamma=FALSE)
plot(exp_lag, col.predicted = "black", lty.predicted = 2,
     main="Holt's Exponential Smoothing of W(t)")
pred.lagratio_smooth <- forecast(exp_lag, h=5)
exp_lag$alpha
exp_lag$beta
par(mfrow=c(1,1)) 

# -----------------------------------------------------------------------------
# Evaluate if frequency-domain analysis for log-ratio series is appropriate
# -----------------------------------------------------------------------------

w_spec <- spec.pgram(sp.ts.ratio.log.100, log="no", 
                     main = "W(t) Raw Periodogram")
# not clear if a frequency is dominating here. try smoothing periodogram
w_spec_smooth <- spec.pgram(sp.ts.ratio.log.100, log="no", 
                            main = "W(t) Smoothed Periodogram, spans = 6", 
                            spans=6)
1 / w_spec_smooth$freq[which.maY(w_spec_smooth$spec)]

# ARIMA Model Selection and Validation -----------------------------------------
# is W(t) consistent with a realization of white noise?
p0d0q0P0D0Q0<-arima(sp.ts.ratio.log.100, order=c(0, 0, 0),
                    seasonal = list(order=c(0, 0, 0)))
cpgram(sp.ts.ratio.log.100)
par(mfrow=c(1,1))
AIC(p0d0q0P0D0Q0)
tsdiag(p0d0q0P0D0Q0)
w_spec$spec
w_spec$n.used

# Do D_c test from Assignment 2
res <- cumulative_periodogram(w_spec$spec, w_spec$n.used)
C_k <- res$C
kOverM <- res$kOverM
M <- res$M
plot_D_hypothesis(C_k, kOverM, M,  "Cumu. Periodogram: W(t)")
plot(kOverM,C_k, type="l", ylab = "C_k", xlab = "k / M", 
     main = "Cumu. Periodogram: W(t)")
pred.whitenoise <- forecast(p0d0q0P0D0Q0, h=5)


p1d0q1P0D0Q0<-arima(sp.ts.ratio.log.100, order=c(1, 0, 1), 
                    seasonal = list(order=c(0, 0, 0)))
p1d0q1P0D0Q0
tsdiag(p1d0q1P0D0Q0)
AIC(p1d0q1P0D0Q0)
mean(sum((p1d0q1P0D0Q0$residuals)^2))  # MSE of the model

predp1d0q1P0D0Q0 <- forecast(p1d0q1P0D0Q0, h=5)
# Backtransform predictions onto original scale
pred.w <- predp1d0q1P0D0Q0$mean

# -----------------------------------------------------------------------------
# Model comparisons
# -----------------------------------------------------------------------------
validation_set <- read.csv(validation_dir, na.strings=".")
actual <- validation_set$VALUE[1:5]

MSPE_model1_base <- mean((expSmooth.pred$mean-actual)^2)
MSPE_model1 <- mean((p2d2q1P0D0Q0.pred$mean - actual)^2)
MSPE_model1.alt <- mean((p2d2q1P0D0Q0.pred$mean - actual)^2)
# decide between MA(1) model and ARMA(2,1) for S(t)
MSPE_model1 < MSPE_model1.alt
# check best model against baseline
MSPE_model1 < MSPE_model1_base
actual_lagged <- log(lagratio(validation_set$VALUE[1:6], lag = 1,
                              recursion = 1, direction = "forward")) + 100
MSPE_model2_holt <- mean((pred.lagratio_smooth$mean - actual_lagged)^2)
MSPE_model2_noise <-mean((pred.whitenoise$mean-actual_lagged)^2)
MSPE_model2 <- mean((pred.w - actual_lagged)^2)
# check against baseline
MSPE_model2 < MSPE_model2_noise
