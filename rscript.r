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
# dir <- "~/Documents/STAT443/Stat443_project/timeseries443/spdata.csv" # Wendy

spdata <- read.csv(dir, na.strings=".")
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
sp.ts <- ts(rev(spdata$Adj.Close))
ts.plot(sp.ts)
acf(sp.ts, lag.max=40)
pacf(sp.ts)
spec.pgram(sp.ts, log="no")

# -----------------------------------------------------------------------------
# Model One: SARIMA models for data with first difference
# -----------------------------------------------------------------------------

#Take the first difference
sp.ts.diff <- diff(sp.ts, 1)
ts.plot(sp.ts.diff)
acf(sp.ts.diff)
pacf(sp.ts.diff)
spec.pgram(sp.ts.diff, log="no")

#ARIMA Model Validation
p1d1q1P0D0Q0<-arima(sp.ts, order=c(1, 1, 1), seasonal = list(order=c(0, 0, 0)))
tsdiag(p1d1q1P0D0Q0, lag.max=40)
AIC(p1d1q1P0D0Q0)
forecast(p1d1q1P0D0Q0, h=5)

# -----------------------------------------------------------------------------
# Holt Winter
# -----------------------------------------------------------------------------

hw <-HoltWinters(sp.ts.diff, alpha=0.2, beta=0.4, gamma=FALSE) 
plot(hw)

# -----------------------------------------------------------------------------
# Model Two: SARIMA models for data after taking forward ratio, applying 
#            log function, then add 100.
# -----------------------------------------------------------------------------

#Lag constant
days <- 0

#Take the forward ratio, applying log function, then add 100
sp.ts.ratio <- lagratio(sp.ts, lag = days, recursion = 1, direction = "forward")
sp.ts.ratio.log.100 <- log(sp.ts.ratio) + 100

ts.plot(sp.ts.ratio.log.100)
acf(sp.ts.ratio.log.100)
pacf(sp.ts.ratio.log.100)
spec.pgram(sp.ts.ratio.log.100, log="no")

#ARIMA Model Validation
p3d0q0P0D0Q0<-arima(sp.ts.ratio.log.100, order=c(3, 0, 0), seasonal = list(order=c(0, 0, 0)))
tsdiag(p3d0q0P0D0Q0, gof.lag = 40)
AIC(p3d0q0P0D0Q0)

# Ratio stuff
# take data weekly, every friday
# e.g. this friday is x, next onis y, lpog ratio


# -----------------------------------------------------------------------------
# Model Three: GARCH
# -----------------------------------------------------------------------------

p3d0q0P0D0Q0$residuals
library(rugarch)
library(fGarch)
s2 <- (sp.ts^2)
plot(s2)
acf(s2, lag.max = 1000)
# experiment with GARCH model using parameters from best SARIMA model
# financial markets are modelled better by t-distribution, use "std"
specs<-ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(3, 0), include.mean = TRUE),
  distribution.model = "std"
)
#
N <- length(sp.ts)
# TODO: what is the definition of a return? Why multiply by 100 and take ratio of successive days?
sp.ts.returns=100*(log(sp.ts[2:N])-log(sp.ts[1:(N-1)]))
modelfit = ugarchfit(spec = specs, data = sp.ts.returns)
modelfit
plot(modelfit)
