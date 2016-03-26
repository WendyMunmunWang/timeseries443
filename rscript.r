# -----------------------------------------------------------------------------
# Function declarations
# -----------------------------------------------------------------------------

install_new_packages <- function(packages) {
  #' Installs each package in packages if not already installed
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

packages <- c("astsa", "dplyr", "FSA", "gogarch")
install_new_packages(packages)
library(FSA)

# -----------------------------------------------------------------------------
# Read data
# -----------------------------------------------------------------------------

# Uncomment the correct directory for your computer (or add it!):
dir <- "~/Documents/time_series_443/PROJECT/spdata.csv" # Geoff
# dir <- "~/Documents/STAT443/Stat443_project/timeseries443/spdata.csv" # Wendy

spdata <- read.csv(dir)

# -----------------------------------------------------------------------------
# Explore data and candidate SARIMA models
# -----------------------------------------------------------------------------

View(spdata)
sp.ts <- ts(rev(spdata$Adj.Close))
ts.plot(sp.ts)
acf(sp.ts)
pacf(sp.ts)
spec.pgram(sp.ts, log="no")

# -----------------------------------------------------------------------------
# SARIMA models for data with first difference
# -----------------------------------------------------------------------------

#Take the first difference
sp.ts.diff <- diff(sp.ts)
plot(sp.ts.diff)
acf(sp.ts.diff)
pacf(sp.ts.diff)
spec.pgram(sp.ts.diff, log="no")

#ARIMA Model Validation
p1d1q1P0D0Q0<-arima(sp.ts.diff, order=c(1, 1, 1), seasonal = list(order=c(0, 0, 0)))
tsdiag(p1d1q1P0D0Q0)
AIC(p1d1q1P0D0Q0)

p1d2q1P0D0Q0<-arima(sp.ts, order=c(1, 2, 1), seasonal = list(order=c(0, 0, 0)))
tsdiag(p1d2q1P0D0Q0)
AIC(p1d2q1P0D0Q0)

#Holt Winter
hw <-HoltWinters(sp.ts.diff, alpha=NULL,beta=NULL, gamma=NULL)
plot(hw)

# -----------------------------------------------------------------------------
# SARIMA models for data after forward ratio
# -----------------------------------------------------------------------------

#Take the forward ratio
sp.ts.ratio <- lagratio(sp.ts, lag = 1L, recursion = 1L, direction = "forward")

plot(sp.ts.ratio)
acf(sp.ts.ratio)
pacf(sp.ts.ratio)
spec.pgram(sp.ts.ratio, log="no")

#ARIMA Model Validation
p3d0q0P0D0Q0<-arima(sp.ts.ratio, order=c(3, 0, 0), seasonal = list(order=c(0, 0, 0)))
tsdiag(p3d0q0P0D0Q0)
AIC(p3d0q0P0D0Q0)

# Ratio stuff
# take data weekly, every friday
# e.g. this friday is x, next onis y, lpog ratio


