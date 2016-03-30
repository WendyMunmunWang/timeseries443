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

packages <- c("astsa", "dplyr", "FSA")
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
# Model One: SARIMA models for data with first difference
# -----------------------------------------------------------------------------

#Take the first difference
sp.ts.diff <- diff(sp.ts)
ts.plot(sp.ts.diff)
acf(sp.ts.diff)
pacf(sp.ts.diff)
spec.pgram(sp.ts.diff, log="no")

#ARIMA Model Validation
p1d1q1P0D0Q0<-arima(sp.ts, order=c(1, 1, 1), seasonal = list(order=c(0, 0, 0)))
tsdiag(p1d1q1P0D0Q0)
AIC(p1d1q1P0D0Q0)

#Holt Winter
hw <-HoltWinters(sp.ts.diff, alpha=NULL,beta=NULL, gamma=NULL)
plot(hw)

# -----------------------------------------------------------------------------
# Model Two: SARIMA models for data after taking forward ratio, applying 
#            log function, then add 100.
# -----------------------------------------------------------------------------

#Take the forward ratio, applying log function, then add 100
sp.ts.ratio <- lagratio(sp.ts, lag = 1L, recursion = 1L, direction = "forward")
sp.ts.ratio.log.100 <- log(sp.ts.ratio) + 100

ts.plot(sp.ts.ratio.log.100)
acf(sp.ts.ratio.log.100)
pacf(sp.ts.ratio.log.100)
spec.pgram(sp.ts.ratio.log.100, log="no")

#ARIMA Model Validation
p3d0q0P0D0Q0<-arima(sp.ts.ratio.log.100, order=c(3, 0, 0), seasonal = list(order=c(0, 0, 0)))
tsdiag(p3d0q0P0D0Q0)
AIC(p3d0q0P0D0Q0)

# Ratio stuff
# take data weekly, every friday
# e.g. this friday is x, next onis y, lpog ratio


