# -----------------------------------------------------------------------------
# Function declarations
# -----------------------------------------------------------------------------

install_new_packages <- function(packages) {
  #' Installs each packages in packages if not already installed
  #' 
  #' @param packages A character vector containing the names of packages
  
  # decide which packages are needed
  to_install <- !(needed_packages %in% installed.packages()[, "Package"])
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

packages <- c("astsa", "dplyr")
install_new_packages(packages)

# -----------------------------------------------------------------------------
# Read data
# -----------------------------------------------------------------------------

# Uncomment the correct director for your computer (or add it!):
dir <- "~/Documents/time_series_443/PROJECT/spdata.csv" # Geoff's
# dir <- "~/Documents/STAT443/Stat443_project/timeseries443/spdata.csv" # Wendy's

spdata <- read.csv(dir)

# -----------------------------------------------------------------------------
# Explore data and candidate SARIMA models
# -----------------------------------------------------------------------------

View(spdata)
sp.ts <- ts(rev(spdata$Adj.Close))
ts.plot(sp.ts)
spec.pgram(sp.ts, log="no")

#Take the first difference
sp.ts.diff <- diff(sp.ts)
plot(sp.ts.diff)
acf(sp.ts.diff)
pacf(sp.ts.diff)
spec.pgram(sp.ts.diff, log="no")

#ARIMA Model Validation
p1d1q1P0D0Q0<-arima(sp.ts.diff, order=c(1, 1, 1), seasonal = list(order=c(0, 0, 0)))
tsdiag(p1d1q1P0D0Q0)

#Holt Winter
hw <-HoltWinters(sp.ts.diff, alpha=NULL,beta=NULL, gamma=NULL)
plot(hw)
#Take the forward ratio
install.packages('FSA')
library(FSA)
sp.ts.ratio <- lagratio(sp.ts, lag = 1L, recursion = 1L, direction = "forward")

plot(sp.ts.ratio)
acf(sp.ts.ratio)
pacf(sp.ts.ratio)
spec.pgram(sp.ts.ratio, log="no")


