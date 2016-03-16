X <- read.csv("Downloads/S&Praw.csv", header=T, skip=35, na.string = "#N/A")
Y <- read.csv("Downloads/S&Praw.csv", header=T, skip=35)
X
X <- X[complete.cases(X),]

SPdata <- read.csv("~/timeseries443/SPdata.txt")
View(SPdata)
sp.ts <- ts(SPdata$VALUE, frequency = 5, include.mean=TRUE)
ts.plot(sp.ts)
acf(sp.ts)
pacf(sp.ts)
plot(diff(sp.ts))
acf(diff(sp.ts))
pacf(diff(sp.ts))

install.packages('astsa')
library(astsa)
spec.pgram(sp.ts, log="no")
