X <- read.csv("Downloads/S&Praw.csv", header=T, skip=35, na.string = "#N/A")
Y <- read.csv("Downloads/S&Praw.csv", header=T, skip=35)
X
X <- X[complete.cases(X),]

SPdata <- read.csv("~/timeseries443/spdata.csv")
View(SPdata)
sp.ts <- ts(rev(SPdata$Adj.Close))
ts.plot(sp.ts)
acf(sp.ts)
pacf(sp.ts)

install.packages('astsa')
library(astsa)
spec.pgram(sp.ts, log="no")

#Take the first difference
sp.ts.diff < - diff(sp.ts)
plot(sp.ts.diff)
acf(sp.ts.diff)
pacf(sp.ts.diff)
spec.pgram(sp.ts.diff, log="no")

#Take the forward ratio
install.packages('FSA')
library(FSA)
sp.ts.ratio <- lagratio(sp.ts, lag = 1L, recursion = 1L, direction = "forward")

plot(sp.ts.ratio)
acf(sp.ts.ratio)
pacf(sp.ts.ratio)
spec.pgram(sp.ts.ratio, log="no")


