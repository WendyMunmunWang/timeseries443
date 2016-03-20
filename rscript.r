spdata <- read.csv("~/Documents/STAT443/Stat443_project/timeseries443/spdata.csv")
View(spdata)
sp.ts <- ts(rev(spdata$Adj.Close), start = c(1990,01,3), end = c(2016,03,14))
#sp.ts <- ts(rev(spdata$Adj.Close))
ts.plot(sp.ts)

install.packages('astsa')
library(astsa)
spec.pgram(sp.ts, log="no")

#Take the first difference
sp.ts.diff <- diff(sp.ts)
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


