# lab work # 6. also see lab6.Rhistory for history of all commands 7/03/2016
library(zoo)
library(forecast)
install.packages('forecast')
dat <- read.table('~/Documents/time_series_443/lab 6/souvenir.txt')
#dat <- dat[,2]
dat.ts <- ts(dat,freq=12,start=c(1987,01))
plot(dat.ts)
plot(diff(dat.ts))
acf(dat.ts)
pacf(dat.ts)
head(dat.ts)
dat.sub <- window(dat.ts,start=c(1987,1),end=c(1991,12))

hw <- HoltWinters(dat.sub, seasonal="multiplicative")
sqrt(hw$SSE)/(nrow(dat.sub)-length(hw$coefficients))
pred <- forecast(hw, h=12 )

hur <- data(LakeHuron)


train <- ts(train, start = c(1875))
test <- ts(test, start = c(1970))
acf(train)
pacf(train)
plot(train)
ar2 <- arima(train,c(2,0,0),include.mean=TRUE)
