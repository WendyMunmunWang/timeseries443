install.packages('astsa')
?spec.pgram
set.seed(3)
wn<-rnorm(100000,0,2)
mean(wn)
spec.pgram(wn,log="no")
arima.sim()
library(astsa)
arma.spec(ar=c(0), ma=-.9, var.noise=4, log="no")
sim <- arima.sim(n=1000, list(ma = c(-0.9)),sd = 2)
spec.pgram(sim, log="no")


arma.spec(ar=c(0.8), var.noise=4, log="no")
sim <- arima.sim(n=1000, list(ar = c(0.8)),sd = 2)
spec.pgram(sim, log="no")
