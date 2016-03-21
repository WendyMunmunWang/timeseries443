?spec.pgram
library(astsa)
sigma = 2
x <- arima.sim(model=list(), 100, sd = sigma)
# spans means moving average
spec.pgram(x, log="no", spans=c(5))
spec.pgram(x, log="no", spans=c(15))
spec.pgram(x, log="no", spans=c(14, 10))
true <- arma.spec(var.noise = 4)

# repeate with 1000 observations
x <- arima.sim(model=list(), 1000, sd = sigma)
# spans means moving average
spec.pgram(x, log="no", spans=c(5))
spec.pgram(x, log="no", spans=c(15))
spec.pgram(x, spans=c(7,5 ))
etrue <- arma.spec(var.noise = 4)

spec.pgram(x, spans=c(5))
spec.pgram(x, spans=c(15))
spec.pgram(x, spans=c(7, 5))

x <- arima.sim(model=list(-0.9), 1000, sd = sigma)
# spans means moving average
spec.pgram(x, log="no", spans=c(5))
for (i in 15:30) {
  est <- spec.pgram(x, log="no", spans=c(i))
  plot(est, col="red")
  true <- arma.spec(ma=-0.9, var.noise = 4)
  plot(true, col="blue", add=TRUE)
}
spec.pgram(x, log="no", spans=c(300, 200))
spec.pgram(x, log="no", spans=c(50, 10))
true <- arma.spec(ma=-0.9, var.noise = 4)
