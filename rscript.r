X <- read.csv("Downloads/S&Praw.csv", header=T, skip=35, na.string = "#N/A")
Y <- read.csv("Downloads/S&Praw.csv", header=T, skip=35)
X
X <- X[complete.cases(X),]

