install.packages("rootSolve")
model <- function(x) {
    F1 = (x[2]/(x[2]+10))^x[1] - 0.75
    F2 = (x[2]/(x[2]+49))^x[1] - 0.25
    c(F1 = F1, F2 = F2)
}
xstart <- c(2,2)
fstart <- model(xstart)
nleqslv(xstart, model, control=list(btol=.0001))

library(coda)
res = read.coda("CODAchain1.txt", "CODAindex.txt")
lambda1 = res[,1]
plot(as.numeric(lambda1), xlim = c(0,2000), type = "l",
xlab = "Iteration", ylab = expression(lambda)) ##trace plot
acf(lambda1, lag.max = 40, xlab = "Lag", ylab = "Correlation", main="") ## autocorrelation plot
geweke.diag(mcmc.list(res1)) ## geweke diagnostic
setwd("/Users/syedrahman/Documents/Spring2014/MonteCarlo/HW4/hw4jags/")
