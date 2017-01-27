library(coda)
res = read.coda("CODAchain1.txt", "CODAindex.txt")
psi1 = exp(res[,2])


pdf("psi1trace.pdf")
plot(as.numeric(psi1),  type= 'l',xlim = c(0,2000),
xlab = "Iteration", ylab = expression("exp(" ~ psi[1]~")")) ##trace plot
dev.off()

pdf("psi1acf.pdf")
acf(psi1, lag.max = 40, xlab = "Lag", ylab = "Correlation", main="")
## autocorrelation plot
dev.off()

pdf("psi1density.pdf")
plot(density(psi1),xlab = "x",ylab = "f(x)", main = expression("Density Plot for exp(" ~ psi[1]~")"))
dev.off()
prob = (1/length(psi1))*sum(psi1>1)
prob
