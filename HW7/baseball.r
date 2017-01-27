Y <-  c(.4,.378,.356,.333,.311,.311,.289,.267,.244,.244,.222,.222,.222,.222,.222,.200,.178,.156)
X = sqrt(45)*asin(2*Y-1)

library(coda)
res = read.coda("CODAchain1.txt", "CODAindex.txt")
p = matrix(0,nrow = dim(res)[1], ncol = 18)
meanp = matrix(0,nrow = 18, ncol = 1)
for (k in 1:18){
p[,k] = (1/2)*(sin(res[,20+k]/sqrt(45))+1)
meanp[k] = mean(p[,k])}
meanp = as.data.frame(meanp)

efm = as.data.frame(c(0.334,0.313, 0.292, 0.277, 0.273, 0.273, 0.268, 0.264, 0.259, 0.259, 0.254, 0.254, 0.254, 0.254, 0.254, 0.249, 0.244, 0.239))
data = baseball
data$name <- paste(data[,2],data[,1],sep=",")
data <- subset(data, select =c(name, BattingAverage,RemainingAverage))
colnames(data) <- c("Player","InSamp","OutSamp")
n <- 45
k <- 18
data$InSampT <- sqrt(n)*asin(2*data[,2]-1)
data$OutSampT <- sqrt(n)*asin(2*data[,3]-1)
global.avg <- mean(data$InSampT)
dev <- data$InSampT - global.avg
data$jst <- global.avg + (1 - (k-3)/sum((data$InSampT -global.avg)^2 ))*dev
data$js <- 0.5*(sin(data$js/sqrt(n))+1)
performance <- sum((data$InSampT - data$OutSampT)^2)/ sum((data$jst - data$OutSampT)^2)
temp <- subset(data, select = c(Player, OutSamp,js))
temp <- cbind(temp,efm,meanp)
colnames(temp) <- c("Player","True Value","Stein","Efron-Morris","Posterior Means")
countperf <- sum(abs(temp$TrueValue - temp$Stein) <
abs(temp$TrueValue - temp$MLE))
library(xtable)
xtable(temp, caption="Table comparing the Stein,Efron-Morris and Bayes Estimates for $\\chi^2_1$",label="tab1",digits=c(0,0,3,3,3,3))
attach(temp)
datamat = as.matrix(temp)
stmse = (sum(as.numeric(datamat[,2]) - as.numeric(datamat[,3]))^2)/18
efmse = (sum(as.numeric(datamat[,2]) - as.numeric(datamat[,4]))^2)/18
bayesmse = (sum(as.numeric(datamat[,2]) - as.numeric(datamat[,5]))^2)/18

prob = (1/length(p[,1]))*sum(p[,1]<p[,2])
prob
stmse 
efmse 
bayesmse


pdf("pdensity1.pdf")
par(mfrow=c(3,3))
plot(density(p[,1]),xlab = "x",ylab = "f(x)", main = bquote("Density Plot for"~ p[1]))
plot(density(p[,2]),xlab = "x",ylab = "f(x)", main = bquote("Density Plot for"~ p[2]))
plot(density(p[,3]),xlab = "x",ylab = "f(x)", main = bquote("Density Plot for"~ p[3]))
plot(density(p[,4]),xlab = "x",ylab = "f(x)", main = bquote("Density Plot for"~ p[4]))
plot(density(p[,5]),xlab = "x",ylab = "f(x)", main = bquote("Density Plot for"~ p[5]))
plot(density(p[,6]),xlab = "x",ylab = "f(x)", main = bquote("Density Plot for"~ p[6]))
plot(density(p[,7]),xlab = "x",ylab = "f(x)", main = bquote("Density Plot for"~ p[7]))
plot(density(p[,8]),xlab = "x",ylab = "f(x)", main = bquote("Density Plot for"~ p[8]))
plot(density(p[,9]),xlab = "x",ylab = "f(x)", main = bquote("Density Plot for"~ p[9]))
dev.off()
pdf("pdensity2.pdf")
par(mfrow=c(3,3))
plot(density(p[,10]),xlab = "x",ylab = "f(x)", main = bquote("Density Plot for"~ p[10]))
plot(density(p[,11]),xlab = "x",ylab = "f(x)", main = bquote("Density Plot for"~ p[11]))
plot(density(p[,12]),xlab = "x",ylab = "f(x)", main = bquote("Density Plot for"~ p[12]))
plot(density(p[,13]),xlab = "x",ylab = "f(x)", main = bquote("Density Plot for"~ p[13]))
plot(density(p[,14]),xlab = "x",ylab = "f(x)", main = bquote("Density Plot for"~ p[14]))
plot(density(p[,15]),xlab = "x",ylab = "f(x)", main = bquote("Density Plot for"~ p[15]))
plot(density(p[,16]),xlab = "x",ylab = "f(x)", main = bquote("Density Plot for"~ p[16]))
plot(density(p[,17]),xlab = "x",ylab = "f(x)", main = bquote("Density Plot for"~ p[17]))
plot(density(p[,18]),xlab = "x",ylab = "f(x)", main = bquote("Density Plot for"~ p[18]))
dev.off()

 

setwd("/Users/syedrahman/Documents/Spring2014/MonteCarlo/HW7/hw72nd")
setwd("/Users/syedrahman/Documents/Spring2014/MonteCarlo/HW7")
