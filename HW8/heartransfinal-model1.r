
                                        #setwd("/Users/syedrahman/Documents/Spring2014/MonteCarlo/HW8/Model1-thin1")


                                        setwd("/Users/syedrahman/Documents/Spring2014/MonteCarlo/HW8/Model1-thin5")

                                        #setwd("/Users/syedrahman/Documents/Spring2014/MonteCarlo/HW8/Model2-start1thin1")



                                        #setwd("/Users/syedrahman/Documents/Spring2014/MonteCarlo/HW8/Model2-start1thin5")

library(coda)
data = read.table("ht-data.txt",header=TRUE)
attach(data)
length(e)
res = read.coda("CODAchain1.txt", "CODAindex.txt")
logpost = matrix(0,dim(res)[1],1)
for(i in 1:94){
    temp1 <- log(dpois(round(res[,96+i]),res[,2+i]*e[i]))
    temp2 <- log(dgamma(res[,2+i],res[,2],rate = res[,2]/res[,1]))
    logpost = logpost + temp1 + temp2
}

colnames(res)

pdf("alpha.pdf")
plot(as.numeric(res[,2]),  type= 'l',xlim = c(0,2000),
xlab = "Iteration", ylab = expression(alpha)) ##trace plot
dev.off()


pdf("mu.pdf")
plot(as.numeric(res[,1]),  type= 'l',xlim = c(0,2000),
xlab = "Iteration", ylab = expression(beta)) ##trace plot
dev.off()

pdf("tracelmd1.pdf")
plot(as.numeric(res[,3]),  type= 'l',xlim = c(0,2000),
xlab = "Iteration", ylab = expression(lambda[1])) ##trace plot
dev.off()

pdf("logpost.pdf")
plot(as.numeric(-logpost),  type= 'l',xlim = c(0,2000),
xlab = "Iteration", ylab = "Negative Log Posterior" ) ##plot for log posterior
dev.off()

pdf("cumsum.pdf")
x = cumsum(-logpost)/1:dim(res)[1]
plot(x,  type= 'l',ylab = "Cumulative Negative Log Posterior")
dev.off()


jpeg("cumsum.jpeg")
x = cumsum(-logpost)/1:dim(res)[1]
plot(x,  type= 'l',ylab = "Cumulative Negative Log Posterior")
dev.off()

pdf("acf.pdf")
acf(res[,3], lag.max = 40, xlab = "Lag", ylab = "Correlation", main="")
## autocorrelation plot
dev.off()

meanlambda = matrix(0,94,1)
for(i in 1:94){
meanlambda[i] = mean(res[,2+i])
}


library(xtable)
temp = as.data.frame(cbind(1:47,meanlambda[1:47],48:94,meanlambda[48:94]))
colnames(temp) = c("i","Estimated Mean","i","Estimated Mean")
temp
temptab = xtable(temp, caption="Estimated means for $\\lambda_i$",label="tab1",digits=c(0,0,3,0,3))
print(temptab,include.rownames=FALSE)

which(meanlambda>max(meanlambda)-0.15)

which(meanlambda<min(meanlambda)+0.15)
