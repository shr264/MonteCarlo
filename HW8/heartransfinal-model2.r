
                                        #setwd("/Users/syedrahman/Documents/Spring2014/MonteCarlo/HW8/Model1-thin1")


                                        #setwd("/Users/syedrahman/Documents/Spring2014/MonteCarlo/HW8/Model1-thin5")

                                        #setwd("/Users/syedrahman/Documents/Spring2014/MonteCarlo/HW8/Model2-start1thin1")



                                        #setwd("/Users/syedrahman/Documents/Spring2014/MonteCarlo/HW8/Model2-start1thin5")
                                        #setwd("/Users/syedrahman/Documents/Spring2014/MonteCarlo/HW8/Model2-start1thin100")
#setwd("/Users/syedrahman/Documents/Spring2014/MonteCarlo/HW8/Model2-start2thin100")
setwd("/Users/syedrahman/Documents/Spring2014/MonteCarlo/HW8/Model2-start3thin100")

library(coda)
data = read.table("ht-data.txt",header=TRUE)
attach(data)
length(e)
res = read.coda("CODAchain1.txt", "CODAindex.txt")

z0 = 0.53
a0 = log(2)/z0
b0 = 1
b1 = 0.65
logpost = matrix(0,dim(res)[1],1)
for(i in 1:94){
    temp1 <- log(dpois(round(res[,96+i]),res[,2+i]*e[i]))
    temp2 <- log(dgamma(res[,2+i],res[,2],rate = res[,1]))
    temp3 <- log(dexp(res[,2],rate=a0))
    temp4 <- log(dgamma(res[,1],b0,rate = b1))
    logpost = logpost + temp1 + temp2 + temp3 + temp4
}

colnames(res)

lik = numeric(dim(res)[1])
for(i in 1:dim(res)[1]){
  dpois.all = 1
  for(j in 1:94){
    dpois.all = dpois.all*dpois(z[j],lambda=res[i,j+2]*e[j])
  }
  lik[i] = dpois.all*
            prod(dgamma(res[i,3:96],shape=res[i,2],
                        rate=res[i,1]))
}

pdf("logpost.pdf")
plot(as.numeric(-logpost),  type= 'l',xlim = c(0,2000),
xlab = "Iteration", ylab = "Negative Log Posterior" ,
     main = "Negative Log Posterior" )
##plot for log posterior
dev.off()

pdf("logpost2.pdf")
plot(as.numeric(-logpost),  type= 'l',xlim = c(dim(res)[1]-2000,dim(res)[1]),
xlab = "Iteration", ylab = "Negative Log Posterior" ,
     main = "Negative Log Posterior" )
##plot for log posterior
dev.off()

pdf("cumsum.pdf")
x = cumsum(-logpost)/1:dim(res)[1]
plot(x,  type= 'l',
     ylab = "Cumulative Average of Negative Log Posterior",
     main = "Cumulative Average of Negative Log Posterior")
dev.off()

png('logliktrace.png',width=6,height=4.5,units='in',res=300)
plot(log(lik),type="l",main="Trace Plot of Log Posterior Likelihood")
dev.off()


pdf("alpha.pdf")
plot(as.numeric(res[,2]),  type= 'l',xlim = c(0,2000),
     ylim = c(0,10),
xlab = "Iteration", ylab = expression(alpha),
     main = expression("Trace Plot for " ~ alpha))
##trace plot
dev.off()


pdf("beta.pdf")
plot(as.numeric(res[,1]),  type= 'l',xlim = c(0,2000),
     ylim = c(0,10),
xlab = "Iteration", ylab = expression(beta),
     main = expression("Trace Plot for " ~ beta))
##trace plot
dev.off()

pdf("tracelmd9.pdf")
plot(as.numeric(res[,2+9]),  type= 'l',xlim = c(0,2000),
     ylim = c(0,10),
xlab = "Iteration", ylab = expression(lambda[9]),
     main=expression("Trace Plot for " ~ lambda[9]))
##trace plot
dev.off()
pdf("acf9.pdf")
acf(res[,2+9], lag.max = 200, xlab = "Lag",
    ylab = "Correlation",
    main=expression("Autocorrelation plot for " ~ lambda[9]))
## autocorrelation plot
dev.off()

pdf("tracelmd63.pdf")
plot(as.numeric(res[,2+63]),  type= 'l',xlim = c(0,2000),
     ylim = c(0,10),
xlab = "Iteration", ylab = expression(lambda[63]),
     main=expression("Trace Plot for " ~ lambda[63]))
##trace plot
dev.off()
pdf("acf63.pdf")
acf(res[,2+63], lag.max = 200, xlab = "Lag",
    ylab = "Correlation",
    main=expression("Autocorrelation plot for " ~ lambda[63]))
## autocorrelation plot
dev.off()

pdf("tracelmd68.pdf")
plot(as.numeric(res[,2+68]),  type= 'l',xlim = c(0,2000),
     ylim = c(0,10),
xlab = "Iteration", ylab = expression(lambda[68]),
     main=expression("Trace Plot for " ~ lambda[68]))
##trace plot
dev.off()
pdf("acf68.pdf")
acf(res[,2+68], lag.max = 200, xlab = "Lag",
    ylab = "Correlation",
    main=expression("Autocorrelation plot for " ~ lambda[68]))
## autocorrelation plot
dev.off()

pdf("tracelmd85.pdf")
plot(as.numeric(res[,2+85]),  type= 'l',xlim = c(0,2000),
     ylim = c(0,10),
xlab = "Iteration", ylab = expression(lambda[85]),
     main=expression("Trace Plot for " ~ lambda[85]))
##trace plot
dev.off()
pdf("acf85.pdf")
acf(res[,2+85], lag.max = 200, xlab = "Lag",
    ylab = "Correlation",
    main=expression("Autocorrelation plot for " ~ lambda[85]))
## autocorrelation plot
dev.off()

jpeg("cumsum.jpeg")
x = cumsum(-logpost)/1:dim(res)[1]
plot(x,  type= 'l',
     ylab = "Cumulative Average of Negative Log Posterior")
dev.off()

pdf("acf.pdf")
acf(res[,3], lag.max = 200, xlab = "Lag", ylab = "Correlation",
    main="")
## autocorrelation plot
dev.off()

pdf("alphaacf.pdf")
acf(res[,2], lag.max = 200, xlab = "Lag", ylab = "Correlation",
    main=expression("Autocorrelation Plot for " ~ alpha))
## autocorrelation plot
dev.off()

pdf("betaacf.pdf")
acf(res[,1], lag.max = 200, xlab = "Lag", ylab = "Correlation",
    main=expression("Autocorrelation Plot for " ~ beta))
## autocorrelation plot
dev.off()

btchsz = floor(dim(res)[1]/sqrt(dim(res)[1]))
nbtch = floor(dim(res)[1]/btchsz)


meanlambda = matrix(0,94,1)
varlambda = matrix(0,94,1)

for(i in 1:94){
meanlambda[i] = mean(res[,2+i])

btchvar = matrix(0,nbtch,1)
for (j in 1:nbtch){
    lb = (j-1)*btchsz
    ub = j*btchsz
btchvar[j] = var(res[lb:ub,2+i])
}

varlambda[i] = (btchsz/(nbtch-1))*sum((btchvar - var(res[,2+i]))^2) 

}

res.sum = summary(res[,1:96])
d.res.sum = as.data.frame(res.sum$statistics)
dim(d.res.sum)

bS=floor(dim(res)[1]/sqrt(dim(res)[1]))

library(xtable)
temp = as.data.frame(cbind(1:47, meanlambda[1:47],
    d.res.sum[(1+2):(47+2),3],
    as.matrix(batchSE(res[,1:96],batchSize=bS)[(1+2):(47+2)]),
    d.res.sum[(1+2):(47+2),4], 48:94, meanlambda[48:94],
    d.res.sum[(48+2):(94+2),3],
    as.matrix(batchSE(res[,1:96],
                      batchSize=bS)[(48+2):(94+2)]),
    d.res.sum[(48+2):(94+2),4]))
colnames(temp) =
    c("i","Mean","SE","SE(BM)","SE(TS)","i",
      "Mean","SE","SE(BM)","SE(TS)")
temptab = xtable(temp,
    caption="Estimated means for $\\lambda_i$",
    label="tab1",digits=c(0,0,3,4,4,4,0,3,4,4,4))
print(temptab,include.rownames=FALSE)

which(meanlambda>max(meanlambda)-0.15)

which(meanlambda<min(meanlambda)+0.15)


0.468 -1.96*0.0012
0.468 +1.96*0.0012

0.368 -1.96*0.0009
0.368 +1.96*0.0009

1.512 -1.96*0.0028
1.512 +1.96*0.0028

1.642-1.96*0.0022
1.642+1.96*0.0022


par(mfrow=c(3,2))
for (i in 91:94){
acf(res[,2+i], lag.max = 200, xlab = "Lag", ylab = "Correlation", main="")
## autocorrelation plot
}

