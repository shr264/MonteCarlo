radio = read.table("breast-cancer-data-radiotherapy.txt",
    header=TRUE,na.string = "---")
attach(radio)
U[is.na(U)]=Inf
lambda1 = 1
sum = sum(L)
a = 33.02833
b = 1143.09157
N = 46
nsim = 100000
lambda1 = rep(0,nsim)
u = rep(0,N)
x = rep(0,N)
unew = rep(0,1)
xnew1 = rep(0,nsim)
    
for (j in 1:nsim)
    {
        lambda1[j] = rgamma(n=1,a+N,(b+sum))
        lambdatemp= lambda1[j]
        low = pexp(L,rate = lambdatemp)
        up = pexp(U,rate = lambdatemp)
        u = runif(n=46,low,up)
        x = (-1/lambdatemp)*log(1-u)
        unew = runif(n=1,0,1)
        xnew1[j] = (-1/lambdatemp)*log(1-unew)
        sum = sum(x)
    }

radiochem = read.table("breast-cancer-data-radioandchemo.txt",
    header=TRUE,na.string="---")
attach(radiochem)
U[is.na(U)]=Inf
lambdatemp = 1
sum = sum(L)
a = 33.02833
b = 1143.09157
N = 46
nsim = 100000
lambda2 = rep(0,nsim)
u = rep(0,N)
x = rep(0,N)
unew = rep(0,1)
xnew2 = rep(0,nsim)
for (j in 1:nsim)
    {
        lambda2[j] = rgamma(n=1,a+N,(b+sum))
        lambdatemp= lambda2[j]
        low = pexp(L,rate = lambdatemp)
        up = pexp(U,rate = lambdatemp)
        u = runif(n=46,low,up)
        x = (-1/lambdatemp)*log(1-u)
        unew = runif(n=1,0,1)
        xnew2[j] = (-1/lambdatemp)*log(1-unew)
        sum = sum(x)
    }

success = sum(lambda1>lambda2)
total = nsim
success/total
pdf("lambda1.pdf")
plot(density(lambda1),xlab = "x",ylab = "f(x)",
     main = expression("Density Plot for" ~ lambda[1]),
     ylim=c(0,140),xlim=c(0.01,0.045))
dev.off()
pdf("lambda2.pdf")
plot(density(lambda2),xlab = "x",ylab = "f(x)",
     main = expression("Density Plot for" ~lambda[2]),
     ylim=c(0,140),xlim=c(0.01,0.045))
dev.off()
pdf("explambda1.pdf")
plot(density(exp(-lambda1*36)),xlab = "x",ylab = "f(x)",
     main = expression("Density Plot for" ~ e^(-36%*%lambda[1])),
     ylim=c(0,10),xlim=c(0.2,0.8))
dev.off()
pdf("explambda2.pdf")
plot(density(exp(-lambda2*36)),xlab = "x",ylab = "f(x)",
     main = expression("Density Plot for" ~e^(-36%*%lambda[2])),
     ylim=c(0,10),xlim=c(0.2,0.8))
dev.off()
pdf("mlambda1.pdf")
plot(density(xnew1),xlab = "x",ylab = "f(x)",
     main = expression("Density Plot for New Patient under Treatment 1"),
     ylim=c(0,0.025),xlim=c(0,400))
dev.off()
pdf("mlambda2.pdf")
plot(density(xnew2),xlab = "x",ylab = "f(x)",
     main = expression("Density Plot for New Patient under Treatment 2"),
     ylim=c(0,0.025),xlim=c(0,400))
dev.off()
