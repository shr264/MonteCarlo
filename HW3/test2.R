set.seed(10)
n = 1000000
X=rcauchy(n)
h = function(x){x*exp(-((1-x)^2)/2)}
Ihat = sqrt(1/(2*pi))*mean(h(X))
SE = (2*pi)*sqrt(var(h(X)))/sqrt(n)

