set.seed(10)
n = 1000000
X=rnorm(n)
h = function(x){exp(-x^4+(x^2)/2)}
Ihat = sqrt(2*pi)*mean(h(X))
SE = (2*pi)*sqrt(var(h(X)))/sqrt(n)

