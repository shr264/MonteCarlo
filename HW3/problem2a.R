set.seed(10)
n = 1000000
th = rnorm(n,1,1)
l = function(x,sigma){((1+x^2)^-1)/((1/sigma)*exp(-(x^2/(2*sigma))))}
h1 = function (x) {x/(l(x))}
h2 = function (x) {1/(l(x))}
u = mean(h1(th))
v = mean(h2(th))
s1 = var(h1(th))
s2 = var(h2(th))
s12 = cov(h1(th),h2(th))
Ihat = u/v
SE = sqrt(s1/(v^2) - 2*s12*(u/(v^3)) + s2*(u^2)/(v^4))/sqrt(n)
