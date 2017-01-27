test = function(x){x*exp(-((1-x)^2)/2)/(sqrt(2*pi)*pi*(1+x^2))}
integrate(test, lower = -Inf, upper = Inf)
