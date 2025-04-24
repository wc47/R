f<-function(x)1-2/pi*(1-x)^2*exp(x)/x*asin(sqrt(x))
# Using pdf y=3/2*sqrt(x).
p<-function(x)3/2*sqrt(x)
curve(f(x),0,1,xlim=c(0.1,1))
curve(2*x-1,0,1,add=TRUE,col='red')
# I tried the functions y=2x, y=x, y=1, and y=arctan(x) before I tried
# this one and settled on it. arctan(x) seemed to fit the best.
# However, the inverse is difficult to take, so I settled on
# y=sqrt(x). y=1 works the best of all the ones I tested, but
# you requested we avoid that one.

# The cdf is F(x)=x^(3/2). The invers of that is x^(2/3)
F<-function(x) x^(3/2)
Finv<-function(x) x^(2/3)
B <- 10000
x <- Finv(runif(B))
# Here we check the fit.
hist(x,freq=FALSE)
# Looks good, so we proceed.

h<-function(x) f(x)/p(x)*(x>=0)*(x<=1)
est<-mean(h(x))
cat(sprintf("The Monte Carlo estimate is %7.5f\n",est))
ci<-est+c(-1,1)*qnorm(.975)*((est*(1-est))/B)
cat(sprintf("The 95%% confidence interval is (%7.5f,%7.5f).\n",ci[1],ci[2]))
