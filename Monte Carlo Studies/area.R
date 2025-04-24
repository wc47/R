draws<-10000
r<-sqrt(.5)
x<-runif(draws,-r,r)
y<-runif(draws,-r,r)
a<-1*mean(x^2+y^2<=0.5 & x^2<y & y<x)
ci<-a+c(-1,1)*qnorm(.975)*((a*(1-a)/draws))
cat(sprintf("The Monte Carlo estimate for the area is %7.5f,
         and the 95%% confidence interval is (%7.5f,%7.5f).",a,ci[1],ci[2]))
