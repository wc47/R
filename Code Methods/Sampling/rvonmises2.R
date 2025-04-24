rvonmises2<-function(n,mu,nu,kappa1,kappa2,lambda)
{
  if(!is.loaded("rvonmises")){
   setwd("~/STAT624/homework/11")
   dyn.load("./rvonmises2.so")
   setwd("~/STAT624/homework/8")
  }
  erf<-"ERROR: Bad Input\n"
  if(n<=0)  return(erf)
  if(!is.numeric(mu)) return(erf)
  if(!is.numeric(nu)) return(erf)
  if(kappa1<=0) return(erf)
  if(kappa2<=0) return(erf)
  if(!is.numeric(lambda)) return(erf)
 
  samples<-numeric(2*n)
  storage.mode(samples)<- "double"
  lap1<-system.time(all<-.C("rsampler", n=as.integer(n), mu=as.double(mu),
           nu=as.double(nu), kappa1=as.double(kappa1), kappa2=as.double(kappa2),
           lambda=as.double(lambda),samples=samples))[3]
  samples<-t(matrix(all$samples,nrow=2,ncol=n)) 
  samples
}

race<-function(mu, nu, kappa1, kappa2, lambda,ntests, diff)
{
  setwd("~/STAT624/homework/11")
  dyn.load("./rvonmises2.so")
  setwd("~/STAT624/homework/8")

  source("rvonmises.R")
  erf<-"ERROR: Bad Input\n"
  if(ntests<=0)  return(erf)
  if(!is.numeric(mu)) return(erf)
  if(!is.numeric(nu)) return(erf)
  if(kappa1<=0) return(erf)
  if(kappa2<=0) return(erf)
  if(!is.numeric(lambda)) return(erf)

  test<-numeric(ntests)
  test<-sapply(1:ntests, function(i){
    system.time(rvonmises(i*diff,mu,nu,kappa1,kappa2,lambda))[3]-system.time(rvonmises2(i*diff,mu,nu,kappa1,kappa2,lambda))[3]
  })
  plot(1:ntests,test,type="l",xlab="Number of samples x 100", ylab="Difference between rvonmises.R and rvonmises.c")
  test
}
race(1,2,1,2,3,10,100)
# We can see the speed gain is approximately linear with n.

