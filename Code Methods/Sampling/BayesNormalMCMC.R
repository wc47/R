# BayesNormalMCMC.R

ratio<-function(y,mu,sigma2){
  (-sum((y-mu)^2/(2*sigma2)))
}

MC5<-function(M,y,mu0,sigma0,cand.sig1,cand.sig2){
  draws<-matrix(nrow=M, ncol=2)
  draws[1,]<-c(mu0,sigma0)
  for(i in 2:M){
    mu.cand<-rnorm(1,draws[i-1,1],cand.sig1)
    r<-ratio(y,mu.cand,draws[i-1,2])-ratio(y,
                                           draws[i-1,1],draws[i-1,2])
    draws[i,1]<-draws[i-1,1]
    if(r >= log(runif(1))){
      draws[i,1]<-mu.cand
    }
    ## Second Part
    nu<-rnorm(1,0,cand.sig2)
    sigma.cand<-draws[i-1,2]*exp(nu)
    r2<-(-n/2)*log(sigma.cand)+ratio(y,draws[i,1],sigma.cand)-(
      (-n/2)*log(draws[i-1,2])+ratio(y,draws[i,1],draws[i-1,2]))
    draws[i,2]<-draws[i-1,2]
    if(r2 >= log(runif(1))){
      draws[i,2]<-sigma.cand
    }
  }
  draws
}