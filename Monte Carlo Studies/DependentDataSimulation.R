library(nlme)
generate<-function(cov.mod,sigma.squared=2.5,rho=0.8,tmt=1,diff=0.5){
  
  # Covariance matrix for correlations whithin subject
#  sigma.squared <- 2.5
#  rho <- 0.8
  times <- 1:4
  if(cov.mod=="AR"){
    vc1<-(sigma.squared * rho^abs(outer(times, times, "-")))
    }else if(cov.mod=="CS"){
    vc1<-(sigma.squared * diag(4)+rho*diag(4))
    }else if(cov.mod=="Sym"){
    vc1<-sigma.squared * diag(4)
    }else 
      {
        cat("INVALID INPUT")
        return(0)
      }
  # Cholesky factorization
  cvc1 <- chol(vc1)
  
  # Random samples
  resp <- NULL
  intercept <- 10
  trial <- c(0:3)
  for (i in 1:5) { # Treatment 1
    resp <- append(resp, t(cvc1) %*% rnorm(4, 0, 1) + intercept + tmt*trial)
  }
  for (i in 1:5) { # Treatment 2
    resp <- append(resp, t(cvc1) %*% rnorm(4, 0, 1) + intercept + (tmt+.5) * trial)
  }

  # Create data set
  tmt <- as.factor(rep(1:2, each = length(resp) / 2))
  subj <- rep(1:10, each = 4)
  trial <- rep(0:3, times = 10)
  dat <- data.frame(
    resp = resp,
    tmt = tmt,
    subj = subj,
    trial = trial
  )
  dat
}
  
test<-function(dat,truth){
  
  fit.ar1 <-
    gls(resp ~ -1 + tmt + tmt:trial,
        correlation = corAR1(form = ~ 1 | subj),
        data = dat,
        method = "ML")
  
  fit.cs <-
    gls(resp ~ -1 + tmt + tmt:trial,
        correlation = corCompSymm(form = ~ 1 | subj),
        data = dat,
        method = "ML")
  
  fit.symm <-
    gls(resp ~ -1 + tmt + tmt:trial,
        correlation = corSymm(form = ~ 1 | subj),
        data = dat,
        method = "ML")
  
  # fit.arma <-
  #   gls(resp ~ -1 + tmt + tmt:trial,
  #       correlation = corARMA(form = ~1|subj),
  #       data = dat,
  #       method = "ML")
  
  AIC<-c(AIC(fit.symm),AIC(fit.cs),AIC(fit.ar1))
  BIC<-c(BIC(fit.symm),BIC(fit.cs),BIC(fit.ar1))
  
  c(which(AIC==min(AIC))==truth,
     which(BIC==min(BIC))==truth)
}
simulate<-function(cov.mod,rho,diff,n.test=10000){
  results<-matrix(nrow=n.test,ncol=2)
  cov.mods<-c("Sym","CS","AR")
  if(!(cov.mod %in% cov.mods)) 
    {
      cat("INVALID INPUT\n")
      return(0)
    }
  truth<-which(cov.mods==cov.mod)
  for(i in 1:n.test){
    dat<-generate(cov.mod,rho=rho,diff=diff)
    itest<-test(dat,truth)
    results[i,]<-itest
  }
  colnames(results)<-c("AIC","BIC")
  apply(results,2,mean)
}

final<-function(n){
  Symm51<-simulate(cov.mod="Sym",rho=0.5,diff=1,n)
  Symm55<-simulate("Sym",rho=0.5,diff=5,n)
  
  CS35<-simulate("CS",1,5,n)
  CS31<-simulate("CS",1,1,n)
  CS85<-simulate("CS",4,5,n)
  CS81<-simulate("CS",4,1,n)
  
  AR35<-simulate("AR",.3,5,n)
  AR31<-simulate("AR",.3,1,n)
  AR85<-simulate("AR",.8,5,n)
  AR81<-simulate("AR",.8,1,n)
  
  list(Symm51,Symm55,CS35,CS31,CS85,CS81
       ,AR35,AR31,AR85,AR81)
}
final<-final(10)
save(final,file="635.sim")
