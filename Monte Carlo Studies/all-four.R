
risk<-function(n,p=c(0.1,0.25,0.25,0.4))
{
R<-numeric(n)
  for(i in 1:n)
  {  
    x<-numeric()
    while(length(unique(x))<length(p))
    {
      c<-sample(length(p),1,replace=T,prob=p)
      x<-c(x,c)
    }
    R[i]<-length(x)
  }
R
}

prob.confint<-function(hat,nreps)
{
hat+c(-1,1)*1.96*sqrt((hat*(1-hat))/nreps)
}

conf.int<-function(obs)
{
  mean(obs) +c(-1,1)*1.96*sd(obs)/sqrt(length(obs))
}

collect<-function(nreps,p=c(0.1,0.25,0.25,0.4))
{
  stopifnot(sum(p)==1)
  ntoys<-length(p)
  equals<-risk(nreps,p=rep(1/ntoys,ntoys))
  casino<-risk(nreps,p=p)
  ehat<-mean(equals)
  chat<-mean(casino)
  epro<-mean(equals>14)
  cpro<-mean(casino>14)
  e.ci<-conf.int(equals)
  c.ci<-conf.int(casino)
  e.pci<-prob.confint(epro,nreps)
  c.pci<-prob.confint(cpro,nreps)
  cat(paste("We conducted a Monte Carlo study using",nreps,"repetitions. The mean number
          of boxes purchased under equal probability is",ehat,". The mean number
          of boxes purchased with unequal probabilities is",chat,".
          The proportion of consumers who will have to purchase more than 14 boxes to 
          complete the set under equal and unequal probability is",epro,"and"
          ,cpro,"respectively",e.sep=" "))
}
collect(10000,p=c(0.1,0.25,0.25,0.4))
