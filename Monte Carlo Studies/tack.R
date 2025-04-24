tacks<-function(p,s=50)
{
  phat<-mean(sample(2,s,prob=c(p,1-p),replace=TRUE)==1)
  ci<-phat+c(1,-1)*1.96*sqrt(phat*(1-phat)/s)
  (p<=ci[1]& p>=ci[2])
}

# mean(sapply(rep(p=.4,1000),tacks)==FALSE)
# For some reason this is using rep_int instead of rep. Stupid R.

nreps<-10000
R<-numeric(nreps)
for(i in 1:nreps)
{
  R[i]<-tacks(.4,s=50)
}
ghat<-mean(R)
cig<-ghat+c(-1,1)*1.96*sqrt((ghat*(1-ghat))/nreps)
cat(paste("We conducted a Monte Carlo Study testing the coverage of the
           confidence interval. We found that the confidence interval
           contained the true mean of 0.4 about",ghat,"percent of the time.
           We are 95% confident that the true coverage for our confidence 
           interval procedure is within",sep=" "),paste0("(",cig[1],",",cig[2],")"))

