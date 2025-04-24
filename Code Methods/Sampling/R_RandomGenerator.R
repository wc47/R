rnorm<-function(n, mean=0,sd=1)
{
  stopifnot(n>0)
  mean<-rep_len(mean,n)
  sd<-rep_len(sd,n)
  u1<-runif(n,0,1)
  u2<-runif(n,0,1)
  X<-seq(1,n, by=1)
  X1<-X[X%%2==0]
  X2<-X[X%%2!=0]
  X2<-mean+sd*sqrt(-2*log(u1[as.numeric(length(X1)+1):n]))*sin(2*pi*u2[as.numeric(length(X1)+1):n])
  if(n>1)
  {
    X1<-mean+sd*sqrt(-2*log(u1[1:length(X1)]))*cos(2*pi*u2[1:length(X1)])
    Xf<-c(X1,X2)
  }
  else{Xf<-X2}
  Xf
}

rlnorm<-function(n, meanlog=0,sdlog=1)
{
  stopifnot(n>0)
  mean<-rep_len(meanlog,n)
  sd<-rep_len(sdlog,n)
  exp(rnorm(n,meanlog,sdlog))
}

rexp<-function(n, rate=1)
{
  stopifnot(which(rate<=0)==0)
  rate<-rep_len(rate,n)
  u<-runif(n,0,1)
  x<- -(log(u)/rate)
  x
}

rpois<-function(n,lambda)
{
  stopifnot(which(lambda<0)==0)
  L<-rep_len(lambda,n)
  X<-vector()
  for(j in 1:n)
  {
    i<-1
    u<-vector()
    while(prod(u)>=exp(-L[j]))
    {
      u[i]<-runif(1,0,1)
      i<-i+1
    }
    X[j]<-length(u)-1
  }
  X
}

rgamma<-function(n, shape, rate=1, scale=1/rate,log=FALSE)
{
  stopifnot(n>0 & which(shape<0)==0 & which(rate <0)==0)
  rate<-1/scale
  X<-numeric(n)
  shape<-rep_len(shape,n)
  rate<-rep_len(rate,n)
  small<-which(shape<1 & shape>=0)
  shape[small]<-shape[small]+1
  d<- shape-(1/3)
  c<- 1/sqrt(9*d)
  for( i in 1:n )
  {
    flag<-TRUE
    while(flag)
    {
      Z<-rnorm(1,0,1)
      if(Z>(-1/c[i]))
      {
        V<-(1+c[i]*Z)^3
        U<-runif(1,0,1)
        flag <- log(U)>(1/2)*Z^2+d[i]-d[i]*V+d[i]*log(V)
      }
    }
    X[i]<-d[i]*V/rate[i]
  }
  X[small]<-(X[small]*runif(length(small))^(1/(shape[small]-1)))
  if(log) {X<-log(X)}
  X
}

rchisq<-function(n, df, ncp=0)
{
  stopifnot(n>0 & df>0 & ncp ==0)
  df<-rep_len(df,n)
  X<-rgamma(n,df/2,1/2)
  X
}

rt<-function(n,df,ncp=0)
{
  stopifnot(n>0 & which(df<=0)==0 & ncp==0)
  df<-rep_len(df,n)
  Y<-rnorm(n,0,1)
  Z<-rchisq(n,df)
  X<-Y/sqrt(Z/df)
  X
}

rf<-function(n,df1,df2,ncp=0)
{
  stopifnot(n>0 & which(df1<=0)==0 & which(df2<=0)==0 & ncp == 0)
  df1<-rep_len(df1,n)
  df2<-rep_len(df2,n)
  Z<-rchisq(n,df1)
  W<-rchisq(n,df2)
  X<-(Z/df1)*(W/df2)
  X
}

rbeta<-function(n, alpha, beta,ncp=0)
{
  stopifnot(alpha >0 & beta > 0)
  Y<-rgamma(n,alpha,1)
  Z<-rgamma(n,beta,1)
  X<-Y/(Y+Z)
  X
}


rbinom<-function(n, size, prob)
{
  stopifnot(n>0 & size > 0 & prob > 0 & prob <= 1)
  size<-rep_len(size,n)
  prob<-rep_len(prob,n)
  for(i in 1:n)
  {
    U<-runif(size[i],0,1)
    W<-ifelse(U<prob[i],1,0)
    X[i]<-sum(W)
  }
  X
}

rnbinom<-function(n, size, prob=1/2, mu=NULL)
{
  stopifnot(n>0 & size > 0 & prob > 0 & prob <= 1)
  if(missing(mu)==FALSE)
  {prob<-size/(size+mu)}
  X<-numeric(n)
  for( i in 1:n)
  {X[i]<-sum(floor(log(runif(size,0,1))/log(1-prob)))}
  X
}
