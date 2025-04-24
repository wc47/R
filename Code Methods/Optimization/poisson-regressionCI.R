# Suppose that data y_1,...y_n are independent and distributed
# Poisson(exp(beta * x_i)), for i=1,...,n.
#
# Implement the function below to make inference on beta.
#
# Function arguments:
#     * y is an integer vector.
#     * x is an numeric vector of the same length as y.
#     * method is one of the following character vectors:
#          - c("mle","bootstrap")
#          - c("mle","wilks")
#          - c("mle","test-inversion")
#     * alpha is the significance level for confidence interval
#
# At this point, the function only uses maximum likelihoood estimation for
# point estimation for beta, but it has three methods for constructing a
# 100(1-alpha)% confidence interval on beta.
#
# Function return value:  A numeric vector of length three with named elements
# giving the point estimate of, the lower bound for, and the upper bound for a
# confidence interval on the parameter.
#
# You are welcome to reuse previous code, but make this script self contained
# and independent (i.e., don't use the 'source' function).  Global functions
# are okay (perhaps even encouraged for the sake of readability), but don't
# define any global variables.
#

bisection<-function(f,x_0,x_1,epsilon=0.0001,max.n=10000){
  count<-1
  stopifnot(f(x_0)*f(x_1)<0 && x_0<x_1)
  while(abs(x_0-x_1)>epsilon){
    if(f(x_0)==0)return(x_0)
    if(f(x_1)==0)return(x_1)
    if(count>=max.n)return("ERROR")
    x_2<-(x_0+x_1)/2
    if(f(x_2)*f(x_0)<0){x_1<-x_2}else{x_0<-x_2}
    count<-count+1
    }
(x_0+x_1)/2
}

make.functions<-function(x,y){

log.like<-function(beta){
sum(beta*x*y-exp(beta*x)-log(factorial(y)))
}

d1.loglike<-function(beta){
sum(x*y-x*exp(beta*x))
}

d2.loglike<-function(beta){
ret<-sum(-x^2*exp(beta*x))
ret
}

list(log.like,d1.loglike,d2.loglike)
}

newton.raphson<-function(funcs,theta0,epsilon=0.01,speak=FALSE){
f0<-funcs[[1]]
f1<-funcs[[2]]
f2<-funcs[[3]]
i<-0
theta<-theta0
B.hat<-theta0
  while(abs(f1(theta))>epsilon){
   i<-i+1
   B.hat<-theta-f1(theta)/f2(theta)
   theta<-B.hat
  }
if(speak)cat(paste0("It took",i,"iterations to converge."))
B.hat
}


bootstrap<-function(x,y,alpha=0.05,B=10000){
 b<-numeric(B) 
 length<-length(x)
 data<-data.frame(x,y)
  for(i in 1:B){
    ides<-sample(1:length,replace=TRUE)
    plugin<-data[ides,]
    init.guess<-log(mean(plugin$y))/mean(plugin$x)
    mle.boot<-make.functions(plugin$x,plugin$y)
    b[i]<-newton.raphson(mle.boot,init.guess)
  }
  c(quantile(b,prob=c(alpha/2,1-alpha/2)))
}

wilks<-function(x,y,mle.hat,alpha=0.05,funcs,B=10000){
  upper.guess<-log(max(y))/min(x)
  lower.guess<-log(min(y+0.01))/max(x)
  guess<-mean(c(lower.guess,upper.guess))

f<-function(theta){
  funcs[[1]](mle.hat)-funcs[[1]](theta)-0.5*qchisq(1-alpha,1)
}
lower<-bisection(f,lower.guess,mle.hat)
upper<-bisection(f,mle.hat,upper.guess)
c(lower,upper)
}


TI<-function(x,y,grid,mle.hat,alpha=0.05,MCsamples=10000){
   n<-length(x)
   seed<-10
   library(parallel)
    cores<-detectCores()
    cluster<-makeCluster(cores)
    on.exit(stopCluster(cluster))
    clusterExport(cluster,c("x","n","y","grid","MCsamples",
                "make.functions","newton.raphson","mle.hat"), 
                 envir=environment() )
    clusterSetRNGStream(cluster,seed)
    contains<-parSapply(cluster,grid,function(m,alpha){
      sample.dist<-sapply(1:MCsamples,function(i){
       d<-rpois(n,exp(x*m))
       init.ges<-log(mean(d))/mean(x)
       newton.raphson(make.functions(x,d),init.ges)
       })
  critical.values<-quantile(sample.dist,prob=c(alpha/2,1-alpha/2))
  prod(critical.values-mle.hat)< 0
  }, alpha=alpha)

  result<-range(grid[contains])

  if(min(result) <= min(grid)) 
        {stop( "Make guesswidth larger")}
  if(max(result) >= max(grid))
        {stop( "Make guesswidth larger")}

  result
}

posreg <- function(y,x,method,alpha=0.05,
          guesswidth=1) {
 mle<-make.functions(x,y)
 init.ges<-log(mean(y))/mean(x)
 mle.hat<-newton.raphson(mle,init.ges)
  if(method[1]=="mle" & method[2]=="bootstrap"){
     ci<-bootstrap(x,y,alpha=alpha)
     giveB<-c(mle.hat,ci)
     names(giveB)<-c("Estimate","Lower","Upper")
     return(giveB)
    }
  if(method[1]=="mle" & method[2]=="wilks"){
      ciw<-wilks(x,y,mle.hat,alpha=alpha,mle)   
      give<-c(mle.hat,ciw)
      names(give)<-c("Estimate","Lower","Upper")
      return(give) 
    }
  if(method[1]=="mle" & method[2]=="test-inversion"){
      grid<-seq(mle.hat-guesswidth,mle.hat+guesswidth,by=0.01)
      ciI<-TI(x,y,grid,mle.hat,alpha=alpha)
      giveI<-c(mle.hat,ciI)
      names(giveI)<-c("Estimate","Lower","Upper")
      return(giveI)
    } else {cat("Invalid Method Vector\n")}
}

