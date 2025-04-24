# A function taking the following arguments:
#     log.density.target: a function with one argument x giving the log of the target density at x.
#     log.density.envelope: a function with one argument x giving the log of the envelope density at x.
#     sample.envelope: a function taking no arguments and returning a sample from the envelope distribution.
#     alpha: a numeric such that the density of envelope distribution divided by alpha is greater than
#        the density of the target distribution for all x.
#     sample.size: a numeric given the number of samples to draw.
# The function returns sample.size realizations from the target distribution.

rejection.sampler <- function(log.density.target,log.density.envelope,sample.envelope,alpha,sample.size) {
  cl<-length(sample.envelope())
  samps<-data.frame(nrow=sample.size,ncol=cl)  
  i<-1
  while(i<=sample.size){
    x<-sample.envelope()
    u<-log(runif(1))+log.density.envelope(x)-log(alpha)
    if(u<=log.density.target(x)){
       samps[i,]<-x
       i<-i+1
      } 
  }
  samps
}
