secant<-function(f,x_0,x_1,epsilon=0.0001,max.n=10000)
{
  stopifnot(x_0<x_1)
  x<-c(x_0,x_1)
  n<-1
  while(abs(x[n+1] - x[n]) > epsilon)
  {
  if(f(x[n])==0)return(x[n])
  if(f(x[n])-f(x[n-1])==0)return((x[n]-x[n-1])/2)
  n<-n+1
  x[n+1]<-x[n]-f(x[n])*(x[n]-x[n-1])/(f(x[n])-f(x[n-1]))
  }
  x[n]
}
