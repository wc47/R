#!/usr/bin/env Rscript
bisection<-function(f,x_0,x_1,epsilon=0.0001,max.n=10000)
{
  count<-1
  stopifnot(f(x_0)*f(x_1)<0 && x_0<x_1)
  while(abs(x_0-x_1)>epsilon)
  {
    if(f(x_0)==0)return(x_0)
    if(f(x_1)==0)return(x_1)
    if(count>=max.n)return("ERROR")
    x_2<-(x_0+x_1)/2
    if(f(x_2)*f(x_0)<0){x_1<-x_2} else{x_0<-x_2}
    count<-count+1
  }
(x_0+x_1)/2
}
