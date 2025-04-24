# This is the R version of my simulation study eli
# the last resort should I not be able to make it.
source("functions.R")
library(xtable)
simulation<-function(nreps)
{
  rows<-c("1/2","2","4")
  rrws<-c(1/2,2,4)

  columns<-c("-2","1/5","4") 
  clms<-c(-2, 0.2,4 ) 

  bys1cs<-matrix( c(1/8,1/4,
                    1/4,2,
                    1/2,4),   2,3)

  bys2cs<-matrix( c(1/4, 1/2,
                    1/4, 1/2,
                    1/2, 1),  2,3)

  size<-c(20,50)

  test<-matrix(,108,nreps)

  counter<-1

  
 for(n in 1:2)
 { 
    for(k in 1:length(columns))
    {
    for(a in 1:length(rows))
    {
        for(o in 1:nreps){
          test.data<-logistic(rnorm(size[n],clms[k],rrws[a]))
          test[counter,o]<-mean.mle(test.data)
          test[counter+1,o]<-shape.mle(test.data,
                                       test[counter,o])

          draws <-bayes.sampler1(0,1,bys1cs[1,a],bys1cs[2,a],
                              test.data,0,2,1)

          draws2 <-bayes.sampler2(0,1,bys2cs[1,a],bys2cs[2,a],
                                test.data,0,2,40,80)

          test[counter+2,o]<-mean(draws[,1])
          test[counter+3,o]<-mean(draws[,2])
          test[counter+4,o]<-mean(draws2[,1])
          test[counter+5,o]<-mean(draws2[,2])
        }
          counter<-counter+6
      }
    }
  }

  test  
}

N<-1000
system.time(simul<-simulation(N))
save.image("simulation.RData")
load("simulation.RData")

estimates<-apply(simul,1,mean)
ME<-apply(simul,1,function(i) qnorm(0.975)*sd(i)/sqrt(N))
lower<-estimates-ME
upper<-estimates+ME
tables<-cbind(estimates,lower,upper)
xindex<-rep_len(c("MLE: Mu","MLE: S2","B1: Mu","B1: S2", "B2: Mu","B2: S2"),108)
rownames(tables)<-xindex
colnames(tables)<-c("Estimate","Lower Bound","Upper Bound")
publish<-list(18)
for(i in 1:18){
  publish[[i]]<-tables[((i-1)*6+1):((i-1)*6+6),]
} 
                     
# This will produce 18 tables to be included in the
# appendix

rws<-c(1/2,2,4)

cls<-c(-2, 0.2,4 ) 

l<-1
for(k in 1:2){
  for(j in 1:3){
    for(i in 1:3){
      names[l]<-paste0("N=",n[k],", ",expression(sigma^2),"=",
                        rrws[i],", ",expression(mu),"=",cls[j])
      l<-l+1
     }
  }
}  
printout<-function(publish,names){
  for(i in 1:18){
    xtable(publish[[i]],caption=names[i])
  }
}
i<-i+1 
printout(publish,names)
organize<-function(estimates)
{
  bias<-numeric(108)
  indexes<-numeric()  

  counter<-1
  for(n in 1:2)
  { 
     for(k in 1:length(cls))
     {
       for(a in 1:length(rws))
       {
           bias[counter]<-estimates[counter]-cls[k]
           bias[counter+1]<-estimates[counter+1]-rws[a]
           bias[counter+2]<-estimates[counter+2]-cls[k]
           bias[counter+3]<-estimates[counter+3]-rws[a]
           bias[counter+4]<-estimates[counter+4]-cls[k]
           bias[counter+5]<-estimates[counter+5]-rws[a]
           
           indexes<-c(indexes,((n-1)*54+(a-1)*18+(k-1)*6)+c(1,3,5))
           
           counter<-counter+6
       }
    } 
 }
 list(bias,indexes)
}

bias<-organize(estimates)[[1]]
indexes<-organize(estimates)[[2]]
Sbias<-matrix(bias[seq(2,108,by=2)],3,18)
Mbias<-matrix(bias[indexes],3,18)
SMSE<-Sbias^2
MMSE<-Mbias^2

treatSbias<-    c("biasN20Mu2", "biasN20Mu15", "biasN20Mu4",
                  "biasN50Mu2", "biasN50Mu15", "biasN50Mu4")

treatSMSE<-    c("MSEN20Mu2", "MSEN20Mu15", "MSEN20Mu4",
                 "MSEN50Mu2", "MSEN50Mu15", "MSEN50Mu4")

treatSn<-   c(expression(paste("N=20, ",mu,"=-2")), 
              expression(paste("N=20, ",mu,"=1/5")),
              expression(paste("N=20, ",mu,"=4")),
              expression(paste("N=50, ",mu,"=-2")),
              expression(paste("N=50, ",mu,"=1/5")),
              expression(paste("N=50, ",mu,"=4")))

treatMbias<-   c("biasN20Sigma2", "biasN20Sigma1", "biasN20Sigma4",
                 "biasN50Sigma2", "biasN50Sigma1", "biasN50Sigma4")

treatMMSE<-    c("MSEN20Sigma2", "MSEN20Sigma1", "MSEN20Sigma4",
                 "MSEN50Sigma2", "MSEN50Sigma1", "MSEN50Sigma4")


treatMn<-   c(expression(paste("N=20, ",sigma,"=1/2")), 
              expression(paste("N=20, ",sigma,"=2")),
              expression(paste("N=20, ",sigma,"=4")),
              expression(paste("N=50, ",sigma,"=1/2")),
              expression(paste("N=50, ",sigma,"=1")),
              expression(paste("N=50, ",sigma,"=4")))



plotter<-function(data,true.param,measure,treatments,titles,p.name,filename,a,b)
{
  pdf(paste0(filename,".pdf"),width=15,height=10)
  par(mfrow=c(2,3))
  for(i in 1:length(treatments))
  {
#    jpeg(paste0(treatments[i],".jpg"))
      tracker<-3*(i-1)+1
      plot(true.param,data[3,tracker:(tracker+2)],
           main=titles[i], xlab=p.name, ylab=measure,type="n",
           xlim=c(min(true.param), max(true.param)),
           ylim=c(a,b), pch=20) 

      grid(col="lightgray",lwd=3,lty=1)

      points(true.param,data[3,tracker:(tracker+2)],
           main=titles[i], xlab=p.name, ylab=measure,type="l",
           xlim=c(min(true.param),max(true.param)),
            pch=20,col="red") 


      points(true.param,data[2,tracker:(tracker+2)],
             col="blue", type="l")

      points(true.param,data[1,tracker:(tracker+2)],
             col="black", type="l")
      
      xmin <- par("usr")[1]
      xmax <- par("usr")[2]
      ymin <- par("usr")[3]
      ymax <- par("usr")[4]

      lgd <- legend(x=mean(c(xmin,xmax)), y = mean(c(ymin,ymax)),
                    c("MLE","Bayes 1", "Bayes 2"), pch=20,
                    col=c("black","blue", "red"),plot=FALSE)

      legend(x=xmax -2*lgd$rect$w, y = ymax - lgd$rect$h,
             c("MLE","Bayes 1", "Bayes 2"),pch=20,
             col=c("black","blue","red"),bg="white",plot=TRUE)

  }
  dev.off()
}

plotter(Sbias, rws,"Bias",treatSbias,treatSn,expression(sigma^2),"SBias",-2,2)
plotter(Mbias, cls,"Bias",treatMbias,treatMn,expression(mu),"Mbias",-0.2,0.2)
plotter(SMSE, rws, "MSE", treatSMSE, treatSn, expression(sigma^2),"SMSE",0,4)
plotter(MMSE, cls, "MSE", treatMMSE, treatMn, expression(mu),"MMSE",0,0.05)

pdf("Pplots1.pdf",height=5,width=10)
  par(mfrow=c(1,2))
  curve(dnorm(x,0,2),xlim=c(-4,4),xlab="",ylab="",main="N(0,2)",col="blue",lwd=2)
  curve(dexp(x,1/2),xlim=c(0,10),xlab="",ylab="",main="Exp(1/2)",col="blue",lwd=2)
dev.off()


pdf("Pplots2.pdf",height=5,width=10)
  par(mfrow=c(1,2))
  curve(dnorm(x,0,2),xlim=c(-4,4),xlab="",ylab="",main="N(0,2)",col="red",lwd=2)
  curve(dIG(x,40,80),xlim=c(0,10),xlab="",ylab="",main="IG(40,80)",col="red",lwd=2)
dev.off()

pdf("Examp.pdf",height=5,width=5)
  plot(milk,milk,type="n",xlim=c(0,1),ylim=c(0,3),bg="blue",
  xlab="",ylab="")
  cnrs<-par("usr")
  rect(cnrs[1],cnrs[3],cnrs[2],cnrs[4],col="cadetblue")
  curve(dltnorm(x,0,1/2),col="red",lwd=2,add=TRUE)
  curve(dltnorm(x,0,1),col="white",lwd=2,add=TRUE)
  curve(dltnorm(x,0,4),col="green",lwd=2,add=TRUE)

  curve(dltnorm(x,-1,1/2),col="blue",lwd=2,add=TRUE)
  curve(dltnorm(x,1/2,4),col="yellow",lwd=2,add=TRUE)
  abline(v=c(0,1),h=0,lwd=2)

  xmin<-cnrs[1]
  ymin<-cnrs[3]
  xmax<-cnrs[2]
  ymax<-cnrs[4]

  lgd <- legend(x=mean(c(xmin,xmax)),
                y = mean(c(ymin,ymax)),
                c("(0,1/2)","(0,1)", "(0,4)","(-1,1/2)"
                ,"(1/2,4)"), pch=20,
                col=c("red","white", "green","blue",
                "yellow"),plot=FALSE)

  legend(x=xmax -lgd$rect$w, y = ymax,
         c("(0,1/2)","(0,1)", "(0,4)","(-1,1/2)"
         ,"(1/2,4)"),,pch=20,
         col=c("red","white","green","blue",
         "yellow"),bg="white",plot=TRUE)
dev.off()



