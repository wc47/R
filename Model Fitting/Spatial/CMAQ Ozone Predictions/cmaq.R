library(scatterplot3d)
library(ForImp)
library(MASS)
library(car)
library(fields)
library(maps)
library(LatticeKrig)
library(splines)
library(bestglm)
library(xtable)
library(nlme)

setwd("~/Documents/School/BYU 2016-2017/Winter 2017/536/Midterm")
CMAQ<-read.csv("CMAQ.csv",header=TRUE)
o3<-read.csv("Ozone.csv",header=TRUE)[,-1]
plocs<-read.csv("PredLocs.csv",header=TRUE)
colnames(o3)[3]<-"O3"
cmaq<-CMAQ[,-3]

##############
# EDA
##############

pdf("cmaq.pdf")
  quilt.plot(cmaq$Lon,cmaq$Lat,cmaq$CMAQ_O3,main=
               '')
  map('state',add=TRUE)
dev.off()

pdf("o3.pdf")
  quilt.plot(o3$Longitude,o3$Latitude,o3$O3,
             main='')
  map('state',add=TRUE)
dev.off()
  
pdf("o3locs.pdf",width=6,height=5)
  plot(cmaq$Longitude,cmaq$Latitude,pch=18,
       xlab="Longitude",ylab="Latitude",
       col='dodgerblue')
  map('state',add=TRUE)
  points(o3$Longitude,o3$Latitude,pch=18,col='red')
  legend('bottomright',c("CMAQ","O3"),col=c("dodgerblue","red"),pch=18)
dev.off()

pdf("predlocs.pdf",width=6,height=5)
  plot(plocs$Longitude,plocs$Latitude,col='red',pch=19,
       xlab="Longitude",ylab='Latitude',cex=0.05)
  map('state',add=TRUE)
dev.off()


# The CMAQ data is so dense this is useless.
plot(cmaq$Longitude,cmaq$Latitude,pch=18)
plot(cmaq$Longitude,cmaq$CMAQ_O3,pch=18)
plot(cmaq$Latitude,cmaq$CMAQ_O3,pch=18)

##############
# MVN Analysis -----------------------------------------
##############

merge.two<-function(side,CMAQ,O3){
  n<-dim(O3)[1]
  indexes<-rep(0,2*n)
  index2<-numeric()
  evens<-seq(2,2*n,by=2)
  chuse<-numeric()
  for( i in evens){
    temp<-which(CMAQ$Longitude >= O3$Longitude[i/2]- side &
            CMAQ$Longitude <= O3$Longitude[i/2]+ side
          & CMAQ$Latitude >= O3$Latitude[i/2] - side &
           CMAQ$Latitude <= O3$Latitude[i/2] + side )
    indexes[i-1]<-O3$O3[i/2]
    indexes[i]<-ifelse(length(temp)>0,CMAQ$CMAQ_O3[temp],NA)
    ifelse(length(temp)>0,index2<-c(index2,i/2),0)
    if(length(temp)>0){
      chuse<-c(chuse,i-1,i)
    }
  }
  list(indexes,chuse,index2)
}

merge.pred<-function(side,CMAQ,O3){
  n<-dim(O3)[1]
  indexes<-rep(0,2*n)
  index2<-numeric()
  evens<-seq(2,2*n,by=2)
  chuse<-numeric()
  for( i in evens){
    temp<-which(CMAQ$Longitude >= O3$Longitude[i/2]- side &
                  CMAQ$Longitude <= O3$Longitude[i/2]+ side
                & CMAQ$Latitude >= O3$Latitude[i/2] - side &
                  CMAQ$Latitude <= O3$Latitude[i/2] + side )
    indexes[i-1]<-NA
    indexes[i]<-ifelse(length(temp)>0,CMAQ$CMAQ_O3[temp],NA)
    ifelse(length(temp)>0,index2<-c(index2,i/2),0)
    if(length(temp)>0){
      chuse<-c(chuse,i-1,i)
    }
  }
  list(indexes,chuse,index2)
}

putout<-function(IND,O3){
  n<-dim(O3)[1]
  indexes<-IND[[1]]
  chuse<-IND[[2]]
  side.dist<-69*0.04
  max.dist<-sqrt(2*side.dist^2)
  max.dist
  evens<-seq(2,2*n,by=2)
  mean(!is.na(indexes[evens]))
  pos.temp<-as.matrix(O3[rep(1:n,each=2),1:2])
  full.t<-cbind(pos.temp,indexes)
  colnames(full.t)<-c("Lon","Lat","o3")
  rownames(full.t)<-rep(c("O3","CMAQ"),times=n)
  # cmaq.t<-cbind(cmaq[-indexes[!is.na(indexes)]],NA)
  # colnames(cmaq.t)<-c("Lon","Lat","cmaq","o3")
  full<-rbind(full.t[chuse,])
  list(full.t,full)
}

sampler<-function(M,all.obs,condense,sites,c.model){
  n<-length(condense[,1])
  se<-matrix(NA,nrow=4,ncol=M)
  r2<-numeric()
  alpha.vec<-numeric()
  nug.vec<- numeric()
  
  fit.out<-gls(O3~ Latitude + Longitude ,data=as.data.frame(sites),
               correlation=corExp(form=~Latitude+Longitude,nugget=TRUE),method='ML')
  alpha<-exp(unlist(fit.out$modelStruct)[1])
  nug <- plogis(unlist(fit.out$modelStruct)[2])
  
  orig.L<-dim(sites)[1]
  
  bhat <- matrix(c(mean(sites$O3),mean(c.model$CMAQ_O3)),ncol=1)
  Mu<-bhat
  sig2 <- fit.out$sigma^2
  X<-cbind(rep(c(1,0),times=orig.L),rep(c(0,1),times=orig.L))
  site.dist<-as.matrix(exp(-as.matrix(dist(sites[,1:2]))/alpha))
  rhoI<-cor(condense[seq(1,nrow(condense),by=2),3],condense[seq(2,nrow(condense),by=2),3])
  site.R <- (1-nug)*site.dist+nug*diag(orig.L)
  sigma<-matrix(c(1,rhoI,rhoI,1),nrow=2)
  var.Sigma<-kronecker(site.dist,sigma)
  miss.cmaq <- which(is.na(all.obs[,3]))
  
  trace<-matrix(nrow=length(miss.cmaq),ncol=M)
  
  pred.mm <- var.Sigma[miss.cmaq,-miss.cmaq]%*%solve(var.Sigma[-miss.cmaq,-miss.cmaq])
  var.mm <- var.Sigma[miss.cmaq,miss.cmaq]-var.Sigma[miss.cmaq,-miss.cmaq]%*%solve(var.Sigma[-miss.cmaq,-miss.cmaq])%*%var.Sigma[-miss.cmaq,miss.cmaq]
  
  for(m in 1:M){
    
    ## Get a Full Data set
    temp.data<-all.obs
    mu1given2 <- X[miss.cmaq,]%*%bhat+pred.mm%*%(all.obs[-miss.cmaq,3]-X[-miss.cmaq,]%*%bhat)
    var1given2 <- sig2*var.mm
    temp.data[miss.cmaq,3] <- mu1given2 + t(chol(var1given2))%*%rnorm(nrow(mu1given2))
    trace[,m]<-temp.data[miss.cmaq,3]
    evens<-seq(2,orig.L*2,by=2)
    temp.fit.data<-cbind(temp.data[evens,],temp.data[-evens,3])
    colnames(temp.fit.data)<-c("Lon","Lat","CMAQ","O3")
    # fit <- gls(temp.data[-evens,3]~ temp.data[-evens,1] + 
    #              temp.data[-evens,2] + temp.data[evens,3],
    #          correlation=corExp(form=~temp.data[-evens,1]+temp.data[-evens,2]),method='ML')
    
    ## Use full dataset to get new parameters
    fit <- gls(O3~ Lon + Lat + CMAQ,data=as.data.frame(temp.fit.data),
               correlation=corExp(form=~Lon+Lat,nugget=TRUE),method='ML')
    
    coef<-cbind(coef,fit$coefficients)
    sig2<-summary(fit)$sigma
    r2[m]<-sig2
    se[,m]<-summary(fit)$tTable[,2]
    
    bhat <- matrix(c(mean(temp.fit.data[,4]),mean(temp.fit.data[,3])),ncol=1)
    
    Mu<-cbind(Mu,bhat)
    alpha.vec[m]<- exp(unlist(fit$modelStruct)[1])
    nug.vec[m] <- plogis(unlist(fit$modelStruct)[2])
    
    ##Get predictions for new locations
    
  }
  list(trace,coef,se,Mu,r2,alpha.vec,nug.vec,temp.data)
}

extract<-function(courier){
  coefs<-matrix(unlist(courier[[2]][,-1]),nrow=2)
  muTrue<-apply(coefs,1,mean)
  
  M<-length(courier[[3]])
  thetam<-coefs
  Vb<-numeric()
  Vw<-apply(courier[[3]],1,function(i) mean(i^2))[1:2]
  for(i in 1:2){
    Vb[i]<-sum((thetam[i,]-muTrue[i])^2)/(M-1)
  }
  Vt<-Vw+Vb+Vb/M
  FMI<-(Vb+Vb/M)/Vt
  nu<-(M-1)/FMI^2
  
  
  SEpool<-qt(0.975,nu)*sqrt(Vt)
  results<-cbind(muTrue,muTrue-SEpool,muTrue+SEpool)
  colnames(results)<-c("Estimate","Lower","Upper")
  
  
  results
  list(results,muTrue,Vb,Vw,Vt,FMI,nu,SEpool)
}

############
# Impute the data
############
indexes.temp<-merge.two(0.04,CMAQ,o3)
dummy1<-putout(indexes.temp,o3)
full.t<-dummy1[[1]]
full<-dummy1[[2]]

system.time(courier<-sampler(2,full.t,full,o3,cmaq))
#load("skate")
#load("midnight")
load('morning')
# trace,coef,se,Mu,r2,alpha.vec,nug.vec,temp.data
results.1<-extract(courier)
xtable(results.1[[1]])
######################
## Model Justification
######################
pdf("converge.pdf")
  two<-sample(444,2)
  par(mfrow=c(1,2))
  plot(courier[[1]][two[1],],type='l',
       xlab="Iteration",ylab="CMAQ Value")
  plot(courier[[1]][two[2],],type='l',
       xlab="Iteration",ylab="CMAQ Value")
dev.off()

pdf("linear.pdf")
  evens<-seq(1,dim(full)[1],by=2)
  OZONE<-full[evens,3]
  CQ <-full[-evens,3]
  plot(OZONE,CQ,pch=18,xlab="Ozone",ylab="CMAQ Prediction")
dev.off()

pdf("hist.pdf")
evens<-seq(1,dim(full)[1],by=2)
OZONE<-full[evens,3]
CQ <-full[-evens,3]
par(mfrow=c(1,2))
hist(OZONE,col="steelblue",xlab='Ozone',freq=FALSE,main='')
curve(dnorm(x,mean(OZONE),sd(OZONE)),col='red',add=TRUE,lwd=2,lty=2)
hist(CQ,col="steelblue",xlab="CMAQ",freq=FALSE,main='')
curve(dnorm(x,mean(CQ),sd(CQ)),col='red',add=TRUE,lwd=2,lty=2)

dev.off()
###############
# Prediction
###############
dummy3<-merge.pred(0.04,cmaq,plocs)
#tem.1<-putout(dummy3,plocs)[[1]]
#mean(is.na(tem.1[,3]))
tem.1 <-as.matrix(cbind(plocs[rep(1:length(plocs[,1]),each=2),],NA))
site.locs<-rbind(o3[,1:2],plocs[,1:2])
colnames(tem.1)<-c("Lon","Lat","o3")
rownames(tem.1)<-rep(c("O3","CMAQ"),times=dim(plocs)[1])
full.pred<-rbind(full.t,tem.1)



  n<-length(full[,1])
  fit.out<-gls(O3~1 ,data=as.data.frame(o3),
               correlation=corExp(form=~Latitude+Longitude,nugget=TRUE),method='ML')
  alpha<-exp(unlist(fit.out$modelStruct)[1])
  nug <- plogis(unlist(fit.out$modelStruct)[2])
  
  orig.L<-dim(full.pred)[1]/2
  
  bhat <- matrix(c(mean(o3$O3),mean(cmaq$CMAQ_O3)),ncol=1)
  Mu<-bhat
  sig2 <- 2313.055
  X<-cbind(rep(c(1,0),times=orig.L),rep(c(0,1),times=orig.L))
  site.dist<-as.matrix(exp(-as.matrix(dist(site.locs))/alpha))
  rhoI<-cor(full[seq(1,nrow(full),by=2),3],full[seq(2,nrow(full),by=2),3])
  site.R <- (1-nug)*site.dist+nug*diag(orig.L)
  sigma<-matrix(c(1,rhoI,rhoI,1),nrow=2)
  var.Sigma<-kronecker(site.dist,sigma)
  miss.cmaq <- which(is.na(full.pred[,3]))
  
  pred.mm <- var.Sigma[miss.cmaq,-miss.cmaq]%*%solve(var.Sigma[-miss.cmaq,-miss.cmaq])
  var.mm <- var.Sigma[miss.cmaq,miss.cmaq]-var.Sigma[miss.cmaq,-miss.cmaq]%*%solve(var.Sigma[-miss.cmaq,-miss.cmaq])%*%var.Sigma[-miss.cmaq,miss.cmaq]

    temp.data<-full.pred
    mu1given2 <- X[miss.cmaq,]%*%bhat+pred.mm%*%(full.pred[-miss.cmaq,3]-X[-miss.cmaq,]%*%bhat)
    var1given2 <- sig2*var.mm
    
    se<-sqrt(diag(var1given2))
    CIu<-mu1given2 + qnorm(0.975) * se
    CIl<-mu1given2 - qnorm(0.975) * se
    select<-miss.cmaq[which(miss.cmaq > 1600 & miss.cmaq %% 2 ==1)]
    pred.locs<- temp.data[select,1:2]
    preds<-UPI<-LPI<-temp.data
    # 
    UPI[miss.cmaq,3]<- CIu
    LPI[miss.cmaq,3]<- CIl
    preds[miss.cmaq,3] <- mu1given2
    #preds[miss.cmaq,3] <- mu1given2 + t(chol(var1given2))%*%rnorm(nrow(mu1given2))
pdf("preds.pdf")
    quilt.plot(preds[select,1],preds[select,2],preds[select,3],
               main='',nx=53,ny=53)
    map('state',add=TRUE)
dev.off()
pdf("UPI.pdf")
    quilt.plot(UPI[select,1],UPI[select,2],UPI[select,3],
               main='',nx=53,ny=53)
    map('state',add=TRUE)
dev.off()
pdf("LPI.pdf")    
    quilt.plot(LPI[,1],LPI[,2],LPI[,3],
               main='',nx=53,ny=53)
    map('state',add=TRUE)
dev.off()

predictions(1,full.pred,full,o3,cmaq)

###############
# Cross-Validation Study
###############

# Fit a GLS model to get the initial
# estimates for phi
fit.out<-gls(O3~1 ,data=as.data.frame(o3),
             correlation=corExp(form=~Latitude+Longitude,nugget=TRUE),method='ML')

RPMSE<-numeric()
coverage<-numeric()
bias<-numeric()

for(jj in 1:3){
  #Sample the existing data for a test
  # set.
  test.obs <- sample(800,40)
  OZ<-o3[-test.obs,]
   indy.2<-merge.two(0.04,CMAQ,OZ)
   dummy2<-putout(indy.2,OZ)
   super.train<-dummy2[[2]]
   full.cv <- dummy2[[1]]
   site.locs<-rbind(OZ[,1:2],o3[test.obs,1:2])
  
  tem.2 <-as.matrix(cbind(o3[rep(test.obs,each=2),1:2],NA))
#  tem.2[,3]<-merge.pred(0.04,cmaq,o3[test.obs,1:2])[[1]]
  
  colnames(tem.2)<-c("Lon","Lat","o3")
  rownames(tem.2)<-rep(c("O3","CMAQ"),times=length(test.obs))
  full.cv<-rbind(full.cv,tem.2)
  
  n<-length(super.train[,1])

  #Set the range and nugget parameters
  alpha<-exp(unlist(fit.out$modelStruct)[1])
  nug <- plogis(unlist(fit.out$modelStruct)[2])
  
  orig.L<-dim(full.cv)[1]/2
# Set parameters for our imputation algorithm
  bhat <- matrix(c(mean(o3$O3),mean(cmaq$CMAQ_O3)),ncol=1)
  Mu<-bhat
  sig2 <- 2313.055#fit.out$sigma^2
  X<-cbind(rep(c(1,0),times=orig.L),rep(c(0,1),times=orig.L))
  
# Create the Variance Matrix
  site.dist<-as.matrix(exp(-as.matrix(dist(site.locs))/alpha))
  rhoI<-cor(super.train[seq(1,nrow(super.train),by=2),3],super.train[seq(2,nrow(super.train),by=2),3])
  site.R <- (1-nug)*site.dist+nug*diag(orig.L)
  sigma<-matrix(c(1,rhoI,rhoI,1),nrow=2)
  var.Sigma<-kronecker(site.dist,sigma)
  miss.cmaq <- which(is.na(full.cv[,3]))

# Calculate the mean and variance
  pred.mm <- var.Sigma[miss.cmaq,-miss.cmaq]%*%solve(var.Sigma[-miss.cmaq,-miss.cmaq])
  var.mm <- var.Sigma[miss.cmaq,miss.cmaq]-var.Sigma[miss.cmaq,-miss.cmaq]%*%solve(var.Sigma[-miss.cmaq,-miss.cmaq])%*%var.Sigma[-miss.cmaq,miss.cmaq]
  
  temp.data<-full.cv
  mu1given2 <- X[miss.cmaq,]%*%bhat+pred.mm%*%(full.cv[-miss.cmaq,3]-X[-miss.cmaq,]%*%bhat)
  var1given2 <- sig2*var.mm
  
# Make predictions
  preds<-UPI<-LPI<- temp.data
  preds[miss.cmaq,3]<-mu1given2
  se<-sqrt(diag(var1given2))
  PIu<-mu1given2 + qnorm(0.975) * se
  PIl<-mu1given2 - qnorm(0.975) * se
  UPI[miss.cmaq,3]<-PIu
  LPI[miss.cmaq,3]<-PIl

# Cross Validation Study
  consid.obs<-seq(1521,1600,by=2)
  test<-preds[consid.obs,3]
  mean((o3[test.obs,3] <= UPI[consid.obs,3] & o3[test.obs,3] >= LPI[consid.obs,3] ))
  
  
  
  
  bias.J<-test-o3[test.obs,3]
  bias<-c(bias,mean(test-o3[test.obs,3]))
  RPMSE<-c(RPMSE,sqrt(mean(bias.J^2)))
  coverage <-c(coverage,mean((o3[test.obs,3] <= UPI[consid.obs,3] & o3[test.obs,3] >= LPI[consid.obs,3] )))
}

CVres<-cbind(mean(coverage),mean(RPMSE),mean(bias))
colnames(CVres)<-c("Coverage","RPMSE","Bias")
CVres
xtable(CVres,caption="Cross-Validation Study Results")


