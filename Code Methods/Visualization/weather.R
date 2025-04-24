#!/usr/bin/env
setwd("~/STAT624/homework/5")
weather<-read.csv("PVU.2015.csv")
attach(weather)

#Change data types
## Time Series
date<-1:length(weather$MST)
months<-c(0,31,28,31,30,31,30,31,31,30,31,30,31)
mnames<-c("Janary","Febuary","March","April","May",
          "June","July","August","September",
          "October","November","December")
days<-numeric(length(months)-1)
for( i in 1:(length(months)-1))
{
  days[i]<-sum(months[1:i+1])
}
ticks<-seq(0,100,by=10)
dtems<-parse(text=paste0(ticks,"*degree"))
tbox<-numeric(12)
sportz<-c(0,days)
for( i in 1:12)
{
  tbox[i]<-1/2*(sportz[i+1]-sportz[i])+sportz[i]
}
weather$MST<-strftime(weather$MST,format="%Y-%m-%d")
maxes<-weather[Max.TemperatureF==max(weather$Max.TemperatureF),][,c(1,2)]
mins<-weather[Min.TemperatureF==min(weather$Min.TemperatureF),][,c(1,4)]
max.temp<-median(which(weather$MST%in%maxes$MST))
min.temp<-median(which(weather$MST%in%mins$MST))

## Bar Plot
mo <- strftime(weather$MST, "%m")
yr <- strftime(weather$MST, "%Y")
rlpp<-aggregate(weather$PrecipitationIn~mo,weather,FUN=sum)
nmpp<-c(1.89,1.85,1.93,2.01,2.09,1.18,0.75,0.98,1.46,2.01,1.69,1.93)
pp<-data.frame(rlpp[,2],nmpp)
pp<-t(pp)
ptik<-seq(0,10,by=2)
labs<-rep_len(c("ACTUAL","NORMAL"),24)
anp<-19.77
## Humidity Series
dpems<-ticks

#Layout

# Annual Temperature Bar Plot
anm<-53.45
arl<-mean(Mean.TemperatureF)
ann<-c(arl,anm)
# Function to draw graph
overlay<-function(ori,width=80,height=30,a,b,names=c("2015","NORMAL","ANNUAL TEMPERATURE"))
{
  left<-ori[1]
  bottom<-ori[2]
  right<-left+width
  top<-bottom+height
  rect(ori[1],ori[2],right,top,
       col='white',border="black")
  
  rect(left+width/9,bottom+height/5,right-width/9,
       top-height/5,col="gray77",border="black")
  segments(c(left+width/9,left+width/9),
           c(bottom+2*height/5,bottom+3*height/5),
           c(right-width/9,right-width/9),
           c(bottom+2*height/5,bottom+3*height/5),col="black")
  
  AT<-a/75*(4/5)*height+bottom
  NM<-b/75*(4/5)*height+bottom
  
  rect(left+(2/9)*width,bottom+height/5,left+(4/9)*width,
       AT,col="white")
  rect(left+(5/9)*width,bottom+height/5,left+(7/9)*width,
       NM,col="white")
  rect(left+(5/9)*width,bottom+height/5,left+(7/9)*width,
       NM,col="black",density=50)
  
  ticks<-rep_len(c(0,25,50,75),8)
  
  text(c(rep(left+(1/18)*width,4),rep(right-(1/18)*width,4)),
         c(c(1,2,3,4)*(1/5)*height+bottom,c(1,2,3,4)*(1/5)*height+bottom),
       labels=parse(text=paste0(ticks,"*degree")),col="black",cex=1/2)
  
  text(c(left+3/9*width,left+6/9*width,left+1/2*width),
       c(rep(bottom+1/10*height,2),top-1/10*height),
       labels=names,cex=1/2,font=2)
}




pdf("weather.pdf",width=7,height=7)
layout(matrix(c(1,2,3),byrow=TRUE),height=c(3,1,1))

# Plot the Time Series
par(ask=F)
par(mar=c(.5,3,2,3))
plot(x=date,y=weather$Max.TemperatureF,type="n",
     xlim=c(0,365),ylim=c(0,115),xaxs="i",yaxs="i"
     ,xaxt="n",xlab="",ylab="",yaxt="n")
axis(2,at=ticks,tick=FALSE,dtems,las=2)
axis(4,at=ticks,tick=FALSE,dtems,las=2)
lim<-par("usr")
rect(lim[1], lim[3], lim[2], lim[4], border = "black", col = "gray")
rect(0,110,365,115,col="black")
text(tbox,112.5,toupper(mnames),col="white",cex=9/16)
abline(h=seq(0,100,by=10),v=days,col="gray48")
lines(x=date,y=weather$Max.TemperatureF)
lines(x=date,y=weather$Min.TemperatureF)
title(main=list("PROVO'S WEATHER FOR 2015",cex=2))
polygon(c(1:length(weather$MST),rev(1:length(weather$MST))),
        c(weather$Max.TemperatureF,
          rev(weather$Min.TemperatureF)),col="black")
max.n<-lowess(weather$Max.TemperatureF)
min.n<-lowess(weather$Min.TemperatureF)
lines(max.n$x,max.n$y,col="cyan",lwd=2)
lines(min.n$x,min.n$y,col="cyan",lwd=2)
#boxes
rect(325,5,355,11,col="white")
rect(300,5,325,11,col="black")

rect(175,100,205,106,col="white")
rect(150,100,175,106,col="black")

rect(50,5,90,15,col="black")
rect(310,65,350,75,col="black")

#text
text(313,8,labels="LOW",col="white",cex=1)
text(340,8,labels=expression(DEC~27~':' ~1*degree),
     cex=3/4)

text(163,103,labels="HIGH",col="white",cex=3/4)
text(190,103,labels=expression(JUL~27~':' ~98*degree),
     cex=3/4)

text(70,13,labels="LINE INDICATES",col="white",cex=3/4)
text(70,8,labels="NORMAL LOW",col="white",cex=3/4)

text(330,73,labels="LINE INDICATES",col="white",cex=3/4)
text(330,68,labels="NORMAL HIGH",col="white",cex=3/4)

#pointers
polygon(c(350,361,345),c(5,1,5),col="black")
polygon(c(155,178,160),c(100,98,100),col="black")

polygon(c(50,40,50),c(13,27.13819,15),col="black")
polygon(c(340,334,345),c(65,44.54771,65),col="black")

# Nested Plot
overlay(c(5,75),width=60,height=30,a=ann[1],b=ann[2])

# Bar Plot
par(mar=c(1,3,.5,3))
a<-0.5
b<-36.5
barplot(pp,space=c(0,1),beside=T,col=c("black","black"),
        xaxs="i",yaxs="i",yaxt="n",ylim=c(-1.5,10),
        xlim=c(a,b),density=c(1000,20))
lim2<-par("usr")
rect(a,0,b,10,border="black")
axis(2,at=ptik,tick=FALSE,las=2)
axis(4,at=ptik,tick=FALSE,las=2)
bar<-seq(a,b,by=(b-a)/12)
lbl<-seq(a,b,by=(b-a)/36)
lbl<-lbl[!(lbl %in% bar)]
rect(0,-1.5,b,0,col='white')
text(lbl,-1,labels=labs,col='black',cex=1/3)
abline(v=bar,h=ptik)

rect(15,8,22,10,col="white")
rect(15,4,22,8,col="black")
finals<-c(paste0("Normal Precipitation","................"
                 ,anp),
                 paste0("Total Precipitation for 2015",
                        "........",sum(PrecipitationIn)),
          "PRECIPITATION IN INCHES")
text(rep(18.5,2),c(5,7),labels=finals[1:2],col="white",cex=1/2)
text(18.5,9,labels=finals[3],col="black",cex=3/4,font=2)
# Humidity Plot
plot(date,y=Mean.Humidity,type="l", xlim=c(0,365),
     ylim=c(0,100),
     xaxs="i",yaxs="i",xaxt="n",yaxt="n",ylab="",xlab="")
axis(2,at=ticks,tick=FALSE,dpems,las=2)
axis(4,at=ticks,tick=FALSE,dpems,las=2)
polygon(c(date,365,0),c(Mean.Humidity,0,0),col="dodgerblue")
abline(h=seq(0,100,by=10),v=days)
rect(151,0,212,15,col="white")
text(365/2,8,labels="AVERAGE HUMIDITY",cex=3/4,font=2)
text(10,95,label="Percent",cex=3/4)


dev.off()
