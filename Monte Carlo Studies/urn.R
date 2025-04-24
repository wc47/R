new.urn <- function(n.balls=2,black.proportion=0.5) {
  c('black'=n.balls*black.proportion,
    'white'=n.balls*(1-black.proportion))
}

proportion <- function(urn) {
  urn['black']/sum(urn)
}

draw.ball <- function(urn) {
  prob.black <- proportion(urn)
  drawn.ball <- c('white','black')[1*(runif(1)<prob.black)+1]
  urn[drawn.ball] <- urn[drawn.ball] + 1
  urn
}

# Initial proportion of black balls
theta <- 0.5
n.balls.to.start <- 10
n.draws <- 100
precision <- 10000

doit <- function(precision=10000,theta=0.5,
                 n.balls.to.start=10,n.draws=100) {
  draws <- numeric(precision)
  for ( i in 1:precision ) {
    urn <- new.urn(n.balls.to.start,
                   black.proportion=theta)
    for ( j in 1:n.draws ) {
      urn <- draw.ball(urn)
    }
    draws[i] <- proportion(urn)
  }
  draws
}

draws <- doit(precision)

observed.test.statistic <- 0.75
p.value <- mean(draws>=observed.test.statistic)
ci <- p.value + c(-1,1)*qnorm(1-0.05/2)*sqrt(p.value*(1-p.value)/length(draws))                   # Which is right?  This one?
ci2 <- p.value + c(-1,1)*qnorm(1-0.05/2)*sd(draws>=observed.test.statistic)/sqrt(length(draws))   # Or this one?
cat("p-value is ",p.value,", with a 95% confidence interval of (",ci[1],",",ci[2],")\n",sep="")

hist(draws,freq=FALSE,main=expression(paste("Sampling Distribution of Test Statistic under ",H[0])),xlab="Test Statistic")
lines(density(draws))
abline(v=observed.test.statistic,lwd=3)
text(observed.test.statistic+0.02,1.5,adj=0,paste("Test Stat. =",observed.test.statistic))

# What is the rejection region?
rej<-quantile(draws,probs=c(1,0.95))
cat("The rejection region is any value less than",rej[1],"and greater than",rej[2],".",sep=" ")

rej<-quantile(draws,probs=c(1,0.95))
# How could you do a power calculation?
power<-function(theta,rej)
{  
  power<-doit(precision,theta=theta)
  ind<-c("two-sided","one-sided")
  out<-function(i)
  {
  p<-mean(power>rej[i])
  ci<-p+c(-1,1)*qnorm(.975)*sqrt((p*(1-p))/precision)
  m<-paste("The power of our",ind[i],"test given theta=",theta," is",p,
      ". We are 95% confident that the true power value lies"
      ,"between",ci[1],"and",ci[2],".",sep=" ")
  m
  }
  paste(out(1),out(2),sep="\n")
}
cat(power(.6,rej))
cat(power(.7,rej))
