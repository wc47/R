# Frequentist Analysis
# MLE 
library(coda)
occup <- read.csv("../data/database.csv", header = TRUE)
occup$count <- 1
occup$page_views <- log(occup$page_views)

p <- as.character(unique(occup$industry))
X <- lapply(p, function(x){
  occup[occup$industry == x,]$page_views
})


ll.weibull<-function(dat,par){
  a=exp(par[1])
  b=exp(par[2])
  ll=-sum(dweibull(dat,a,scale=b,log=T))
}

mydweibull<-function(x,lam,bet){lam*bet*x^(bet-1)*exp(-lam*x^bet)}
ll.weibull=function(dat, par){
  a=exp(par[1])
  b=exp(par[2])
  Nk <- length(dat)
  -sum(sapply(dat, function(x) log(mydweibull(x, a, b))))
}

# The optimum values for lambda and beta are 
# 4.352717e-19 and 15.84415, respectively
weibullMLE.E=function(X){
  MLE <- sapply(1:length(X), function(j){
    a=0.4
    b=0.4
    par=c(a,b)
    dat=X[[j]]  

    weibull.optim<-optim(par=par,fn=ll.weibull,dat=dat)
    a0 <- exp(weibull.optim$par[1]); b0 <- exp(weibull.optim$par[2])
    gamma <- a0
    lambda <- exp(log(b0)/(-gamma))
    c(lambda, gamma)
})
 c(MLE, apply(MLE, 2, function(x) E(x[1]^(-x[2]), x[2])))
}
E=function(lambda, gamma){
  lambda*gamma(1+1/gamma)
}


bootWeib=function(n.iter){
  p <- as.character(unique(occup$industry))
  t(sapply(1:n.iter, function(i){
  bootInd <- sample(nrow(occup), nrow(occup), replace = TRUE)
  bootData <- occup[bootInd,]
  X <- lapply(p, function(x){
    bootData[bootData$industry == x,]$page_views
  })
  weibullMLE.E(X)
  }))
}

bootEst <- bootWeib(5)
tableF <- apply(bootEst, 2, quantile, c(0.025, 0.5, 0.975))
tableF <- t(tableF[,-1:-(27*2)])

p <- as.character(unique(occup$industry))
X <- lapply(p, function(x){
  occup[occup$industry == x,]$page_views
})
weibullMLE.E(X)


p <- as.character(rownames(tableF))
q <- as.character(unique(occup$domain))
X <- sapply(p, function(x){
  which(q == unique(occup[occup$industry == x,]$domain))
})
tableF <- tableF[order(X),]

refX <- lapply(1:length(table(X)), function(x){
  refX <- table(X)
  if(refX[x] == 1) return(x)
  seq(-0.4, 0.4, length.out = refX[x]) + x 
})
refX <- unlist(refX)
xx <- (seq(12,17, length.out = 1000))
pdf("~/Desktop/651present/CI.pdf", height = 6, width = 6)
par(mar = c(5,10,1,5))
plot(xx,seq(0,length(q)+1, length.out = 1000),type = "n",
     xlim=(range(xx)),yaxt="n", xaxt="n",
     xlab = "Mean log(Page Views)",
     ylab = "")
#axis(2, at = seq(1, length(q), by = 1), las=2)
axis(1, at = seq(12, 17, by = 0.5), las=2)
color3 <- c("red", "orange", "yellow","green", "blue", "purple", "black", "gray")
for(i in 1:nrow(tableF)){
    jitter <- runif(1, -1/2, 1/2)
    lines(c(tableF[i,1], tableF[i,3]),rep(refX[i], 2), 
          col = color3[X[i]],
          lwd = 2)
    lines(rep(tableF[i,1], 2), rep(refX[i], 2) + c(-1,1)*0.05,
          lwd = 2, col = color3[X[i]])
    lines(rep(tableF[i,3], 2), rep(refX[i], 2) + c(-1,1)*0.05,
          lwd = 2, col = color3[X[i]])
}
i <- 9
lines(c(tableF[i,1], tableF[i,3]),rep(refX[i], 2), 
      col = "tomato",
      lwd = 3)
lines(rep(tableF[i,1], 2), rep(refX[i], 2) + c(-1,1)*0.05,
      lwd = 3, col = "tomato")
lines(rep(tableF[i,3], 2), rep(refX[i], 2) + c(-1,1)*0.05,
      lwd = 3, col = "tomato")
axis(2, at = seq(1, 8, by = 1), las=2, labels=q)
dev.off()
# rownames(tableF) <- p
# lambda <- MLE[1,1]
# gamma <- MLE[2,1]
# Delta <- b0
# 
# myWeib=function(x, k, lambda){
#   (k*lambda)*(x^(k-1))*exp(-lambda*x^k)
# }
# 
# theirWeib=function(x, k, Delta){
#   (k/Delta)*(x/Delta)^(k-1)*exp(-(x/Delta)^k)
# }
# 
# 
# lambda2 <- exp(-log(Delta)/gamma)
# 
# curve(myWeib(x, gamma, lambda^(gamma*gamma)), 0,20)
# curve(theirWeib(x, gamma, lambda^(-gamma)), col = "green", add = TRUE, lwd = 2)
# curve(dweibull(x, gamma, lambda^(-gamma)), add = TRUE, col = "blue")
# curve(dweibull(x, a0, b0), add = TRUE, col = "red", lty = 2, lwd = 2)
# 
# 
# E(lambda^(-gamma), gamma)
# apply(MLE, 2, function(x){
#   E(x[1]^(-x[2]), x[2])
# })
# 
# 
# b*gamma(1+1/a)
# exp(weibull.optim$par[2])*gamma(1+1/(exp(weibull.optim$par[1])))
# parm <- log(weibull.optim$par)
# gamma <- parm[1]
# lambda <- log(parm[2])/(-gamma)
# var(rweibull(10000, shape = exp(exp(parm[1])), scale = exp(exp(parm[2]))))
# 
# X <- X[[1]]
# MLEs <- optim(par = rep(mean(X), 2), f1, method = "L-BFGS-B", lower = 1e-20)
# curve(dweibull(x, MLEs$par[2], MLEs$par[1]^(-MLEs$par[2])), 0, 50)
# curve(dweibull(x, MLEs$par[1], MLEs$par[2]), 0, 20)
# mean(rweibull(10000, shape = MLEs$par[2], scale = MLEs$par[1]^(-MLEs$par[2])))
# var(rweibull(10000, MLEs$par[2], MLEs$par[1]^(-MLEs$par[2])))
# curve(dweibull(x, MLEs$par[27 + 1], MLEs$par[1]^(-MLEs$par[27+1])))
