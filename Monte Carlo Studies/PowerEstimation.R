library(foreach)
library(doMC)
registerDoMC(detectCores())

paired.permutation.test <- function(x,y,nSamples=5000) {
  d <- cbind(x,y)
  test.statistic <- t.test(d[,1],d[,2],paired=TRUE)$statistic
  dist.of.test.statistic <- sapply(1:nSamples,function(i) {
    e <- t(apply(d,1,sample))
    t.test(e[,1],e[,2],paired=TRUE)$statistic
  })
  mean( abs(dist.of.test.statistic) >= abs(test.statistic) )
}

paired.t.test <- function(x,y) {
  t.test(x,y,paired=TRUE)$p.value
}

power <- function(sampler1,sampler2,test,alpha=0.05,nSamples=5000,nCores=detectCores(),seed=runif(1,-.Machine$integer.max,.Machine$integer.max)) {
  p.values <- foreach(1:nSamples, .combine=c) %dopar% test(sampler1(),sampler2())
  power <- mean(p.values < alpha)
  attr(power,"95% CI") <- power + c(-1,1)*sqrt(power*(1-power)/nSamples)
  power
}

mu.seq <- c(0,0.5,1.5)
n.seq  <- c(10,20,40)
dist.seq <- c("normal","skewed")
func.seq <- c(paired.permutation.test,paired.t.test)

sigma  <- 2
power.matrix <- array(NA,dim=c(length(n.seq),length(mu.seq),length(dist.seq),length(func.seq)))
dimnames(power.matrix) <- list(paste("n=",n.seq,sep=""),paste("mu=",mu.seq,sep=""),c("Normal","Skewed"),c("Permutation","Parametric"))

a1 <- 0.1
a2 <- 1
mean <- a1/(a1+a2)
sd <- sqrt(a1*a2/((a1+a2)^2*(a1+a2+1)))

nSamples <- 10000
for ( i in 1:length(n.seq) ) {
  cat("i=",i,"\n",sep="")
  for ( j in 1:length(mu.seq) ) {
    cat("j=",j,"\n",sep="")
    for ( k in 1:length(dist.seq) ) {
      cat("k=",k,"\n",sep="")
      if ( dist.seq[k] == "normal" ) {
        sampler1 <- function() rnorm(n.seq[i],mean=0,        sd=sigma)
        sampler2 <- function() rnorm(n.seq[i],mean=mu.seq[j],sd=sigma)
      } else if ( dist.seq[k] == "skewed" ) {
        sampler1 <- function() (rbeta(n.seq[i],a1,a2)-mean)/sd*sigma
        sampler2 <- function() (rbeta(n.seq[i],a1,a2)-mean)/sd*sigma + mu.seq[j]
      } else stop("Unrecognized 'dist' value.") 
      for ( l in 1:length(func.seq) ) {
        cat("l=",l,"\n",sep="")
        power.matrix[i,j,k,l] <- power(sampler1,sampler2,func.seq[[l]],nSamples=nSamples)
      }
    }
  }
}

for ( k in 1:length(dist.seq) ) {
  for ( l in 1:length(func.seq) ) {
    cat("-------------\n")
    cat(paste(dimnames(power.matrix)[[3]][k],dimnames(power.matrix)[[4]][l],sep=', '),'\n\n')
    me <- qnorm(0.975)*sqrt(power.matrix[,,k,l]*(1-power.matrix[,,k,l])/nSamples)
    cat('Monte Carlo estimates of power.\n')
    print(power.matrix[,,k,l])
    cat("\n")
    cat('Lower bounds of 95% confidence interval.\n')
    print(power.matrix[,,k,l] - me)
    cat("\n")
    cat('Upper bounds of 95% confidence interval.\n')
    print(power.matrix[,,k,l] + me)
    cat("\n")
  }
}

