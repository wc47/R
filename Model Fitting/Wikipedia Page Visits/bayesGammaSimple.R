# Metropolis Within Gibbs Sampler- Simpler Model
library(coda)
occup <- read.csv("../data/database.csv", header = TRUE)
occup$count <- 1
occup$page_views <- log(occup$page_views)
occup$industryID <- as.numeric(occup$industry)
occup$domainID <- as.numeric(occup$domain)

p <- 1:length(unique(occup$industryID))
groupList <- lapply(unique(occup$domainID), function(x){
  tempGroup <- unique(occup[occup$domainID == x,]$industryID)
  which(p %in% tempGroup)
})

metroUpdate=function (current, logLikelihood, tune){
  proposal <- rnorm(1, current, tune)
  if(proposal > 0){
    diff <- logLikelihood(proposal) - logLikelihood(current)
    if (diff > log(runif(1))){
      return(proposal)
    }
  }
  current
}

projGibbsSimple=function(n.iter = 200, nburn = 100, occup = occup, tune.init=0.1,
                         inits=c(13,1), parms= c(13, 1, 3, 3)){
  # Set Hyperparameter estimates.
  a <- parms[1]; b <- parms[2]; c <- parms[3]; d <- parms[4]
  # Set Data constants for convenience
  No <- length(unique(occup$industry))
  Ni <- length(unique(occup$domain))
  p <- 1:length(unique(occup$industryID))
  X <- lapply(p, function(x){
    occup[occup$industryID == x,]$page_views
  })
  # Set other parameter estimates
  alpha <- matrix(0, nrow=n.iter, ncol=No); beta <- matrix(0, nrow=n.iter, ncol=No)
  alpha[1, ] <- inits[1]; beta[1,] <- inits[2] 
  tune <- rep(tune.init, No)
  ## Actual MCMC algorithm
  for(i in 2:n.iter){
    for(k in 1:Ni){
      possJ <- groupList[[k]]
      for(j in possJ){
        xj <- X[[j]]
        Nk <- length(xj)
        # Sample beta from a Gibbs sampler
        astar <- alpha[k]*Nk + c
        bstar <- sum(xj) + d
        beta[i,j] <- rgamma(1, astar, 
                              rate = bstar)
        # Define the likelihood for alpha
        lg=function(xj, alpha, beta){
          -Nk*log(gamma(alpha)) + (alpha)*sum(log(xj)) + (a-1)*log(alpha) - b*alpha
        }
        cand <- rnorm(1,alpha[i-1,j],tune[j])
        alpha[i,j] <- alpha[i-1,j]
        if(cand > 0){
          laccept <- lg(xj,cand, beta[i,j]) - lg(xj,alpha[i,j], beta[i,j])
          if(laccept > log(runif(1,0,1))){
            alpha[i,j] <- cand
          }
        }
      }
    }
  }
  samps <- cbind(alpha, beta)
  colnames(samps) <- c(paste0("alpha", 1:No),
                       paste0("beta", 1:No))
  samps[-1:-nburn,]
}

chains <- lapply(1:1, function(m){
  projGibbsSimple(n.iter = 50000, nburn = 10000, occup = occup, tune.init = 0.4,
                          inits = runif(2, 0.01, 14))
})

#chainsActual <- chains[[1]]
chainsActual <- rbind(chains[[1]], chains[[2]], chains[[3]])
length(unique(chainsActual[,1]))/nrow(chainsActual)
length(unique(chainsActual[,28]))/nrow(chainsActual)
dev.off()

par(mfrow = c(2,2))
plot(chainsActual[,1], type = "l")
plot(density(chainsActual[,1]))
plot(chainsActual[,28], type = "l")
plot(density(chainsActual[,28]))

dev.off()
par(mfrow = c(3, 2))
for(m in 1:length(X)){
  ppDraws <- rgamma(nrow(chains), chains[,m], rate = chains[,27 + m])
  plot(density(ppDraws), main = p[m])
}

gDiag <- geweke.diag(as.mcmc(chains))
sims <- as.mcmc(chains)
bad <- which(abs(gDiag$z[-1:-88]) > 3) + 88
par(mfrow = c(3,2))
for(d in bad){
  plot(chains[,d], type = "l", main = colnames(chains)[d])
}
souv <- sample(ncol(chains), 6)
for(d in souv){
  plot(chains[,d], type = "l", main = colnames(chains)[d])
}

ppDraws <- rgamma(nrow(chains), chains[,1], chains[,28])


logLikelihood=function(parms){
  ind <- cumsum(rep(27, 6))
  alpha <- parms[1:ind[1]]; beta <- parms[(1+ind[1]):ind[2]]
  lambda <- parms[(1+ind[2]):ind[3]]; theta <- parms[(1+ind[3]):ind[4]]
  phi <- parms[(1+ind[4]):ind[5]]; psi <- parms[(1+ind[5]):ind[6]]
  sum(sapply(1:length(X), function(i){
    xj <- X[[i]]; Nk <- length(xj)
    (Nk*alpha[i])*log(beta[i])-Nk*log(gamma(alpha[i])) + (alpha[i]-1)*sum(log(xj))-beta[i]*sum(xj)
  } ))
}

logLikelihood4=function(parms){
  ind <- cumsum(rep(27, 6))
  alpha <- parms[1:ind[1]]; beta <- parms[(1+ind[1]):ind[2]]
  #lambda <- parms[(1+ind[2]):ind[3]]; theta <- parms[(1+ind[3]):ind[4]]
  #phi <- parms[(1+ind[4]):ind[5]]; psi <- parms[(1+ind[5]):ind[6]]
  sum(sapply(1:length(X), function(i){
    sum(dgamma(X[[i]], alpha[i], beta[i], log =TRUE))
  }))
}

# DIC
p <- as.character(unique(occup$industry))
X <- lapply(p, function(x){
  as.numeric(occup[occup$industry == x,]$page_views)
})
system.time(logLikelihood4(bayes.estimate))
profvis(logLikelihood(bayes.estimate))
bayes.estimate <- apply(chains, 2, mean)
p.dic <- 2*(logLikelihood4(bayes.estimate[1:54]) - mean(apply(chains,1,function(x) logLikelihood4(x[1:54]))) )
dic <- -2*logLikelihood(bayes.estimate) + 2*p.dic   # Smaller is better
# AIC
mle <- optim(bayes.estimate[1:54], function(par) -logLikelihood4(par),
             lower = 0.001, method = "L-BFGS-B")$par
mle <- MLE.est(occup)[1:54]
aic <- -2*logLikelihood4(mle) + 2*(ncol(chains) - 2)
# WAIC
y <- occup$page_views
ref <- occup$domain
ref <- sapply(ref, function(x) which(q == x))
p.waic1 <- 0
for ( i in 1:nrow(occup) ) {
  profvis(p.waic1 <- p.waic1 + 2*log(mean(apply(chains,1,
                                                function(m){
                                                  dgamma(y[i], m[ref[i]], m[ref[i]+27])
                                                }))))
  p.waic1 <- p.waic1 - 2*mean(apply(chains,1,function(m){ 
    dgamma(y[i], m[ref[i]], m[ref[i]+27], log = TRUE)
  })
  )
}
p.waic2 <- sum(sapply(1:nrow(e),function(i) var(apply(tauChains,1,function(m) dpois(y[i], m[i], log = TRUE)))))
waic <- -2*sum(sapply(1:nrow(e), function(i) log(mean(apply(tauChains,1,function(m) dpois(y[i], m[i])))))) + 2*p.waic2
waic

# LPML
lpml <- sum(-log(sapply(1:length(y), function(i){
  mean(apply(chains, 1, function(try){
    1/dgamma(y[i], try[ref[i]], try[ref[i]+27])
  }))
})))
