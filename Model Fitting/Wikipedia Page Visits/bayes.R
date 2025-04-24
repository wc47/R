# Metropolis Within Gibbs Sampler
library(coda)
occup <- read.csv("../data/database.csv", header = TRUE)
occup$count <- 1
occup$page_views <- log(occup$page_views)

metroUpdate=function (current, logLikelihood, tune){
  proposal <- current + rnorm(1,0, tune)
  #cat("Current: ", current, "\nProposal: ", proposal,"\n")
  if(proposal <= 0) proposal <- 0.001
  if (logLikelihood(proposal) - logLikelihood(current) > log(runif(1))){
    return(proposal)
  }
  current
}
projGibbs=function(n.iter, nburn, data, tune.init){
  No <- length(unique(data$occupation))
  Ni <- length(unique(data$industry))
  p <- as.character(unique(data$occupation))
  groupList <- lapply(as.character(unique(data$industry)), function(x){
    tempGroup <- unique(data[data$industry == x,]$occupation)
    which(p %in% tempGroup)
  })
  X <- lapply(p, function(x){
    data[data$occupation == x,]$page_views
  })
  nVec <- aggregate(count~occupation, sum, data=data)
  sumVec <- aggregate(page_views~occupation, sum, data = data)
  lambda <- matrix(1, nrow=n.iter, ncol=No); gamma <- matrix(1, nrow=n.iter, ncol=No)
  alpha <- matrix(1, nrow=n.iter, ncol=No); beta <- matrix(1, nrow=n.iter, ncol=No)
  kappa <- matrix(1, nrow=n.iter, ncol=No); theta <- matrix(1, nrow=n.iter, ncol=No)
  muAlpha <- tauAlpha <- muBeta <- tauBeta <- muKappa <- tauKappa <- muTheta <- tauTheta <- 1
  tune <- matrix(0.5, nrow = 3, ncol = No)
  tuneUpdate=function(tune, draws){
    tuneF <- tune
    if(length(draws) < 100) return(tuneF)
    check <- length(unique(draws))/length(draws)
    #if(tune >= 0.2 & tune <= 0.6) return(tune)
    if(check < 0.2) tuneF <- tune - (1-length(draws)/n.iter)
    if(check > 0.6) tuneF <- tune + (1-length(draws)/n.iter)
    if(tuneF <= 0) tuneF <- 1/2
    tuneF
  }
  for(i in 2:n.iter){
    startInd <- max(1, i-100)
    for(j in 1:No){
      xj <- X[[j]]
      # Sample Lambda from a Gibbs sampler
      lambda[i,j] <- rgamma(1, alpha[i-1,j] + length(xj), beta[i-1,j] + sum(xj^gamma[i-1,j]))
      # Define the likelihood for gamma
      logLikeG=function(gamma){
        ((length(xj) + theta[i-1,j]-1)*log(gamma) -
          lambda[i,j]-sum(xj^gamma)+gamma*sum(log(xj)))
      }
      gamma[i,j] <- metroUpdate(gamma[i-1,j], logLikeG, tune[1,j])
      tune[1,j] <- tuneUpdate(tune[1,j], gamma[startInd:i,j])
      logLikeA=function(alpha){
        (-log(gamma(alpha))+(muAlpha-1)*log(alpha)+
           alpha*(log(beta[i-1,j])+log(lambda[i,j])-tauAlpha))*(alpha>0)*(beta[i-1,j]>0)*(lambda[i,j]>0)
      }
      alpha[i,j] <- metroUpdate(alpha[i-1,j], logLikeA, 1)
      tune[2,j] <- tuneUpdate(tune[1,j], alpha[startInd:i,j])
      beta[i,j] <- rgamma(1, muBeta + alpha[i-1,j], tauBeta + lambda[i,j])
      logLikeT=function(theta){
        (-log(gamma(theta))+(muTheta-1)*log(theta)+
           theta*(log(kappa[i-1,j])+log(gamma[i,j])-tauTheta))
      }
      theta[i,j] <- metroUpdate(theta[i-1,j], logLikeT, 1)
      tune[3,j] <- tuneUpdate(tune[1,j], theta[startInd:i,j])
      kappa[i,j] <- rgamma(1, muKappa + theta[i-1,j], tauKappa + gamma[i-1,j])
    }
  }
  samps <- cbind(lambda, gamma, alpha, beta, theta, kappa)
  colnames(samps) <- c(paste0("lambda", 1:No),
                       paste0("gamma", 1:No),
                       paste0("alpha", 1:No),
                       paste0("beta", 1:No),
                       paste0("theta", 1:No),
                       paste0("kappa", 1:No))
  list(samps[-1:-nburn,], tune)
}

system.time(sims <- projGibbs(10000, 1000, occup))
chains <- sims[[1]]
gDiag <- geweke.diag(as.mcmc(chains))
bad <- which(abs(gDiag$z) > 3)
par(mfrow = c(1,1))
hist(gDiag$z, freq = FALSE)
curve(dnorm(x), add = TRUE)
par(mfrow = c(3,2))
for(d in bad){
  plot(chains[,d], type = "l", main = colnames(chains)[d])
}
souv <- sample(ncol(chains), 10)
for(d in souv){
  plot(chains[,d], type = "l", main = colnames(chains)[d])
}
#write.csv(chains, file = "bayesChains1.csv")

rm(sims)

system.time(sims2 <- projGibbs(50000, 10, occup))
chains2 <- sims2[[1]]
# gDiag <- geweke.diag(as.mcmc(chains))
# bad <- which(abs(gDiag$z) > 3)
# par(mfrow = c(1,1))
# hist(gDiag$z)
# par(mfrow = c(3,2))
# for(d in bad){
#   plot(chains[,d], type = "l", main = colnames(chains)[d])
# }
write.csv(chains2, file = "bayesChains2.csv")
