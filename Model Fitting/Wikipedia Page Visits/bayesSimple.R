# Metropolis Within Gibbs Sampler- Simpler Model
library(coda)
occup <- read.csv("../data/database.csv", header = TRUE)
occup$count <- 1
occup$page_views <- log(occup$page_views)

p <- as.character(unique(occup$industry))
groupList <- lapply(as.character(unique(occup$domain)), function(x){
  tempGroup <- unique(occup[occup$domain == x,]$industry)
  which(p %in% tempGroup)
})
X <- lapply(p, function(x){
  occup[occup$industry == x,]$page_views
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

projGibbsSimple=function(n.iter = 200, nburn, occup = occup, tune.init = 1, 
                   parm.init = matrix(1, nrow = 8, ncol = 4),
                   upper.init = rbind(rep(1e-9, 27), rep(15, 27)),
                   tolerance = 100, adapt = FALSE){
  # Set Hyperparameter estimates.
  alpha <- parm.init[,1]
  beta <- parm.init[,2]
  theta <- parm.init[,3]
  kappa <- parm.init[,4]
  # Set Data constants for convenience
  No <- length(unique(occup$industry))
  Ni <- length(unique(occup$domain))
  p <- as.character(unique(occup$industry))
  groupList <- lapply(as.character(unique(occup$domain)), function(x){
    tempGroup <- unique(occup[occup$domain == x,]$industry)
    which(p %in% tempGroup)
  })
  X <- lapply(p, function(x){
    occup[occup$industry == x,]$page_views
  })
  # Set other parameter estimates
  lambda <- matrix(0, nrow=n.iter, ncol=No); gamma <- matrix(0, nrow=n.iter, ncol=No)
  lambda[1,] <- upper.init[1,]
  gamma[1,] <- upper.init[2,]
  # Initialize tuning parameters
  if(length(tune.init) == 1){
    tune <- rep(tune.init, No)
  } else {
    tune <- tune.init
  }
  # coverP <- rep(0.5, No)
  # coverN <- rep(0.5, No)
  # # Find out how much a set has changed
  # findCover=function(draws) length(unique(draws))/length(draws)
  # # Create function to adjust tuning paraemeters
  # tuneUpdate=function(tuneI, coverPt, coverNt){
  #   tuneF <- tuneI
  #   prop <- tuneI + rnorm(1, 0, 0.1)
  #   if(prop > 0){
  #   if(dbeta(coverNt,4,4,log=TRUE) - dbeta(coverPt,4,4,log = TRUE) > log(runif(1))){
  #     tuneF <- prop
  #     return(c(tuneF, coverNt))
  #   }
  #   }
  #   c(tuneF, mean(coverPt, coverNt))
  # }
  
  ## Actual MCMC algorithm
  for(i in 2:n.iter){
    startInd <- max(c(1, i-tolerance))
    for(k in 1:Ni){
      possJ <- groupList[[k]]
      for(j in c(1)){
        xj <- X[[j]]
        Nk <- length(xj)
        # Sample Lambda from a Gibbs sampler
        astar <- alpha[k] + Nk
        bstar <- 1/(sum(xj^gamma[i-1,j] + 1/beta[k]))
        lambda[i,j] <- rgamma(1, astar, 
                              scale = bstar)
        # # Define the likelihood for gamma
        # logLikeG=function(gamma){
        #   ((Nk+theta[k]-1)*log(gamma) + (gamma-1)*sum(log(xj)) - lambda[i,j]*sum(xj^gamma)-
        #     gamma/kappa[k])
        # }
        # gamma[i,j] <- metroUpdate(gamma[i-1,j], logLikeG, tune[j])
        # 
        
        cand <- rnorm(1,gamma[i-1,j],tune[j])
        gamma[i,j] <- gamma[i-1,j]
        if(cand >0){
          laccept <- lg(xj,lambda[i,j],cand) - lg(xj,lambda[i,j],gamma[i-1,j])
          if(laccept > log(runif(1,0,1))){
            gamma[i,j] <- cand
            acc <- acc + 1
          }
        }
        
        # if(i %% tolerance == 0 & adapt){
        #   coverP[k] <- coverN[k]
        #   coverN[k] <- findCover(gamma[startInd:i,j])
        #   updateTemp <- tuneUpdate(tuneI = tune[j], coverPt = coverP[k], coverNt = coverN[k])
        #   tune[j] <- updateTemp[1]; coverN[k] <- updateTemp[2]
        # }
      }
    }
  }
  samps <- cbind(lambda, gamma)
  colnames(samps) <- c(paste0("lambda", 1:No),
                       paste0("gamma", 1:No))
  list(samps[-1:-nburn,], tune, alpha, beta, theta, kappa)
}

parm.init <- cbind(rep(0.0001, 88), rep(0.0001, 88), rep(0.0001, 88),
                   rep(0.0001, 88))

sims <- projGibbsSimple(n.iter = 1000, nburn = 500, occup = occup,
                        tune.init = 0.04, parm.init = parm.init)
chains <- sims[[1]]
plot(chains[,1], type = "l")
plot(chains[,28], type = "l")

gDiag <- geweke.diag(as.mcmc(chains))
bad <- which(abs(gDiag$z[-1:-88]) > 3) + 88
par(mfrow = c(3,2))
for(d in bad){
  plot(chains[,d], type = "l", main = colnames(chains)[d])
}
souv <- sample(ncol(chains), 6)
for(d in souv){
  plot(chains[,d], type = "l", main = colnames(chains)[d])
}

tuneDef <- sims[[2]]
upperDef <- chains[nrow(chains),]
plot(tune[-1:-100,67], type = "l")
apply(tune[-1:-100,], 2, function(x){
  length(unique(x))/length(x)
})
chains <- sims[[1]]
apply(chains, 2, function(x){
  length(unique(x))/length(x)
})
gDiag <- geweke.diag(as.mcmc(chains))
bad <- which(abs(gDiag$z) > 3)
par(mfrow = c(1,1))
hist(gDiag$z, freq = FALSE)
curve(dnorm(x), add = TRUE, col = "red")
par(mfrow = c(3,2))
for(d in bad){
  plot(chains[,d], type = "l", main = colnames(chains)[d])
}
souv <- sample(ncol(chains), 6)
for(d in souv){
  plot(chains[,d], type = "l", main = colnames(chains)[d])
}
#write.csv(chains, file = "bayesChains1.csv")

# Posterior Predictive ----------------------------------------------------


p <- as.character(unique(occup$occupation))
groupList <- lapply(as.character(unique(occup$industry)), function(x){
  tempGroup <- unique(occup[occup$industry == x,]$occupation)
  which(p %in% tempGroup)
})
X <- lapply(p, function(x){
  occup[occup$occupation == x,]$page_views
})
for(m in 1:88){
  ppDraws <- rweibull(nrow(chains), chains[, 88+m], chains[,m]^(-chains[, 88 + m]^2))
  plot(density(ppDraws[ppDraws<59]))
  if(length(X[[m]]) > 1){
    dens0 <- density(X[[m]])
    lines(dens0$x, dens0$y, col = "red")
    for(i in 1:10){
    curve(dweibull(x, i, 1^(-i)), 0, 20)
    }
  }
}
