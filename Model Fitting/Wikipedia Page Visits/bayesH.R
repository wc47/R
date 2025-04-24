# Metropolis Within Gibbs Sampler
library(coda); library(xtable)
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


# Create Table 1 and Introductory Plots -----------------------------------

numVars <- c("birth_year", "latitude", "longitude", "article_languages", "page_views",
             "average_views", "historical_popularity_index")
cutoff <- c(0, 2, 2, 0, 2, 0, 2)
numSum <- t(sapply(1:length(numVars), function(x){
  v <- data.matrix(occup[,numVars])[,x]
  c(paste0(round(mean(v, na.rm=TRUE), digits = cutoff[x]),
           " (", round(sd(v, na.rm=TRUE), digits = cutoff[x]), ")"),
    paste0(round(median(v, na.rm = TRUE), digits = cutoff[x])," (",
           round(IQR(v, na.rm = TRUE), digits = cutoff[x]), ")"))
}))

rownames(numSum) <- numVars
colnames(numSum) <- c("Mean (SD)", "Median (IQR)")
xtable(numSum)

# Find out proportion of Women
xtable(cbind(table(occup$sex), table(occup$sex)/nrow(occup)))
# Find out number of countries
length(unique(occup$country))
# Find out domain distribution
xtable(cbind(table(occup$domain), table(occup$domain)/nrow(occup)), digits =  2)
pdf("~/Desktop/651present/explore.pdf", height = 7, width =2)
pdf("explore.pdf", height = 2.5, width =7)
par(mfrow = c(1, 2))
hist(exp(occup$page_views), col = "dodgerblue", xlab = "Page Views", freq = FALSE, 
     main = "")
hist(occup$page_views, col = "dodgerblue", xlab = "Page Views (log)", freq = FALSE, 
     main = "")
dev.off()
# Hierarchical ------------------------------------------------------------

projGibbsH=function(n.iter, nburn, data, inits = rep(1, 14),
                    a = rep(1, 4), b = rep(0.3, 4),
                    c = rep(1, 4), d = rep(0.3,4)){
  No <- length(unique(data$occupation))
  Ni <- length(unique(data$industry))
  p <- as.character(unique(data$industry))
  groupList <- lapply(as.character(unique(data$industry)), function(x){
    tempGroup <- unique(data[data$industry == x,]$occupation)
    which(p %in% tempGroup)
  })
  X <- lapply(p, function(x){
    data[data$occupation == x,]$page_views
  })
  # p lambda coeffiecients, p gamma coefficients, 
  # p alpha coefficients, p beta coefficients, 
  # p kappa coefficients, p theta coefficients,
  # p muAlpha coefficients, p tauAlpha Coefficients, 
  # p muBeta coefficients, p tauBeta Coefficients, 
  # p muKappa coefficients, p tauKappa coefficients,
  # p muTheta coefficients, p tauTheta coefficients,
  nVec <- aggregate(count~occupation, sum, data=data)
  sumVec <- aggregate(page_views~occupation, sum, data = data)
  lambda <- matrix(0, nrow=n.iter, ncol=No); gamma <- matrix(0, nrow=n.iter, ncol=No)
  alpha <- matrix(0, nrow=n.iter, ncol=No); beta <- matrix(0, nrow=n.iter, ncol=No)
  kappa <- matrix(0, nrow=n.iter, ncol=No); theta <- matrix(0, nrow=n.iter, ncol=No)
  lambda[1,] <- inits[1]; gamma[1, ] <- inits[2]
  alpha[1,] <- inits[3]; beta[1, ] <- inits[4]
  theta[1,] <- inits[5]; kappa[1, ] <- inits[6]
  tune <- matrix(0.9, nrow = 3, ncol = No)
  tuneH <- matrix(0.9, nrow = 4, ncol = Ni)
  tuneUpdate=function(tune, draws){
    draws <- as.numeric(draws)
    tuneF <- tune
    check <- length(unique(draws))/length(draws)
    #cat(check, "\n")
    if(length(draws) < 100) return(tuneF)
    #if(tune >= 0.2 & tune <= 0.6) return(tune)
    if(check < 0.2) tuneF <- tune - (1-length(draws)/n.iter)
    if(check > 0.6) tuneF <- tune + (1-length(draws)/n.iter)
    if(tuneF < 0) tuneF <- 0.5
    tuneF
  }
  muAlpha <- matrix(0, nrow=n.iter, ncol=Ni); tauAlpha <- matrix(0, nrow=n.iter, ncol=Ni)
  muBeta <- matrix(0, nrow=n.iter, ncol=Ni); tauBeta <- matrix(0, nrow=n.iter, ncol=Ni)
  muKappa <- matrix(0, nrow=n.iter, ncol=Ni); tauKappa <- matrix(0, nrow=n.iter, ncol=Ni)
  muTheta <- matrix(1, nrow=n.iter, ncol=Ni); tauTheta <- matrix(0, nrow=n.iter, ncol=Ni)
  muAlpha[1, ] <- inits[7]; tauAlpha[1,] <- inits[8]
  muBeta[1, ] <- inits[9]; tauBeta[1,] <- inits[10]
  muTheta[1, ] <- inits[11]; tauTheta[1,] <- inits[12]
  muKappa[1, ] <- inits[13]; tauKappa[1,] <- inits[14]
  
  createHyperLike=function(tau, param, a, b, Nj){
    logLikeH=function(mu){
      
      -Nj*log(gamma(mu)) + mu*Nj*log(tau) + mu*sum(log(param)) + (a-1)*log(mu)
    }
    logLikeH
  }
  for(i in 2:n.iter){
    startInd <- max(1, i-100)
    for(k in 1:length(groupList)){
      possJ <- groupList[[k]]
      for(j in possJ){
        xj <- X[[j]]
        # Sample Lambda from a Gibbs sampler
        lambda[i,j] <- rgamma(1, alpha[i-1,j] + length(xj),
                              rate = beta[i-1,j] + sum(xj^gamma[i-1,j]))
        # Define the likelihood for gamma
        logLikeG=function(gamma){
          ((length(xj) + theta[i-1,j]-1)*log(gamma) -
             lambda[i,j]-sum(xj^gamma)+gamma*sum(log(xj)))
        }
        gamma[i,j] <- metroUpdate(gamma[i-1,j], logLikeG, tune[1,j])
        tune[1,j] <- tuneUpdate(tune[1,j], gamma[startInd:i,j])
        
        # Update Alpha
        logLikeA=function(alpha){
          (-log(gamma(alpha))+(muAlpha[i-1]-1)*log(alpha)+
             alpha*(log(beta[i-1,j])+log(lambda[i,j])-tauAlpha[i-1]))*
            (alpha>0)*(beta[i-1,j]>0)*(lambda[i,j]>0)
        }
        alpha[i,j] <- metroUpdate(alpha[i-1,j], logLikeA, tune[2,j])
        tune[2,j] <- tuneUpdate(tune[1,j], alpha[startInd:i,j])
        
        # Update Beta
        beta[i,j] <- rgamma(1, muBeta[i-1] + alpha[i-1,j], rate = tauBeta[i-1] + lambda[i,j])
        # Update Theta
        logLikeT=function(theta){
          (-log(gamma(theta))+(muTheta[i-1]-1)*log(theta)+
             theta*(log(kappa[i-1,j])+log(gamma[i,j])-tauTheta[i-1]))
        }
        theta[i,j] <- metroUpdate(theta[i-1,j], logLikeT, tune[3,j])
        tune[3,j] <- tuneUpdate(tune[1,j], theta[startInd:i,j])
        # Update Kappa
        kappa[i,j] <- rgamma(1, muKappa[i-1] + theta[i-1,j],
                             rate = tauKappa[i-1] + gamma[i-1,j])
      }
      #cat("Checkpoint H\n")
      Nj <- length(possJ)
      # Update muAlpha
      logLikeMuAlpha <- createHyperLike(tauAlpha[i-1, k], alpha[i, possJ],
                                        a[1], b[1], Nj)
      muAlpha[i, k] <- metroUpdate(muAlpha[i-1, k], logLikeMuAlpha, tuneH[1, k])
      tuneH[1,k] <- tuneUpdate(tuneH[1, k], muAlpha[startInd:i, k])
      #cat("Checkpoint Alpha\n")
      # Update muBeta
      logLikeMuBeta <- createHyperLike(tauBeta[i-1, k], beta[i, possJ], a[2], b[2],
                                       Nj)
      muBeta[i, k] <- metroUpdate(muBeta[i-1, k], logLikeMuBeta, tuneH[2, k])
      tuneH[2, k] <- tuneUpdate(tuneH[2, k], muBeta[startInd:i, k])
      #cat("Checkpoint Beta\n")
      # Update muTheta
      logLikeMuTheta <- createHyperLike(tauTheta[i-1, k], theta[i, possJ],
                                        a[3], b[3], Nj)
      muTheta[i, k] <- metroUpdate(muTheta[i-1, k], logLikeMuTheta, tuneH[3, k])
      tuneH[3, k] <- tuneUpdate(tuneH[3, k], muTheta[startInd:i, k])
      #cat("Checkpoint Theta\n")
      # Update muKappa
      logLikeMuKappa <- createHyperLike(tauKappa[i-1, k], kappa[i,possJ],
                                        a[4], b[4], Nj)
      muKappa[i, k] <- metroUpdate(muKappa[i-1, k], logLikeMuKappa, tuneH[4, k])
      tuneH[4, k] <- tuneUpdate(tuneH[4, k], muKappa[startInd:i, k])
      #cat("Checkpoint Kappa\n")
      # Update tau's
      tauAlpha[i, k] <- rgamma(1, muAlpha[i, k]*Nj+c[1],
                               rate = sum(alpha[i, possJ]) + d[1])
      if(tauAlpha[i,k] <= 0) tauAlpha[i,k] <- 0.0001
      tauBeta[i, k] <- rgamma(1, muBeta[i, k]*Nj+c[2],
                              rate = sum(beta[i, possJ]) + d[2])
      if(tauBeta[i,k] <= 0) tauBeta[i,k] <- 0.0001
      tauTheta[i, k] <- rgamma(1, muTheta[i, k]*Nj+c[3],
                               rate = sum(theta[i, possJ]) + d[3])
      if(tauTheta[i,k] <= 0) tauTheta[i,k] <- 0.0001
      tauKappa[i, k] <- rgamma(1, muKappa[i, k]*Nj+c[4],
                               rate = sum(kappa[i, possJ]) + d[4])
      if(tauKappa[i,k] <= 0) tauKappa[i,k] <- 0.0001
      #cat("Finished Group ", k, "\n")
    }
    #cat("Finished Iteration ", i, "\n")
  }
  
  samps <- cbind(lambda, gamma, alpha, beta, theta, kappa, muAlpha, muBeta,
                 muTheta, muKappa, tauAlpha, tauBeta, tauTheta, tauKappa)
  colnames(samps) <- c(paste0("lambda", 1:No),
                       paste0("gamma", 1:No),
                       paste0("alpha", 1:No),
                       paste0("beta", 1:No),
                       paste0("theta", 1:No),
                       paste0("kappa", 1:No),
                       paste0("muAlpha", 1:Ni),
                       paste0("muBeta", 1:Ni),
                       paste0("muTheta", 1:Ni),
                       paste0("muKappa", 1:Ni),
                       paste0("tauAlpha", 1:Ni),
                       paste0("tauBeta", 1:Ni),
                       paste0("tauTheta", 1:Ni),
                       paste0("tauKappa", 1:Ni))
  samps[-1:-nburn,]
}


system.time(chainsH <- projGibbsH(5000, 10, occup))

#write.csv(chainsH, file = "bayesChainsH.csv")
gDiag <- geweke.diag(as.mcmc(chainsH))
badH <- which(abs(gDiag$z) > 3)
par(mfrow = c(1,1))
hist(gDiag$z, freq = FALSE)
curve(dnorm(x), add = TRUE)
par(mfrow = c(3,2))
for(d in badH){
  plot(chainsH[,d], type = "l", main = colnames(chainsH)[d])
}
