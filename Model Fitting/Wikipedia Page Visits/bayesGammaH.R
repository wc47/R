
# Metropolis Within Gibbs Sampler- Hierarchical Model

# Function Definition -----------------------------------------------------


library(coda); library(MASS); set.seed(2)
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

projGibbsH=function(n.iter = 200, nburn = 100, occup = occup, tune.init=0.4,
                         tuneH.init = 0.8,
                         inits, parms = c(13, 1, 2, 2, 3, 0.1, 3, 0.1)){
  # Set Hyperparameter estimates.
  a <- parms[1]; b <- parms[2]; c <- parms[3]; 
  d <- parms[4]; f <- parms[5]
  g <- parms[6]; q <- parms[7]; r <- parms[8]
  # Set Data constants for convenience
  No <- length(unique(occup$industry))
  Ni <- length(unique(occup$domain))
  p <- 1:length(unique(occup$industryID))
  X <- lapply(p, function(x){
    occup[occup$industryID == x,]$page_views
  })
  groupList <- lapply(unique(occup$domainID), function(x){
    tempGroup <- unique(occup[occup$domainID == x,]$industryID)
    which(p %in% tempGroup)
  })
  # Set other parameter estimates
  alpha <- matrix(0, nrow=n.iter, ncol=No); beta <- matrix(0, nrow=n.iter, ncol=No)
  lambda <- matrix(0, nrow=n.iter, ncol=Ni); theta <- matrix(0, nrow=n.iter, ncol=Ni)
  phi <- matrix(0, nrow=n.iter, ncol=Ni); psi <- matrix(0, nrow=n.iter, ncol=Ni)
  alpha[1, ] <- inits[1]; beta[1,] <- inits[2]
  lambda[1, ] <- inits[3]; theta[1,] <- inits[4]
  phi[1, ] <- inits[5]; psi[1,] <- inits[6]
  tune <- rep(tune.init, No)
  tuneH <- rep(tuneH.init, Ni)
  ## Actual MCMC algorithm
  for(i in 2:n.iter){
    for(k in 1:Ni){
      possJ <- groupList[[k]]
      for(j in possJ){
        xj <- X[[j]]
        Nk <- length(xj)
        # Sample beta from a Gibbs sampler
        astar <- alpha[i-1,j]*Nk + phi[i-1, k]
        bstar <- sum(xj) + psi[i, k]
        beta[i,j] <- rgamma(1, astar, 
                            rate = bstar)
        # Define the likelihood for alpha
        lgA=function(xj, alpha, beta){
          -Nk*log(gamma(alpha)) + (alpha)*sum(log(xj)) + (lambda[k]-1)*log(alpha) - theta[k]*alpha
        }
        cand <- rnorm(1,alpha[i-1,j],tune[j])
        alpha[i,j] <- alpha[i-1,j]
        if(cand > 0){
          laccept <- lgA(xj,cand, beta[i,j]) - lgA(xj,alpha[i,j], beta[i,j])
          if(laccept > log(runif(1,0,1))){
            alpha[i,j] <- cand
          }
        }
      }
      Nj <- length(possJ)
      # Sample theta from a Gibbs sampler
      cstar <- lambda[i-1, k]*Nj + c
      dstar <- sum(alpha[i, possJ]) + d
      theta[i,k] <- rgamma(1, cstar, rate = dstar)
      # Sample psi from a Gibbs sampler
      qstar <- theta[i-1, k]*Nj + q
      rstar <- sum(beta[i, possJ]) + r
      psi[i,k] <- rgamma(1, qstar, rate = rstar)
      # Define the likelihood for lambda
      lgL=function(alpha, lambda, theta){
        (-(lambda*Nj)*log(theta)-Nj*log(gamma(lambda)) + 
           (lambda-1)*sum(log(alpha))+(a-1)*log(lambda)-b*lambda)
      }
      cand <- rnorm(1,lambda[i-1,k],tuneH[k])
      lambda[i,k] <- lambda[i-1,k]
      if(cand > 0){
        laccept <- lgL(alpha[i, possJ], cand, theta[i,k]) - lgL(alpha[i,possJ],lambda[i,k], theta[i,k])
        if(laccept > log(runif(1,0,1))){
          lambda[i,k] <- cand
        }
      }
      # Define the likelihood for phi
      lgP=function(beta, phi, psi){
        Nj*phi*log(psi)-Nj*log(gamma(phi))+(phi-1)*sum(log(beta))+(f-1)*log(phi)-phi*g
      }
      cand <- rnorm(1,phi[i-1,k],tuneH[k])
      phi[i,k] <- phi[i-1,k]
      if(cand > 0){
        laccept <- lgP(beta[i, possJ],cand, psi[i,k]) - lgP(beta[i, possJ],phi[i,k], psi[i,k])
        if(laccept > log(runif(1,0,1))){
          phi[i,k] <- cand
        }
      }
    }
  }
  samps <- cbind(alpha, beta, lambda, theta, phi, psi)
  colnames(samps) <- c(paste0("alpha", 1:No),
                       paste0("beta", 1:No),
                       paste0("lambda", 1:Ni),
                       paste0("theta", 1:Ni),
                       paste0("phi", 1:Ni),
                       paste0("psi", 1:Ni))
  samps[-1:-nburn,]
}


# Get Chains --------------------------------------------------------------


chains <- lapply(1:3, function(m){ 
  projGibbsH(n.iter = 50000, nburn = 10000, occup = occup, tune.init = 0.4,
             tuneH.init=0.8, inits = runif(6, 0.01, 20), parms = c(13.81551,1,1,1,1,1,1,1))
})
chainsActual <- rbind(chains[[1]], chains[[2]], chains[[3]])
write.csv(chainsActual, file = "chainsNormal.csv")
# Our process takes roughly 5 minutes seconds
length(unique(chainsActual[,73]))/nrow(chainsActual)
dev.off()
par(mfrow = c(2,2))
plot(chainsActual[,1], type = "l")
plot(density(chainsActual[,1]))
plot(chainsActual[,28], type = "l")
plot(density(chainsActual[,28]))
dev.off()

# Plot Joint Posterior ----------------------------------------------------


library(RColorBrewer)
k <- 11
my.cols <- rev(brewer.pal(k, "RdYlBu"))
pullInd <- sample(1:nrow(chainsActual), 0.3*nrow(chainsActual))
plotAlphaBeta=function(pullInd1){
  plot(chainsActual[pullInd ,pullInd1[1]], chainsActual[pullInd, pullInd1[2]], cex = 0.5, pch = 18, col = "black",
       xlab = expression(alpha), ylab = expression(beta), xlim = c(11, 18.5), ylim = c(0.7, 1.3))
  z <- kde2d(chainsActual[pullInd, pullInd1[1]], chainsActual[pullInd, pullInd1[2]], n=50)
  contour(z, drawlabels=FALSE, nlevels=k, add=TRUE, col = my.cols)
}

plotPhiPsi=function(pullInd1){
  plot(chainsActual[pullInd, pullInd1[3]], chainsActual[pullInd, pullInd1[4]], cex = 0.5, pch = 18, col = "black",
       xlab = expression(phi), ylab = expression(psi), xlim = c(0, 5), ylim = c(0, 4))
  z <- kde2d(chainsActual[pullInd, pullInd1[3]], chainsActual[pullInd, pullInd1[4]], n=50)
  contour(z, drawlabels=FALSE, nlevels=k, add=TRUE, col = my.cols)
}

plotFour=function(industry){
  pdf(paste0("Lookie/",industry, ".pdf"))
  temp1 <- occup[occup$industry == industry,]
  inds1 <- c(unique(temp1$industryID), unique(temp1$domainID))
  pullInd1 <- which(colnames(chainsActual) %in% c(paste0(c("alpha", "beta"), inds1[1]),
                                                  paste0(c("phi", "psi"), inds1[2])))
  par(mfrow = c(1, 2))
  plotAlphaBeta(pullInd1)
  plotPhiPsi(pullInd1)
  mtext(industry, side = 3, line = -3, outer = TRUE)
  dev.off()
}
sapply(unique(occup$industry), plotFour)

temp1 <- occup[occup$industry == "Math",]
inds1 <- c(unique(temp1$industryID), unique(temp1$domainID))
pullInd1 <- which(colnames(chainsActual) %in% c(paste0(c("alpha", "beta"), inds1[1]),
                                                paste0(c("phi", "psi"), inds1[2])))
# Joint Distribution for alpha, beta for history
pdf("mathPost.pdf", height = 6, width = 6)
  plotAlphaBeta(pullInd1)
dev.off()
# Joint Distribution for lambda, theta for history
pdf("sciencePost.pdf", height = 6, width = 6)
  plotPhiPsi(pullInd1)
dev.off()

# industryID = 23 and domainID = 6
temp2 <- occup[occup$industry == "History",]
inds2 <- c(unique(temp2$industryID), unique(temp2$domainID))
pullInd2 <- which(colnames(chainsActual) %in% c(paste0(c("alpha", "beta"), inds2[1]),
                                                paste0(c("phi", "psi"), inds2[2])))

plotAlphaBeta=function(pullInd1){
  plot(chainsActual[pullInd ,pullInd1[1]], chainsActual[pullInd, pullInd1[2]], cex = 0.5, pch = 18, col = "black",
       xlab = expression(alpha), ylab = expression(beta), xlim = c(8, 15.5), ylim = c(0.5, 1.3))
  z <- kde2d(chainsActual[pullInd, pullInd1[1]], chainsActual[pullInd, pullInd1[2]], n=50)
  contour(z, drawlabels=FALSE, nlevels=k, add=TRUE, col = my.cols)
}
# Joint Distribution for alpha, beta for history
pdf("historyPost.pdf", height = 6, width = 6)
  plotAlphaBeta(pullInd2)
dev.off()
# Joint Distribution for lambda, theta for history
pdf("socialPost.pdf", height = 6, width = 6)
plotPhiPsi(pullInd2)
dev.off()


# Posterior Predictive Distribution ---------------------------------------

# Posterior Predictive Distribution for Math
mathDraw <- rgamma(nrow(chainsActual), chainsActual[,pullInd1[1]],
           chainsActual[,pullInd1[2]])
histDraw <- rgamma(nrow(chainsActual), chainsActual[,pullInd2[1]],
                   chainsActual[,pullInd2[2]])
temp3 <- occup[occup$industry == "Companions",]
inds3 <- c(unique(temp3$industryID), unique(temp3$domainID))
pullInd3 <- which(colnames(chainsActual) %in% c(paste0(c("alpha", "beta"), inds3[1]),
                                                paste0(c("phi", "psi"), inds3[2])))
outDraw <- rgamma(nrow(chainsActual), chainsActual[,pullInd3[1]],
                  chainsActual[,pullInd3[2]])
pdf("mathHistPP.pdf", width = 7, height = 3)
par(mfrow = c(1, 1))
plot(density(mathDraw), main = "",
     xlab = "log(Page Views)")
lines(density(histDraw), col = "red", lwd = 2)
lines(density(outDraw), col = "dodgerblue", lty = 2, lwd = 2)
legend("topright", c("Math", "History", "Companions"), lwd = c(1,2,2),
       col = c("black", "red", "dodgerblue"), lty =c(1,1,2))
dev.off()


# Posterior Predictive Mean Plot ------------------------------------------

# Lines Plot for posterior predictive means
pdf("linesBayesE.pdf", height = 3, width = 7)
tableB <- t(sapply(p, function(k){
  temp3 <- occup[occup$industryID == k,]
  inds3 <- c(unique(temp3$industryID), unique(temp3$domainID))
  pullIndIn <- which(colnames(chainsActual) %in% c(paste0(c("alpha", "beta"), inds3[1]),
                                                  paste0(c("phi", "psi"), inds3[2])))
  c(quantile(chainsActual[,pullIndIn[1]]/chainsActual[,pullIndIn[2]],
           c(0.025, 0.5, 0.975)), unique(temp3$domainID))
}))
X <- sapply(p, function(x){
  which(p == unique(occup[occup$industryID == x,]$domainID))
})
refX <- lapply(1:length(table(X)), function(x){
  refX <- table(X)
  if(refX[x] == 1) return(x)
  seq(-0.4, 0.4, length.out = refX[x]) + x 
})
tableB <- tableB[order(tableB[,4]),]
tableB <- cbind(tableB, unlist(refX))
xx <- (seq(11,18, length.out = 1000))
q <- sapply(1:length(unique(occup$domainID)), function(i){
  unique(occup[occup$domainID == i,]$domain)
})
indNames <- sapply(p, function(i){
  unique(occup[occup$industryID == i,]$industry)
})
rownames(tableB) <- indNames
par(mar = c(5,10,1,5))
plot(xx,seq(0,length(q)+1, length.out = 1000),type = "n",
     xlim=(range(xx)),yaxt="n", xaxt="n",
     xlab = "E(log(Page Views))",
     ylab = "")
#axis(2, at = seq(1, length(q), by = 1), las=2)
axis(1, at = seq(11, 18, by = 0.5), las=2)
color3 <- c("red", "orange", "yellow","green", "blue", "purple", "black", "gray")
for(i in 1:nrow(tableB)){
  lines(c(tableB[i,1], tableB[i,3]),rep(tableB[i, 5], 2), 
        col = color3[tableB[i, 4]],
        lwd = 2)
  lines(rep(tableB[i,1], 2), rep(tableB[i, 5], 2) + c(-1,1)*0.05,
        lwd = 2, col = color3[tableB[i,4]])
  lines(rep(tableB[i,3], 2), rep(tableB[i,5], 2) + c(-1,1)*0.05,
        lwd = 2, col = color3[tableB[i,4]])
}
axis(2, at = seq(1, 8, by = 1), las=2, labels=q)
dev.off()

# Table for Credible Intervals --------------------------------------------
credTable <- matrix(sapply(c("Math", "History", "Companions"),function(moniker){
  indNum <- which(indNames == moniker)
  pullNum <- which(colnames(chainsActual) %in% paste0(c("alpha","beta"), indNum))
  apply(chainsActual[,pullNum],2, quantile, c(0.5, 0.025, 0.975))
}), nrow = 3)
xtable(credTable, digits = 3)


# Mixing Chains Plots -----------------------------------------------------
# Too many points is overloading my Latex compiler, so we'll
# thin the data set

# Dr. Dahl, I know you hate global variables, but I'm running out of time.
underN <- 100
chooseInd <- seq(1, nrow(chains[[1]]), length.out = 1/underN*nrow(chains[[1]]))
# Multiple Chains mixing plot for alpha and beta
pdf("alphaBetaMix.pdf", width = 7, height = 3)
  par(mfrow = c(1, 2))
  plot(chains[[1]][chooseInd, 1], type = "l", ylab = expression(alpha))
  lines(1:(nrow(chains[[1]])/underN), chains[[2]][chooseInd,1], col = "indianred")
  lines(1:(nrow(chains[[1]])/underN), chains[[3]][chooseInd,1], col = "cyan")

  plot(chains[[1]][, 28], type = "l", ylab = expression(beta), col = "cadetblue")
  lines(1:(nrow(chains[[1]])/underN), chains[[2]][chooseInd,28], col = "purple")
  lines(1:(nrow(chains[[1]])/underN), chains[[3]][chooseInd,28], col = "orange")
dev.off()

# Multiple Chains mixing plot for other parameters
pdf("otherMix.pdf", width = 7, height = 3)
  par(mfrow = c(1, 4)) # 61, 69, 77, 85
  plot(chains[[1]][chooseInd, 61], type = "l", ylab = expression(lambda))
  lines(1:(nrow(chains[[1]])/underN), chains[[3]][chooseInd,61], col = "cyan")
  lines(1:(nrow(chains[[1]])/underN), chains[[2]][chooseInd,61], col = "indianred")

  plot(chains[[1]][chooseInd, 69], type = "l", ylab = expression(theta))
  lines(1:(nrow(chains[[1]])/underN), chains[[3]][chooseInd,69], col = "cyan")
  lines(1:(nrow(chains[[1]])/underN), chains[[2]][chooseInd,69], col = "indianred")

  plot(chains[[1]][chooseInd, 77], type = "l", ylab = expression(phi)) 
  lines(1:(nrow(chains[[1]])/underN), chains[[3]][chooseInd,77], col = "cyan")
  lines(1:(nrow(chains[[1]])/underN), chains[[2]][chooseInd,77], col = "indianred")

  plot(chains[[1]][chooseInd, 85], type = "l", ylab = expression(psi))
  lines(1:(nrow(chains[[1]])/underN), chains[[3]][chooseInd,85], col = "cyan")
  lines(1:(nrow(chains[[1]])/underN), chains[[2]][chooseInd,85], col = "indianred")
dev.off()

# Sensitivity Analysis ----------------------------------------------------

# # Heavy
# chainsH <- lapply(1:3, function(m){ 
#   projGibbsH(n.iter = 50000, nburn = 10000, occup = occup, tune.init = 0.4,
#              tuneH.init=0.8, inits = runif(6, 0.01, 20), parms = c(1381551,100000,1000,1000,1000,1000,1000,1000))
# })
# chainsHActual <- rbind(chainsH[[1]], chainsH[[2]], chainsH[[3]])
# write.csv(chainsHActual, file = "chainsHeavy.csv")
# 
# # Noninformative
# chainsD <- lapply(1:3, function(m){ 
#   projGibbsH(n.iter = 50000, nburn = 10000, occup = occup, tune.init = 0.4,
#              tuneH.init=0.8, inits = runif(6, 0.01, 20), parms = c(0,0,0,0,0,0,0,0))
# })
# chainsDActual <- rbind(chainsD[[1]], chainsD[[2]], chainsD[[3]])
# write.csv(chainsDActual, file = "chainsDiffuse.csv")
# 
# # Biased
# chainsB <- lapply(1:3, function(m){
#   projGibbsH(n.iter = 50000, nburn = 10000, occup = occup, tune.init = 0.4,
#              tuneH.init=0.005, inits = runif(6, 0.01, 20), parms = c(1000,14000,1,1,1,1,1,1))
# })
# chainsBActual <- rbind(chainsB[[1]], chainsB[[2]], chainsB[[3]])
# write.csv(chainsBActual, file = "chainsBiased.csv")
# # Hyper-parameters Biased
# chainsLiB <- lapply(1:3, function(m){
#   projGibbsH(n.iter = 50000, nburn = 10000, occup = occup, tune.init = 0.4,
#              tuneH.init=3, inits = runif(6, 0.01, 20), parms = c(14,1,1,1,10,1000,1000,10))
# })
#  # plot(chainsLiB[[1]][,58], type = "l")
#  # length(unique(chainsLiB[[1]][,58]))/(nrow(chainsLiB[[1]]))
# # beep("mario")
# chainsLiBActual <- rbind(chainsLiB[[1]], chainsLiB[[2]], chainsLiB[[3]])
# write.csv(chainsLiBActual, file = "chainsLightBiased.csv")
# Both Biased
# chainsBB <- lapply(1:3, function(m){
#   projGibbsH(n.iter = 50000, nburn = 10000, occup = occup, tune.init = 0.6,
#              tuneH.init=0.005, inits = runif(6, 0.01, 20), parms = c(1000,14000,10,1000,10,1000,10,1000))
# })
# # plot(chainsBB[[1]][,58], type = "l")
# # length(unique(chainsBB[[1]][,58]))/(nrow(chainsBB[[1]]))
# # beep("mario")
# chainsBBActual <- rbind(chainsBB[[1]], chainsBB[[2]], chainsBB[[3]])
# write.csv(chainsBBActual, file = "chainsBothBiased.csv")
# Each set of three takes about 6 minutes
chainsHActual <- read.csv("chainsHeavy.csv", header = TRUE)
chainsDActual <- read.csv("chainsDiffuse.csv", header = TRUE)
chainsBActual <- read.csv("chainsBiased.csv", header = TRUE)
chainsLiBActual <- read.csv("chainsLightBiased.csv", header = TRUE)
chainsBBActual <- read.csv("chainsBothBiased.csv", header = TRUE)

# Sensitivity Plot --------------------------------------------------------
chooseInd <- sample(nrow(chainsActual), 0.4*nrow(chainsActual))
pdf("sens.pdf", width = 7, height = 3)
  par(mfrow = c(1,1))
  plot(density(chainsActual[chooseInd, 5]), xlab = expression(theta), 
       lty = 3, lwd = 3, main = "")
  lines(density(chainsHActual[chooseInd,5]), col = "orange",
        lwd = 2)
  lines(density(chainsDActual[chooseInd,5]), col = "red", lty = 2,
        lwd = 2)
  lines(density(chainsBActual[chooseInd,5]), col = "dodgerblue2")
  lines(density(chainsLiBActual[chooseInd, 5]), col = "forestgreen")
  lines(density(chainsBBActual[chooseInd, 5]), col = "purple")
  legend("topright", c("Original", "Heavy", "Noninformative", "Lambda Biased", "Phi/Psi Biased", "All Biased"),
         col = c("black", "orange", "red", "dodgerblue2", "forestgreen", "purple"),
         lty = c(3, 1, 2, 1, 1,1), lwd = c(3, 2, 2, 1, 1,1), cex = 0.6)
dev.off()


# Model Diagnostics -------------------------------------------------------

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

logLikelihood5=function(parms){
  sum(dgamma(occup$page_views, parms[1], parms[2], log = TRUE))
}

# DIC
X <- lapply(p, function(x){
  as.numeric(occup[occup$industryID == x,]$page_views)
})
# system.time(logLikelihood4(bayes.estimate))
# profvis(logLikelihood(bayes.estimate))
calcDIC=function(chains){
  bayes.estimate <- apply(chains, 2, mean)
  p.dic <- 2*(logLikelihood4(bayes.estimate[1:54]) - mean(apply(chains[,1:54],1,function(x) logLikelihood4(x[1:54]))) )
  dic <- -2*logLikelihood(bayes.estimate) + 2*p.dic   # Smaller is better
  dic
}
# We thin the chains to make them run faster
chooseInd <- round(seq(1, nrow(chainsActual),
                 length.out = 1/10*nrow(chainsActual)))
# Each takes around 27 seconds
system.time(origDIC <- calcDIC(chainsActual[chooseInd,]))
heavDIC <- calcDIC(chainsHActual[chooseInd,])
diffDIC <- calcDIC(chainsDActual[chooseInd,])
biasDIC <- calcDIC(chainsBActual[chooseInd,])
lightDIC <- calcDIC(chainsLiBActual[chooseInd,])
bothDIC <- calcDIC(chainsBBActual[chooseInd,])
c(origDIC, heavDIC, diffDIC, biasDIC, lightDIC, bothDIC)

# AIC ---------------------------------------------------------------------

bayes.estimate <- apply(chainsActual, 2, mean)
# AIC
mle <- optim(bayes.estimate[1:54], function(par) -logLikelihood4(par),
             lower = 0.001, method = "L-BFGS-B")$par
#mle <- MLE.est(occup)[1:54]
aic <- -2*logLikelihood4(mle) + 2*(ncol(chainsActual) - 2)
aic
mle2 <- optim(c(1, 1), function(par) -logLikelihood5(par),
              lower = 0.001, method = "L-BFGS-B")$par
aic2 <- -2*logLikelihood5(mle2) + 2*(ncol(chains) - 2)

# WAIC --------------------------------------------------------------------


# # WAIC
calcWAIC=function(chainsT){
  y <- occup$page_views
  p.waic1 <- 0
  for ( i in 1:nrow(occup) ) {
    p.waic1 <- p.waic1 + 2*log(mean(apply(chainsT[,1:54],1,
                                          function(m){
                                            indTemp <- occup[i, "industryID"]
                                            dgamma(y[i], m[indTemp],
                                                   m[indTemp+27])
                                            })))
    p.waic1 <- p.waic1 - 2*mean(apply(chainsT[,1:54],1,function(m){
      indTemp <- occup[i, "industryID"]
      dgamma(y[i], m[indTemp], m[indTemp+27], log = TRUE)
      })
    )
  }
  p.waic1
}
chooseInd <- round(seq(1, nrow(chainsActual),
                 length.out = 1/1000*nrow(chainsActual)))
system.time(origWAIC <- calcWAIC(chainsActual[chooseInd, ]))
heavWAIC <- calcWAIC(chainsHActual[chooseInd,])
diffWAIC <- calcWAIC(chainsDActual[chooseInd,])
biasWAIC <- calcWAIC(chainsBActual[chooseInd,])
lightWAIC <- calcWAIC(chainsLiBActual[chooseInd,])
bothWAIC <- calcWAIC(chainsBBActual[chooseInd,])
c(origWAIC, diffWAIC, biasWAIC, lightWAIC, bothWAIC)

# LPML --------------------------------------------------------------------


# # LPML
calcLPML=function(chains){
  y <- occup$page_views
  lpml <- sum(-log(sapply(1:length(y), function(i){
    mean(apply(chains, 1, function(try){
      indTemp <- occup[i, "industryID"]
      1/dgamma(y[i], try[indTemp], try[indTemp+27])
    }))
  })))
  lpml
}

chooseInd <- round(seq(1, nrow(chainsActual),
                 length.out = 1/1000*nrow(chainsActual)))
system.time(origLPML <- calcLPML(chainsActual[chooseInd, ]))
heavLPML <- calcLPML(chainsHActual[chooseInd,])
diffLPML <- calcLPML(chainsDActual[chooseInd,])
biasLPML <- calcLPML(chainsBActual[chooseInd,])
lightLPML <- calcLPML(chainsLiBActual[chooseInd,])
bothLPML <- calcLPML(chainsBBActual[chooseInd,])
c(origLPML, diffLPML, biasLPML, lightLPML, bothLPML)

calcR=function(chainsList){
  n <- nrow(chainsList[[1]])
  W <- sapply(1:ncol(chainsList[[1]]), function(parmN){
    mean(sapply(1:length(chainsList), function(chainN){
      pull <- chainsList[[chainN]][,parmN]
      var(pull)
    }))
  })
  B <- sapply(1:ncol(chainsList[[1]]), function(parmN){
         tempIn <- sapply(1:length(chainsList), function(chainN){
                pull <- chainsList[[chainN]][,parmN]
                mean(pull)
              })
         n*var(tempIn)
        })
  VFD <- (n-1)/n*mean(W) + 1/n*mean(B)
  sqrt(VFD/mean(W))
}
split=function(chainsActual){
  chains <- list(3)
  for(i in 1:3){
    chains[[i]] <- chainsActual[(i*1):(i*40000),]
  }
  chains
}
chainsH <- split(chainsHActual)
chainsD <- split(chainsDActual)
chainsB <- split(chainsBActual)
chainsLi <- split(chainsLiBActual)
chainsBB <- split(chainsBBActual)
system.time(origR <- calcR(chains))
heavR <- calcR(chainsH)
diffR <- calcR(chainsD)
biasR <- calcR(chainsB)
lightR <- calcR(chainsLi)
bothR <- calcR(chainsBB)
c(origR, heavR, diffR, biasR, lightR, bothR)
# pdf("~/Desktop/651present/sens.pdf", width = 11, height = 7)
# par(mfrow = c(1, 2))
# plot(density(chains1[, 1]), type = "n", main = "", xlab = expression(alpha))
# xx1 <- density(chains1[,1])$x
# yy1 <- density(chains1[,1])$y
# xx2 <- density(chains2[,1])$x
# yy2 <- density(chains2[,1])$y
# xx3 <- density(chains3[,1])$x
# yy3 <- density(chains3[,1])$y
# lines(xx1, yy1, lwd = 3)
# lines(xx2, yy2, col = "red", lwd = 2)
# lines(xx2, yy2, col = "blue", lty = 2)
# 
# 
# plot(density(chains1[, 28]), type = "n", main = "", xlab = expression(beta))
# xx1 <- density(chains1[,28])$x
# yy1 <- density(chains1[,28])$y
# xx2 <- density(chains2[,28])$x
# yy2 <- density(chains2[,28])$y
# xx3 <- density(chains3[,28])$x
# yy3 <- density(chains3[,28])$y
# lines(xx1, yy1, lwd = 3)
# lines(xx2, yy2, col = "red", lwd = 2)
# lines(xx2, yy2, col = "blue", lty = 2)
# legend("topright", c("Weak", "Improper", "Heavy"), col = c("black", "red", "blue"),
#        lty = c(1, 1, 2), lwd = c(3, 2, 1))
# dev.off()
# 
# 
# pdf("~/Desktop/651present/post.pdf", width = 11, height = 7)
# par(mfrow = c(2, 3))
# indCheck <- c(0, cumsum(c(27, 27, 8, 8, 8, 8)))
# 
# plot(density(chains3[, 7 + indCheck[1]]),
#      main = "", xlab = expression(alpha),
#      col = "dodgerblue")
# plot(density(chains3[, 7 + indCheck[2]]),
#      main = "", xlab = expression(beta),
#      col = "dodgerblue2")
# plot(density(chains3[, 3 + indCheck[3]]),
#      main = "", xlab = expression(lambda),
#      col = "dodgerblue3")
# plot(density(chains3[, 3 + indCheck[4]]),
#      main = "", xlab = expression(theta),
#      col = "dodgerblue4")
# plot(density(chains3[, 3 + indCheck[5]]),
#      main = "", xlab = expression(phi),
#      col = "cadetblue")
# plot(density(chains3[, 3 + indCheck[6]]),
#      main = "", xlab = expression(psi),
#      col = "cornflowerblue")
# dev.off()
# 
# pdf("~/Desktop/651present/ppDraws.pdf", height = 10, width = 10)
# par(mfrow = c(3, 2))
# for(m in 7:12){
#   ppDraws <- rgamma(nrow(chains3), chains3[,m], rate = chains3[,27 + m])
#   plot(density(ppDraws), main = p[m], col = sample(rainbow(100), 1))
# }
# dev.off()
