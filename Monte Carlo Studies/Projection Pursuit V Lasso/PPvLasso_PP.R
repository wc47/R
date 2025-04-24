library("glmnet"); library(foreach); library(doMC); library(MASS);
library(xtable)
logit=function(x){
  log(x/(1-x))
}
testDiff=function(y, x){
  # Compare the different regression schemes
  n <- nrow(x)
  test <- sample(1:n, round(n/10))
  train <- (1:n)[-test]
  grid <- seq(0, 10, by = 0.1)
  #RPMSE lasso
  cv.out <- cv.glmnet(x[train, ], y[train], alpha = 1)
  bestlam <- cv.out$lambda.min
  lasso.mod <- glmnet(x[train, ], y[train], alpha = 1, lambda = bestlam)
  yhatTest <- predict(lasso.mod, newx = x[test, ], type = "response")
  RPMSElasso <- sqrt(mean((y[test] - yhatTest)^2))
  #RPMSE ridge
  ridge.mod <- glmnet(x[train, ], y[train], alpha = 0, lambda = bestlam)
  yhatTest <- predict(ridge.mod, newx = x[test, ], type = "response")
  RPMSEridge <- sqrt(mean((y[test] - yhatTest)^2))
  #RPMSE PP
  methodsPP <- c("supsmu", "spline")
  RppHist <- numeric();
  #for(nt in 1:3){
  fullRpp <- mcmapply(function(nt){
    RppHist <- numeric();
    for(m in methodsPP){
      pprMod <- ppr(x[train, ], y[train], nterms = nt, optlevel = 0,
                    sm.method = m)
      yhatPP <- predict(pprMod, newdata = x[test, ], type = "response")
      RPMSEpp <- sqrt(mean((y[test] - yhatPP)^2))
      RppHist <- c(RppHist, RPMSEpp)
    }
    RppHist
  }, 1:15)
  c(RPMSElasso, RPMSEridge, min(fullRpp), c(fullRpp))
}
simPop=function(n, rho, p, sigma2, nreps){
  #cat(p, "\n")
  stretch <- 1:p
  H <- abs(outer(stretch, stretch, "-"))
  V <- sigma2 * rho^H
  p <- nrow(V)
  #V[cbind(1:p, 1:p)] <- V[cbind(1:p, 1:p)] * sigma2
  tempPop <- mvrnorm(n, rep(0, p), V)
  impVar <- sample(1:p, round(p/10))
  y <- tempPop %*% (1:p %in% impVar) + rnorm(n, 0, 1)
  #y <- scale(y)
  testDiff(y , tempPop)
}

simPop2=function(n, rho, p, sigma2, nreps){
  # Simulate Second Data set
  stretch <- 1:p
  H <- abs(outer(stretch, stretch, "-"))
  V <- sigma2 * rho^H
  p <- nrow(V)
  tempPop <- mvrnorm(n, rep(0, p), V)
  impVar <- sample(1:p, round(p/2))
  qR <- length(impVar)
  y <- (tempPop[, -impVar] %*% rep(1, p - qR) +
          (tempPop[, impVar]^2) %*% rep(1, qR) + 
          (tempPop[, impVar]^3) %*% rep(1, qR) + rnorm(n, 0, 1))
  #y <- scale(y)
  testDiff(y , tempPop)
}

# Run Simulation Study
options <- expand.grid(c(20, 50), c(0, 0.5), c(0.5, 2),
                       c(20, 200))
system.time(simResults <- lapply(1:nrow(options), function(x){
  cat("Iteration: ", x, "\n")
  sapply(1:500, function(m) simPop(n = options[x, 1],
                                   rho = options[x, 2],
                                   p = options[x, 4],
                                   sigma2 = options[x, 3],
                                   nreps = 10))
}))
save(simResults, file = "lassoSim4")

system.time(simResults <- lapply(1:nrow(options), function(x){
  cat("Iteration: ", x, "\n")
  sapply(1:500, function(m) simPop(n = options[x, 1],
                                   rho = options[x, 2],
                                   p = options[x, 4],
                                   sigma2 = options[x, 3],
                                   nreps = 10))
}))
save(simResults, file = "ppSim3")

