library("glmnet"); library(foreach); library(doMC); library(MASS)
logit=function(x){
  log(x/(1-x))
}
testDiff=function(y, x){
  n <- nrow(x)
  test <- sample(1:n, round(n/10))
  train <- (1:n)[-test]
  grid <- seq(0, 10, by = 0.1)
  #lasso.mod <- glmnet(x[train, ], y[train], alpha = 1, lambda = grid)
  cv.out <- cv.glmnet(x[train, ], y[train], alpha = 1)
  #plot(cv.out)
  bestlam <- cv.out$lambda.min
  lasso.mod <- glmnet(x[train, ], y[train], alpha = 1, lambda = bestlam)
  yhatTest <- predict(lasso.mod, newx = x[test, ], type = "response")
  RPMSElasso <- sqrt(mean((y[test] - yhatTest)^2))
  #RPMSElasso
  lasso.mod <- glmnet(x[train, ], y[train], alpha = 0, lambda = bestlam)
  yhatTest <- predict(lasso.mod, newx = x[test, ], type = "response")
  RPMSEridge <- sqrt(mean((y[test] - yhatTest)^2))
  #RPMSEridge
  methodsPP <- c("supsmu", "spline")
  RppHist <- numeric()
  #for(nt in 1:3){
  fullRpp <- mcmapply(function(nt){
    RppHist <- numeric(); AICpp <- numeric()
    for(m in methodsPP){
      pprMod <- ppr(x[train, ], y[train], nterms = nt, optlevel = 0,
                    sm.method = m)
      yhatPP <- predict(pprMod, newdata = x[test, ])
      RPMSEpp <- sqrt(mean((y[test] - yhatPP)^2))
      RppHist <- c(RppHist, RPMSEpp)
    }
    RppHist
  }, 1:20)
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

setwd("~/STAT666/")
popData <- read.csv("health.csv", header = TRUE)
bad <- which(substr(colnames(popData), 1, 2) == "ZZ" | 
               substr(colnames(popData), sapply(colnames(popData), nchar) - 4,
                      sapply(colnames(popData), nchar)) == "Total")
# Get rid of 298:299 because of missing data
popData <- popData[, -c(bad, 1, 298:299)]
dim(popData)
moreBad <- which(sapply(1:nrow(popData), function(x){
  any(is.na(popData[x, ]))
}))
popData <- popData[-moreBad,]
popData <- data.matrix(popData)
# Now we begin the actual analysis.
pull <- sample(1:nrow(popData), round(nrow(popData)/5)) 
all(apply(popData, 2, function(x) is.numeric(x)))
# Column 603 is our desired response variable
options <- expand.grid(c(20, 50), c(0.1, 0.9), c(0.5, 2),
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
