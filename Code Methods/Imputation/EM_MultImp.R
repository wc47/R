
library(MASS)

EM=function(Xinit, epsilon = 0.000000001){
  # An EM algorithm to fill in matrix X.
  # Args: 
  #   Xinit- a swiss-cheese numeric matrix with missing data
  #   epsilon- numeric, tolerance before convergance
  # Returns: 
  #   X - a filled numeric matrix
  X <- Xinit
  ThetaPrev <- ThetaDiff <- ThetaTemp <- -150
  # Identifies which rows in X are missing data
  missInd <- which(sapply(1:nrow(X), function(i){
    length(which(is.na(X[i, ]))) > 0
  }))
  # Creates a list, with each element containing the indeces missing 
  # for its attendant row
  chooseInd <- lapply(1:nrow(X), function(i){
    which(is.na(X[i, ]))
  })
  #store <- numeric()
  Mu <- MuInit <- as.numeric(colMeans(X[-missInd, ]))
  Sigma <- SigmaInit <- data.matrix(cov(X[-missInd, ]))
  # EM algorithm
  while(abs(ThetaDiff) > epsilon){
    X <- Xinit
    for(i in 1:length(missInd)){
      missP <- which(is.na(X[missInd[i], ]))
      x2 <- X[missInd[i], -missP]
      B <- Sigma[missP, -missP] %*% solve(Sigma[-missP, -missP])
      x1 <- Mu[missP] + B %*% (x2 - Mu[-missP])
      X[missInd[i], missP] <- x1
    }
    Mu <- data.matrix(colMeans(X))
    Sigma <- data.matrix(cov(X))
    # Calculate log-likelihood
    ThetaTemp <- c(Mu, unique(c(Sigma)))
    ThetaDiff <- sum((ThetaTemp - ThetaPrev)^2)
    ThetaPrev <- ThetaTemp
    #store <- c(store, ThetaPrev)
    convergeInd   <- lapply(1:length(chooseInd), function(h){
      X[h, chooseInd[[h]]]
    })
  }
  X
}

multImp=function(Xinit, Xem, n.it = 1000, more = FALSE){
  # A function to perform multiple imputation after an EM algorithm.
  # Args:
  #   Xinit- numeric matrix with missing values, original argument for
  #     EM algorithm.
  #   Xem- filled in Xinit matrix output from EM algorithm
  #   n.it- numeric, number of iterations for multiple imputation
  #   more- logical, whether or not to output R.
  # Returns:
  #   X- numeric, matrix with imputed values
  #   R- numeric, contains in rows every esitmate for a missing value from
  #   imputation.
  X <- Xinit
  # Identifies which rows in X are missing data
  missInd <- which(sapply(1:nrow(X), function(i){
    length(which(is.na(X[i, ]))) > 0
  }))
  # Creates a list, with each element containing the indeces missing 
  # for its attendant row
  chooseInd <- lapply(1:nrow(X), function(i){
    which(is.na(X[i, ]))
  })
  # Use the Mean and Variance from the EM algorithm
  Mu <- data.matrix(colMeans(Xem))
  Sigma <- data.matrix(cov(Xem))
  R <- unlist(chooseInd)
  # Impute the Data
  for(j in 1:n.it){
    for(i in 1:length(missInd)){
      missP <- which(is.na(X[missInd[i], ]))
      x2 <- X[missInd[i], -missP]
      B <- Sigma[missP, -missP] %*% solve(Sigma[-missP, -missP])
      e <- mvrnorm(1, rep(0, length(missP)), Sigma[missP, missP] - 
                     Sigma[missP, -missP] %*% solve(Sigma[-missP, -missP]) %*%
                     Sigma[-missP, missP]) 
      x1 <- Mu[missP] + B %*% (x2 - Mu[-missP]) + e
      X[missInd[i], missP] <- x1
    }
    convergeInd <- lapply(1:length(chooseInd), function(h){
      X[h, chooseInd[[h]]]
    })
    X <- Xinit
    R <- cbind(R, unlist(convergeInd))
  }
  # Insert the mean of the draws back into the data.
  R <- R[, -1]
  fillHat <- rowMeans(R)
  k <- 1
  for(h in 1:length(chooseInd)){
    kPlus <- length(chooseInd[[h]])
    if(kPlus != 0){
      indeces <- k:(k + kPlus - 1)
      X[h, chooseInd[[h]]] <- fillHat[indeces]
      k <- k + kPlus
    }
  }
  if(more) return(list(X, R))
  X
}

Xnew <- EM(wine2)
Ximp <- multImp(wine2, Xnew)
