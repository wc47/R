### Mini Project 1
### Fraud Detection ###

#####
# Libraries and Functions
#####
BoxCox=function(x, lambda, eval = FALSE){
  # Univariate Box-Cox Transformation
  n <- length(x)
  if(lambda == 0){
    retX <- log(x)
  } else {
    retX <- (x^lambda - 1)/lambda
  }
  sLambda <- 1/n * sum((retX - mean(retX))^2)
  if(eval){
    return( -n/2 * log(sLambda) + (lambda - 1)*sum(log(x)))
  }
  retX
}

Lammit=function(x, lambda, eval = FALSE){
  # Multivariate Box-Cox Transformation
  n <- nrow(x)
  if(lambda == 0){
    retX <- log(x)
  } else {
    retX <- (x^lambda + 1)/lambda
  }
  if(eval){
    return( -n/2 * log(det(var(retX))) + sum((lambda - 1)*colSums(log(x))) ) 
  }
  retX
}

ChiQQ=function(data, xlim = c(0), ylim = c(0)){
  # Produces a Chi-Squared QQ Plot
  Si <- cov(data)
  xbar <- colMeans(data)
  chi <- apply(data, 1, function(x) t(x - xbar) %*% Si %*% (x - xbar))
  chi <- chi[order(chi)]
  par(mfrow = c(1, 1), mai = c(1, 1, 1/2, 1/4))
  x.plot <- qchisq((1:n-0.5)/n, p)
  y.plot <- chi
  if(sum(xlim) == 0 | sum(ylim) == 0){
    plot(x.plot,y.plot, type = 'l', xlab = "Theoretical Quantile",
         ylab = "Empirical Quantile")
  } else {
    plot(x.plot,y.plot, type = 'l', xlab = "Theoretical Quantile",
         ylab = "Empirical Quantile", xlim = xlim, ylim = ylim)
  }
  abline(c(0, 1), col = "red" )
}

ChiRank=function(data, f = mean){
  # Produces a Chi-Squared QQ Plot
  Si <- solve(cov(data))
  xbar <- apply(data, 2, f)
  chi <- apply(data, 1, function(x) t(x - xbar) %*% Si %*% (x - xbar))
  order(chi, decreasing = TRUE)
}

IterChiRank=function(data1, f = mean){
  # Finds 40 Worst Outliers by our iterative process
  frauds <- numeric(50)
  refIndex <- 1:nrow(data1)
  for(i in 1:50){
      Si <- solve(cov(data1))
      xbar <- apply(data1, 2, FUN = f)
      chi <- apply(data1, 1, function(x) t(x - xbar) %*% Si %*% (x - xbar))
      worstIndex <- which.max(chi)
      returnIndex <- refIndex[worstIndex]
      data1 <- data1[-worstIndex, ]
      refIndex <- refIndex[-worstIndex]
      frauds[i] <- returnIndex
  }
  frauds
}

