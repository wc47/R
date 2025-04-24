edaClust=function(data, method = "complete", k = 3, kFolds = 5, kMeans = FALSE){
  # A function to evaluate clustering methods
  # Args:
  #   data: numeric matrix
  #   method: linkage method i.e. single, complete, centroid
  #   k: number of groups to cluster into.
  #   kFolds: numeric, number of smaller groups to 
  #     evaluate for predictive ability.
  #   kMeans: logical, uses k-means rather than hierarchical 
  #     clustering if TRUE.
  # Results:
  #   list, 2 elements
  #     1) list, containing clusterings of length k.
  #     2) average f measure.
  if(kMeans){
    link <- kmeans(data, 3)
    truth <- link$cluster
  } else {
    link <- hclust(dist(data), method = method)
    truth <- cutree(link, k)
  }
  testInd <- sample(1:1000, 1000, replace = FALSE)
  kBreaks <- nrow(data)/kFolds
  fMeasures <- mean(sapply(0:(kFolds - 1), function(i){
    getFmeasure(truth[testInd[(i*kBreaks + 1):((i + 1)*kBreaks)]],
                data[testInd[(i*kBreaks + 1):((i + 1)*kBreaks)], ] )
  }))
  list(lapply(1:k, function(x){
    data[truth == x, ]
  }), fMeasures, cbind(data.matrix(data), truth))
}

superGenre=function(data2){
  data2$SuperGenre <- ifelse(data2$Genre <= 3, 1, data2$Genre)
  data2$SuperGenre <- ifelse(data2$Genre %in% 4:6 , 2, data2$SuperGenre)
  data2$SuperGenre <- ifelse(data2$Genre == 7, 3, data2$SuperGenre)
  data2$SuperGenre <- ifelse(data2$Genre %in% 8:9, 4, data2$SuperGenre)
  data2$SuperGenre <- ifelse(data2$Genre >= 10, 5, data2$SuperGenre)
  data2$SuperGenre
}

standardize=function(x){
  apply(x, 2, function(x){
    (x- mean(x))/sd(x)
  })
}

discrim=function(data, group1, group2, start = 1, end = 18){
  pop1 <- data[data[, ncol(data)] %in% group1, start:18]
  pop2 <- data[data[, ncol(data)] %in% group2, start:18]
  Spl <- 1/(nrow(pop1) + nrow(pop2) - 2)*(cov(pop1)*(nrow(pop1) - 1) + cov(pop2)*(nrow(pop2) - 1))
  (sqrt(diag(Spl))* solve(Spl) %*% (colMeans(pop1) - colMeans(pop2)))
}  

evalDiscrim=function(data, poss){
  tempInd <- data.matrix(expand.grid(1:poss, 1:poss))
  chooseInd <- sapply(1:nrow(tempInd), function(x){
    out <- tempInd[x, 1] != tempInd[x, 2]
    if(x > 1 & out){
      out <- !any(sapply(1:(x - 1),
                         function(y) all(unique(tempInd[x, ]) %in%
                                           unique(tempInd[y, ]))))
    }
    out
  })
  finalInd <- tempInd[which(chooseInd), ]
  finalInd <- finalInd[order(finalInd[, 1]), ]; finalInd <- finalInd[order(finalInd[, 2]), ]
  finalInd <- cbind(finalInd[, 2], finalInd[, 1])
  compDiscrim <- sapply(1:nrow(finalInd), function(i){
    discrim(data, finalInd[i, 1], finalInd[i, 2])
  })
  compDiscrim <- cbind(compDiscrim, sapply(1:poss, function(m){
    vers <- (1:poss)[-which(1:poss == m)]
    discrim(data, vers, m)
  }))
  colnames(compDiscrim) <- c(sapply(1:nrow(finalInd), function(b){
    paste0(finalInd[b, 1], "v", finalInd[b, 2])
  }), paste0(1:poss, "vAll"))
  compDiscrim
}

interpDiscrim=function(X){
  varN <- colnames(data)[1:18]
  rankX <- sapply(1:ncol(X), function(x) order(abs(X[, x]), decreasing = TRUE))
  colnames(rankX) <- colnames(X)
  lapply(1:ncol(X), function(m){
    keepInd <- which(abs(X[, m]) >= abs(X[rankX[1, m], m])/2)
    out <- X[keepInd, m]; names(out) <- varN[keepInd]
    out <- out[order(abs(out), decreasing = TRUE)]; out
  })
}

getFmeasure <- function(test, true){
  pairs <- t(combn(1:length(test),2))
  pairs_test <- cbind(test[pairs[,1]],test[pairs[,2]])
  pairs_true <- cbind(true[pairs[,1]],true[pairs[,2]])
  
  a <- sum(pairs_test[,1] == pairs_test[,2] & pairs_true[,1] == pairs_true[,2])
  b <- sum(pairs_test[,1] != pairs_test[,2] & pairs_true[,1] != pairs_true[,2])
  c <- sum(pairs_test[,1] == pairs_test[,2] & pairs_true[,1] != pairs_true[,2])
  d <- sum(pairs_test[,1] != pairs_test[,2] & pairs_true[,1] == pairs_true[,2])
  
  P <- a/(a + c)
  R <- a/(a + d)
  F_measure <- (2*P*R)/(P + R)
  F_measure
}

possK <- 3:7
possFolds <- 2:7
possMethod <- c("single", "complete", "ward.D2", "average", "centroid")
expand.grid(list(possK, possFolds, possMethod))

