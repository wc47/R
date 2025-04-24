# Frequentist Gamma
library(coda); library(xtable)
occup <- read.csv("../data/database.csv", header = TRUE)
occup$count <- 1
occup$page_views <- log(occup$page_views)
occup$industryID <- as.numeric(occup$industry)
occup$domainID <- as.numeric(occup$domain)

# Functions ---------------------------------------------------------------

ll.gamma=function(dat, par){
  a <- exp(par[1]); b <- exp(par[2])
  -sum(dgamma(dat, a, rate = b, log = TRUE))
}
MLE.est=function(X){
  p <- 1:length(unique(X$industryID))
  groupList <- lapply(unique(X$domainID), function(x){
    tempGroup <- unique(X[X$domainID == x,]$industryID)
    which(p %in% tempGroup)
  })
  X <- lapply(p, function(x){
    X[X$industryID == x,]$page_views
  })
  reorder <- sapply(1:length(X), function(i){
    gamma.optim <- optim(c(1, 1), ll.gamma, dat = X[[i]])
    parms <- exp(gamma.optim$par)
    c(parms, parms[1]/parms[2])
  })
  c(reorder[1,], reorder[2,], reorder[3,])
}

# Actual Calculation ------------------------------------------------------

p <- 1:length(unique(occup$industryID))
groupList <- lapply(unique(occup$domainID), function(x){
  tempGroup <- unique(occup[occup$domainID == x,]$industryID)
  which(p %in% tempGroup)
})
X <- lapply(p, function(x){
  occup[occup$industryID == x,]$page_views
})
q <- sapply(1:length(unique(occup$domainID)), function(i){
  unique(occup[occup$domainID == i,]$domain)
})
indNames <- sapply(p, function(i){
  unique(occup[occup$industryID == i,]$industry)
})

indDom <- sapply(p, function(i){
  unique(occup[occup$industryID == i,]$domainID)
})
Xt <- sapply(p, function(x){
  which(p == unique(occup[occup$industryID == x,]$domainID))
})
refX <- lapply(1:length(table(Xt)), function(x){
  refX <- table(Xt)
  if(refX[x] == 1) return(x)
  seq(-0.4, 0.4, length.out = refX[x]) + x 
})
MLE.est(occup)
bootStudy <- sapply(1:1000, function(x){
  p <- 100
  while(length(p) < 27){
  bootInd <- sample(nrow(occup), nrow(occup), replace = TRUE)
  p <- length(unique(bootInd$industryID))
  }
  bootData <- occup[bootInd,]
  MLE.est(bootData)
})

rownames(bootStudy) <- c(paste0(rep(c("alpha", "beta"), each = 27),
                              rep(1:27, times = 2)), paste0("mean", 1:27))
tableFtemp <- apply(bootStudy, 1, quantile, c(0.025, 0.5, 0.975))
freqTable <- matrix(sapply(c("Math", "History", "Companions"),function(moniker){
  indNum <- which(indNames == moniker)
  pullNum <- which(colnames(tableFtemp) %in% paste0(c("alpha","beta"), indNum))
  tableFtemp[,pullNum]
}), nrow = 3)
xtable(freqTable)
tableF <- t(tableFtemp[,-1:-(27*2)])
rownames(tableF) <- p
rownames(tableF) <- indNames
tableF <- cbind(tableF, indDom)
tableF <- tableF[order(tableF[,4]),]
tableF <- cbind(tableF, unlist(refX))
pdf("linesFreqE.pdf", height = 3, width = 7)

xx <- (seq(12,16.5, length.out = 1000))
par(mar = c(5,10,1,5))
plot(xx,seq(0,length(q)+1, length.out = 1000),type = "n",
     xlim=(range(xx)),yaxt="n", xaxt="n",
     xlab = "E(log(Page Views))",
     ylab = "")
#axis(2, at = seq(1, length(q), by = 1), las=2)
axis(1, at = seq(12, 16.5, by = 0.5), las=2)
color3 <- c("red", "orange", "yellow","green", "blue", "purple", "black", "gray")
for(i in 1:nrow(tableF)){
  lines(c(tableF[i,1], tableF[i,3]),rep(tableF[i, 5], 2), 
        col = color3[tableF[i, 4]],
        lwd = 2)
  lines(rep(tableF[i,1], 2), rep(tableF[i, 5], 2) + c(-1,1)*0.05,
        lwd = 2, col = color3[tableF[i,4]])
  lines(rep(tableF[i,3], 2), rep(tableF[i,5], 2) + c(-1,1)*0.05,
        lwd = 2, col = color3[tableF[i,4]])
}
axis(2, at = seq(1, 8, by = 1), las=2, labels=q)
dev.off()
