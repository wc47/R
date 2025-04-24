library("glmnet"); library(foreach); library(doMC); library(MASS)
library(xtable); library(modreg)

setwd("~/Downloads/health-analytics")
popData <- read.csv("Key_indicator_districtwise.csv", header = TRUE)
setwd("~/Documents/School/BYU 2017-2018/666/Final Project")
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
popData <- popData[, 1:408]
popData <- data.matrix(popData)
# Now we begin the actual analysis.
pull <- sample(1:nrow(popData), round(nrow(popData)/5)) 
all(apply(popData, 2, function(x) is.numeric(x)))
# Column 603 is our desired response variable
y <- scale(popData[, 408]); x <- scale(popData[, 2:385])
######
## EDA
######
pdf("666eda1.pdf", width = 3, height = 3)
hist(y, col = "tomato", freq = FALSE, main = "",
     xlab = "Child Death Rate")
dev.off()
pdf("666eda2.pdf", width = 3, height = 3)
hist(cor(y, x), col = "cornflowerblue", main = "", freq = FALSE,
     xlab = "Corr(X,y)")
dev.off()
######
testInd <- sample(1:nrow(x), round(1/10*nrow(x)))
# for(i in 1:15){
  # mod1 <- ppr(x = x[-testInd, ], y = y[-testInd], nterms = 5)
  # summary(mod1)
  # RPMSE <- sqrt(mean(((y[testInd] - 
  #                        predict(mod1, newdata = x[testInd, ]))^2)))
#   cat("i: ", i, " RPSME: ", RPMSE, "\n")
# }
# So we use 1 term
modPP <- ppr(x = x, y = y, nterms = 1, sm.method = "spline")
alpha <- summary(modPP)$alpha
alpha <- alpha[order(abs(alpha), decreasing = TRUE)]
xtable(t(t(alpha[abs(alpha) > 0.1])))

cv.out <- cv.glmnet(x, y, alpha = 1)
bestlam <- cv.out$lambda.min
modLasso <- glmnet(x, y, alpha = 1, lambda = bestlam)
betas <- coef(modLasso, s = "lambda.1se")
weCare <- betas[betas > 0]
names(weCare) <- rownames(betas)[which(betas  > 0)]
form <- rownames(betas)[which(betas  > 0)]
modTrue <- lm(y ~ popData[, form[-1]])
resTable <- summary(modTrue)$coefficients %*% cbind(c(1, 0, 0, 0),
                                        c(1, -1.96, 0, 0),
                                        c(1, 1.96, 0, 0))
rownames(resTable) <- NULL
rownames(resTable) <- c("Intercept", form[-1])
resTable <- resTable[order(summary(modTrue)$coefficients[, 4]), ]
coefTable <- summary(modTrue)$coefficients
coefTable <- coefTable[order(coefTable[, 4]), ]
topSig <- max(which(coefTable[, 4] < 0.05))
xtable(resTable[1:topSig,])
weCare <- weCare[order(weCare, decreasing = TRUE)]
xtable(t(weCare[1:12]))
RMSElasso <- sqrt(mean((y -fitted(modTrue))^2)) 
RMSEpp <- sqrt(mean((y - fitted(modPP))^2))
# Bootstrap RMSE
bootRMSE_l <- sapply(1:100, function(i){
  bootInd <- sample(nrow(x), replace = TRUE)
  xB <- x[bootInd, ]; yB <- y[bootInd]
  cv.out <- cv.glmnet(xB, yB, alpha = 1)
  bestlam <- cv.out$lambda.min
  modLasso <- glmnet(xB, yB, alpha = 1, lambda = bestlam)
  betas <- coef(modLasso, s = "lambda.1se")
  #weCare <- betas[betas > 0]
  #names(weCare) <- rownames(betas)[which(betas  > 0)]
  form <- rownames(betas)[which(betas  > 0)]
  modTrue <- lm(yB ~ popData[bootInd, form[-1]])
  sqrt(mean((yB - fitted(modTrue))^2)) 
}) 
bootRMSE_pp <- sapply(1:100, function(i){
  bootInd <- sample(nrow(x), replace = TRUE)
  xB <- x[bootInd, ]; yB <- y[bootInd]
  modPP <- ppr(x = xB, y = yB, nterms = 1, sm.method = "spline")
  sqrt(mean((yB - fitted(modPP))^2)) 
})
bootTable <- rbind(c(RMSElasso, mean(bootRMSE_l),
                     quantile(bootRMSE_l, c(0.025, 0.975))),
                   c(RMSEpp, mean(bootRMSE_pp),
                     quantile(bootRMSE_pp, c(0.025, 0.975))))
colnames(bootTable) <- c("Full Data RMSE", "Bootstrap Average",
                         "95% Bootstrap LB",
                         "95% Bootstrap UB")
rownames(bootTable) <- c("Lasso", "Projection Pursuit")
xtable(bootTable, digits = 3)
cvStudy <- sapply(1:100, function(d){
  testInd <- sample(1:nrow(x), round(1/10*nrow(x)))
  modLasso <- lm(y[-testInd] ~ popData[-testInd, form])
  modPP <- ppr(x = x[-testInd, ], y = y[-testInd], nterms = 1,
               sm.method = "spline")
  newTest <- as.data.frame(popData[testInd, form ])
  RPMSElasso <- sqrt(mean((y[testInd] - as.matrix(cbind(1, newTest)) %*%
                             as.numeric(data.matrix(coef(modLasso))))^2))
  RPMSEpp <- sqrt(mean((y[testInd] - 
                          predict(modPP, newdata = x[testInd, ]))^2))
  c(RPMSElasso, RPMSEpp)
})
cvRes <- cbind(apply(cvStudy, 1, mean),
  apply(cvStudy, 1, quantile, 0.025),
  apply(cvStudy, 1, quantile, 0.975))
xtable(cvRes)


# Simulation Study Results
options <- expand.grid(c(20, 50), c(0, 0.5), c(0.5, 2),
                       c(20, 200))
options <- cbind(options, rep(c(0.1, 0.9), each = 2))
options <- options[, c(1, 2, 5, 3, 4)]
colnames(options) <- c("n", "rho1", "rho2", "var", "p")
rownames(options) <- NULL
xtable(options, digits = c(0, 0, 1, 1, 0, 0))
pdf("DataFull.pdf")
par(mfrow = c(2, 1))
for(d in 1:2){
  ifelse(d == 1, load("lassoSim4"), load("ppSim3"))
  if(d == 2){
      simResults[[1]] <- sapply(1:length(simResults[[1]]),
                                function(x){
                                  out <- as.numeric(simResults[[1]][[x]][1:3])
                                  out[is.na(out)] <- Inf
                                  t(matrix(out, nrow = 3))
                                })
  }
  simRes <- sapply(1:16, function(ell){
    tempRes <- apply(simResults[[ell]][1:3, ], 2, function(bad){
        which(bad == min(bad))
      })
    sapply(1:3, function(may) mean(tempRes == may))
  })
  lowSim <- sapply(1:16, function(ell){
    tempRes <- apply(simResults[[ell]][1:3, ], 2, function(bad){
      which(bad == min(bad))
    })
    sapply(1:3, function(may){
      pi <- mean(tempRes == may)
      pi + -1*qnorm(0.975)*sqrt(pi*(1-pi))/sqrt(500)
    })
  })
  highSim <- sapply(1:16, function(ell){
    tempRes <- apply(simResults[[ell]][1:3, ], 2, function(bad){
      which(bad == min(bad))
    })
    sapply(1:3, function(may){
      pi <- mean(tempRes == may)
      pi + 1*qnorm(0.975)*sqrt(pi*(1-pi))/sqrt(500)
    })
  })
  plot(simRes[1, ], col = "red", pch = 18, ylim = c(0, 1),
       ylab = "Percent Best RPMSE", xaxt = 'n',
       xlab = paste0("Dataset ", d), type = "n")
  plotCols <- c("red", "blue", "black")
  addPlot <- c(0, -1/3, 1/3)
  for(k in 1:3){
    points(1:16 + addPlot[k], simRes[k, ], col = plotCols[k], pch = 18, cex = 0.8)
    arrows(1:16 + addPlot[k], lowSim[k, ], 1:16 + addPlot[k], highSim[k, ],
           length=0.02, angle=90, code=3, col = plotCols[k])
  }
  legend("topleft", c("Lasso", "Ridge", "PP"), col = c("red", "blue", "black"),
         pch = c(18, 18, 18), cex = 0.8)
}
dev.off()

tables <- list()
for(d in 1:2){
  ifelse(d == 1, load("lassoSim4"), load("ppSim3"))
  if(d == 2){
    simResults[[1]] <- sapply(1:length(simResults[[1]]),
                              function(x){
                                out <- as.numeric(simResults[[1]][[x]][1:3])
                                out[is.na(out)] <- Inf
                                t(matrix(out, nrow = 3))
                              })
  }
  meanTable <- sapply(1:length(simResults), function(x){
    apply(simResults[[x]][1:3, ], 1, function(z){
      z[is.infinite(z)] <- NA
      mean(z, na.rm = TRUE)
    })
  })
  tables[[d]] <- xtable(meanTable, digits = 3)
}
tables[[1]]; tables[[2]]
