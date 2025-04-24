### Experiment Design for Project
library(FrF2); library(daewr); library(lme4); library(xtable)
set.seed(2)
d <- FrF2(16, 7)
p <- pb(nruns = 12, randomize = FALSE)
y <- runif(16)
aliases(lm(y~(.)^4, data = d))
dF1 <- FrF2(16, 7, randomize = TRUE)
dF <- data.matrix(dF1)
colnames(dF) <- paste0("x",1:7)
dFinal <- apply(dF, 2, function(x){
  y <- x
  y[y==1] <- -1; y[y==2] <- 1 
  y
})
write.csv(dFinal, file = "experimentStage1.csv")
# So this is a Resolution 4 Design
# We'll run the experiment then augment the design.

stage1 <- read.csv("Experiments1.csv", header = TRUE)[,-1:-2]
xtable(stage1)
sMod1 <- lm(shrinkage~(.)^2, data = stage1)
summary(sMod1)
wpEffects <- coef(sMod1)[-1]
wpEffects <- wpEffects[!is.na(wpEffects)]
pdf("norms1.pdf", height = 2.5, width = 7.5)
halfnorm(wpEffects, alpha = 0.2)
dev.off()
# So there are more than 2 effects. We therefore need to augment the design.

f2 <- fold.design(dF1, columns = 3)
y  <- runif(32); aliases(lm(y~(.)^2, data = f2))
d2F <- data.matrix(f2)
colnames(d2F) <- c(paste0("x",1:4), "block",paste0("x", 5:7))
d2Final <- apply(d2F, 2, function(x){
  y <- x
  y[y==1] <- -1; y[y==2] <- 1 
  y
})
write.csv(d2Final[17:32, -5], file = "experimentStage2.csv")

stage2 <- read.csv("Experiments2.csv")[,-1:-2]
stageFinal <- rbind(stage1, stage2)
stageFinal$block <- rep(c("block", "mirror"), each = 16)
rownames(stageFinal) <- NULL
#colnames(stageFinal) <- c("A", "B","C","D","E","F","G", "shrinkage", "block")
# sMod2 <- lmer(shrinkage ~ A + B + C + D + E + F + G + A:B + A:E + B:D + A:C + A:D  +
#                     A:G + B:C + B:E + B:G + C:D + C:E + C:F + C:G + 
#                     A:B:C + A:C:E + B:D:C + A:G:C + B:E:C + B:G:C +
#               +(1|block),
#               data = stageFinal)

sMod2 <- lmer(shrinkage ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x1:x2 + x1:x5 + x2:x4 + x1:x3 + x1:x4  +
                x1:x7 + x2:x3 + x2:x5 + x2:x7 + x3:x4 + x3:x5 + x3:x6 + x3:x7 + 
                x1:x2:x3 + x1:x3:x5 + x2:x4:x3 + x1:x7:x3 + x2:x5:x3 + x2:x7:x3 +
                +(1|block),
              data = stageFinal)

# sMod2 <- lmer(shrinkage ~ x1 + x2 + x3 + x4  + A:B + A:E + B:D + A:C + A:D  +
#                 A:G + B:C + B:E + B:G + C:D + C:E + C:F + C:G + 
#                 A:B:C + A:C:E #+ B:D:C
#               +(1|block),
#               data = stageFinal)
# sMod2 <- lm(shrinkage ~ A + B + C + D+E+F+G+ A:B + A:E + B:D + A:C + A:D  +
#                 A:G + B:C + B:E + B:G + C:D + C:E + C:F + C:G + 
#                 A:B:C + A:C:E + B:D:C + A:C:D + A:G:C + B:C:E + 
#                 B:G:C + 
#               +block,
#               data = stageFinal)
resTable <- summary(sMod2)$coef
resTable


ciInt <- rbind(c(resTable[-1,1] - qt(0.975, 26)*resTable[-1,2]), 
               summary(sMod2)$coef[-1,1],
           c(resTable[-1,1] + qt(0.975, 26)*resTable[-1,2]))
rownames(ciInt) <- c("Lower Bound", "Estimate", "Upper Bound")
ciInt
library(xtable)
xtable(ciInt)
pdf("norms2.pdf", height = 2.5, width = 7.5)
halfnorm(fixef(sMod2)[-1], alpha = 0.03)
#halfnorm(resTable[-1,1], alpha = 0.01)
dev.off()

sModF <- lmer(shrinkage ~ A+C+E+A:C+A:E+C:E+(1|block),
              data = stageFinal)
coefs <- summary(sModF)$coef[,1]
sum(coefs*c(1, -1, 1, -1, 1))
pdf("inter1a.pdf", height = 6, width = 6)
#par(mfrow = c(1, 2))
interaction.plot(stageFinal$x1, stageFinal$x5, stageFinal$shrinkage,
                 xlab = "x1", ylab = "Shrinkage",
                 legend = FALSE, col = c("red","black"))
legend("bottomright", c("x5: 1", "x5: -1"), col = c("black", "red"),
       lty = c(1,2))
dev.off()

pdf("inter1b.pdf", height = 6, width = 6)
interaction.plot(stageFinal$x3, stageFinal$x6, stageFinal$shrinkage,
                 xlab = "x3", ylab = "Shrinkage",
                 legend = FALSE , col = c("red","black"))
legend("bottomleft", c("x6: 1", "x6: -1"), col = c("black", "red"),
       lty = c(1,2))
dev.off()


par(mfrow = c(1,1))
pdf("inter2a.pdf")
#par(mfrow = c(1, 2))
tempHigh <- stageFinal[stageFinal$x7 == 1,]
tempLow <- stageFinal[stageFinal$x7 == -1,]
interaction.plot(tempLow$x3, tempLow$x2, tempLow$shrinkage,
                 xlab = "x3", ylab = "Shrinkage",
                 legend = FALSE, col = c("red","black"))
legend("bottomleft", c("x2: 1", "x2: -1"), col = c("black", "red"),
       lty = c(1,2))
dev.off()
pdf("inter2b.pdf")
interaction.plot(tempHigh$x3, tempHigh$x2, tempHigh$shrinkage,
                 xlab = "x3", ylab = "Shrinkage",
                 legend = FALSE, col = c("red","black"))
legend("bottomleft", c("x2: 1", "x2: -1"), col = c("black", "red"),
       lty = c(1,2))
dev.off()

sum(c(1, -1, -1, 1, 0, -1, 1, 1, -1, -1, 1, 1)*fixef(sMod2)[c(1:8, 15, 25, 10, 20)])
combData <- rbind(stageFinal, c(-1, -1, 1, 0, -1, 1, 1, 0, "mirror"))
effectTable <- matrix(fixef(sMod2)[c(2:8, 15, 25, 10, 20)], ncol = 1)
rownames(effectTable) <- names(fixef(sMod2)[c(2:8, 15, 25, 10, 20)])
xtable(effectTable, digits = 3)
preds <- predict(sMod2, newdata = combData)
predF
pdf("assume.pdf", width = 6, height = 4)
par(mfrow = c(1,2))
qqnorm(y = residuals(summary(sModF)),
       xlab = "Normal Scores", ylab = "Residuals",
       main = "")
abline(a=0,b=1, col = "red")
hist(residuals(summary(sModF)), col = "red", freq = FALSE,
     main = "", xlab = "Residuals")
xx <- seq(-3,3, by = 0.001); yy <- dnorm(xx)
lines(xx, yy, col = "blue", lty = 2, lwd = 2)
dev.off()
