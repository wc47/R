# Data
x <- c(-0.86,-0.30,-0.05,0.74)  # Dose in log(g/ml)
n <- rep(5,length(x))           # Number at animal at risk
y <- c(0,1,3,5)                 # Number of deaths

logit <- function(p) log(p/(1-p))
logitInv <- function(y) 1/(1+exp(-y))
logLogitInv <- function(y)       -log(1+exp(-y))
log1MinusLogitInv <- function(y) -log(1+exp( y))

likelihood <- function(alpha,beta) {
  lc <- alpha+beta*x
  li <- logitInv(lc)
  prod( li^y * (1 - li)^(n-y) )
}

logLikelihood <- function(alpha,beta) {
  lc <- alpha+beta*x
  sum( y*logLogitInv(lc) + (n-y)*log1MinusLogitInv(lc) )
}

posterior <- function(alpha,beta) {     # Unnormalized
  likelihood(alpha,beta) * prior(alpha,beta)
}

logPosterior <- function(alpha,beta) {  # Unnormalized
  logLikelihood(alpha,beta) + logPrior(alpha,beta)
}

# Implied dosage toxicity curve
plotDosageVsProbability <- function(alpha,beta,nCurves=100) {
  x.seq <- seq(-2,2,length=100)
  plot(NA,xlim=c(-2,2),ylim=c(0,1),ylab="Probability",xlab="Dosage",main="Dosage Toxicity Curve")
  for ( counter in 1:100 ) {
    b <- sample(length(alpha),1)
    prob.seq <- sapply(x.seq, function(x) logitInv(alpha[b] + beta[b]*x))
    lines(x.seq,prob.seq)
  }
}

# Griddy sampling method
samplePosteriorUsingGrid <- function(nSamples=10000, nBreaks=1000, plot=TRUE) {
  if ( plot ) par(ask=TRUE)
  grid.alpha <- seq( -5,10,length=nBreaks)
  grid.beta <- seq(-10,40,length=nBreaks)
  grid <- expand.grid(grid.alpha,grid.beta)
  logPOnGrid <- apply(grid, 1, function(params) logPosterior(params[1],params[2]))
  logPAlphaBetaGivenY <- matrix(logPOnGrid, nrow=length(grid.alpha))
  pAlphaBetaGivenY <- exp(logPAlphaBetaGivenY - max(logPAlphaBetaGivenY))  # Stabalize computations
  if ( plot ) contour(grid.alpha,grid.beta,pAlphaBetaGivenY,main="Like Figure 3.3(a)")  # Figure 3.3(a)
  pAlphaGivenY <- apply(pAlphaBetaGivenY, 1, sum)   # Marginalize over beta.
  plot(grid.alpha, pAlphaGivenY, type="l", main="Marginal posterior of alpha")
  plot(grid.beta, apply(pAlphaBetaGivenY, 2, sum), type="l", main="Marginal posterior of beta")
  alpha <- sample(grid.alpha, size=nSamples, prob=pAlphaGivenY, replace=TRUE)
  beta <- sapply(alpha, function(a) sample(grid.beta,1,prob=pAlphaBetaGivenY[grid.alpha==a,]))
  width.alpha <- grid.alpha[2]-grid.alpha[1]
  alpha <- alpha + runif(length(alpha),-width.alpha/2, width.alpha/2)
  width.beta <- grid.beta[2]-grid.beta[1]
  beta <- beta + runif(length(beta),-width.beta/2, width.beta/2)
  if ( plot ) plot(alpha,beta,xlim=c(-5,10),ylim=c(-10,40),main="Like Figure 3.3(b)")   # Figure 3.3(b)
  if ( plot ) plotDosageVsProbability(alpha,beta)  # Posterior dosage toxicity curve.
  if ( plot ) par(ask=FALSE)
  cbind(alpha,beta)
}

## Prior in Gelman.
prior <- function(alpha,beta) 1         # Unnormalized
logPrior <- function(alpha,beta) 0      # Unnormalized
plotDosageVsProbability(runif(1000,-5,10), runif(1000,-10,40))  # Prior dosage toxicity curve.

# Implied posterior on dosage and probability
samples <- samplePosteriorUsingGrid()

# LD50 calculations
ld50 <- -samples[,1]/samples[,2]
plot(density(ld50))

mean(samples[,2]>0)             # Prob("Drug is harmful" | data) = Prob(beta > 0 | data)
ld50.est <- mean(ld50[samples[,2]>0])  # Estimated dose for which the probability of death is 50%.
ld50.est
pis <- logitInv(samples[,1] + samples[,2]*ld50.est)
mean(pis)                       # Which doesn't equal 0.5, but it's close.

# Posterior predictive distribution of death for death at LD50.
y.posterior.predictive <- rbinom(length(pis),size=5,prob=pis)
mean(y.posterior.predictive)    # Which doesn't equal 2.5, but it's close.

# Note that this is "flatter" than a binomial distribution.  Why?
table(y.posterior.predictive)/length(y.posterior.predictive)
dbinom(0:5,size=5,prob=mean(pis))





# Make log prior density function on alpha and beta from prior samples
mkImpliedLogPrior <- function(alpha,beta,n=200,plot=TRUE) {
  if ( plot ) par(ask=TRUE)
  if ( plot ) plotDosageVsProbability(alpha,beta)             # Prior dosage toxicity curve.
  library(MASS)
  density <- kde2d(alpha,beta,n=n)
  if ( plot ) contour(density$x,density$y,density$z,xlab="alpha",ylab="beta",main="Prior Density")
  prior <- function(alpha,beta) {
    i <- which.min(abs(alpha - density$x))
    j <- which.min(abs(beta - density$y))
    density$z[i,j]
  }
  if ( plot ) par(ask=FALSE)
  function(alpha,beta) log(prior(alpha,beta))
}


####
## Alternative prior 1:
#  Let pi1 = Pr("Success" | dose = -0.5)
#  Let pi2 = Pr("Success" | dose =  0.0)
#  pi1       \sim Uniform(0,1)
#  pi2 | pi1 \sim Uniform(pi1,1)
####

x1 <- -0.5
x2 <-  0.0
pi1 <- runif(100000,0,1)
pi2 <- runif(length(pi1),pi1,1)
betaFromPrior <- ( logit(pi2) - logit(pi1) ) / ( x2 - x1 )
alphaFromPrior <- logit(pi1) - betaFromPrior*x1
logPrior <- mkImpliedLogPrior(alphaFromPrior,betaFromPrior)

# Implied posterior on dosage and probability
samples <- samplePosteriorUsingGrid()


####
## Alternative prior 2:
#  Let pi1 = Pr("Success" | dose = -0.5)
#  Let pi2 = Pr("Success" | dose =  0.0)
#  pi1 \sim Uniform(0,0.5)
#  pi2 \sim Uniform(0.5,0.9)
####

x1 <- -0.5
x2 <-  0.0
pi1 <- runif(100000,0,0.5)
pi2 <- runif(length(pi1),0.5,0.9)
betaFromPrior <- ( logit(pi2) - logit(pi1) ) / ( x2 - x1 )
alphaFromPrior <- logit(pi1) - betaFromPrior*x1
logPrior <- mkImpliedLogPrior(alphaFromPrior,betaFromPrior)

# Implied posterior on dosage and probability
samples <- samplePosteriorUsingGrid()


####
## Alternative prior 3:
#  Let pi1 = Pr("Success" | dose = -0.5)
#  Let pi2 = Pr("Success" | dose =  0.0)
#  p(pi1,pi2) \sim Normal(mean=0.2, sd=0.05) x Normal(mean=0.7, sd=0.05), where I{ 0 < pi1 < pi2 < 1}
####

x1 <- -0.5
x2 <-  0.0
pi1 <- rnorm(100000,mean=0.2,sd=0.05)
pi2 <- rnorm(100000,mean=0.7,sd=0.05)
which <- 0<pi1 & pi1<pi2 & pi2<1
pi1 <- pi1[which]
pi2 <- pi2[which]
betaFromPrior <- ( logit(pi2) - logit(pi1) ) / ( x2 - x1 )
alphaFromPrior <- logit(pi1) - betaFromPrior*x1
logPrior <- mkImpliedLogPrior(alphaFromPrior,betaFromPrior)

# Implied posterior on dosage and probability
samples <- samplePosteriorUsingGrid()