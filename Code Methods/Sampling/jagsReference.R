# jagsReference.R

# Beta-Binomial Model
mod14<-'
model{
  for(i in 1:68){
    x[i] ~ dbinom(theta[n.stat[i]],n[i])
  }
  for(i in 1:40){
  theta[i]~ dbeta(1,1)
  }
}
'

mod14b<-'
model{
  for(i in 1:68){
    x[i] ~ dbinom(theta,n[i])
  }
  theta~ dbeta(2,1)
}
'
n.stat<-as.numeric(as.factor(fermi$Station))
data.jags<-list(x=fermi$x,n=fermi$n,n.stat=n.stat)
writeLines(mod14,"mod14.txt")
writeLines(mod14b,"mod14b.txt")
parms <- c('theta')
fourteen.sim <- jags(data=data.jags,inits=NULL,
                     parameters.to.save=parms,model.file="mod14.txt",
                     n.iter=40000,n.burnin=2000,n.chains=1,n.thin=1) 
# Simple Model
data.jags<-list(x=fermi$x,n=fermi$n)
fourteenb.sim <- jags(data=data.jags,inits=NULL,
                      parameters.to.save=parms,model.file="mod14b.txt",
                      n.iter=55000,n.burnin=5000,n.chains=1,n.thin=1) 
sims<-as.mcmc(fourteen.sim)
chains<-as.matrix(sims)
DICb<-fourteenb.sim$BUGSoutput$DIC
models.hier<-c("Hierarchical","Simple")
models.hier[which.min(c(DIC,DICb))]

# Gamma-Gamma Model
############
#Exponential
############
# JAGS
exp.mod<-'
  model{
    for(i in 1:n){
      jets[i] ~ dexp(lambda)
    }
    lambda ~ dgamma(5,1)
  }
  '
data.jags<-c('jets','n')
writeLines(exp.mod,"mod.txt")
parms <- c('lambda')
exp.sim <- jags(data=data.jags,inits=NULL,parameters.to.save=parms,
                model.file="mod.txt",n.iter=55000,n.burnin=5000,
                n.chains=1,n.thin=1) 
sims<-as.mcmc(exp.sim)
chains<-as.matrix(sims)
# We have a complete conditional for 
# lambda, so the acceptance should be 1 after
# burnin.
length(unique(chains[,2]))/50000
# Mixing Plot
plot(chains[,2],type='l')
# BIC
BIC.E<-sum(-2*dexp(jets,mean(chains[,2]),
                   log=TRUE))+log(length(jets))*1
##########
# Weibull
##########
# JAGS
w.mod<-'
  model{
    for(i in 1:n){
      jets[i] ~ dweibull(beta,lambda)
    }
    beta ~ dgamma(1,1)
    lambda ~ dgamma(1,1)
  }
  '
data.jags<-c('jets','n')
writeLines(w.mod,"mod.txt")
parms <- c('beta','lambda')
w.sim <- jags(data=data.jags,inits=NULL,parameters.to.save=parms,
              model.file="mod.txt",n.iter=55000,n.burnin=5000,
              n.chains=1,n.thin=1) 
sims<-as.mcmc(w.sim)
chains<-as.matrix(sims)
# Acceptance Rate- Gibbs Sampler so should be 1.
apply(apply(chains,2,unique),2,length)[-2]/50000
# Mixing Plots
par(mfrow=c(1,2))
plot(chains[,1],type='l')
plot(chains[,3],type='l')
#BIC
BIC.W<-sum(-2*dweibull(jets,mean(chains[,1]),
                       mean(chains[,3])^(-mean(chains[,1])),
                       log=TRUE))+log(length(jets))*2
############
# Lognormal
############
# JAGS
L.mod<-'
  model{
    for(i in 1:n){
      jets[i] ~ dlnorm(mu,tau)
    }
    mu ~ dnorm(0,0.1*tau)
    tau.t ~ dgamma(0.1,0.1)
    tau = 1/tau.t
  }
  '
data.jags<-c('jets','n')
writeLines(L.mod,"mod.txt")
parms <- c('mu','tau')
L.sim <- jags(data=data.jags,inits=NULL,parameters.to.save=parms,
              model.file="mod.txt",n.iter=55000,n.burnin=5000,
              n.chains=1,n.thin=1) 
sims<-as.mcmc(L.sim)
chains<-as.matrix(sims)
#Gamma
#########
# JAGS
gam.mod<-'
  model{
    for(i in 1:n){
      fail[i] ~ dgamma(mu,tau)
    }
    mu ~ dgamma(5,1)
    tau ~ dgamma(5,1)
  }
  '

data.jags<-c('fail','n')
writeLines(gam.mod,"mod.txt")
parms <- c('mu','tau')
gam.sim <- jags(data=data.jags,inits=NULL,parameters.to.save=parms,
                model.file="mod.txt",n.iter=55000,n.burnin=5000,
                n.chains=1,n.thin=1) 

sims<-as.mcmc(gam.sim)
chains<-as.matrix(sims)

# Multinomial
"model {
for (i in 1:ndat){
y[i] ~ dbin(p[i],n[i]);
logit(p[i]) = b0[n.fc[i],n.reg[i]] + b1[n.fc[i],n.reg[i]]*Total_Percent_Trucks[i] + b2[n.fc[i],n.reg[i]]*SPEED_LIMIT[i] + b3[n.fc[i],n.reg[i]]*Num_Lanes[i] + b4[n.fc[i],n.reg[i]]*VMT[i]
}
for (j in 1:uni.fc){
for (k in 1:uni.reg){
b0[j,k] ~ dnorm(0,1)
 b1[j,k] ~ dnorm(0,1)
 b2[j,k] ~ dnorm(0,1)
 b3[j,k] ~ dnorm(0,1)
 b4[j,k] ~ dnorm(0,1)
}
}
}
"

"model {
# Likelihood
for(i in 1:N){
n[i] <- freq[i, 1] + freq[i, 2] + freq[i, 3] + freq[i, 4] + freq[i, 5];
freq[i, 1:5] ~ dmulti(pi[i, 1:5], n[i]);
phi[i,1] <- 1;
# For identifiability
log(phi[i, 2]) <- beta0[1] + (beta1[1]*AADT_2012[i]);
log(phi[i, 3]) <- beta0[2] + (beta1[2]*AADT_2012[i]);
log(phi[i, 4]) <- beta0[3] + (beta1[3]*AADT_2012[i]);
log(phi[i, 5]) <- beta0[4] + (beta1[4]*AADT_2012[i]);
for(j in 1:N.CLASS){
pi[i, j] <- phi[i, j]/sum(phi[i, ]);
# Normalising
fitted[i, j] <- pi[i, j]*n[i];
# Fitted values #
}
#
}
# Priors
for (k in 1:4){
beta0[k]~dnorm(0, 0.01);
beta1[k]~dnorm(0, 0.01);
}
# Scalars
for (k in 1:4){
or1[k] <- exp(beta1[k]);
}
}"