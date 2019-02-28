rm(list=ls())

install.packages("car")
library("car")
data("Leinhardt")
?Leinhardt

head(Leinhardt)
str(Leinhardt)

pairs(Leinhardt)

plot(infant ~ income, data = Leinhardt)
hist(Leinhardt$infant)
hist(Leinhardt$income)


Leinhardt$loginfant = log(Leinhardt$infant)
Leinhardt$logincome = log(Leinhardt$income)

plot(loginfant ~ logincome, data = Leinhardt)


### Modeling
lmod = lm(loginfant ~ logincome, data = Leinhardt)
summary(lmod) # Iran, Haiti, Laos, Nepal Missing for infant variable.

dat = na.omit(Leinhardt) # 4 countries ommitted

install.packages("rjags")
library("rjags")

mod1_string = " model {
  # Likelihood
  for (i in 1:n){
    y[i] ~ dnorm(mu[i], prec)
    mu[i] = b[1] + b[2]*log_income[i]
  }
  # prior for beta
  for (j in 1:2) {
    b[j] ~ dnorm(0.0, 1/1e6) # mean 0, variance 1e6
  }
  # prior for variance (sig2)
  prec ~ dgamma(5/2, 5*10/2)
  sig2 = prec
  sig = sqrt(sig2)
} "

set.seed(72)
data1_jags = list(y=dat$loginfant, n=nrow(dat), log_income=dat$logincome)

params1 = c("b", "sig")

inits1 = function() {
  inits = list("b"=rnorm(2,0,100), "prec"=rgamma(1,1,1))
}

library(coda)
mod1 = jags.model(textConnection(mod1_string), data=data1_jags,
                  inits=inits1, n.chains=3)

update(mod1, 1000) #give a burnin period for 1000 iterations.

mod1_sim = coda.samples(model = mod1, variable.names = params1, n.iter=5e3)

mod1_csim = do.call(rbind, mod1_sim)


### Convergence
plot(mod1_sim)
gelman.diag(mod1_sim)
autocorr.diag(mod1_sim)
effectiveSize(mod1_sim) ## out of 15,000 = 5e3 * 3
summary(mod1_sim)
