rm(list=ls())
### sig1 and sig2 follows inverse-gamma distribution. Take N = 1000.
### sig1 and sig2 are conjugate priors for sigma^2 assuming normal distribution.
z1 <- rgamma(1000, 16.5, 6022.9)
sig1 <- 1/z1
muB <- rnorm(1000, 609.3, sqrt(sig1/27.1))
quantile(x=muB, probs = c(.025, .975))


200 + (29/2)*403.1 + (3/(2*30.1))*(122.8**2)
(.1*500+30*622.8)/30.1

z2 <- rgamma(1000, 18, 6796.44)
sig2 <- 1/z2
muA <- rnorm(1000, 622.39, sqrt(sig2/30.1))
mean(muA > muB)
