source("Metropolis Hastings - Random walk example.R", echo=F)
getwd()
setwd("C:/Users/Jaehwan M/Dropbox/Bayesian Statistics")
## example of good traceplot
set.seed(61)

post0 = mh(n=n, ybar=ybar, n_iter=4e3, mu_init=0, cand_sd=0.9)
library("coda")
traceplot(as.mcmc(post0$mu[-c(1:500)]))

start_indx = 1
nsim = length(post0$mu)
ylim = range(post0$mu)

pdf("animation/goodchain.pdf", width=9, height=4)
for (t in 1:nsim){
  plot(start_indx:(start_indx - 1 + t), post0$mu[1:t], type="l", ylim=ylim,
       xlim = c(start_indx, start_indx + nsim),
       xlab = "iterartion", ylab = expression(mu), main = "traceplot")
  Sys.sleep(0.001)
}
dev.off()
start_indx

set.seed(61)
post1 = mh(n=n, ybar=ybar, n_iter=1e3, mu_init=0.0, cand_sd=0.04)
coda::traceplot(as.mcmc(post1$mu[-c(1:500)]))

set.seed(61)
post1b = mh(n=n, ybar=ybar, n_iter=100e3, mu_init=0.0, cand_sd=0.04)
coda::traceplot(as.mcmc(post1b$mu))

##auotocorrelation.
autocorr.plot(as.mcmc(post0$mu))
autocorr.diag(as.mcmc(post0$mu))

autocorr.plot(as.mcmc(post1$mu))
autocorr.diag(as.mcmc(post1$mu))

str(post1b)
effectiveSize(as.mcmc(post1b$mu)) ##effective sample size
autocorr.plot(as.mcmc(post1b$mu), lag.max =500)

thin_interval = 400
thin_index = seq(from=400, 100e3, by =400)
head(thin_index)

par(mfrow=c(2,1))
traceplot(as.mcmc(post1b$mu))
traceplot(as.mcmc(post1b$mu[thin_index]))

autocorr.plot(as.mcmc(post1b$mu[thin_index]))
effectiveSize(post1b$mu[thin_index])
length(thin_index)
## effective sample size in the trimmed out chain == actual sample size
## because the the values are approximately uncorrelated

# effective sample size in MC.
# 1. how many independent samples you'd need to get the same info
# 2. length of chain you'd have left over if you removed iterations
#	or thinned the chain until you got rid of the autocorrelation.

effectiveSize(as.mcmc(post0$mu)) # effective sample size of nearly 1000, out of 4000.

raftery.diag(as.mcmc(post0$mu))
?raftery.diag
