lg = function(mu, n ,ybar){
  mu2 = mu^2
  n * (ybar * mu - mu2 / 2) - log(1 + mu2)
}

mh = function(n, ybar, n_iter, mu_init, cand_sd){
  mu_out = numeric(n_iter)
  accpt = 0
  mu_now = mu_init
  lg_now = lg(mu = mu_now, n = n, ybar = ybar)
  
  for (i in 1:n_iter){
    mu_cand = rnorm(1, mean = mu_now, sd = cand_sd)
    
    lg_cand = lg(mu = mu_cand, n = n, ybar = ybar)
    lalpha = lg_cand - lg_now
    alpha = exp(lalpha)
    
    u = runif(1)
    if (u < alpha){
      mu_now = mu_cand
      accpt = accpt + 1
      lg_now = lg_cand
    }
    
    mu_out[i] = mu_now
  }
  list(mu = mu_out, accpt = accpt/n_iter)
}

y = c(1.2, 1.4, -0.5, 0.3, 0.9, 2.3, 1.0, 0.1, 1.3, 1.9)
y2 = c(-0.2, -1.5, -5.3, 0.3, -0.8, -2.2)

ybar = mean(y)
n = length(y)

ybar2 = mean(y2)
n2 = length(y2)

hist(y2, freq = FALSE, xlim = c(-6, 3))
points(y2, rep(0,n2))
points(ybar2,0, pch = 19)
curve(dt(x, df = 1), lty = 2, add = T) # t distribution with d.f 1 --> plot with dashed linetype.


### posterior sampling
set.seed(43)
post = mh(n = n2, ybar = ybar2, n_iter = 1e3, mu_init = 300, cand_sd = 0.5)
str(post)
install.packages("coda")
library("coda")

traceplot(as.mcmc(post$mu))
post$mu_keep = post$mu[-c(1:400)]
mean(post$mu_keep)

traceplot(as.mcmc(post$mu_keep))
plot(density(post$mu_keep), xlim = c(-1,3))
curve(dt(x, df = 1), lty=2, add = T)
points(ybar, 0.0, pch=19)

sd(post$mu_keep)


###Quiz4 Q8.

#draw candidate
theta_cand = rnorm(1, 0, 10)
#evaluate log of g with the candidate
lg_cand = lg(theta = theta_cand)
#evaluate 

