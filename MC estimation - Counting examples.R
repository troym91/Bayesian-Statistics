rm(list= ls())
set.seed(32)
# Gamma dist. could represent the posterior dist. of theta
# if our data(y's) came from a Poisson(theta)
# then theta is the conjugate gamma prior.
m = 100
a = 2
b = 1/3
b
theta = rgamma(n = m, shape = a, rate = b) # shape: 2, rate: 1/3
head(theta)
tail(theta)

hist(theta, freq = F) #freq = F로 함으로써 count가 아닌 density를 구함)
curve(dgamma(x, shape = a, rate = b), col='blue', add = TRUE)
?dgamma
sum(theta)/m
mean(theta)
a/b # true mean of the gamma(2, 1/3)
# our Monte Carlo Approximation was okay, but not great.
# Can improve approx by simulating more values.
m = 10000
mean(theta) # MC Estimate of mean for gamma(2, 1/3)
# Monte Carlo Estimate of variance
var(theta) 
# Theoretival variance
a/(b^2)
## MC Estimates and Theoretical values of parameters are very close.
ind = theta < 5
head(ind)
head(theta)
# MC prob of theta being less than 5
mean(ind)
# prob of theta being less than 5
pgamma(q=5, shape = a, rate = b)
# MC estimate of 90th percentile
quantile(theta, probs = 0.9)
# True 90th percentile of Gamma(2, 1/3)
qgamma(p = 0.9, shape = a, rate = b)

# 만약에 beta, gamma 와 같은 좋은 함수가 아니라 이상한 함수가 나오는 경우에는
# pgamma, qbeta 같은 함수를 쓰지 못한다. 그러면 어떻게 해야하는가?
# Monte Carlo Estimation을 사용한다.
