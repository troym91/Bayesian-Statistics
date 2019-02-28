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

hist(theta, freq = F) #freq = F�� �����ν� count�� �ƴ� density�� ����)
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

# ���࿡ beta, gamma �� ���� ���� �Լ��� �ƴ϶� �̻��� �Լ��� ������ ��쿡��
# pgamma, qbeta ���� �Լ��� ���� ���Ѵ�. �׷��� ��� �ؾ��ϴ°�?
# Monte Carlo Estimation�� ����Ѵ�.