rm(list=ls())
set.seed(32)
m = 10000
a = 2
b = 1/3
theta = rgamma(n = m, shape = a, rate = b)
se_theta = sd(theta)/sqrt(m)
se_theta
2*se_theta
mean(theta) - 2*se_theta
mean(theta) + 2*se_theta

ind = theta < 5
mean(ind)
pgamma(5,a,b)
se_ind = sd(ind)/sqrt(m)
se_ind
2*se_ind
mean(ind) - 2*se_ind
mean(ind) + 2*se_ind

############################
# 1. Simulate phi_i from Beta(2,2)
# 2. Simulate y_i from Binom(10, phi_i)
m = 1e5

y = numeric(m)
head(y)
phi = numeric(m)
?numeric

for (i in 1:m){
  phi[i] = rbeta(1, shape1 = 2, shape2 = 2)
  y[i] = rbinom(1, size = 10, prob=phi[i])
}

# R tends to be very slow with loops.
# One way to speed it up : Vectorized code.

phi = rbeta(m, shape1 = 2, shape2 = 2)
y = rbinom(m, size = 10, prob = phi)

# If we're only interested in the marginal distribution of y,we can ignore the phi_s,
# and treat the draws of y as a sample of its marginal distribution.
# That marginal distribution of y will not be a binomial distribution, but a Beta-Binomial Distribution.
# Conditional on phi, y follows a Binomial distribution.
# But unconditionally, marginal distribution of y is not Binomial.

table(y)
table(y)/m
# Monte Carlo Approximation of the distribution of y(beta binomial)
plot(table(y)/m)
# Marginal Expected Value of y(Ignore phi variables)
mean(y)
