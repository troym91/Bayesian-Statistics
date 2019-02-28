set.seed(34)

rm(list=ls())
n = 100
x = numeric(n)
x
class(x)

for (i in 2:n){
  x[i] = rnorm(1, mean = x[i-1], sd =)
}

plot.ts(x)
?plot.ts

#transtion matrix
Q = matrix(c(0.0, 0.5, 0.0, 0.0, 0.5,
             0.5, 0.0, 0.5, 0.0, 0.0,
             0.0, 0.5, 0.0, 0.5, 0.0,
             0.0, 0.0, 0.5, 0.0, 0.5,
             0.5, 0.0, 0.0, 0.5, 0.0),
           nrow = 5, byrow= T)


Q %*% Q # Matrix multiplication in R. This is Q^2.
(Q %*% Q)[1,3] #P(X(t+2)=3???X(t)=1)

### Staionary Distribution ###
## Suppose we want to know the probability distribution of the your secret number in the distant future,
## say p(X(t+h)|X(t)) where h is a large number. Let¡¯s calculate this for a few different values of h.

# h = 5 steps in the future
Q5 = Q %*% Q %*% Q %*% Q %*% Q 
round(Q5,3)

# h= 10 steps in the future
Q10 = Q5 %*% Q5 
round(Q10, 3)

# h= 30 steps in the future
Q30 = Q
for (i in 2:30){
  Q30 = Q30 %*% Q
}
round(Q30,3)

# h= 100 steps in the future
Q100 = Q
for (i in 2:100){
  Q100 = Q100 %*% Q
}
round(Q100, 3)
# Interpretation
# If you run the Markov chain for a very long time, (i.e. as h approaches to infinity) 
# the probability that you will end up in any particular state is 1/5=.2 for each of the five states. 
# These (.2,.2,.2,.2,.2) (long-range probabilities) are equal to what is called the stationary distribution of the Markov chain.
# Once a chain reaches its stationary distribution, the stationary distribution will remain the distribution of the states thereafter.


###Continuous example
## Transition distribution: P(X(t+1)|X(t)=x(t))=N(??x(t),1) where -1 < ?? < 1.
## i.e. the prob. dist. for the next state is Normal with mean(?? times the current state) and variance 1.
## As long as ?? is between -1 and 1, then the stationary distribution will exist for this model.
## Let's simulate this chain for ?? = -0.6

set.seed(38)

n = 10000
x = numeric(n)
phi = -0.6

for (i in 2:n){
  x[i] = rnorm(1, mean = phi*x[i-1], sd = 1)
}

plot.ts(x)

## The theoretical stationary distribution for this chain is normal with mean 0 and variance 1/(1?????^2) = 1.562
## Let¡¯s look at a histogram of our chain and compare that with the theoretical stationary distribution.

hist(x, freq = FALSE)
curve(dnorm(x, 0, 1/(1-phi^2)), col='red', add=T)
legend("topright", legend = "theoretical stationary\ndistribution", col="red", lty=1, bty="n")
## It appears that the chain has reached the stationary distribution. 
## Therefore, we could treat this simulation from the chain like a Monte Carlo sample from the stationary distribution, 
## a normal with mean 0 and variance 1.562.

## Most posterior distributions we will look at are continuous,
## --> MCMC will be similar to this example.
