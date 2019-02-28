# Suppose we are giving two students a multiple-choice exam with 40 questions,
# where each question has four choices. We don't know how much the students
# have studied for this exam, but we think that they will do better than just
# guessing randomly.

# 1) What are the parmaeters of interest?

# Ans. Parameters of interest are theta1 = true probability the first student
# will answer a question correctly, and theta2 = true probability the second
# student will answer a question correctly.

# 2) What is our likelihood?

# Ans. Likelihood is Binomial(40, theta), if we assume that each question is
# independent and that the probability a student gets each question right
# is the same for all questions for that student.

# 3) What prior should we use?

# Ans. The conjugate prior is a beta prior. Plot the density with dbeta.
theta = seq(0,1,.01)
plot(theta, dbeta(theta,1,1), type = "l") # default standard uniform prior = Beta(1,1)
plot(theta, dbeta(theta,4,2), type = "l") # mean 2/3
plot(theta, dbeta(theta,8,4), type = "l") # same mean, different effective sample size

# 4) What is the prior probability P(theta > .25)? P(theta > .5)? P(theta > .8)?

# Ans. Find probabilities using the pbeta function.
1 - pbeta(.25,8,4)
1 - pbeta(.5,8,4)
1 - pbeta(.8,8,4)
# 5) Suupose the 1st student gets 33 questions right. What is the posterior
#    distribution for theta1? P(theta1 > .25)? P(theta1 > .5)? P(theta1 > .8)?
#    What is a 95% posterior credible interval for theta1?

# Ans. Posterior distribution ~ Beta(8 + 33, 4 + 7) = Beta(41, 11)
# posterior probabilities.
1 - pbeta(.25, 41, 11) # P(theta1 > .25)
1 - pbeta(.5, 41, 11) # P(theta1 > .5)
1 - pbeta(.8, 41, 11) # P(theta1 > .8)

41/(41+11) # posterior mean
33/40 # MLE
## Posterior mean is somewhere in between the prior mean and MLE.
lines(theta, dbeta(theta,41,11))

plot(theta, dbeta(theta,41,11), type = "l") #posterior pdf
lines(theta, dbeta(theta, 8,4), lty = 2) #prior pdf

lines(theta, 44*dbinom(33, size = 40, p = theta), lty = 3) #likelihood

# 95% credible interval.
round(c(qbeta(.025, 41, 11), qbeta(.975, 41, 11)), 4)

# 6) Suupose the 2nd student gets 24 questions right. What is the posterior
#    distribution for theta2? P(theta2 > .25)? P(theta2 > .5)? P(theta2 > .8)?
#    What is a 95% posterior credible interval for theta2?

# Ans. Posterior ~ Beta(8 + 24, 4 + 16) = Beta(32, 20)

32/(32+20) #posterior mean
24/40 #MLE

plot(theta, dbeta(theta, 32, 20), type ='l')
lines(theta, dbeta(theta, 8, 4), lty = 2)
lines(theta, 44*dbinom(24,40,p=theta), lty=3)

1-pbeta(.25,32,20)
1-pbeta(.5,32,20)
1-pbeta(.8,32,20)

c(qbeta(.025,32,20),qbeta(.975,32,20)) #95% credible interval

# 7) What is the posterior probability that theta1 > theta2? i.e., that the
#    1st student has a better chance of getting a question right than the 2nd?

theta1 = rbeta(1000, 41, 11)
theta2 = rbeta(1000, 32, 20)
mean(theta1>theta2) # close to 1, but not exactly 1.


##########Quiz: Lesson 7#########
# Q5.posterior probability that еш < 0.5 if we observe the sequence (T,T,T,T).
theta3 = rbeta(10000, 1, 5)
round(mean(theta3 < 0.5),2)
pbeta(0.5, 1, 5)
1 - .5^5

# Q7. equal-tailed 95% credible interval for theta.
round(c(qbeta(.025,8,16),qbeta(.975,8,16)),2)

# Q8. The engineer tells you that the process is considered 
# promising and can proceed to another phase of testing 
# if we are 90% sure that the failure rate is less than .35.
# Calculate posterior probability P(еш < .35???x). 
# Should this new chemical pass?

# Ans8.
mean(rbeta(10000,8,16) < .35)
mean(rbeta(10000,8,16) < .35) < .9 ## The chemical shouldn't pass.

# Q9. five more samples to be tested. 
# These tests are conducted and none of them fail.

# Ans9.
mean(rbeta(10000,8,21) < .35)
mean(rbeta(10000,8,16) < .35) < .9  ## The chemical shouldn't pass.





# Note for other distributions:
# dgamma, pgamma, qgamma, rgamma
# dnorm(return density)
# pnorm(return cdf value at q)
# qnorm(return quantile at p, which is probability)
# rnorm(generate pseudorandom from N(0,1))
