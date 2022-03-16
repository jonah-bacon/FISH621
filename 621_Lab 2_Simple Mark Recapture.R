#==================================================================================================
#Project Name: FISH 621 Estimation of Fish Abundance - Lab 2
#Creator: Curry James Cunningham, College of Fisheries and Ocean Sciences, UAF
#Date: 1.21.2022
#
#Purpose: To explore simple (single-release, single-recovery) mark-recapture
#           estimators and the their uncertainty, and related aspects of
#           probability theory.
#
#
#
#==================================================================================================

# Exercise 1: Required Packages ================================================

# In the first section of this lab we will load standard packages, and 
#   what are likely to be new packages

# STANDARD PACKAGES: 
#   (install any packages for which you have not already done so):
library(tidyverse)
library(dplyr)
library(ggthemes)

# NEW PACKAGES:

# The "recapr" package provides functionality for estimating and simulating
#   mark-recapture data and estimators for single-release single-recapture 
#     experiments
#   
# install.packages("recapr")
library(recapr)

# Exercise 2: Uncertainty in Abundance Estimates with Known Observation Probability ========

# To think about uncertainty, let's return to our simple example of abundance
#   estimation if we know the probability of observing individuals, perhaps
#     from a previous study.

# We can think again about Curry counting flowers in a field (sounds relaxing).

# The observation probability is p=0.5
p <- 0.5

# Again if we observe (n) individuals 
n <- 160

# Then our abundance estimate N_hat = n/p
N_hat <- n/p
N_hat

# We will recall from Week 1 Lecture, and Lab 1 that we can use
#   the Binomial distribution to quantify the relative probability of different
#     population sizes

# Define trial abundance estimates to explore (N_hat)'s
trial.N <- seq(from=250, to=400, by=1)

# Calculate the probability of alternative "true" abundances N_hat
prob.trial.N <- dbinom(x=n, size=trial.N, prob=p)

plot(x=trial.N, y=prob.trial.N, type="l", col="blue",
     xlab="Abundance Estimate (N_hat)",
     ylab="Probability of Alternative Abundances")
grid()
polygon(x=trial.N, y=prob.trial.N, col=rgb(0,1,1, alpha=0.25))

# The maximum likelihood estimate (MLE) for N_hat is n/p=320
n/p

points(x=n/p, y=dbinom(x=n, size=n/p, prob=p), pch=21, bg="yellow")

# However beyond this we can of course use this same probability theory 
#   to make probabilistic statements about the range of values we expect to see

# We can ask what is the range of abundances within which we expect the
#   true (but unknown) abundance to lie with 95% certainty, given our data?

# This is a 95% CI for N_hat

# 95% = 100*(1-alpha), so alpha is 0.05

# To find this we need the values for N_hat that retain alpha/2 =0.025 probability
#   or 2.5% in each tail.

# But how to do this?

# A simple way is to just use out profile of N_hat and extend it out
#   to ensure we are getting a suitably wide range. 
trial.N <- seq(from=1, to=1000, by=1)

# Calculate the probability of alternative "true" abundances N_hat
prob.trial.N <- dbinom(x=n, size=trial.N, prob=p)

# Standardize the probability to sum to 1
# The calculate the cumulative probability
cumProb.trial.N <- cumsum(prob.trial.N/sum(prob.trial.N))

plot(x=trial.N, y=cumProb.trial.N)

# Now find the values for N_hat that represent a probability of 0.025, and 0.975
#   i.e. the bound at 2.5% and 97.5% (such that the middle 95%)
#     of probability is retained.

# We can use our max and which() functions in R to specify that we want to 
#   find the largest values of N_hat that agree with a criteria < some probability

# Lower (2.5%) bound
lb.N_hat <- trial.N[max(which(cumProb.trial.N<=0.025))]
lb.N_hat
# Cumulative probability at this level
lb.N_hat.cumProb <-  cumProb.trial.N[max(which(cumProb.trial.N<=0.025))]

# Upper (97.5%) bound
ub.N_hat <- trial.N[max(which(cumProb.trial.N<=0.975))]
ub.N_hat
# Cumulative probability at this level
ub.N_hat.cumProb <-  cumProb.trial.N[max(which(cumProb.trial.N<=0.975))]


# Let's plot these bounds on our cumulative probability to make sure it looks correct

plot(x=trial.N, y=cumProb.trial.N, type="l", col="blue",
     xlab="Abundance Estimate (N_hat)",
     ylab="Cumulative of Alternative Abundances",
     xlim=c(250,400), lwd=3,
     main="Approximate 95% CI Points")
grid()

# Add lower bound
segments(x0=lb.N_hat, y0=-1, x1=lb.N_hat, y1=lb.N_hat.cumProb,
         col="red", lty=1)
segments(x0=-1, y0=lb.N_hat.cumProb, x1=lb.N_hat, y1=lb.N_hat.cumProb,
         col="red", lty=1)
points(x=lb.N_hat, y=lb.N_hat.cumProb, pch=21, bg="red")

# Add upper bound
segments(x0=ub.N_hat, y0=-1, x1=ub.N_hat, y1=ub.N_hat.cumProb,
         col="red", lty=1)
segments(x0=-1, y0=ub.N_hat.cumProb, x1=ub.N_hat, y1=ub.N_hat.cumProb,
         col="red", lty=1)
points(x=ub.N_hat, y=ub.N_hat.cumProb, pch=21, bg="red")

# OK, that looks correct. Now let's plot these confidence bounds on our
#   profile of alternative values for N_hat, to make it easier to see
#     our 95% CI

# Now lets 

plot(x=trial.N, y=prob.trial.N, type="l", col="blue",
     xlab="Abundance Estimate (N_hat)",
     ylab="Cumulative of Alternative Abundances",
     xlim=c(250,400),
     main="Approximate 95% CI")
grid()
polygon(x=trial.N, y=prob.trial.N, col=rgb(0,1,1, alpha=0.25))

# Add points and lines describing the approx. 95% CI

# First calculate the (non cumulative) probability density of the N_hat values
#   representing the lower and upper bounds

lb.N_hat.prob <- dbinom(x=n, size=lb.N_hat, prob=p)
lb.N_hat.prob

ub.N_hat.prob <- dbinom(x=n, size=ub.N_hat, prob=p)
ub.N_hat.prob

# Upper 95% CI
segments(x0=ub.N_hat, y0=-1, 
         x1=ub.N_hat, y1=ub.N_hat.prob,
         col="red", lwd=2)
# Lower 95% CI
segments(x0=lb.N_hat, y0=-1, 
         x1=lb.N_hat, y1=lb.N_hat.prob,
         col="red", lwd=2)

# Shade this region.
polygon(x=c(lb.N_hat,lb.N_hat:ub.N_hat,ub.N_hat),
        y=c(0,dbinom(x=n, size=lb.N_hat:ub.N_hat, prob=p),0),
        col=rgb(1,0,0, alpha=0.2))

# CHALLENGE ====================================================================
# PLEASE REPEAT THIS EXERCISE DRAWING FIGURES THAT HIGHLIGHT THE 50% AND 90% CI
#   RESPECTIVELY










# Exercise 3: Simple Mark-Recapture Estimators =================================
# In lecture this week we have discussed several of our simple mark-recapture
#   abundance estimators, applicable in the context of an experiment
#     Where some number of individuals are captured and marked at time t1, then
#       released and some number of individuals are captured at time t2 and
#         inspected for marks. 

# 500 individuals are captured at t1, and marked
n1 <- 500

# 400 individuals are captured at t2, 
n2 <- 400

# Of these 500, 100 are marked
m2 <- 100

# All of our estimators based on this type of experiment are based on the 
#   same assumption:
# The proportion of the total population that is marked (n1/N) should be 
#   equal to the proportion of individuals marked in our sample at time t2 (m2/m2)

# Petersen Estimator ===========================================================

# p1 is the proportion of individuals marked at time t1 (n1/N), or probability of
#   marking.
# Of course we don't know N, but we can approximate p1 as (m2/n2)
p1_hat <- m2/n2
p1_hat

# p2 is our probability of being captured at time t2 (n2/N), but again is 
#   a function of our unknown population size (N). 
# So we approximate p2 as (m2/n1)
p2_hat <- m2/n1
p2_hat

# The Petersen estimator is the most simplistic

N_pete <- (n1*n2)/m2
N_pete

# Our Petersen abundance estimate is N_hat = 2,000

# This is of course equivalent to:
n2/(m2/n1)

# or the number caught at time t2 divided by the probability of capture in 
#   time t2

n2/p2_hat

# Estimate of variance in m2 from Peterson estimator
var_m2_pete <- m2*(N_pete/(N_pete-1))*(1-(n1/N_pete))*(1-(n2/N_pete))
var_m2_pete

# However, we will recall from lecture that our Petersen estimator is BIASED
#   due to the fact the m2 appears in the denominator. 

# Chapman Estimator ============================================================
# Chapman (1951) recommended an alternative estimator, now called the
#   "Chapman Estimator"
# Which is unbiased if n1 + n2 > N

# The Chapman estimator is...
N_chap <- ((n1+1)*(n2+1))/(m2+1) - 1
N_chap

# So fairly close to Petersen, just a bit lower.
N_pete
N_chap

# Variance in the Chapman estimator
var_N_chap <- ((n1+1)*(n2+1)*(n1-m2)*(n2-m2)) / ((m2+1)^2 * (m2+2))
var_N_chap

# Estimate CV
cv_N_chap <- sqrt(var_N_chap)/N_chap
cv_N_chap

# As previously discussed Chapman Estimator is unbiased if n1 + n2 > N
#   and has a small bias otherwise, given by: E(N*) - true N

# Let's substitute in our Chapman estimate (N*) for the true N to get a sense of
#   potential bias

bias.chap <- -N_chap*exp( -( (n1+1)*(n2+1) )/N_chap )
bias.chap

# Bailey Estimator =============================================================

# The third estimator we discussed was the Bailey Estimator, or "Bailey's Binomial Model"
#   This estimator is appropriate in cases were sampling WITH REPLACEMENT
#     occurs
# i.e. you can't tell if an individual observed at time t2 has already been observed.

# Instead of modelling the probability of observing m2 recaptures
#   based on the hypergeometric distribution, Bailey's model leverages
#    the binomial distribution, where the probability is (p1) the probability of being marked

# Estimator under Bailey's model
N_bailey <- (n1*(n2+1)) / (m2+1)
N_bailey

# Variance of Bailey estimator
var_N_bailey <- (n1^2 * (n2+1) * (n2-m2)) / ((m2+1)^2 * (m2+2))
var_N_bailey


# We can compare the abundance estimates from all three estimators:
N_pete
N_chap
N_bailey

# As well as the variance of the Chapman and Bailey Estimators:
var_N_chap
var_N_bailey

# CHALLENGE ====================================================================

# PLEASE EXPLORE AND DESCRIBE THE ABUNDANCE ESTIMATES AND UNCERTAINTY FROM THESE
#   ESTIMATORS FOR SEVERAL SCENARIOS.

# SCENARIO 1: n1=1000, n2=100, m2=20
# SCENARIO 2: n1=1000, n2=1000, m2=200
# SCENARIO 3: n1=100,  n2=1000, m2=10
# SCENARIO 4: n1=100,  n2=1000, m2=50
# SCENARIO 5: n1=100,  n2=1e4, m2=50

# PLEASE BE PREPARED TO PRESENT RESULTS FOR THE CLASS AND DISCUSS!

# Exercise 4: Hypergeometric Distribution ======================================

# We can recall from lecture that when the assumptions of our simple mark-recapture
#   experiment are satisfied then the hypergeometric distribution 
#     approximates the probability of observing m2 recaptures, given 
#       n1 individuals tagged in time t1, n2 total individuals are sampled at
#         time t2, and N is our total population size. 

# More generally the hypergeometric distribution describes the probability
#   of some number of "successes" or the observed number of items that possess
#     a property, given (a) some number of items in the population that have the
#        property and (b) some number of items in the population that 
#         DO NOT have the property, and given some number a specific sample size.

# R includes the hypergeometric distribution with the family of functions related
#   to this distribution

?dhyper

# PLEASE INSPECT THE DOCUMENTATION FOR THIS FAMILY OF FUNCTIONS

# x= is the number of "successes" or the observed number of items that possess
#     a property

# m= is number of items in the population that have the that property

# n= is the number of items that DO NOT have the property

# k= is the sample size

# We will recall the standard dxxx(), pxxx(), qxxx(), and rxxx()
#   syntax for probability functions

#   dhyper(x, m, n, k) - Probability of observing some number of "successes"

# What is the probability of observing x=100 white balls, out of a sample
#   of k=250 balls, from an urn that contains m=600 white balls, and n=800 black
#     balls?
dhyper(x=100, m=600, n=800, k=250)

# What about drawing x=200 white balls from this same urn?
dhyper(x=200, m=600, n=800, k=250)

# What about drawing x=150 white balls from this same urn?
dhyper(x=110, m=600, n=800, k=250)

# The expected number of white balls drawn (x) is m/(m+n)*k
m <- 600
n <- 800
k <- 250

m/(m+n)*k

# Notice this is not an integer value...

# We can show that this is in fact that this is the value with the highest
#   probability

trial.white.balls <- 80:140
trial.white.balls

trial.prob.white.balls <- dhyper(x=trial.white.balls, m=m, n=n, k=k)

plot(x=trial.white.balls, y=trial.prob.white.balls, type="l",
       xlab="Number of White Balls",
       ylab="Probability of Drawing x White Balls")
grid()
polygon(x=trial.white.balls, y=trial.prob.white.balls, col=rgb(0,1,1, alpha=0.25))
points(x=trial.white.balls, y=trial.prob.white.balls, pch=21, bg="red")

abline(v=floor(m/(m+n)*k), col=rgb(0,0.5,0, alpha=0.75), lwd=2)

#   phyper(q, m, n, k) - Cumulative probability of observing less than or equal
#                           to some number of successes

trial.white.balls <- 80:140

trial.cumProb.white.balls <- phyper(q=trial.white.balls, m=m, n=n, k=k)

plot(x=trial.white.balls, y=trial.cumProb.white.balls, type="l",
     xlab="Number of White Balls",
     ylab="Probability of Drawing Less Than x White Balls")
grid()
points(x=trial.white.balls, y=trial.cumProb.white.balls, pch=21, bg="red")

abline(v=floor(m/(m+n)*k), col=rgb(0,0.5,0, alpha=0.75), lwd=2)

#   qhyper(p, m, n, k) - The number of "successes" expected with some probability p

value <- qhyper(p=0.2, m=m, n=n, k=k)
value

plot(x=trial.white.balls, y=trial.cumProb.white.balls, type="l",
     xlab="Number of White Balls",
     ylab="Probability of Drawing Less Than x White Balls")
grid()
points(x=trial.white.balls, y=trial.cumProb.white.balls, pch=21, bg="red")

# WHY IS THIS POINT OFFSET FROM THE OTHERS WE SIMULATED FROM THE CUMULATIVE
#   DISTRIBUTION FUNCTION?

# Plot expected number of white balls, with p=0.2
segments(x0=-1, y0=0.2, x1=value, y1=0.2, col="blue", lty=2)
segments(x0=value, y0=-1, x1=value, y1=0.2, col="blue", lty=2)
points(x=value, y=0.2, pch=21, bg="blue")



#   rhyper(nn, m, n, k) - Simulates random draws from the hypergeometric distribution
#                           condition.

par(mfrow=c(2,2), mar=c(2,3,3,1))

draws <- rhyper(nn=10, m=m, n=n, k=k)
draws
hist(draws, main="10 draws")

draws <- rhyper(nn=100, m=m, n=n, k=k)
draws
hist(draws, main="100 draws")

draws <- rhyper(nn=1000, m=m, n=n, k=k)
draws
hist(draws, main="1,000 draws")

draws <- rhyper(nn=1e4, m=m, n=n, k=k)
draws
hist(draws, main="10,000 draws")

dev.off()

# OK, now that we are thoroughly familiar with application of the 
#   hypergeometric distribution implementation in R, we might ask:
#       HOW DOES THIS RELATE TO OUR SIMPLE MARK-RECAPTURE EXPERIMENT ????


# Let's exchange are balls in an urn example for MARKED and UNMARKED individuals

# Lets 

# Number of captured and marked individuals at time t1
n1 <- 50

# Number of individuals sampled at time t2
n2 <- 40

# Number of marked individuals 
m2 <- 10

# Our Peterson estimator for the true population size N is:
N <- (n1*n2)/m2
N

# Using our dhyper function to evaluate the probability of observing different
#   numbers of marked individuals (m2), conditional on the number of individuals
#     captured and marked at time t1 (n1), the number of individuals sampled
#       and checked for marks at time t2 (n2), and the true population size N

# But first, we need to fit our mark-recapture parameters within the context
#   of our dhyper(x, m, n, k) function arguments

?dhyper

# x = m2 (number of MARKED individuals observed at time t2)
# m = n1 (number of MARKED individuals in the population)
# n = N-n1 (number of UNMARKED individuals in the population)
# k = n2 (number of individuals inspected at time t2 for marks)

dhyper(x=m2, m=n1, n=N-n1, k=n2)

# We can prove to ourselves that this function is working correctly
#   by putting our mark-recapture parameters back into the equation
#     we saw in Lecture 3

# To replicate the equation we will need the choose() function
?choose

# Where choose(x, y) describes all of the different combinations of
#   choosing y items from a population of x different items

# Taking a simple example: We have 3 items a, b, c
# We can evaluate the number of different combinations of pairs that we can select

choose(3,2)

# a and b
# a and c
# b and c

# So, the probability of observing m2 marked individuals our of a sample of n2,
#   given n1 marked individuals and a total population of N is:
choose(n1, m2) * choose(N-n1, n2-m2) / choose(N, n2)
dhyper(x=m2, m=n1, n=N-n1, k=n2)

# Or putting things in more tangible terms...
# Number of individuals within the population that were NOT MARKED
non.cap <- N-n1
non.cap

# Number of individuals observed at time t2 WITHOUT MARKS
non.mark <- n2-m2
non.mark

# Probability of observing m2 marked individuals
choose(n1, m2) * choose(non.cap, non.mark) / choose(N, n2)


# Profiling with the hypergeometric distribution ===============================

# Now that we are familiar with how to integrate our data and unknown parameters
#   within the dhyper() function, let's do some exploration

# First, let's calculate the probability of observing different numbers of
#   marked fish in our recapture event (t2) using our original values,
#     and conditional on a true population size of N=200
n1
n2
m2
N

# We can use a range from 1 to the number of fish sampled in n2
trial.m2 <- 1:n2
trial.m2

trial.m2.prob <- dhyper(x=trial.m2, m=n1, n=N-n1, k=n2)

# And plot this distribution
plot(x=trial.m2, y=trial.m2.prob, type="l",
     xlab="Number of Markes Observed (m2)",
     ylab="Probability of Observing",
     main="Hypergeometric")
grid()
polygon(x=trial.m2, y=trial.m2.prob, col=rgb(0,1,1, alpha=0.25))
points(x=trial.m2, y=trial.m2.prob, pch=21, bg="red")

# Our most likely (highest probability) number of observed marked individuals
#   is p1_hat * n2 (est. probability of marking * number observed at t2) 

p1_hat <- m2/n2
p1_hat

most_likely_m2 <- p1_hat*n2
most_likely_m2

# Add this point to our figure!
points(x=most_likely_m2, y=dhyper(x=most_likely_m2, m=n1, n=N-n1, k=n2),
       pch=21, bg="yellow")

# But, what if we sampled with replacement?

# Let's create a similar profile under Bailey's Binomial model
?dbinom

trial.m2.prob_bailey <- dbinom(x=trial.m2, size=n2, prob=n1/N)

plot(x=trial.m2, y=trial.m2.prob_bailey, type="l",
     xlab="Number of Markes Observed (m2)",
     ylab="Probability of Observing",
     main="Bailey's Binomial")
grid()
polygon(x=trial.m2, y=trial.m2.prob_bailey, col=rgb(0,1,1, alpha=0.25))
points(x=trial.m2, y=trial.m2.prob_bailey, pch=21, bg="red")

# And add a different colored point for our most likely number of recaptures (m2)

# The expected value from the binomial model is: E(m2) = n2*p1 = n2*(n1/N)
exp_m2_bailey <- n2*(n1/N)

points(x=exp_m2_bailey, y=dbinom(x=exp_m2_bailey, size=n2, prob=n1/N),
         pch=21, bg="yellow")


# Now, lets try this again but this time, we will specify more realistic values
#   for our mark-recapture experiment, for a large population
# Number of captured and marked individuals at time t1
n1 <- 500

# Number of individuals sampled at time t2
n2 <- 600

# Number of marked individuals 
m2 <- 100

# Our Peterson estimator for the true population size N is:
N <- (n1*n2)/m2
N

# We can use a range from 1 to the number of fish sampled in n2
trial.m2 <- 50:150
trial.m2

trial.m2.prob <- dhyper(x=trial.m2, m=n1, n=N-n1, k=n2)

# And plot this distribution
plot(x=trial.m2, y=trial.m2.prob, type="l",
     xlab="Number of Markes Observed (m2)",
     ylab="Probability of Observing",
     main="Hypergeometric")
grid()
polygon(x=trial.m2, y=trial.m2.prob, col=rgb(0,1,1, alpha=0.25))
points(x=trial.m2, y=trial.m2.prob, pch=21, bg="red")

# Our most likely (highest probability) number of observed marked individuals
#   is p1_hat * n2 (est. probability of marking * number observed at t2) 

p1_hat <- m2/n2
p1_hat

most_likely_m2 <- p1_hat*n2
most_likely_m2

# Add this point to our figure!
points(x=most_likely_m2, y=dhyper(x=most_likely_m2, m=n1, n=N-n1, k=n2),
       pch=21, bg="yellow")

# But, what if we sampled with replacement?

# Let's create a similar profile under Bailey's Binomial model
?dbinom

trial.m2.prob_bailey <- dbinom(x=trial.m2, size=n2, prob=n1/N)

plot(x=trial.m2, y=trial.m2.prob_bailey, type="l",
     xlab="Number of Markes Observed (m2)",
     ylab="Probability of Observing",
     main="Bailey's Binomial")
grid()
polygon(x=trial.m2, y=trial.m2.prob_bailey, col=rgb(0,1,1, alpha=0.25))
points(x=trial.m2, y=trial.m2.prob_bailey, pch=21, bg="red")

# And add a different colored point for our most likely number of recaptures (m2)

# The expected value from the binomial model is: E(m2) = n2*p1 = n2*(n1/N)
exp_m2_bailey <- n2*(n1/N)

points(x=exp_m2_bailey, y=dbinom(x=exp_m2_bailey, size=n2, prob=n1/N),
       pch=21, bg="yellow")


# Profiling N from Simple Mark-Recapture =======================================

# Now that we are comfortable exploring the probability of observing different
#   values for observed marks (m2), conditional on the true population size,
#     let's turn this around and profile across our potential values for population
#       size given our actual observations from the experiment
trial.N_hat <- 2000:4200
trial.N_hat.prob <- dhyper(x=m2, m=n1, n=trial.N_hat-n1, k=n2)


plot(x=trial.N_hat, y=trial.N_hat.prob, type="l",
     xlab="Trial Population Sizes (N_hat)",
     ylab="Probability of True Value",
     main="Hypergeometric")
grid()
polygon(x=trial.N_hat, y=trial.N_hat.prob, col=rgb(0,1,1, alpha=0.25))

# Let's add our Petersen and Chapman estimators
N_hat.pete <- (n1*n2)/m2
N_hat.pete

N_hat.chap <- ((n1+1)*(n2+1))/(m2+1) - 1
N_hat.chap

# Add lines for these two estimators
abline(v=N_hat.pete, col="red")
abline(v=N_hat.chap, col="darkgreen")

# Now what if we our recapture sampling was WITH REPLACEMENT
trial.N_hat.prob_bailey <- dbinom(x=m2, size=n2, prob=n1/trial.N_hat)


plot(x=trial.N_hat, y=trial.N_hat.prob_bailey, type="l",
     xlab="Trial Population Sizes (N_hat)",
     ylab="Probability of True Value",
     main="Bailey's Binomial Model")
grid()
polygon(x=trial.N_hat, y=trial.N_hat.prob_bailey, col=rgb(0,1,1, alpha=0.25))

# And add in the Bailey Estimator
N_hat.bailey <- (n1*(n2+1)) / (m2+1)
N_hat.bailey
abline(v=N_hat.bailey, col="red")

# CHALLENGE ====================================================================
# PLEASE CREATE PROFILES FOR N_HAT ASSUMING BOTH WITH AND WITHOUT REPLACEMENT
#   ASSUMING THE NUMBER OF RECAPTURED INDIVIDUALS WAS m2= 50, 100, 400, 500

# PLEASE PLOT THE PROFILES AND ESTIMATORS AS WELL AS THE APPROXIMATE 95% CI
#   FOR CHAPMAN AND BAILEY: N_hat +/- 1.96*SE = N_hat +/- 1.96*sqrt(var_hat)


# Exercise 5: Sample Design For Simple Mark-Recapture ==========================

# In this portion of the lab our challenge is identify trade-offs in the 
#   variance of our Chapman and Bailey mark-recapture estimators, as we change
#     our sample design (n1 and n2).

# Let's, assume our population has N=1,000 individuals.

# Please explore a range of possible levels of effort for tagging n1 and
#   recapture n2, under the assumption it costs $2.00 to capture and tag
#     a fish at time t1 and $1.00 to capture a fish at time t2 and determine if 
#       it is marked. 

# Explore how the CV in your estimators change across ranges of n1 and n2, 
#   as a function of the associated total cost. 
#   We will discuss your results at the end of the lab. 




# SOLUTION =====================================================================








