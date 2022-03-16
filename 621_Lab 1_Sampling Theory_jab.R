#==================================================================================================
#Project Name: FISH 621 Estimation of Fish Abundance - Lab 1
#Creator: Curry James Cunningham, College of Fisheries and Ocean Sciences, UAF
#Date: 3.10.17
#
#Purpose: To explore basic elements of sampling theory and simple abundance estimators
#
#
#
#==================================================================================================

# Exercise 1: Loading Required Packages ========================================

# Over the course of this semester we will rely on several packages
#   either containing specific datasets or statistical tools.

# There is an R package associated with the textbook "Estimating Animal Abundance"
#   by Borchers, Buckland, and Zucchini.
# The package is called WiSP (Wildlife Simulation Package)

# This package isn't on the CRAN repository, so we will have to install it from
#   github, using the following call:.

remotes::install_github("DistanceDevelopment/WiSP")

# If you don't already have the remotes package installed, please uncomment and 
#   do the following:
install.packages("remotes")
remotes::install_github("DistanceDevelopment/WiSP")

# We will also rely on the RMark package by Jeff Laake
install.packages("RMark")

# Finally, let's install the unmarked package by Richard Chandler, 
#   Andy Royal, Rebecca Hutchinson, Marc Kery, Auriel Fournier, and others. 
install.packages("unmarked")

# Exercise 2: Probability Distributions in R ===================================

# As you will no doubt recall from prior courses or your own research,
# we have easy access to a wide range of probability functions in R.

# The rxxx() family of functions (rnorm(), rbinom(), ect.) allow us to 
#   quickly and easily simulate random draws from a particular distribution 

# The first argument n= represents the number of values we wish to simulate,
#   conditional on the parameters of the distribution

# We can simulate 10 values from a normal distribution with a mean of 100, and standard deviation of 10
x <- rnorm(n=10, mean=100, sd=10)
# We can visualization the distribution of these values
hist(x)
# And some summary statistics
mean(x)
sd(x)
summary(x)
# RE-RUN THIS CODE MULTIPLE TIMES TO SEE THE VARIABILITY AMONG RANDOM SETS OF TEN DRAWS

# We can see how the distribution looks when we simulate a larger number of values (n=1000)
#   from the same distribution.
y <- rnorm(n=1000, mean=100, sd=10)
hist(y)
mean(y)
sd(y)
# REPEAT THIS CODE MULTIPLE TIMES AS WELL
# What do you see?
# A larger sample size, results in a better approximation of the underlying distribution.

# Probability functions starting with “d”, like dnorm(x, …) will return the probability density 
#   for a value x, given the distribution. 

# The other two distributions we will routinely draw upon in this course
#   are the binomial (number of successes our of some number of trials)
#     and the Poisson (counts of things per unit time or space).
# Let's explore these a bit. 

# Binomal distribution
?rbinom

# The parameters for the binomial distribution are the number of trials (size=)
#   and the probability of our outcome (prob=)

# For simplicity we can think of a simple example where we are flipping a coin
#   size=100 times, and recording the number of "heads" we get out of these 100
#   trials. Here we have a fair coin where the probability of our outcome
#     is prob=0.5

# We can repeat our experiment with 100 coin flips, n=10 times and 
#   return the expected number of "successes" or "heads" our of our 100 flips
rbinom(n=10, size=100, prob=0.5)

# This returns a vector including the number of "successes" or "heads"
#   out of 100 flips, for each trial

# Of course each time we simulate our 10 experiments we will get a slightly 
#   different result
rbinom(n=10, size=100, prob=0.5)
rbinom(n=10, size=100, prob=0.5)
rbinom(n=10, size=100, prob=0.5)
rbinom(n=10, size=100, prob=0.5)

# We can of course look at a histogram to visualize the range of successes among
#   our 10 experiments
heads <- rbinom(n=10, size=100, prob=0.5)
hist(heads)

# That is pretty ugly/uninformative with n=10 experiments, so let's 
#   repeat the experiment 10,000 times

heads.2 <- rbinom(n=1e4, size=100, prob=0.5)
hist(heads.2)

# Now that is a bit more informative!

# But what if we have an unfair coin, where the probability of heads is higher
#  prob=0.75?

heads.3 <- rbinom(n=1e4, size=100, prob=0.75)
hist(heads.3)

# Or we can plot the proportion of heads
hist(heads.3/100)

# But what happens when we increase the number of coin flips (size=) 
dev.off()

bin.1 <- rbinom(n=1e4, size=10, prob=0.75)
bin.2 <- rbinom(n=1e4, size=100, prob=0.75)
bin.3 <- rbinom(n=1e4, size=1000, prob=0.75)

par(mfrow=c(3,1))
hist(bin.1, main="size=10")
hist(bin.2, main="size=100")
hist(bin.3, main="size=1,000")

# Well obviously we get more successes the more times we flip our coin.

# This is because the expected value increases as a function of 
#   n (i.e. size=)

# E(x) = np, or in r terms E(x) = size*prob

# Let's convince ourselves of this...

size <- 100
prob <- 0.75

# Expected value
size*prob

# Now let's generate several datasets and and take the mean of our 
#   random draws
mean(rbinom(n=10, size=size, prob=prob))
mean(rbinom(n=10, size=size, prob=prob))
mean(rbinom(n=10, size=size, prob=prob))

# Closer
mean(rbinom(n=100, size=size, prob=prob))
mean(rbinom(n=100, size=size, prob=prob))
mean(rbinom(n=100, size=size, prob=prob))

# Very close
mean(rbinom(n=1000, size=size, prob=prob))
mean(rbinom(n=1000, size=size, prob=prob))
mean(rbinom(n=1000, size=size, prob=prob))

# Near exact
mean(rbinom(n=1e5, size=size, prob=prob))
mean(rbinom(n=1e5, size=size, prob=prob))
mean(rbinom(n=1e5, size=size, prob=prob))

# But what about the variance? You may be asking yourself
#   We recall from lecture that for the Binomial distribution
#     var(x) = np(1-p), or in r terms size*prob*(1-prob)

# Our variance should be
size*prob*(1-prob)

# Let's check with our random draws
var(rbinom(n=10, size=size, prob=prob))
var(rbinom(n=10, size=size, prob=prob))
var(rbinom(n=10, size=size, prob=prob))

# Closer
var(rbinom(n=100, size=size, prob=prob))
var(rbinom(n=100, size=size, prob=prob))
var(rbinom(n=100, size=size, prob=prob))

# Very close
var(rbinom(n=1000, size=size, prob=prob))
var(rbinom(n=1000, size=size, prob=prob))
var(rbinom(n=1000, size=size, prob=prob))

# Near exact
var(rbinom(n=1e5, size=size, prob=prob))
var(rbinom(n=1e5, size=size, prob=prob))
var(rbinom(n=1e5, size=size, prob=prob))

# Ok, now that we are comfortable generating random draws from 
#   the Binomial distribution, let's turn our attention to Poisson
# Remember counts of things per unit time, depending on the 
#   rate parameter (lambda)

?rpois

rpois(n=10, lambda=20)

par(mfrow=c(2,2))
hist(rpois(n=10, lambda=20))
hist(rpois(n=100, lambda=20))
hist(rpois(n=1000, lambda=20))
hist(rpois(n=1e4, lambda=20))

# We will recall from lecture that both the expected value and
#   variance are equal to the rate parameter

# EXPECTED VALUE
# Close
mean(rpois(n=10, lambda=20))
mean(rpois(n=10, lambda=20))
mean(rpois(n=10, lambda=20))
mean(rpois(n=10, lambda=20))

# Exact
mean(rpois(n=1e4, lambda=20))
mean(rpois(n=1e4, lambda=20))
mean(rpois(n=1e4, lambda=20))
mean(rpois(n=1e4, lambda=20))

# VARIANCE
# Close
var(rpois(n=10, lambda=20))
var(rpois(n=10, lambda=20))
var(rpois(n=10, lambda=20))
var(rpois(n=10, lambda=20))

# Exact
var(rpois(n=1e4, lambda=20))
var(rpois(n=1e4, lambda=20))
var(rpois(n=1e4, lambda=20))
var(rpois(n=1e4, lambda=20))

# Next let's explore the effect of changing lambda, our expected
#   count (and variance in counts) per unit time or area

# We will use common x-axis limits for ease of visualization
x.lim <- c(-5,15)

par(mfrow=c(2,3), mar=c(4,3,2,2))
hist(rpois(n=1e4, lambda=0.1), xlim=x.lim, main="")
hist(rpois(n=1e4, lambda=1), xlim=x.lim, main="")
hist(rpois(n=1e4, lambda=2), xlim=x.lim, main="")
hist(rpois(n=1e4, lambda=5), xlim=x.lim, main="")
hist(rpois(n=1e4, lambda=7), xlim=x.lim, main="")
hist(rpois(n=1e4, lambda=10), xlim=x.lim, main="")

# The dxxx() family of probability funcitons allow us to calculate the
#   of a given value or values, conditional on the parameters of 
#   a probability distribution. 

# Let's look at the difference in probabiliity density of two values,
#   under a normal distribution with mean=100 and standard deviation of 10.

# First let's look at a value of 90
val <- 90
prob <- dnorm(val, mean=100, sd=10)
prob

# And next a value closer to the true mean
val.2=101
prob.2 <- dnorm(val.2, mean=100, sd=10)
prob.2

# Let's visualize this
dev.off()

test <- 50:150
probs <- dnorm(x=test, mean=100, sd=10)

plot(x=test, y=probs, type="l",
     xlab="value", ylab="Probability Density")
# grid(col="black")
polygon(x=test, y=probs, col=rgb(0,0.5,1, alpha=0.5))

# Value of 90
segments(x0=val, y0=-100, x1=val, y1=prob, col="orange", lwd=2, lty=2)
segments(x0=-100, y0=prob, x1=val, y1=prob, col="orange", lwd=2, lty=2)
points(x=val, y=prob, pch=21, bg="orange")

# Value of 101
segments(x0=val, y0=-100, x1=val.2, y1=prob.2, col="red", lwd=2, lty=2)
segments(x0=-100, y0=prob.2, x1=val.2, y1=prob.2, col="red", lwd=2, lty=2)
points(x=val.2, y=prob.2, pch=21, bg="red")

# The same is true of our friends the Binomal and Poisson

# What is the probability of getting 20 heads out of 100 trials, with a fair coin?

?dbinom

dbinom(x=20, size=100, prob=0.5)
# Quite small... assuming our coin in fair

# What about 45?
dbinom(x=45, size=100, prob=0.5)

# 49?
dbinom(x=49, size=100, prob=0.5)

# What about 49.2 heads?
dbinom(x=49.2, size=100, prob=0.5) # Right, that IS NOT POSSIBLE!

# Let's visualize this
dev.off()

test <- 1:100
probs <- dbinom(x=test, size=100, prob=0.5)

plot(x=test, y=probs, type="l",
     xlab="value", ylab="Probability Density")
# grid(col="black")
polygon(x=test, y=probs, col=rgb(0,0.5,1, alpha=0.5))

vals <- c(20, 45, 49)
cols <- c("green","orange","red")

for(i in 1:length(vals)) {
  val <- vals[i]
  # Calculate probability
  prob <- dbinom(x=val, size=100, prob=0.5)
  
  segments(x0=val, y0=-100, x1=val, y1=prob, col=cols[i], lwd=2, lty=2)
  segments(x0=-100, y0=prob, x1=val, y1=prob, col=cols[i], lwd=2, lty=2)
  points(x=val, y=prob, pch=21, bg=cols[i])

}



# We can ask equivalent questions of the Poisson distribution...


# What is the probability of counting 100 fish over the weir in an hour,
#   if we expect a rate of 200/hour.

dpois(x=100, lambda=200)

# What about 170 fish?
dpois(x=170, lambda=200)

# We can of ourse easily visualize the Poisson pdf
# By simulating draws from it...
hist(rpois(n=1e3, lambda=200))

# Or calculating the explicit probability density across a range 
#   of values, given the rate parameter (lambda=200)
pois.vals <- 150:250
pois.probs <- dpois(x=pois.vals, lambda=200)

plot(x=pois.vals, y=pois.probs, type="l", col="red",
       ylab="Probability Density")
# grid(col="black")
polygon(x=pois.vals, y=pois.probs, col=rgb(1,0,0, alpha=0.25))

# 

# We can use the cumulative probability density CDF to calculate the area under the curve
#   to the left of (less than) a given value.

# Probability functions starting with “p”, like pnorm(q, …) will return the cumulative probability density (area under the curve) for all values less q=, given the distribution.

val <- 90
pnorm(q=val, mean=100, sd=10)

# And next a value closer to the true mean
val.2=101
pnorm(q=val.2, mean=100, sd=10)

# To visualize the CUMULATIVE probability density of a range of values
#   we can create a vector of trial values and calculate the cumulative probability
vals <- 50:150
probs <- pnorm(q=vals, mean=100, sd=10)

# And plot the probability of each value
plot(x=vals, y=probs,
     ylab="Cumulative Probability", xlab="Value",
     main="Normal Distribution with mean=100, sd=10")

# And overlay the values of our original 
abline(v=val, col="red")
abline(v=val.2, col='blue')

# By adjusting the type= argument in the base plot() function we can change
#   what is plotted 
plot(x=vals, y=probs, type="l",
     ylab="Cumulative Probability", xlab="Value",
     main="Normal Distribution with mean=100, sd=10")

# And overlay the values of our original 
abline(v=val, col="red")
abline(v=val.2, col='blue')

plot(x=vals, y=probs, type="h",
     ylab="Cumulative Probability", xlab="Value",
     main="Normal Distribution with mean=100, sd=10")

# And overlay the values of our original 
abline(v=val, col="red")
abline(v=val.2, col='blue')

# The cumulative probability distribution is useful in the context of
#   our other standard distributions.
# Recalling our fish passing a weir example at an expected rate (lambda)
#   of 200 fish/hour.
# We can ask the question: What is the probability of seeing LESS THAN
#   170 fish in an hour?

ppois(q=170, lambda=200)

# What is the probability of seeing less than 240 fish/hour
ppois(q=240, lambda=200)

# But what if we want to calculate the probability over a range?

# For a continuous distribution, this is the integral (area under the curve) between two values.

# We can accomplish this by subtracting the cumulative probability of the
# smaller value from the cumulative probability of the larger value,
#   for the random variable.

# What is the probability of values between 75 and 95 under a normal
#   distribution with mean 100 and standard deviation of 10

pnorm(q=95, mean=100, sd=10) - pnorm(q=75, mean=100, sd=10)

# For a discrete distribution like the Poisson, this amounts
#   to summing the probabilities of values within the range
#     so functional equivalent to taking the integral across the range
#       for a continuous distribution.

# What is the probability of seeing 100-150 fish pass the weir in an hour,
#   given our expected value of 200 fish/hour
ppois(q=150, lambda=200) - ppois(q=100, lambda=200)

# What is the probability of passing 190-225 fish/hour, given an expected rate
#   of 200/hour
ppois(q=225, lambda=200) - ppois(q=190, lambda=200)

# These expected fish passage calculation are of course conditional on our
#  expected value for the rate, and that our probability truely follows a
#   Poisson distribution, where the variance and expected value are equal.

# For a given system where fish move upstream in clusters or groups,
#   which is not uncommon, we might expect an alternative distribution to 
#     better represent process.  negative binomial????
#   But more on that in the coming weeks. #spoilerAlert



# Exercise 3: Binomial Likelihood: Known Detection Probability =================

# In order to familiarize ourselves with the principles of maximum likelihood
#   estimation and the simple abundance estimator we discussed in lecture, 
#   we begin by exploring the binomial distribution.

# So if we recall from class, our simple abundance estimator is:
# N_hat=n/p

# Where:
#   N_hat is our abundance estimate
#   n is our sample size (the number we count during one 20-minute foray 
#                           into our flower field)
#   p is our detection probability (or observation probability)

# Assuming we only detect p=0.5 or 50% of flowers in a field on average
#   with one observer during a 20-minute interval, and there are truly N=324
#     flowers in our field, we can calculate the probability of observing
#       different numbers of flowers (n)

p <- 0.5
N <- 324

trial.n <- 100:225
prob.trial.n <- dbinom(x=trial.n, size=N, prob=p)

dev.off()

plot(x=trial.n, y=prob.trial.n, type="l", col="blue",
       xlab="Number of Flowers Detected (n)",
       ylab="Probability of Detecting n Flowers")
# grid(col="black")
polygon(x=trial.n, y=prob.trial.n, col=rgb(0,0,1, alpha=0.25))

# We should expect to observe (detect) N*p=162 flowers on average during a
#   sampling event
N*p

points(x=N*p, y=dbinom(x=N*p, size=N, prob=p), pch=21, bg="yellow")

# But the wonderful thing about probability distributions, is that we can turn
#   this into a likelihood

# If n is our data, and N is a parameter we want to estimate,
#   and again for some reason we know p=0.5,
#     we can quantify the likelihood for different values of the unknown 
# population size. 

# Lets say we detect n=25 flowers (our data):
n <- 25

# What is the probability of different values for our total population size
#   for flowers in our field, given that we know the true value for p=0.5

# To ask that question we can profile across different values for N_hat
N_hat.trial <- 1:100

probs.N_hat <- dbinom(x=n, size=N_hat.trial, prob=p)

# Plot out our profile
plot(x=N_hat.trial, y=probs.N_hat, type="l",
       xlab="Possible Total Population Sizes (N_hat)",
       ylab="Probability of Population Sizes",
       main="p=0.5")
grid(col="black")
polygon(x=N_hat.trial, y=probs.N_hat, col=rgb(0,0,1, alpha=0.25))

# But what happens if our detection probability is actually p=0.75
#   i.e. we detect a much large proportion of the total flowers in the field
#     on any give pass?

N_hat.trial <- 1:100

probs.N_hat <- dbinom(x=n, size=N_hat.trial, prob=0.75)

# Plot out our profile
plot(x=N_hat.trial, y=probs.N_hat, type="l",
     xlab="Possible Total Population Sizes (N_hat)",
     ylab="Probability of Population Sizes",
     main="p=0.75")
grid(col="black")
polygon(x=N_hat.trial, y=probs.N_hat, col=rgb(0,0,1, alpha=0.25))


# PLEASE EXPLORE WHAT HAPPENS AS YOU REDUCE p= 
#   UNDER THE ASSUMPTION THAT WE OBSERVE A SMALLER PROPORTION OF FLOWERS
    # WITH EACH PASS.

p <- c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
N_hat.trial <- 1:200
probs.N_hat <- matrix(data = NA, nrow = 200, ncol = 8)

for (i in 1:length(p)) {
  probs.N_hat[,i] <- dbinom(x=n, size=N_hat.trial, prob=p[i])
}

# GENERATE A FIGURE COMPARING YOUR PROFILES UNDER DIFFERENT OBS. PROBABILITY
#   VALUES AND THEIR IMPACT ON YOUR ESTIMATE OF POPULATION SIZE TO SHARE
#     WITH THE CLASS.

# Plot out our profile
plot(x=N_hat.trial, y=probs.N_hat[,8], type="l",
     xlab="Possible Total Population Sizes (N_hat)",
     ylab="Probability of Population Sizes") +
  lines(x=N_hat.trial, y=probs.N_hat[,1]) +
  lines(x=N_hat.trial, y=probs.N_hat[,2]) +
  lines(x=N_hat.trial, y=probs.N_hat[,3]) +
  lines(x=N_hat.trial, y=probs.N_hat[,4]) +
  lines(x=N_hat.trial, y=probs.N_hat[,5]) + 
  lines(x=N_hat.trial, y=probs.N_hat[,6]) +
  lines(x=N_hat.trial, y=probs.N_hat[,7])
legend(x = 150, y = 0.25, legend = c("0.9", "0.8", "0.7", "0.6", "0.5", "0.4", "0.3", "0.2"),
       fill = c(9,8,7,6,5,4,3,2), title = "Detection probability")
polygon(x=N_hat.trial, y=probs.N_hat[,8], col=9)
polygon(x=N_hat.trial, y=probs.N_hat[,7], col=8)
polygon(x=N_hat.trial, y=probs.N_hat[,6], col=7)
polygon(x=N_hat.trial, y=probs.N_hat[,5], col=6)
polygon(x=N_hat.trial, y=probs.N_hat[,4], col=5)
polygon(x=N_hat.trial, y=probs.N_hat[,3], col=4)
polygon(x=N_hat.trial, y=probs.N_hat[,2], col=3)
polygon(x=N_hat.trial, y=probs.N_hat[,1], col=2)

# Exercise 4: Simple Random Sampling ===========================================

# The theoretical basis of simple random sampling is, well simple.

# For some number of possible samples within our sample frame N, we sample
#   n of these with equal probability AND WITHOUT REPLACEMENT. 

# Lets begin with a theoretical example.

# We will pretend we a lake with N=1,000 fish, each having a different length
#   and we sample a subset of these fish and measure them (n).

# The beauty of R is that we can easily create our own N=1,000 population of
#   fish. We will create a population that has a mean length of 100 mm, and a
#     standard deviation of 20.

N <- 1000

set.seed(101)
lengths <- rnorm(n=N, mean=100, sd=20)

lengths

# Here is our distribution of length for these 1,000 fish
hist(lengths, main="Histogram of Fish Length",
     col="royalblue")

# OK, next we can go to our lake and randomly sample 100 fish
n <- 100

#   Under simple random sampling we should have equal probability of sampling
#     each fish, so the probability of sampling any individual fish is:
n/N

# But how to generate a random sample?
#   Thankfully there is a function in R
?sample

# Our set of unique fish IDs from which to sample is
IDs <- 1:N
head(IDs)

# Let's generate a random sample without replacement from this set
samp <- sample(x=IDs, size=n, replace=FALSE)
samp
# samp is a vector of the unique fish IDs we have randomly sampled. 

# In reality, unless we had tagged these fish individual disk tags with numbers
#   1-1,000 we wouldn't actually generate unique IDs, but equivalently 
#     we could drag a net through the water until we got 100 which we
#       retained and measured. 

# So our lengths for this sample of 100 fish are:
len.samp <- lengths[samp]

# Here we are subsetting our complete "lengths" vector for only those fish 
#   that were sampled

# And we can have a look at the histogram of our sampled lengths, relative 
#   to our complete (but unobserved) length distribution
dev.off()

par(mfrow=c(2,1), mar=c(2,4,2,1))
hist(lengths, main="Population Lengths",
       col="royalblue",
       xlim=c(0,200))
hist(len.samp, main="Sample Lengths",
       col="chartreuse",
       xlim=c(0,200))

# Ok, let's begin calculating attributes of the true population and 
#   our sample

# Population mean: mu
pop.mean <- (1/N)*sum(lengths)
pop.mean

mean(lengths) #Same!

# Sample mean: y_bar
samp.mean <- (1/n)*sum(len.samp)
samp.mean

mean(len.samp) #Same

# NOTE: The sample mean (y_bar) is an unbiased estimator of the population mean (mu)

# Finite-population variance: sigma^2
pop.var <- (1/(N-1))*sum((lengths-pop.mean)^2)
pop.var

var(lengths) # Same!

# NOTE: as "lengths" is a vector, and pop.mean is a single value, this
#   subtraction will be done element-wise.
test <- 1:5
val <- 2
test-val

# Sample variance: s^2
samp.var <- (1/(n-1))*sum((len.samp-samp.mean)^2)
samp.var

var(len.samp) # Same!

# Next, let's calculate the variance of the estimator: var(y_bar)

var.y_bar <- ((N-n)/N)*(pop.var/n)
var.y_bar

# And, we can calculate an UNBIASED ESTIMATOR of 
#   this variance: var_hat(y_bar)
est.var.y_bar <- ((N-n)/N)*(samp.var/n)
est.var.y_bar

# Unbiased, but not perfect!

# From this we can calculate the standard error (SE) as the square
#   root of the estimated variance of our sample mean
se.y_bar <- sqrt(est.var.y_bar)
se.y_bar

# Ok, now we have walked through the calculations, but let's
#  convince ourselves that these estimators are unbiased. 

# We will do so by generating many random samples from our 
#   population of fish lengths. 

# We can write a quick function to calculate our sampling statistics

#' Function to calculate sampling statistics
#'
#' @param samps Samples
#' @param N Size of population (total number of sampling units)
#'
#' @return out: A list of sampling statistics

samp_stats <- function(samps=NULL, N=NULL) {
  
  # First, to make is function generalizable let's calculate the 
  #   sample size
  n <- length(samps)
  
  # Sample mean: y_bar
  samp.mean <- (1/n)*sum(samps)
  # Sample variance: s^2
  samp.var <- (1/(n-1))*sum((samps-samp.mean)^2)
  # And, we can calculate an UNBIASED ESTIMATOR of 
  #   this variance: var_hat(y_bar)
  est.var.y_bar <- ((N-n)/N)*(samp.var/n)
  # Standard error (SE) of our sample mean
  se.y_bar <- sqrt(est.var.y_bar)
  
  # Return a list of calculated values
  out <- NULL
  out$samp.mean <- samp.mean
  out$samp.var <- samp.var
  out$est.var.y_bar <- est.var.y_bar
  out$se.y_bar <- se.y_bar
  return(out)
}

# Lets test out this function
samp_stats(samps=len.samp, N=N)

# Now that we have that function in our back pocket, we can have some
#   real fun!

# Let's repeat our sampling experiment 50 times, and see if our estimates
#   are truly unbiased.

n.exp <- 50

# To store our outputs from the 50 experiments, lets create a series of vectors
exp.samp.mean <- vector(length=n.exp)
exp.samp.var <- vector(length=n.exp)
exp.est.var.y_bar<- vector(length=n.exp)
exp.se.y_bar<- vector(length=n.exp)

i <- 1
for(i in 1:n.exp) {
  # Generate sample of lengths
  trial.len <- lengths[sample(x=1:N, size=n, replace=FALSE)]
  print(trial.len)
  # Calculate sampling statistics
  out <- samp_stats(samps=trial.len, N=N)
  
  
  # Save statistics to vectors
  exp.samp.mean[i] <- out$samp.mean
  exp.samp.var[i] <- out$samp.var
  exp.est.var.y_bar[i] <- out$est.var.y_bar
  exp.se.y_bar[i] <- out$se.y_bar
}

# Lets visualize our sample means from across our 50 sampling events
#   We will delineate the true population mean in Red
dev.off()

par(mfrow=c(2,1), mar=c(3,3,1,1))

plot(exp.samp.mean, pch=21, bg="blue")
abline(h=pop.mean, col="red")

hist(exp.samp.mean)
abline(v=pop.mean, col="red")

# Result: YAY! It appears our estimator unbiased. 


# OK, next lets think about the estimated variance of our estimator
#   We can first examine the range of our (purportedly) UNBIASED estimator for 
#     the variance of our sample mean across our 50 sampling events
dev.off()

hist(exp.est.var.y_bar)
summary(exp.est.var.y_bar)

# Now, let's compare this with the true variance in our sample means across 
#   our 50 experiments
var(exp.samp.mean)   

# Result: It appears that our estimate estimate of the variance in our sample
#            mean (y_bar), is pretty close to the variability we see around
#               the true population mean (mu) when we conduct repeat experiments.

# From this we have affirmed the supposition that these estimators are unbiased.

# And that from a single sample we can get a fairly robust, although imperfect,
#   estimate of the population mean, and we can do a pretty good job 
#      of quantifying how uncertain our sample mean is as an estimator for the 
#        population mean. 

# PLEASE USE THE FUNCTION WE HAVE CREATED TO EXPLORE HOW THE PRECISION
#   IN OUR ESTIMATE OF AVERAGE FISH SIZE CHANGES ACROSS A WIDE RANGE OF 
#     SAMPLE SIZES (n)

len.samp <- lengths[sample(x = N, size = 100, replace = FALSE)]
samp_stats(samps=len.samp, N=N)

# SIMULATE A RANGE OF SAMPLING EVENTS ACROSS A RANGE OF SAMPLE SIZES FROM 5 TO 200.

samp.size <- seq(5,200,5)

simul.mat <- matrix(data = NA, nrow = 40, ncol = 4)
for (i in 1:length(samp.size)) {
  # Generate sample of lengths
  trial.len <- lengths[sample(x=1:N, size=samp.size[i], replace=FALSE)]
  print(trial.len)
  # Calculate sampling statistics
  out <- samp_stats(samps=trial.len, N=N)
  
  
  # Save statistics to vectors
  simul.mat[i,1] <- out$samp.mean
  simul.mat[i,2] <- out$samp.var
  simul.mat[i,3] <- out$est.var.y_bar
  simul.mat[i,4] <- out$se.y_bar
}

dev.off()

plot(x = samp.size, y = simul.mat[,1], pch = 16,
     xlab = "Sample size",
     ylab = "Sample Mean")

plot(x = samp.size, y = simul.mat[,2], pch = 16,
     xlab = "Sample size",
     ylab = "Sample Variance")

plot(x = samp.size, y = simul.mat[,3], pch = 16,
     xlab = "Sample size",
     ylab = "Estimator Variance")

plot(x = samp.size, y = simul.mat[,4], pch = 16,
     xlab = "Sample size",
     ylab = "SE of sample mean (y_bar)")

# Exercise 5: SRS - Alaskan Caribou ============================================

# The Alaskan caribou aerial survey we discussed in lecture this week
#   presents a fun test case for simple random sampling. 

# As a reminder, 1 mile wide transects were flown.
#   n=15 transects were flown out of a possible N=286.
# So, 15 miles out of the 286 mile wide region of the Arctic Coastal Plain.

# Our data (count of caribou per transect) were
y <- c(1,50,21,98,2,36,4,29,7,15,86,10,21,5,4)

# Our number observations is
n <- length(y)
n

# Total number of survey units in our sample frame
N <- 286

# Sample mean: y_bar
samp.mean <- (1/n)*sum(y)
samp.mean

mean(y) #Same

# NOTE: The sample mean (y_bar) is an unbiased estimator of the population mean (mu)

# Sample variance: s^2
samp.var <- (1/(n-1))*sum((y-samp.mean)^2)
samp.var

var(y) # Same!


# Estimated the variance of the sample mean which is UNBIASED 
est.var.y_bar <- ((N-n)/N)*(samp.var/n)
est.var.y_bar

# Equivalently
((N-n)/N)*(var(y)/n)

# Unbiased, but not perfect!

# From this we can calculate the standard error (SE) as the square
#   root of the estimated variance of our sample mean
se.y_bar <- sqrt(est.var.y_bar)
se.y_bar

# Equivalently
sqrt(((N-n)/N)*(var(y)/n))


# The Estimate of the population total is:
(N/n)*sum(y)

# or equivalently: N*y_bar
N*samp.mean

# The variance associated with the estimate of the total is:
#   N(N-n)*(s^2/n) = N^2*(estimate of the variance of the sample mean)

N*(N-n)*(samp.var/n)

# Equivalently 
N*(N-n)*(var(y)/n)


est.var.total <- N^2*est.var.y_bar
est.var.total

# Standard error of the estimate for the total caribou population
sqrt(est.var.total)

# The coefficient of variation for our estimate of the total caribou on the
#   Arctic Coastal Plain CV=SE/tau

sqrt(est.var.total) / (N*samp.mean)

# Or equivalently,

sqrt(est.var.total) / ((N/n)*sum(y)) 

