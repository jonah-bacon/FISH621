# Lab 10 - Expanded Bayesian Mark Recapture Models 


# Exercise 1: Loading Required Packages =======================================
require(tidyverse)
require(dplyr)
require(ggthemes)
require(rstan)

# Exercise 2: Simple State-space Models =======================================

# In this exercise we will explore several simple state-space models implemented
#  in stan. 


#  The first example is a simple growth model, where abundance from one
#    time point (t) to the next is assumed to change change based on a multplicative
#      process paramer (lambda).

# Lambda is varies randomly by year, but year-specific lambdas are assumed
#   to arise from a normal distribution, the mean and standard deviation of 
#     which are estimated directly from the data. 

# N_t+1 = N_t*lambda_t

# This represents a state-space model because there is assume to be both 
#   variation in state dynamics (i.e. random variation in lambda among years)
#     and observation error. 

# In this case we assume our observation errors in fitting this model to data
#   are normally-distributed, with a standard deviation that is estimated
#     directly from the data. 


# Simulate Data for Simple State-space Model ===================================

# Number of years to simulate data
n.years <- 25

# Initial population size
N1 <- 30

# Mean population growth rate (lambda_bar)
mean.lambda <- 1.02

# Process (temporal) variation of the growth rate, for each year (t)
# Here we will specify the variance and calculate the standard deviation
var.lambda <- 0.02

sigma.lambda <- sqrt(var.lambda)
sigma.lambda

# This means our population growth rate (lambda) will vary randomly by year
#   with a distribution of
hist(rnorm(1e3, mean.lambda, sigma.lambda))

# A value of lambda=1 would mean that the population neither growths nor shrinks,
#   but remains constant between t and t+1.
# Lambda values <1 would mean the population declines over the time step
#   and lambda>1 would indicate the population grows.

# To simulate our observations we can first create vectors for:

#  Our state variables
N <- vector(length=n.years)

# Our observations
y <- vector(length=n.years)

# Now we can simulate process variation in your year specific lambdas
#   by drawing random deviates from a normal distribution
set.seed(999)

# Here we will draw n.years-1 deviates, because we start with the N_t+1 = N_t*lambda_t

lambda <- rnorm(n=n.years-1, mean=mean.lambda, sd=sigma.lambda)
lambda

# And visualize our simulated deviates
hist(lambda)

# Next we can simulate our population dynamics

# We have defined the abundance in the first year t=1
N[1] <- N1

# For all other years, we will use our lambdas to describe changes in abundance
#   among years
for(t in 1:(n.years-1)) {
  N[t+1] <- N[t]*lambda[t]
} # next t

# Let's plot our simulated population trend
plot(x=1:n.years, y=N, type="l", xlab="Year", ylab="Abundance",
       main="True Population States", col="red")
points(x=1:n.years, y=N, pch=21, bg="red")
grid(col="black")

# Next we can simulate the observation process, or conditional on our true population
#   states N_t we can create our observations y_t.

# Recall that we are assuming our observation error is normally distributed
# The variance  for our observation error will be 20
var.y <- 20

sigma.y <- sqrt(var.y)

# We can simulate our observations by drawing random deviates from a normal
#   distribution where the expected value is the true population state in each 
#     year
set.seed(999)

for(t in 1:n.years) {
  y[t] <- rnorm(1, N[t], sigma.y)
}

# Let's plot our simulated population trend again, now with observations
plot(x=1:n.years, y=N, type="l", xlab="Year", ylab="Abundance",
     main="True Population States", col="red")
points(x=1:n.years, y=N, pch=21, bg="red")
grid(col="black")

# Observations
lines(x=1:n.years, y=y, col='black')

# Create Stan input data ================
stan.data <- list("y"=y, "T"=n.years)
stan.data

# Inspect Stan Model: ssm.stan =================================================

# PLEASE OPEN THE STAN MODEL FILE ssm.stan, AND INSPECT ITS CONTENTS AND STRUCTURE

# Challenge A: Interpreting ssm.stan ===========================================

# Please address the following questions:

# What are the free parameters being estimated?
# Mean_lambda = mean population growth rate, sigma_proc = SD of state process, sigma_obs = SD
# of observation process, lambda = population growth rate at time T, N_est1 = population size
# at T=1


# What are the derived parameters included as part of this model?
# N_est = population estimate at time T

# What priors are placed on model parameters?
# N_est1 ~ normal(y[1], 100);
# mean_lambda ~ uniform(0, 10);
# sigma_proc ~ uniform(0, 10);
# sigma_obs ~ uniform(0, 100);

# What are the names of generated quantities and what is their purpose/interpretation?
# cv_obs and cv_proc; These are the coefficients of variations in our estimate of the time-
# varying lambda and the estimate of our observations y


# Fit Stan Model : ssm.stan ====================================================


## MCMC settings
n.chains <- 4
n.iter <- 10000
n.thin <- 5
n.samples <- (n.iter/n.thin)*0.5*n.chains
n.samples

## Initial values
inits <- lapply(1:n.chains, function(i) {
  list(sigma_proc = runif(1, 0, 5),
       mean_lambda = runif(1, 0.1, 2),
       sigma_obs = runif(1, 0, 10),
       N_est1 = runif(1, 20, 40))})

## Call Stan from R
ssm <- stan("ssm.stan",
            data = stan.data, init = inits,
            chains = n.chains, iter = n.iter, thin = n.thin,
            seed = 1,
            control = list(adapt_delta = 0.999),
            open_progress = FALSE)

# Inspect Output: ssm.stan =====================================================

summary(ssm)$summary
# 


# Traceplots of lambda
rstan::traceplot(ssm, pars=c("lambda"))

# Traceplots of sigmas
rstan::traceplot(ssm, pars=c("sigma_proc", "sigma_obs"))

# Plot model fit to predictions

# To plot model predictions against observed and true population abundances
#   we will create a function

graph.ssm <- function(ssm, N, y) {
  n.years <- length(y)
  
  fitted <- vector(length=n.years)
  lower <- vector(length=n.years)
  upper <- vector(length=n.years)
  
  # Extract parameter estimates
  pars <- rstan::extract(ssm)
  
  i <- 1
  for(i in 1:n.years) {
    fitted[i] <- mean(pars$N_est[,i])
    lower[i] <- quantile(pars$N_est[,i], probs=0.025)
    upper[i] <- quantile(pars$N_est[,i], probs=0.975)
  }
  
  # Define y-axis dimensions
  m1 <- min(c(y, fitted, N, lower))
  m2 <- max(c(y, fitted, N, upper))
  # Plot
  par(mar=c(4.5,4,1,1), cex=1.2)
  plot(0,0, ylim=c(m1,m2), xlim=c(0.5,n.years),
       ylab="Population Size", xlab='Year',
       las=1, col="black")
  polygon(x=c(1:n.years, rev(1:n.years)), y=c(lower, upper[n.years:1]),
          col="gray90", border="gray90")
  lines(N, col="red", lwd=2)
  lines(y, col="black", lwd=2)
  lines(fitted, col="blue", lwd=2)
  # Legend
  legend("topright", legend=c("True","Observed","Est."),
         lty=c(1,1,1), lwd=c(2,2,2), col=c("red","black","blue"),
         bty="n", cex=1)
  
}

# Graph fit
graph.ssm(ssm, N, y)

# Plot Estimated vs. Observed Parameters =======================================
pars <- rstan::extract(ssm)

# Process and Observation Error Sigmas:
par(mfrow=c(1,2))
hist(pars$sigma_proc, col="lightblue", main="Process Error SD")
abline(v=sigma.lambda, col=rgb(1,0,0, alpha=0.5), lwd=3)
hist(pars$sigma_obs, col="lightblue", main="Observation Error SD")
abline(v=sigma.y, col=rgb(1,0,0, alpha=0.5), lwd=3)

# Average Population Growth Rate
hist(pars$mean_lambda, col="lightblue", main="Mean Population Growth Rate")
abline(v=mean.lambda, col=rgb(1,0,0, alpha=0.5), lwd=3)

# Initial Population Size in t=1
hist(pars$N_est1, col="lightblue", main="Init. Pop Size N[t=1]")
abline(v=N1, col=rgb(1,0,0, alpha=0.5), lwd=3)

dev.off()

# 

# Estimated vs. Predicted Population Growth Rates, by year
par(mfrow=c(5,5), mar=c(4,4,1,1))
for(y in 1:(n.years-1) ) {
  # Predicted
  hist(pars$lambda[,y], col="lightblue", xlab=paste0("N[",y,"]"),
         ylab="", main="", xlim=c(min(pars$lambda),max(pars$lambda)))
  # True
  abline(v=lambda[y], col=rgb(1,0,0, alpha=0.5), lwd=3)
}



# Challenge B: Repeated Realizations of Simple Model ===========================
# Please simulate 20 new datasets, with random process and observation error, 
#   but with the same values for the parameters.

# Next, please plot the posterior distribution from each fitted model to the true
#   value for the following parameters:

# 1) Average population growth rate: mean.lambda
# 2) Process error standard deviation for lambda: sigma.lambda
# 3) Observation error standard deviation: sigma.y


# SOLUTION =====================================================================












# Exercise 3: State-Space Model for House Martin Data ==========================

# In this example we will explore fitting a state-space model of similar structure
#   to data from a population of House Martins from the village of 
#     Magden in Northern Switzerland


# However, in this case we will also project the population forward in time 
#  for 6 years in order to:
# a) Explore how uncertainty in abundance estimates increase as we project
#      into the future.
# b) Ask some probabilistic questions about the population trajectory.


# Below are the data for House Martin from Madgen

## Data generation code is transplanted from original bpa-code.txt
## House martin population data from Magden
pyears <- 6 # Number of future years with predictions
hm <- c(271, 261, 309, 318, 231, 216, 208, 226, 195, 226, 233,
        209, 226, 192, 191, 225, 245, 205, 191, 174)
year <- 1990:2009


# We can plot the counts across time

plot(x=year, y=hm, type="l", col="blue", lwd=2,
       xlab="Year", ylab="Count",
       main="House Martins")
points(x=year, y=hm, pch=21, bg="blue")
grid(col="black")

# Create Stan input data ================
# Next, we can create our stan input data named list, which in addition
#   to the data and number of observations, includes the number 
#     of projection years (pyears)

stan.data2 <- list(y = log(hm), T = length(year), pyears = pyears)
stan.data2

# Inspect Stan Model: ssm2.stan ================================================

# PLEASE OPEN THE STAN MODEL FILE ssm2.stan, AND INSPECT ITS CONTENTS AND STRUCTURE

# Challenge C: Interpreting ssm2.stan ==========================================

# Please address the following questions:

# What are the free parameters being estimated?
#   real logN_est1; // Initial log population size
# mean_r; // Mean growth rate
# sigma_proc; // SD of state process
# sigma_obs; // SD of observation process
# r; // Growth rate in year t

# What are the derived parameters included as part of this model?
# logN_est; // Log population size in years 1990-2009

# What priors are placed on model parameters?
# logN_est1 ~ normal(5.6, 10);
# mean_r ~ normal(1, 50);
# sigma_proc ~ uniform(0, 1);
# sigma_obs ~ uniform(0, 1);

# What are the names of generated quantities and what is their purpose/interpretation?
# pr; // Predicted r population growth rate for years 2010-2015
# plogN_est; // Predicted log population size in years 2010-2015
# N_est; // Population size from 1990-2015

# In what section of the Stan script to we conduct the population projection: 2010-2015
# Generated quantities section

# Fit Stan Model : ssm.stan ====================================================


## MCMC settings
n.chains <- 4
n.iter <- 40000
n.thin <- 10
n.samples <- (n.iter/n.thin)*0.5*n.chains
n.samples


## Call Stan from R
ssm2 <- stan("ssm2.stan",
            data = stan.data2, 
            chains = n.chains, iter = n.iter, thin = n.thin,
            seed = 1,
            control = list(adapt_delta = 0.999),
            open_progress = FALSE)

# Inspect Output: ssm2.stan =====================================================

summary(ssm2)$summary
# 


# Traceplots of lambda
rstan::traceplot(ssm2, pars=c("mean_r"))

# Traceplots of sigmas
rstan::traceplot(ssm2, pars=c("sigma_proc", "sigma_obs"))

# Traceplots for estimated initial population size
rstan::traceplot(ssm2, pars=c("logN_est1"))

# Plot fit to data =======================

# First we extract the parameter estimate MCMC chains
pars2 <- rstan::extract(ssm2)

# Create projection quantities
hm.proj <- c(hm, rep(NA, pyears))
hm.proj

years.proj <- 1990:(2009+pyears)
years.proj

n.years.proj <- length(years.proj)

# Create vectors to hold mean and lower/upper quantiles for predicted
#   abundance
fitted <- vector(length=n.years.proj)
lower <- vector(length=n.years.proj)
upper <- vector(length=n.years.proj)

# Calculate posterior mean and quantiles for 95% Credible Interval
i <- 1
for(i in 1:n.years.proj) {
  fitted[i] <- mean(pars2$N_est[,i]) # Posterior mean
  lower[i] <- quantile(pars2$N_est[,i], probs=0.025)  # Lower bound of 95% CI
  upper[i] <- quantile(pars2$N_est[,i], probs=0.975) 
} #next i

fitted
lower
upper

# Define xis minimum and maximum for plottin
m1 <- min(c(fitted, hm.proj, lower), na.rm=TRUE)
m2 <- max(c(fitted, hm.proj, upper), na.rm=TRUE)

# Plot fitted and observed
par(mar=c(4.5,4,1,1))

plot(0,0, ylim=c(m1,m2), xlim=c(min(years.proj), max(years.proj)),
     ylab="Population Size", xlab="Year", col="black", type="l",
     lwd=2)

# 95% CI for prediction
polygon(x=c(years.proj, rev(years.proj)), y=c(lower, rev(upper)),
        col="gray90", border="gray90")
lines(x=years.proj, y=hm.proj, col="blue", lwd=2)
points(x=years.proj, y=hm.proj, bg="blue", pch=21)
# Fitted values
lines(x=years.proj, y=fitted, col="black", lwd=2)
# Legend
legend(x=1990, y=150, legend=c("Counts", "Estimates"), 
         lwd=c(2,2), col=c("blue","black"),
         bty="n", cex=1)


# QUESTION: What is the probability that the population size in 2015 is 
#               lower than the population size in 2009 ?
prob.less <- mean(pars2$N_est[,26] < pars2$N_est[,20])
prob.less

# Conversely, what is the probability that the 2015 population size is greater
#   than that estimated in 2009?
prob.more <- mean(pars2$N_est[,26] > pars2$N_est[,20])
prob.more

# This means
paste("A decline in the house martin population until 2015 is:", 
        round(prob.less/prob.more,1), "times more likely.")

# QUESTION: What is the probability that the house martin population falls
#             below 100 individuals in 2015
plot(0,0, ylim=c(m1,m2), xlim=c(min(years.proj), max(years.proj)),
     ylab="Population Size", xlab="Year", col="black", type="l",
     lwd=2)
polygon(x=c(years.proj, rev(years.proj)), y=c(lower, rev(upper)),
        col="gray90", border="gray90")
lines(x=years.proj, y=hm.proj, col="blue", lwd=2)
points(x=years.proj, y=hm.proj, bg="blue", pch=21)
lines(x=years.proj, y=fitted, col="black", lwd=2)
legend(x=1990, y=150, legend=c("Counts", "Estimates"), 
       lwd=c(2,2), col=c("blue","black"),
       bty="n", cex=1)
abline(h=100, lwd=3, col="red", lty=2)

# To answer this question we need to look at the posterior distribution
#   for predicted house martin abundance in 2015
hist(pars2$N_est[,26], col='blue', 
     main="Estimated Population Size in 2015")
abline(v=100, lwd=3, col="red", lty=2)

# We can answer this probabilistic question in two ways:

# 1) We can calculate the proportion of posterior samples for the abundance
#       in 2015 that are below our N=100 threshold
sum(pars2$N_est[,26]<=100)/length(pars2$N_est[,26])

# 2) We can use Empirical Cumulative Distribution Function (ecdf)

?ecdf

# This function will create a function based on the distribution of values 
#   we provide, representing the cumulative probability distribution. 

# By specifying a value to this new function we can calculate the probability
#   conditional on the values we provide of observing a value less than 
#     or equal to  a given value
#    

fxn <- ecdf(pars2$N_est[,26])
fxn(100)


# Exercise 4: Simulation Individual Mark-Recapture =============================

# We will recall from lecture that the core of our analysis of individual mark-recapture
#   data for the purpose of abundance estimation is the X_ij matrix 
#     of individual (re)capture or (re)sighting histories.

# Each row (i) in the matrix is an individual, each column (j) represents
#   an individual sampling occasion across time, with the elements of this matrix
#   recording capture or non-capture as a 1 or 0. 

# A row describes the capture history across time (sampling events) for an individual.

# IT IS CRITICAL TO REMEMBER that the true population size N is greater
#   than the number of rows in X_ij, as there are likely many individuals
#     who were never encountered/sampled/marked and as such would have 
#       recapture histories of all zeros (0 0 0 0 0 0 0 0 ...)

# To visualize this process we can simulate recapture histories under 
#   the (null) model M0 we discussed in lecture, where all individuals
#     during all sampling events have the same probability of capture.
#   

# Here is a function for simualting recapture histories
#   the arguments are:
# 1) N - the true population size
# 2) p - the shared capture probability
# 3) TT - the number of sampling time periods

data.fxn <- function(N=100, p=0.5, TT=3) {
  y.full <- array(NA, dim=c(N,TT))
  y.obs <- array(NA, dim=c(N,TT))
  
  for(j in 1:TT) {
    y.full[,j] <- rbinom(n=N, size=1, prob=p)
  }
  ever.detected <- apply(y.full, 1, max)
  C <- sum(ever.detected)
  y.obs <- y.full[ever.detected==1, ]
  # Print results
  print(paste(C, "out of", N, "animals present were detected"))
  # Return simulation output
  out <- list(N=N, p=p, C=C, TT=TT, y.full=y.full, y.obs=y.obs)
  
  return(out)
}

# This function returns several things including the original N, p, and TT
#   but also: 
#     C the total number of unique individuals captured and marked
#     y.full is the true underlying recapture history with all individuals
#               even those we never observe (0 0 0 0 0 0 0 ...)
#     y.obs is the data we would actually have in hand following such an 
#            experiment X_ij 

# Simulate new individual recapture history for population of 100,
#   capture probability of 0.5 (50%), sampled across three periods
set.seed(101)
sim1 <- data.fxn(N=100, p=0.5, TT=3)

str(sim1)

# Repeat this simulation reducing capture pobability, what is the anticipated 
#   result? How many total individuals to you expect to detect?
#     and what does this mean for the dimensions of your observed X_ij matrix (y.obs)?
sim2 <- data.fxn(N=100, p=0.05, TT=3)

sim2


# Repeat the simulation with the new lower capture probability, but this 
#   time increasing the number of sampling periods (TT).

# What do you expect to see?
sim3 <- data.fxn(N=100, p=0.05, TT=10)

sim3



names(sim1)

# Challenge D: Simulation of Mt Model ==========================================

# Please create a new function called "data.fxn.Mt" that simulates individual
#   recapture histories under a model where capture probability is shared
#     across individuals but varies over time (Model Mt)



# SOLUTION ===================================

data.fxn.Mt <- function(N=100, TT=3) {
  y.full <- array(NA, dim=c(N,TT))
  y.obs <- array(NA, dim=c(N,TT))
  p <- vector(length = 3)
  
  for(j in 1:TT) {
    p[j] <- runif(n = 1, min = 0, max = 1)
    y.full[,j] <- rbinom(n=N, size=1, prob=p[j])
  }
  ever.detected <- apply(y.full, 1, max)
  C <- sum(ever.detected)
  y.obs <- y.full[ever.detected==1, ]
  # Print results
  print(paste(C, "out of", N, "animals present were detected with detection probabilities in time
  points j = 1, 2, 3 of", round(p[1], 2), round(p[2], 2), round(p[3],2), "respectively"))
  # Return simulation output
  out <- list(N=N, p=p, C=C, TT=TT, y.full=y.full, y.obs=y.obs)
  
  return(out)
}

set.seed(101)
mt.sim1 <- data.fxn.Mt(N=100, TT=3)

set.seed(102)
mt.sim2 <- data.fxn.Mt(N=100, TT=3)

set.seed(103)
mt.sim3 <- data.fxn.Mt(N=100, TT=3)

set.seed(104)
mt.sim4 <- data.fxn.Mt(N=100, TT=3)
str(mt.sim1)

# Exercise 5: M0 ===============================================================
# In this exercise we will explore our most simplistic model for 
#  individual recapture histories M0, where we assume capture probabilities
#    are constant across all individuals, all capture periods, and the 
#     history of capture does not influence future capture probability. 

# First, we will load data simulated under the M0 model.
stan.data <- read_rdump("M0.data.R")

# Inspect this data object, what do you notice about y (out X_i,j)?
str(stan.data)

stan.data

# We have added in a large number of rows of (0 0 0 ...) recapture histories
#   this is known as "data augmentation", which helps the code efficiently 
#     estimate the unknown population size as somewhere between 
#       the number of unique recapture histories (rows in X_ij) and 
#         the number of rows in the augmented dataset 

## Initial values
inits <- function() {
  list(p = runif(1, 0, 1), omega = 0.5)}

## Parameters monitored
params <- c("N", "p", "omega")

## MCMC settings
n.chains <- 4
n.iter <- 4000
n.thin <- 2
n.samples <- (n.iter/n.thin)*0.5*n.chains
n.samples

## Call Stan from R
out <- stan("M0.stan",
            data = stan.data, init = inits, pars = params,
            chains = n.chains, iter = n.iter, thin = n.thin,
            seed = 2,
            open_progress = FALSE)

## Summarize posteriors
print(out, digits = 3)

# PLEASE USE THE FUNCITON YOU CREATED ABOVE TO SIMULATE A NEW DATASET
#   AND REFIT  THE M0 MODEL

set.seed(101)
data.sim <- data.fxn(N=100, p=0.35, TT=3)

new.stan.data <- list(y = rbind(data.sim$y.obs, ), M = nrow(sim1$y.full), T = ncol(sim1$y.full))
str(Mt.data)
sim1$
## Initial values
inits <- function() {
  list(p = runif(1, 0, 1), omega = 0.5)}

## Parameters monitored
params <- c("N", "p", "omega")

## MCMC settings
n.chains <- 4
n.iter <- 4000
n.thin <- 2
n.samples <- (n.iter/n.thin)*0.5*n.chains
n.samples

## Call Stan from R
out2 <- stan("M0.stan",
            data = Mt.data, init = inits, pars = params,
            chains = n.chains, iter = n.iter, thin = n.thin,
            seed = 2,
            open_progress = FALSE)

## Summarize posteriors
print(out2, digits = 3)

# Exercise 6: Mt ===============================================================

# In this exercise we will add some complexity to our individual mark-recapture
#   model, by allowing our capture probabilities to vary across time (sampling events)
#     but still assuming they are constant across individuals.

# First, we will load data simulated under the Mt model.
stan.data <- read_rdump("Mt.data.R")

# Inspect this data object
str(stan.data)

stan.data

# Here again you will notice the large number of rows of (0 0 0 ...) recapture histories
#  appended to our X_ij observed matrix as part of the data augmentation process. 

## Initial values
inits <- function() list(p = runif(3, 0, 1))

## Parameters monitored
params <- c("N", "p", "omega")

## MCMC settings
n.chains <- 4
n.iter <- 4000
n.thin <- 2
n.samples <- (n.iter/n.thin)*0.5*n.chains
n.samples

## Call Stan from R
out <- stan("Mt.stan",
            data = stan.data, init = inits, pars = params,
            chains = n.chains, iter = n.iter, thin = n.thin,
            seed = 2,
            open_progress = FALSE)

## Summarize posteriors
print(out, digits = 3)

# PLEASE USE THE FUNCITON YOU CREATED ABOVE TO SIMULATE A NEW DATASET
#   AND REFIT  THE Mt MODEL

# Exercise 7: Mb ===============================================================

# In this exercise we will explore the behavioral variant of this model,
#   where the history of capture influences future capture probability.

# There are two parameters p the initial capture probability, or c for 
#   the capture probability after the initial capture event.

# This encompasses the outcomes of both trap-happiness (c > p), and trap-shyness
#   (c < p)

# First, we will load data simulated under the Mb model.
stan.data <- read_rdump("Mb.data.R")

# Inspect this data object
str(stan.data)

stan.data

# Here again you will notice the large number of rows of (0 0 0 ...) recapture histories
#  appended to our X_ij observed matrix as part of the data augmentation process. 

## Initial values
inits <- function() list(p = runif(1, 0, 1))

## Parameters monitored
params <- c("N", "p", "c", "trap_response", "omega")

## MCMC settings
n.chains <- 4
n.iter <- 4000
n.thin <- 2
n.samples <- (n.iter/n.thin)*0.5*n.chains
n.samples

## Call Stan from R
out <- stan("Mb.stan",
            data = stan.data, init = inits, pars = params,
            chains = n.chains, iter = n.iter, thin = n.thin,
            seed = 2,
            open_progress = FALSE)

## Summarize posteriors
print(out, digits = 3)


# THANK YOU ALL FOR A FANTASTIC SEMESTER!
