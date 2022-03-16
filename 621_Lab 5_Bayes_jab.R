#==================================================================================================
#Project Name: FISH 621 Estimation of Fish Abundance - Lab 5
#Creator: Curry James Cunningham, College of Fisheries and Ocean Sciences, UAF
#Date: 2.25.22
#
#Purpose: Explore development of Bayesian models with Stan
#
#
#
#==================================================================================================

# Exercise 1: Jolly-Seber Model ================================================

# This Exercise will be completed in Excel: Jolly Seber.xlsx.

# Exercise 2: Loading Required Packages ========================================
library(tidyverse)
library(ggthemes)
library(dplyr)
library(rstan)
library(gtools)

# The ShinyStan package is very useful for both inspecting elements of your
#   fitted Stan model and diagnosing convergence issues along the way.

# Please install the shinystan package if you have not done so already.

# install.packages("shinystan")

library(shinystan)



# Exercise 3: Bayesian Linear Regression =======================================

# In this exercise we will simulate data from a simple linear regression of
#   the form: y = alpha + beta*x, and then construct a Bayesian model in 
#     Stan to explore our ability to estimate the parameters of this model.

# Simulate Data =====================

# Our first step is to simulate our dataset.

# We will begin by defining the TRUE values for our model parameters
int <- 5
slp <- 2

# And we will define the value for our "predictor" variable, or 
# Independent variable
x <- 1:10
x

# Next we will generate values for our "response" variable (y), or our
# Expected value
y <- int + slp*x

# y is a vector
y

# The final step in the process is to add some random observation error to the 
#   TRUE value for our response.
# Specifically, we will add normally-distributed observation error,
#   with a standard deviation equal to:
sigma <- 3

# Observed values:
set.seed(101)
obs <- y + rnorm(n=length(y), mean=0, sd=sigma)

obs

# Now, just so we are clear on the structure of our simulated data
#   we can plot both our observations (obs) that we will feed into our model
#     and the true underlying response variable value (y), across the range
#      of our independent variable (x)

# Plot relationship
plot(x=x, y=obs, pch=21, bg="blue")
lines(x=x, y=y, col="red")
legend("topleft", legend=c("True","Observed"), fill=c("red","blue"))

# Now we will construct our Bayesian linear regression model in Stan 
# PLEASE OPEN THE linear_reg.stan FILE AND EXPLORE ITS CONTENTS ================

# Fit Model in Stan
library(rstan)

# MCMC Control
n.chains <- 3
n.iter <- 2e3
n.thin <- 2
n.samples <- (n.iter/n.thin)*0.5*n.chains
n.samples

stan.data <- list("N"=length(x),
                  "obs"=obs,
                  "x"=x)

fit <- stan(file="linear_reg.stan",
              data=stan.data,
              chains=n.chains, iter=n.iter, thin=n.thin,
              verbose=TRUE)

# Extracting Outputs
fit

# Alternative for extracting summary
summary(fit)$summary

# Extracting posterior samples for each parameter
pars <- rstan::extract(fit)

pars$alpha

# Extract and use to view marginal posterior
post.alpha <- pars$alpha

hist(post.alpha)

# Convergence Diagnostics

# Traceplots
stan_trace(fit, pars=c("alpha","beta","sigma"))

# Inspect with ShinyStan
library(shinystan)
shinystan::launch_shinystan(fit)

# Density plots of marginal posteriors
stan_dens(fit, pars=c("alpha","beta","sigma"))

# Histograms  of marginal posteriors
stan_hist(fit, pars=c("alpha","beta","sigma"))

# Compare estimates to true values
par(mfrow=c(1,3), mar=c(2,2,4,1))
hist(pars$alpha, main="alpha")
abline(v=int, lwd=4, col=rgb(1,0,0, alpha=0.5))
hist(pars$beta, main="beta")
abline(v=slp, lwd=4, col=rgb(1,0,0, alpha=0.5))
hist(pars$sigma, main="sigma")
abline(v=sigma, lwd=4, col=rgb(1,0,0, alpha=0.5))

# Compare with linear model predictions

trial.glm <- glm(obs~x)
summary(trial.glm)

sd(trial.glm$residuals)

# CHALLENGE A: PLEASE REGRESSION WITH INCREASING OBS ERROR =====================

# Please create new simulated data sets with the same values for the slope
#   and intercept, but progressively increasing the observation error standard
#     deviation. 

# HOW DOES THE UNCERTAINTY IN YOUR PARAMETER ESTIMATES CHANGE AS YOU INCREASE
#   THE TRUE LEVEL OF OBSERVATION ERROR YOU SIMULATE. 

# Please plot several examples comparing posterior distributions with the 
#   true values of model parameters, across a range of observation error
#     standard deviations.

# Be prepared to share your results with the class, I will use a random number
#   generator to select a presenter!

int <- 5
slp <- 2
x <- 1:10
y <- int + slp*x

## SD = 2 ----------------------------------------------

set.seed(101)
obs1 <- y + rnorm(n=length(y), mean=0, sd=2)

stan.data1 <- list("N"=length(x),
                  "obs"=obs1,
                  "x"=x)

fit1 <- stan(file="linear_reg.stan",
            data=stan.data1,
            chains=n.chains, iter=n.iter, thin=n.thin,
            verbose=TRUE)

pars1 <- rstan::extract(fit1)

## SD = 5 ----------------------------------------------

set.seed(101)
obs2 <- y + rnorm(n=length(y), mean=0, sd=5)

stan.data2 <- list("N"=length(x),
                  "obs"=obs2,
                  "x"=x)

fit2 <- stan(file="linear_reg.stan",
            data=stan.data2,
            chains=n.chains, iter=n.iter, thin=n.thin,
            verbose=TRUE)

pars2 <- rstan::extract(fit2)

## SD = 10 ----------------------------------------------

set.seed(101)
obs3 <- y + rnorm(n=length(y), mean=0, sd=10)

stan.data3 <- list("N"=length(x),
                  "obs"=obs3,
                  "x"=x)

fit3 <- stan(file="linear_reg.stan",
            data=stan.data3,
            chains=n.chains, iter=n.iter, thin=n.thin,
            verbose=TRUE)

pars3 <- rstan::extract(fit3)

## SD = 20 ----------------------------------------------
set.seed(101)
obs4 <- y + rnorm(n=length(y), mean=0, sd=20)

stan.data4 <- list("N"=length(x),
                  "obs"=obs4,
                  "x"=x)

fit4 <- stan(file="linear_reg.stan",
            data=stan.data4,
            chains=n.chains, iter=n.iter, thin=n.thin,
            verbose=TRUE)

pars4 <- rstan::extract(fit4)

## Plots ------------------------------------------------------
par(mfrow=c(4,3), mar=c(2,2,4,1))

## SD = 2
hist(pars1$alpha, main="alpha", xlim = c(-40,40))
abline(v=int, lwd=4, col=rgb(1,0,0, alpha=0.5))
hist(pars1$beta, main="beta", xlim = c(-10,10))
abline(v=slp, lwd=4, col=rgb(1,0,0, alpha=0.5))
hist(pars1$sigma, main="sigma", xlim = c(0,40))
abline(v=2, lwd=4, col=rgb(1,0,0, alpha=0.5))

## SD = 5
hist(pars2$alpha, main="alpha", xlim = c(-40,40))
abline(v=int, lwd=4, col=rgb(1,0,0, alpha=0.5))
hist(pars2$beta, main="beta", xlim = c(-10,10))
abline(v=slp, lwd=4, col=rgb(1,0,0, alpha=0.5))
hist(pars2$sigma, main="sigma", xlim = c(0,40))
abline(v=5, lwd=4, col=rgb(1,0,0, alpha=0.5))

## SD = 10
hist(pars3$alpha, main="alpha", xlim = c(-40,40))
abline(v=int, lwd=4, col=rgb(1,0,0, alpha=0.5))
hist(pars3$beta, main="beta", xlim = c(-10,10))
abline(v=slp, lwd=4, col=rgb(1,0,0, alpha=0.5))
hist(pars3$sigma, main="sigma", xlim = c(0,40))
abline(v=10, lwd=4, col=rgb(1,0,0, alpha=0.5))

## SD = 20
hist(pars4$alpha, main="alpha", xlim = c(-40,40))
abline(v=int, lwd=4, col=rgb(1,0,0, alpha=0.5))
hist(pars4$beta, main="beta", xlim = c(-10,10))
abline(v=slp, lwd=4, col=rgb(1,0,0, alpha=0.5))
hist(pars4$sigma, main="sigma", xlim = c(0,40))
abline(v=20, lwd=4, col=rgb(1,0,0, alpha=0.5))


# Exercise 4: Poisson Regression ===============================================

# The Poisson distribution is an optimal likelihood function for
#   describing the probability of observing some number of counts (integers)
#     per unit time, or per unit space

# To explore how we would construct a Poisson regression model in a Bayesian
#   framework we will use a simple example of the counts of Peregrine falcons
#     over time.

# If you are like me and you grew up reading My Side of the Mountain by
#   Jean Craighead George, you will know that the Peregrine falcon is the coolest
#    of all birds. #TeamFrightful.

# It is important to realize that while our independent variable in this case
#   is time (year), or more specifically a standardized verion thereof, 
#     this general Stan model structure could easily be adapted to model
#      counts of something as a function of any independent variable(s)

# We will create a function to generate counts of Peregrine falcons
#   across time as a function of a cubic polynomial.
#   In this case year (time) is our independent variable.

# Create function to simulate data
data.fn <- function(n=40, alpha=3.5576, beta1=-0.0912, beta2=0.0091, beta3=-0.00014) {
  # n: Number of years
  # alpha, beta1, beta2, beta3: coefficients of a cubic polynomial of count by year
  
  # Generate values of time covariate
  year <- 1:n
  # Signal: Build up systematic part of GLM
  log.expected.count <- alpha + beta1*year + beta2*year^2 + beta3*year^3
  expected.count <- exp(log.expected.count)
  # Noise: Generate random part of the GLM: Poisson noise around expected counts
  C <- rpois(n=n, lambda=expected.count)
  # Plot simulated data
  dev.off()
  plot(year, C, type="b", lwd=2, col="black",
       las=1, ylab="Population size", xlab="Year",
       cex.lab=1.2, cex.axis=1.2)
  lines(year, expected.count, type="l", lwd=3, col="red")
  # Output Section
  out <- list(n=n, alpha=alpha, beta1=beta1, beta2=beta2,
              beta3=beta3, year=year, expected.count=expected.count,
              C=C)
  return(out)
}

# Simulate data
set.seed(101)

per.dat <- data.fn()
per.dat


# PLEASE OPEN THE GLM_Poisson.stan FILE AND EXPLORE ITS CONTENTS ===============

# Define Stan Options
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
set.seed(123)

# Create Stan data inputs
# Zscore years
mean.year <- mean(per.dat$year)
sd.year <- sd(per.dat$year)
year.std <- (per.dat$year-mean.year)/sd.year

stan.data <- list("n"=per.dat$n,
                  "C"=per.dat$C,
                  "year"=year.std)

## Initial values
inits <- function() list(alpha = runif(1, -2, 2),
                         beta1 = runif(1, -3, 3))

## MCMC settings
n.chains <- 4
n.iter <- 2e3
n.thin <- 1
n.samples <- (n.iter/n.thin)*0.5*n.chains
n.samples

## Call Stan from R
fit <- stan("GLM_Poisson.stan", data = stan.data,
            init = inits,
            chains = n.chains, thin = n.thin, iter = n.iter,
            seed = 1)

# Extract parameters
pars <- rstan::extract(fit)

# View summary output
summary(fit)$summary

# Plot Traceplots
stan_trace(fit, pars=c("alpha","beta1","beta2","beta3"))

# OK, looks good!

# Inspect with ShinyStan
library(shinystan)
shinystan::launch_shinystan(fit)

# Plot Parameter Estimates
stan_dens(fit, pars=c("alpha","beta1","beta2","beta3"))

stan_hist(fit, pars=c("alpha","beta1","beta2","beta3"))

# Plot fit to data

plot(x=per.dat$year, y=per.dat$C)

# Extract model estimates for expected counts
pred.count <- pars$lambda
dim(pred.count)

# So, we have a matrix with number of rows equal to the number of posterior
#   samples and number of columns equal to our number of years

# We can use the quantile function to summarize our credible intervals for the prediction
pred.quants <- apply(pred.count, 2, quantile, probs=c(0.025,0.25,0.5,0.75,0.975))
pred.quants

per.dat

# Create data frame of "observations" and predictions
per.df <- data.frame(per.dat$year, year.std, per.dat$C, per.dat$expected.count,
                     t(pred.quants))
str(per.df)
names(per.df) <- c("year", "year.std", "C","expected.count","low95","low50","median",
                   "up50","up95")

# Plot
per.df %>% ggplot(aes(x=year, y=C)) +
             theme_fivethirtyeight() +
             geom_point() +
             geom_ribbon(aes(ymin=low95, ymax=up95), fill="red", color=NA, alpha=0.25) +
             geom_ribbon(aes(ymin=low50, ymax=up50), fill="red", color=NA, alpha=0.25) +
             geom_line(aes(y=median), color="red") +
             ggtitle("Fit to Count Data")
  
# Fit equivalent glm
per.glm <- glm(C ~ year.std + I(year.std^2) + I(year.std^3), data=per.df,
                 family=poisson(link="log"))


summary(per.glm)  

per.df <- per.df  
  
# Exercise 5: Binomial Regression ============================================

# While the Poisson distribution is the standard model describing the 
#   uncertainty in unbounded count data, we are often in a situation
#     where there is an upper limit on the number of counts we observe.

# In this case we can approximate counts as a binomial process
#   where some number (n) things are counted out of some number of possible
#     things (N), with some probability (p)

# n ~ Binomial(N, p)

# Recall our EXTENSIVE discussion of coin flips and "successes"

# In this exercise we will be modelling the number of successful breeding pairs
#   of birds, out of some number of monitored pairs. 

# Where we might expect the probability of successful breeding (p) to vary over
#   time. p = alpha + beta1*year + beta2*year^2

# Remember that probabilities (p) are bounded 0-1, so we need some way
#   to ensure our predicted value for p remains (0-1)...

# Logit link
#   The (inverse) logit function or logit link provide a handy way to scale things on 
#     any scale to the 0-1 range, which is needed for most probability statements
#       and parameters

# This is why we often leverage a logit link in the case of logistic regression
#   with binary or binomial responses

# logit(probability) = regression stuff: a + bx ...

# or equivalently

# probability = inverse_logit( regression stuff: a + bx...)

# The logit transform of a probability is the "log odds ratio" of that probability
#   logit(p) = log(p/(1-p))

trial.p <- seq(0, 1, length.out=100)
trial.p

plot(x=trial.p, y=logit(trial.p), type="l",
       xlab="Probability (p)", ylab="logit(p)",
       lwd=2, col="red")
grid(col="black")
points(x=trial.p, y=logit(trial.p), pch=21, bg="red")

# Thinking about this another way we can have a range of values,
#   and use the inverse-logit function to scale them 0-1

?plogis

# Here is an example of the inverse logit function
vals <- -10:10
plot(x=vals, y=plogis(vals), type="l",
     xlab="Value", ylab="p = inverse-logit(value)",
     lwd=2, col="red")
grid(col="black")
points(x=vals, y=inv.logit(vals), pch=21, bg="red")

# Let's create a function to generate data on successful breeding pairs

data.fn_2 <- function(nyears=40, alpha=0, beta1=-0.1, beta2=-0.9) {
  # nyears: Number of years
  # alpha, beta1, beta2: coefficients
  
  # Generate untransformed and transformed values of time covariates
  year <- 1:nyears
  YR <- (year-round(nyears/2))/(nyears/2)
  # Generate values of binomial totals breeding pairs (N)
  N <- round(runif(nyears, min=20, max=100))
  # Signal: build systematic part of the GLM
  exp.p <- plogis(alpha + beta1*YR + beta2*(YR^2))
  # Noise: generate random part of the GLM:
  #   Binomial noise around expected counts
  C <- rbinom(n=nyears, size=N, prob=exp.p)
  # Plot simulated data
  plot(x=year, y=C/N, type="b", lwd=2, col="black",
          ylab="Proportion Successful Pairs",
          xlab="Year", ylim=c(0,1), las=1)
  points(x=year, y=exp.p, type="l", lwd=3, col="red")
  axis(side=3, at=year, labels=YR)
  # Return section
  out <- list(nyears=nyears, alpha=alpha, beta1=beta1, beta2=beta2,
              year=year, YR=YR, exp.p=exp.p, C=C, N=N)
  return(out)
}

# Simulate data
set.seed(101)
bp.dat <- data.fn_2()
bp.dat

# PLEASE OPEN THE GLM_Binomial.stan FILE AND EXPLORE ITS CONTENTS ==============

# Create Stan data inputs
stan.data <- list("nyears"=bp.dat$nyears,
                  "C"=bp.dat$C,
                  "N"=bp.dat$N,
                  "year"=bp.dat$YR)

## Initial values
inits <- function() list(alpha = runif(1, -1, 1),
                         beta1 = runif(1, -1, 1),
                         beta2 = runif(1, -1, 1))

## MCMC settings
n.chains <- 4
n.iter <- 5000
n.thin <- 2
n.samples <- (n.iter/n.thin)*0.5*n.chains
n.samples

## Call Stan from R
fit <- stan("GLM_Binomial.stan", data = stan.data,
            init = inits,
            chains = n.chains, thin = n.thin, iter = n.iter,
            seed = 1)

# Extract parameters
pars <- rstan::extract(fit)

# View summary output
summary(fit)$summary

# Plot Traceplots
stan_trace(fit, pars=c("alpha","beta1","beta2"))

# OK, looks good!

# Inspect with ShinyStan
library(shinystan)
shinystan::launch_shinystan(fit)

# Plot Parameter Estimates
stan_dens(fit, pars=c("alpha","beta1","beta2"))

stan_hist(fit, pars=c("alpha","beta1","beta2"))

# Plot fit to data

names(pars)

# Extract model-predicted proportion of successful breeders
pars.p <- pars$p

dim(pars.p)

boxplot(pars.p)

# Calculate median, 50%, 95% CI for proportion of successful breeders
pars.p <- pars$p
p.quants <- apply(pars.p, 2, quantile, probs=c(0.025,0.25,0.5,0.75,0.975))
p.quants

# Create data frame of "observations" and predictions
bp.df <- data.frame(bp.dat$year, bp.dat$YR, 
                    bp.dat$C, bp.dat$N,
                     t(p.quants))

names(bp.df) <- c("year", "YR", "C", "N",
                  "low95", "low50", "median",
                  "up50", "up95")
# Plot
bp.df %>% ggplot(aes(x=year, y=C/N)) +
  theme_economist() +
  geom_line() +
  geom_point(pch=21, bg="red") +
  geom_ribbon(aes(ymin=low95, ymax=up95), fill="blue", color=NA, alpha=0.25) +
  geom_ribbon(aes(ymin=low50, ymax=up50), fill="blue", color=NA, alpha=0.25) +
  geom_line(aes(y=median), color="blue") +
  ggtitle("Fit to Count Breeding Success Data") +
  ylab("Proportion of Successful Breeders")

# Plot observed successful breeding pairs vs. predicted

# Calculate model-predicted breeding pairs
bp.df$C.med <- bp.df$N * bp.df$median
bp.df$C.low95 <- bp.df$N * bp.df$low95
bp.df$C.low50 <- bp.df$N * bp.df$low50
bp.df$C.up50 <- bp.df$N * bp.df$up50
bp.df$C.up95 <- bp.df$N * bp.df$up95

# Plot
bp.df %>% ggplot(aes(x=year, y=C)) +
  theme_economist() +
  geom_line() +
  geom_point(pch=21, bg="red") +
  geom_ribbon(aes(ymin=C.low95, ymax=C.up95), fill="blue", color=NA, alpha=0.25) +
  geom_ribbon(aes(ymin=C.low50, ymax=C.up50), fill="blue", color=NA, alpha=0.25) +
  geom_line(aes(y=C.med), color="blue") +
  ggtitle("Fit to Count Breeding Success Data") +
  ylab("Successful Breeding Pairs")

# Challenge B: Stock-Recruitment Data ==========================================

# Bristol Bay stock recruitment data are here:
bb.dat <- read.csv("Bristol Bay Spawner-Recruit Data.csv")

str(bb.dat)
bb.dat$system <- as.factor(bb.dat$system)
bb.dat$spawn <- as.integer(bb.dat$spawn)
bb.dat$rec <- as.integer(bb.dat$rec)
levels(bb.dat$system)
bb.dat$YR <- ((bb.dat$broodYr - min(bb.dat$broodYr) + 1) - 50/2)/(50/2)

# Please construct a Bayesian model in stan to fit the relationship

# log(Recruits/Spawners) = alpha - beta(Spawners)

# Assuming normally-distributed error. 

# And fit this model to data from a single river system.

alagnak.df <- bb.dat[bb.dat$system == "Alagnak",]

## Create Stan data inputs
alagnak.stan.data <- list("nyears"= length(alagnak.df$spawn),
                  "spawn"=alagnak.df$spawn,
                  "ln_rps"=log(alagnak.df$rec/alagnak.df$spawn))

## Initial values
inits <- function() list(alpha = runif(1, 1, 2),
                         beta = runif(1, 0, 0.001),
                         sigma = runif(1, 0, 1))

## MCMC settings
n.chains <- 4
n.iter <- 2000
n.thin <- 2
n.samples <- (n.iter/n.thin)*0.5*n.chains
n.samples

## Call Stan from R
fit <- stan("BristolBay_Binomial.stan", data = alagnak.stan.data,
            init = inits,
            chains = n.chains, thin = n.thin, iter = n.iter,
            seed = 1,
            cores = 1,
            open_progress = TRUE)

stan_trace(fit, pars = c("alpha","beta","sigma"))
stan_hist(fit, pars = c("alpha","beta","sigma"))
pairs(fit, pars = c("alpha","beta","sigma"))

fit  
pars <- rstan::extract(fit)
summary(pars$beta)


  