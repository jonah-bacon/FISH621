#==================================================================================================
#Project Name: FISH 621 Estimation of Fish Abundance - Lab 8
#Creator: Curry James Cunningham, College of Fisheries and Ocean Sciences, UAF
#Date: 2.25.22
#
#Purpose: Explore distance sampling methods, Bayesian mark-recapture estimators
#            implemented with Stan, and spatio-temporal models implemented with VAST
#
#
#
#==================================================================================================

# Exercise 1: Loading Required Packages ========================================

setwd("/Users/curryc2/Documents/Students/2022/622 - Estimation of Fish Abundance -2022/Content/Week 8/Lab")

wd <- getwd()

# Please install the following packages

# 
# install.packages("Distance")
library(Distance)

# install.packages("mcmcplots")
library(mcmcplots)

# Please load the following packages we have used before
library(tidyverse)
library(ggthemes)
library(dplyr)
library(mgcv)
library(VAST)
library(TMB)
library(FishStatsUtils)
library(INLA)

# Exercise 2: Monte Vista Ducks ================================================

# As an initial exploration of distance sampling methods we will use 
#   a dataset from a survey of duck nests in the Monte Vista National Wildlife Refuge in Colorado.

# Line transect surveys of duck nests occurred in 
#   1967 and 1968.

# In total 20 line transects were walked
#   each 25.75 km in length.

# Observers explored 2.4 meters to each side of the transect
#   and measured the distance from the transect line
#     to each detected nest.

# Total sampling effort, or total transect length was 128.75 km

# The OBJECTIVE of this survey was to estimate the total
#   number of duck nests within the population area (sampling frame)

# The population area over which the number of nests was estimated is 40.5 km^2

# First let's load the dataset

nests <- read.csv("ducks-area-effort.csv")

# And inspect its contents
str(nests)

summary(nests$distance)

head(nests)

tail(nests)

# Each row in this dataframe is an individual nest observation

nrow(nests)

# In total 534 nests were detected during this survey

# The variables (columns) in this dataframe are:

# Study.Area - name of the study region (Monte Vista NWR)
# Region.Label - identifier of regions or strata (in this case there is only one
#                                                  region and it is set to ‘Default’)
# Area - size of the stratum
# Sample.Label - line transect identifier
# Effort - length of each transect
# distance - perpendicular distances (m)

# First, we can plot a histogram of perpendicular distances
#   to get an idea of how detection changes a function of distance from the
#     center line of the transect.

hist(nests$distance, xlab="Distance (m)", col="blue")

# We can see that beyond distances of ~ 1.25 m from the transect, fewer
#   nests are detected.
# That is to say, assuming nests are distributed independently of the location
#   of each transect, the detection probability declines as a function of 
#     distance from the transect. 

# We can inspect the data a bit further...

# How many nests were detected in each of the 20 transects?

# To answer this question we need to summarize the number of detections by 
#   transect, identified by Sample.Label

# By grouping by Sample.Label and summarizing the total number of observations (rows)
#   with the n() function we can count the number of observations (nests)
#     by transect.

?n

nests.summary <- nests %>% group_by(Sample.Label) %>% summarize(n.nests=n())
nests.summary

# Let's plot number of nests detected (y-axis) by transect (x-axis)
ggplot(nests.summary, aes(x=Sample.Label, y=n.nests, fill=n.nests)) +
  theme_wsj() +
  scale_fill_viridis_c() +
  geom_col()

# Are there differences in nest distances among transects?

# Next, let's visualize the distribution of our observed nest distances
#   from the center line of the transect with boxplots
ggplot(nests, aes(x=distance, y=factor(Sample.Label))) +
  theme_bw() +
  scale_fill_viridis_c() +
  geom_boxplot()

# Or alternatively...
ggplot(nests, aes(x=distance, fill=Sample.Label, group=Sample.Label)) +
  theme_bw() +
  scale_fill_viridis_c() +
  geom_density(alpha=0.2)

# Or alternatively...
ggplot(nests, aes(x=distance, fill=Sample.Label, group=Sample.Label)) +
  theme_bw() +
  scale_fill_viridis_c() +
  geom_density(alpha=0.2) +
  facet_wrap(~Sample.Label)

# It appears that there are fairly equal distributions of nest distances
#   from each transect.

# Fit Distance Model ======================================

# We can use the Distance package from Miller (2017)
#   to analyze our distance sampling data and calculate our
#     estimators

library(Distance)

?Distance

# The underlying objective in analysis of distance sampling data is 
#   estimate parameters of a "sighting model" g(x) that describes how 
#     detection declines as distance from the transect increases

# If we do so, we can estimate what proportion of nests we likely missed in
#   our observations of nests from 0m - 2.4 m from the transect.

# In this case our nominal width (w) is 2.4 m.
# By fitting our sighting model to the distance data we can estimate
#   the effective with (mu) we have surveyed: mu < w

# With this we can estimate Pa, the probability of detection in our sampled
#   area a. Pa = mu/w

# With our effective with estimated and the total effort (total length of all transects)
#   we can calculate an estimate of overall abundance in our population area A

# N_hat = (n/Pa) / (a/A) = (nA) / (a*Pa)

# Or based on our nominal width "w" and our total length of all transects "L"

# N_hat = (n*A) / (2*w*L*Pa)

# NOTE: we have the 2* in here because we surveyed a nominal width w=2.4 m 
#        on EACH SIDE OF THE TRANSECT

# To fit our sighting function and generate our estimates we can use the 
#   ds() function

?ds

# The first argument for the ds() function is a dataframe.
#   The function will look for specific column labels that identify
#     different attributes of the data:

# "distance" observed distance to object
# "Sample.Label" Identifier for the sample (transect id)
# "Effort" effort for this transect (e.g. line transect length or number of times point transect was visited)
# "Region.Label" label for a given stratum 
# "Area" area of the strata

# The next important artument is key=
#   this defines the sighting function that will be fit.
# There are several options

# key="hn"  is for a half-normal model
# key="hr"  is for a hazard rate model

# We will fit a half-normal model to begin

# The next important artument is convert_units=

# This allows us to put our distance (m), and our Effort (km) and Area (km^2)
#   values into the same units.

# Since each kilometer is 1000 m, specifying convert_units=0.001 will convert
# km/0.001 to meters.

nest.model1 <- ds(nests, key="hn", adjustment=NULL, convert_units=0.001)

# The fitted model object is a named list
typeof(nest.model1)

# There are two parts to the model
names(nest.model1)

# The ddf element of the named list contains information about the detection
#   function

# We can plot our detection function to evaluate whether it is doing an adequate
#   job of describing our data
plot(nest.model1, nc=12)

# Goodness of fit for sighting (detection) function
# We can evaluate how well our detection function fit our distance data
# Using the gof_ds() function.

# This function will create a q-q plot 
#   showing how well the empirical (data) and fitted (predicted) values align
#   with one another.

gof_ds(nest.model1)

# In an ideal world points should fall along the 1:1 line

# This function will also evaluate goodness of fit using a Vramer-von Mises test
#   if the p-value is GREATER than 0.05, then we are achieving a good fit 
#     by your detection function to the observed distances


# The summary for the $ddf element of our fitted model provides 
#   the estimated detection probability in our survey area Pa (Average P) 
#     as well as the estimate of abundance in the covered area (Na_hat)
summary(nest.model1$ddf)


# Density and abundance estimates are stored in the $dht element of
#   

str(nest.model1$dht$individuals, max=1)

# Summary of survey information

nest.model1$dht$individuals$summary

# Including :
#   Area: Size of the survey area (km^2)
#   CoveredArea: Area covered by sampling (km^2)
#   Effort: total length of all transects
# Plus encounter rate estimates


# Abundance estimates for the total survey area (A) can be found in:

nest.model1$dht$individuals$N

# Density estimates can be found in:
nest.model1$dht$individuals$D

# The summary function called on the fitted model object will return
#   all of this information
summary(nest.model1)

# Challenge A: Alternative Detection (sighting) Functions ======================

# In the example above example we used a half-normal detection (sighting)
#   function.

# Please fit a model with a hazard rate model for the detection function
#   Compare the fit of this detection model with that of the half-normal
#     both visually and using AIC.
#   In addition please compare the Density and Abundance (N_hat)  estimates from
#     this model with those from the half-normal model

# SOLUTION ================================

nest.model1 <- ds(nests, key="hn", adjustment=NULL, convert_units=0.001) # AIC = 928.134

summary(nest.model1$ddf) # p = 0.8693, N_a = 614.2533
nest.model1$dht$individuals$summary
nest.model1$dht$individuals$N # N_hat = 2011.232 (CV = 0.059)
nest.model1$dht$individuals$D # D = 49.7 (CV = 0.059)
summary(nest.model1)

nest.model2 <- ds(nests, key="hr", adjustment=NULL, convert_units=0.001)  # AIC = 929.793

plot(nest.model2, nc=12)
gof_ds(nest.model2)

summary(nest.model2$ddf) # p = 0.8891, N_a = 600.6309
nest.model2$dht$individuals$summary
nest.model2$dht$individuals$N # N_hat = 1966.629 (CV = 0.0677)
nest.model2$dht$individuals$D # D = 48.59 (CV = 0.0677)
summary(nest.model2)







# Exercise 3: Minke Whale Distance Sampling ====================================
library(Distance)

# To further explore distance sampling estimators we will utilize a 
#   simulated dataset of minke whale sightings from surveys in the 
#     Southern Ocean.

# In this case we have two different strata: North and South

# This is dataset included with the Distance package
data(minke)

# Let's inspect the dataset
head(minke)

# We notice that the format of this dataset is the same, with each 
#   row being an individual sighting of a minke whale.

nrow(minke)

# There are 99 observations (sightings) in total

# We have two strata with different total areas (A) in nautical miles ^2

unique(minke$Region.Label)

unique(minke$Area)

# This survey included 25 transects of different lengths (nautical miles)
unique(minke$Sample.Label)
unique(minke$Effort)

# Ranging from 0.19 - 144.9 nautical miles
min(minke$Effort)
max(minke$Effort)


# First, we can plot a histogram of distances of minke whale sightings
#   here again in nautical miles
dev.off()
hist(minke$distance, xlab="Distance (nautical miles)")

# We notice that there are very few detections at distances greater
#   than 1.5 nautical miles from the transect

# Our first step will be to fit the detection (sighting) function
# Here we will specify the truncation=1.5 argument to truncate
#   the data used for fitting the detection function to observations
#     less than 1.5 nautical miles.

# This is done to exclude very infrequent observations that might
#   have high leverage on the tail shape of the detection function

minke_df <- ds(minke, truncation=1.5, adjustment=NULL) # AIC = 46.872


# First let's inspect the fit of the detection function to the 
#   frequency of sightings at distance
plot(minke_df, nc=12)

# Looks pretty good!

# Next, let's inspect the goodness of fit metrics for the detection function

gof_ds(minke_df)

# Not quite as good a q-q plot as we saw for the duck nest data, but
#   not horrible. (I've seen worse!)

# Our Cramer-von Mises test p-value is >> 0.05, so we are getting a good fit
#   of our detection function to the sighting data.

# The dht2() function will allow us to estimate abundance in each of our
#   two strata separately and together

?dht2

# We will recall that the stratum labels are in the Region.Label column

# The first argument is our fitted model minke_df

# Next we specify the stratification= argument to describe what each of the 
#   strata represent:
#   "geographical" if each stratum represents a different geographical 
#        areas and you want the total over all the areas

# We also define a strata formula, designating which column is defining 
#   the strata: strat_formula=~Region.Label

minke_dht2 <- dht2(minke_df, flatfile=minke, stratification="geographical",
                   strat_formula=~Region.Label)


# The output of this object will show us our:
#   Summary statistics
#   Abundance estimates, uncertainty, and lower/upper confidence intervals
#     by stratum and for the total sampling area

minke_dht2

# And also the breakdown in how variance is distributed (%) between 
#   the detection function and the encounter rate

# Detection = Percent of variance in abundance/density associated with detection function uncertainty
# 
# ER = Percent of variance in abundance/density associated with variability in encounter rate

# We can also extract individual components

# Abundance estimates by strata and in total across the population area A:
print(minke_dht2, report="abundance")
minke_dht2$Abundance


# Alternatively we can report out density only
print(minke_dht2, report="density")

# Exercise 4: Bayesian Mark-Recapture Estimators ===============================
library(rstan)

# In this portion of the lab we will explore Bayesian implementations of the 
#   estimators we saw previously for simple Petersen mark-recapture experiments

# As a reminder the study design is that we capture (n1) animals at time
#   t1, mark and release them, then collect a second sample (n2) at time t2
#     of which some number (m2) are marked. m2 < n2

# In order to test our Bayesian mark-recapture estimator let's simulate some
#   data with a known "true" abundance

# True abundance
N <- 1000

# Sample at time t1, of which all individuals are marked. 
#   This is the number of marked individuals in the population
n1 <- 100

# Sample at time t2, which is inspected for marks
n2 <- 200

# In order to simulate data we need to calculate the mark fraction
#   based on our specified population size
# Probability of capture at t1, or the proportion marked:
p1 <- n1/N
p1

# Now we can simulate a Petersen experiment, conditional on a binomial
#   distribution, thus assuming we are sampling WITH REPLACEMENT
set.seed(101)
m2 <- rbinom(n=1, size=n2, prob=p1)
m2

# For this simulated experiment we have 9 recaptures 

# OPEN STAN SCRIPT ==========================
# Next please open the Petersen.stan model file to inspect the structure
#   of the simple Petersen mark-recapture model.

# Our next step will be to define our the controls for our Bayesian sampling
# MCMC Control
n.chains <- 3
n.iter <- 2e3
n.thin <- 2
n.samples <- (n.iter/n.thin)*0.5*n.chains
n.samples

# In the end we will generate 1500 posterior samples from 3 chains, given
#   our thinning rate (i.e. saving every second iteration), and our default
#     50% burn-in that is removed at the beginning. 

# Next we define our data inputs for our Stan model, as a named list.
#   The name in quotations should match the name in the data section of our .stan
#     script

stan.data <- list("n1"=n1, # Initial number captured and marked at t1
                  "n2"=n2, # Number captured at t2
                  "m2"=m2  # Number of recaptures at t2
                  )

stan.data

# Now we fit the Stan model with a call to the stan() function
?stan

fit <- stan(file="Petersen.stan",
            data=stan.data,
            chains=n.chains, iter=n.iter, thin=n.thin,
            verbose=TRUE)

# We can inspect our fitted model object
fit

# Alternative for extracting summary
summary(fit)$summary

# Inference: Our number of effective posterior samples is decently high,
#              and our Rhat for our single parameter N is close to 1 (less than 1.1)

# Next, we can extract posterior samples for our parameter
pars <- rstan::extract(fit)

# These are the posterior samples for our single parameter, the abundance estimate
#  N_hat
pars$N

# Let's inspect our Traceplot, to diagnose any sampling issues
stan_trace(fit, pars=c("N"))

# Looks like decent sampling has occurred, our traceplot resembles a hairy
#   caterpillar and none of our chains is sampling a distinctly different area
#     of parameter space.

# NOTE: If we did have chains diverging from one another, this would also
#         be reflected in an Rhat > 1.1

# Extract and plot marginal posterior for abundance estimate (N)
hist(pars$N, main="Abundance Estimate")
abline(v=N, lwd=3, col=rgb(1,0,0,alpha=0.5))

# The vertical red line is the "true" abundance N=1000

# Although the posterior mean and median don't align precisely with our true
#   population size, the 95% credible interval (CI) contains the true value.
# Which is good.

# Using this same simulation experiment, we can ask some interesting questions:

# What happens if we increase the recaptue sample size (n2), from n2=100 to 
#   n2=500?

# WHAT SHOULD WE ESPECT TO HAPPEN TO OUR ESTIMATED UNCERTAINTY IN ABUNDANCE?

# Simulate data

N <- 1000
n1 <- 100
n2 <- 500

# Probability of capture at t1, or the proportion marked
p1 <- n1/N
p1

set.seed(101)
m2 <- rbinom(n=1, size=n2, prob=p1)
m2


# MCMC Control
n.chains <- 3
n.iter <- 2e3
n.thin <- 2
n.samples <- (n.iter/n.thin)*0.5*n.chains
n.samples

stan.data <- list("n1"=n1, # Initial number captured and marked at t1
                  "n2"=n2, # Number captured at t2
                  "m2"=m2  # Number of recaptures at t2
)

fit <- stan(file="Petersen.stan",
            data=stan.data,
            chains=n.chains, iter=n.iter, thin=n.thin,
            verbose=TRUE)

# Extracting Outputs
fit

# Alternative for extracting summary
summary(fit)$summary

# The first thing we notice is that we get a much tighter 95% CI, which
#   still encompasses the true value N=1,000.

# Remember, the 95% CI indicates that we anticipate there is a 95/100 chance
#   that the true value for abundance falls within this range.

# Extracting posterior samples for parameter
pars <- rstan::extract(fit)

pars$N

# Traceplots
stan_trace(fit, pars=c("N"))

# Extract and plot marginal posterior for abundance estimate (N)
hist(pars$N, main="Abundance Estimate")
abline(v=N, lwd=3, col=rgb(1,0,0,alpha=0.5))

# Simulate data: Hypergeometric Distribution ====================
# Next we can update our experiment to simulate data (m2) using the 
#   hypergeometric distribution (sampling without replacement)


N <- 1000
n1 <- 100
n2 <- 500

# For reference the parameters of our rhyper() function that 
#   simulates draws from our hypergeometric distribution are:

?rhyper

# nn= number of random samples from distribution to draw
# m= is number of items in the population that have the that property
# n= is the number of items that DO NOT have the property
# k= is the sample size

# Simulate data
set.seed(101)
m2 <- rhyper(nn=1, m=n1, n=(N-n1), k=n2)
m2

# MCMC Control
n.chains <- 3
n.iter <- 2e3
n.thin <- 2
n.samples <- (n.iter/n.thin)*0.5*n.chains
n.samples

stan.data <- list("n1"=n1, # Initial number captured and marked at t1
                  "n2"=n2, # Number captured at t2
                  "m2"=m2  # Number of recaptures at t2
)

fit <- stan(file="Petersen.stan",
            data=stan.data,
            chains=n.chains, iter=n.iter, thin=n.thin,
            verbose=TRUE)

# Extracting Outputs
fit

summary(fit)$summary

# Extracting posterior samples for each parameter
pars <- rstan::extract(fit)

pars$N

# Traceplots
stan_trace(fit, pars=c("N"))

# Extract and plot marginal posterior for abundance estimate (N)
hist(pars$N, main="Abundance Estimate")
abline(v=N, lwd=3, col=rgb(1,0,0,alpha=0.5))


# Repeated trials with random hypergeometric variation =========================

# With the series of one-off experiments above we can test the efficiency of our
#   Bayesian mark-recapture estimator for only one realization of the 
#   data conditional on our true abundance N, and sampling process n1 and n2

# To get a better understanding of whether our uncertainty is well calibrated
#   we need to simulate multiple datasets and re-fit our estimation model

# We will simulate 25 datasets, based on the conditions we have described
#   for our mark recapture experiment, using the hypergeometric
#     distribution to represent random variation in our number of marks detected (m2)
n.sims <- 25

# Simulate 25 mark-recapture experiments

N <- 1000
n1 <- 100
n2 <- 500

set.seed(101)
m2.vect <- rhyper(nn=n.sims, m=n1, n=(N-n1), k=n2)

m2.vect

# Inspect the distribution of m2 observations, conditional on N, n1 and n2
hist(m2.vect)

# We will next define parameters for our Bayesian sampling
# NOTE: we need to do this ahead of time so we can create a vector
#         to hold our posterior samples for N_hat, for each simulation

# MCMC Control
n.chains <- 3
n.iter <- 2e3
n.thin <- 2
n.samples <- (n.iter/n.thin)*0.5*n.chains
n.samples

# OK, we know that we will be saving 1500 posterior samples for N_hat

# We can create an output object with dimensions n.sims X n.samples

N_hat <- matrix(nrow=n.sims, ncol=n.samples)

# Next, we will loop through re-fitting our Stan model to each of our simulated
#   datasets

# Note: this might take a couple of minutes

i <- 1
for(i in 1:n.sims) {
  print(paste("Evaluating simulation", i, "of", n.sims))
  # Create Stan input data
  stan.data <- list("n1"=n1, # Initial number captured and marked at t1
                    "n2"=n2, # Number captured at t2
                    "m2"=m2.vect[i]  # Number of recaptures at t2
  )
  # Fit model with rstan
  fit <- stan(file="Petersen.stan",
              data=stan.data,
              chains=n.chains, iter=n.iter, thin=n.thin,
              verbose=TRUE)
  
  # Extract posterior samples for N_hat
  pars <- rstan::extract(fit)
  N_hat[i,] <- pars$N
}

# Each row in our N_hat object contains the posterior samples for N_hat
#   for one of the 25 simulations
hist(N_hat[1,])
hist(N_hat[2,])

# Ok, now that we have fit our model to each of our n.sims=25 simulated
#   datasets, lets plot the results.


library(mcmcplots)
caterplot(t(N_hat), quantiles=list(outer=c(0.025,0.975), inner=c(0.25,0.75)),
                    reorder=FALSE, labels=paste("m2:", m2.vect))

abline(v=N, lwd=3, col=rgb(1,0,0, alpha=0.5))

# Alternatively if caterplot isn't working for you, the base R boxplot is
#   always handy for summarizing distributions

boxplot(t(N_hat), names=paste("m2:", m2.vect), las=2)
abline(h=N, lwd=3, col=rgb(1,0,0, alpha=0.5))

# Inference: The majority of our 95% CI's overlap the true value N=1,000
#   and about 1/2 of the 50% CI's overlap the true value.
# From this we can infer that our uncertainty is well calibrated.

# Next, let's see what happens when we reduce our recapture effort? ========

n.sims <- 25

# Simulate 25 mark-recapture experiments

N <- 1000
n1 <- 100
n2 <- 100

set.seed(101)
m2.vect <- rhyper(nn=n.sims, m=n1, n=(N-n1), k=n2)

m2.vect

# Inspect the distribution of m2 observations, conditional on N, n1 and n2
hist(m2.vect)

# We will next define parameters for our Bayesian sampling
# NOTE: we need to do this ahead of time so we can create a vector
#         to hold our posterior samples for N_hat, for each simulation

# MCMC Control
n.chains <- 3
n.iter <- 2e3
n.thin <- 2
n.samples <- (n.iter/n.thin)*0.5*n.chains
n.samples

# OK, we know that we will be saving 1500 posterior samples for N_hat

# We can create an output object with dimensions n.sims X n.samples

N_hat <- matrix(nrow=n.sims, ncol=n.samples)

# Next, we will loop through re-fitting our Stan model to each of our simulated
#   datasets

# Note: this might take a couple of minutes

i <- 1
for(i in 1:n.sims) {
  print(paste("Evaluating simulation", i, "of", n.sims))
  # Create Stan input data
  stan.data <- list("n1"=n1, # Initial number captured and marked at t1
                    "n2"=n2, # Number captured at t2
                    "m2"=m2.vect[i]  # Number of recaptures at t2
  )
  # Fit model with rstan
  fit <- stan(file="Petersen.stan",
              data=stan.data,
              chains=n.chains, iter=n.iter, thin=n.thin,
              verbose=TRUE)
  
  # Extract posterior samples for N_hat
  pars <- rstan::extract(fit)
  N_hat[i,] <- pars$N
}

hist(N_hat[1,])
hist(N_hat[2,])

# Ok, now that we have fit our model to each of our n.sims=25 simulated
#   datasets, lets plot the results.



library(mcmcplots)
caterplot(t(N_hat), quantiles=list(outer=c(0.025,0.975), inner=c(0.25,0.75)),
          reorder=FALSE, labels=paste("m2:", m2.vect))

abline(v=N, lwd=3, col=rgb(1,0,0, alpha=0.5))

# Alternatively if caterplot isn't working for you, the base R boxplot is
#   always handy for summarizing distributions

boxplot(t(N_hat), names=paste("m2:", m2.vect), las=2)
abline(h=N, lwd=3, col=rgb(1,0,0, alpha=0.5))

# WHAT DO YOU NOTICE ABOUT THE RELATIONSHIP BETWEEN THE SIMULATED M2 VALUE,
#   AND THE RESULTING ESTIMATE OF ABUNDANCE?


# Exercise 5: VAST Exploration =================================================

# We have covered in lecture and in last week's lab how we can fit a 
#  vector autregressive spatio-temporal (VAST) model to NOAA-AFSC bottom
#   trawl survey data.

# In this lab we will explore sensitivity of model estimates to some of 
#   the model settings.



# First, let's load the BTS dataset with which we have become so familiar.
bts.dat <- read.csv("race_cpue_by_haul.csv")

str(bts.dat)

# Next, let's separate our data for survey and species specific
#   dataframes

# EBS Pollock
ebs.pol.dat <- bts.dat %>% dplyr::filter(Survey=="EBS_SHELF", 
                                         Common.Name=="walleye pollock",
                                         !is.na(Effort..km2.),
                                         Effort..km2.>0)
# GOA Northern Rockfish
goa.nr.dat <- bts.dat %>% dplyr::filter(Survey=="GOA", 
                                        Common.Name=="northern rockfish",
                                        !is.na(Effort..km2.),
                                        Effort..km2.>0)

# GOA Pollock
goa.pol.dat <- bts.dat %>% dplyr::filter(Survey=="GOA", 
                                         Common.Name=="walleye pollock",
                                         !is.na(Effort..km2.),
                                         Effort..km2.>0)

# We will begin by exploring the impact of changing the level of spatial complexity
#  We do so by adjusting the number of knots (n_x) we specify.
# The more knots, the finer the resolution of our spatial and spatio-temporal
#   fields.


# For an example lets compare 100 vs 500 knots for our GOA Northern Rockfish model

# Let's create two folders to hold the results


dir.goa.nr.100 <- file.path(wd, "GOA_nr_nx_100")
dir.goa.nr.500 <- file.path(wd, "GOA_nr_nx_500")

dir.create(dir.goa.nr.100)
dir.create(dir.goa.nr.500)

# GOA Northern Rockfish n_x=100 =================================
setwd(dir.goa.nr.100)
# FieldConfig is a bit complicated, so lets define it here
FieldConfig <- array("IID", dim=c(3,2),
                     dimnames=list(c("Omega","Epsilon","Beta"),
                                   c("Component_1", "Component_2")))
# Define settings
settings = make_settings( n_x = 100,
                          Region = "gulf_of_alaska",
                          purpose = "index2",
                          fine_scale = FALSE,
                          FieldConfig = FieldConfig,
                          RhoConfig=c("Beta1"=0,"Beta2"=0,
                                      "Epsilon1"=0,"Epsilon2"=0),
                          ObsModel=c(2,0),  # Delta-model with gamma distribution for PCR
                          bias.correct = FALSE)

# Fit Model
# NOTE: This should take several minutes
fit <- NULL
fit = fit_model( settings = settings,
                 Lat_i = goa.nr.dat[,"Starting.Latitude..dd."],
                 Lon_i = goa.nr.dat[,"Starting.Longitude..dd."],
                 t_i = goa.nr.dat[,"Year"],
                 b_i = goa.nr.dat[,"Weight..kg."],
                 a_i = goa.nr.dat[,"Effort..km2."])

# Plot results
plot(fit)

# Reset working directory
setwd(wd)


# GOA Northern Rockfish n_x=500 =================================
setwd(dir.goa.nr.500)
# FieldConfig is a bit complicated, so lets define it here
FieldConfig <- array("IID", dim=c(3,2),
                     dimnames=list(c("Omega","Epsilon","Beta"),
                                   c("Component_1", "Component_2")))
# Define settings
settings = make_settings( n_x = 500,
                          Region = "gulf_of_alaska",
                          purpose = "index2",
                          fine_scale = FALSE,
                          FieldConfig = FieldConfig,
                          RhoConfig=c("Beta1"=0,"Beta2"=0,
                                      "Epsilon1"=0,"Epsilon2"=0),
                          ObsModel=c(2,0),  # Delta-model with gamma distribution for PCR
                          bias.correct = FALSE)

# Fit Model
# NOTE: This should take several minutes
fit <- NULL
fit = fit_model( settings = settings,
                 Lat_i = goa.nr.dat[,"Starting.Latitude..dd."],
                 Lon_i = goa.nr.dat[,"Starting.Longitude..dd."],
                 t_i = goa.nr.dat[,"Year"],
                 b_i = goa.nr.dat[,"Weight..kg."],
                 a_i = goa.nr.dat[,"Effort..km2."])

# Plot results
plot(fit)

# Reset working directory
setwd(wd)

# Explore Obs Model Specification GOA Pollock =========================
# Next using our less computationally demanding 100 knot model

# Lets explore fitting a VAST model to GOA pollock 
dir.goa.pol.obsModel20 <- file.path(wd, "GOA_pol_ObsModel_20")
dir.create(dir.goa.pol.obsModel20)
setwd(dir.goa.pol.obsModel20)
# FieldConfig is a bit complicated, so lets define it here
FieldConfig <- array("IID", dim=c(3,2),
                     dimnames=list(c("Omega","Epsilon","Beta"),
                                   c("Component_1", "Component_2")))
# Define settings
settings = make_settings( n_x = 100,
                          Region = "gulf_of_alaska",
                          purpose = "index2",
                          fine_scale = FALSE,
                          FieldConfig = FieldConfig,
                          RhoConfig=c("Beta1"=0,"Beta2"=0,
                                      "Epsilon1"=0,"Epsilon2"=0),
                          ObsModel=c(2,0),  # Delta-model with gamma distribution for PCR
                          bias.correct = FALSE)

# Fit Model
# NOTE: This should take several minutes
fit <- NULL
fit = fit_model( settings = settings,
                 Lat_i = goa.pol.dat[,"Starting.Latitude..dd."],
                 Lon_i = goa.pol.dat[,"Starting.Longitude..dd."],
                 t_i = goa.pol.dat[,"Year"],
                 b_i = goa.pol.dat[,"Weight..kg."],
                 a_i = goa.pol.dat[,"Effort..km2."])

# Plot results
plot(fit)

# Reset working directory
setwd(wd)

# Now let's adjust our observation model and see if it makes a difference
#   we will specify ObsModel=c(1,0) for a delta-model with a lognormal
#     distribution for the positive catch rate component 
dir.goa.pol.obsModel10 <- file.path(wd, "GOA_pol_ObsModel_10")
dir.create(dir.goa.pol.obsModel10)
setwd(dir.goa.pol.obsModel10)
# FieldConfig is a bit complicated, so lets define it here
FieldConfig <- array("IID", dim=c(3,2),
                     dimnames=list(c("Omega","Epsilon","Beta"),
                                   c("Component_1", "Component_2")))
# Define settings
settings = make_settings( n_x = 100,
                          Region = "gulf_of_alaska",
                          purpose = "index2",
                          fine_scale = FALSE,
                          FieldConfig = FieldConfig,
                          RhoConfig=c("Beta1"=0,"Beta2"=0,
                                      "Epsilon1"=0,"Epsilon2"=0),
                          ObsModel=c(1,0),  # Delta-model with LOGNORMAL distribution for PCR
                          bias.correct = FALSE)

# Fit Model
# NOTE: This should take several minutes
fit <- NULL
fit = fit_model( settings = settings,
                 Lat_i = goa.pol.dat[,"Starting.Latitude..dd."],
                 Lon_i = goa.pol.dat[,"Starting.Longitude..dd."],
                 t_i = goa.pol.dat[,"Year"],
                 b_i = goa.pol.dat[,"Weight..kg."],
                 a_i = goa.pol.dat[,"Effort..km2."])

# Plot results
plot(fit)

# Reset working directory
setwd(wd)
