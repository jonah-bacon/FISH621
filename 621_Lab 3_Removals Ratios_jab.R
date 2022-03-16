#==================================================================================================
#Project Name: FISH 621 Estimation of Fish Abundance - Lab 3
#Creator: Curry James Cunningham, College of Fisheries and Ocean Sciences, UAF
#Date: 2.10.22
#
#Purpose: Explore change-in-ratio, catch-effort, and ratio estimators
#
#
#
#==================================================================================================


# Exercise 1: Loading Required Packages ========================================
library(tidyverse)
library(ggthemes)
library(dplyr)
library(wisp)
library(lubridate)


# Install FSAdata package for some example depletion datasets
# install.packages("FSAdata")
library(FSAdata)

# Exercise 2: Exploring Confidence Intervals ===================================

# In previous lectures we have defined the meaning of the confidence
#   intervals we often use to quantify uncertainty. 

# In short, a 95% confidence interval for an estimate should be expected to
#   encapsulate the true (but unknown) value for a parameter 95% of the time
#   with repeated sampling.

# We can demonstrate this idea to ourselves with several simple experiments.

# First, let's use an example from simple random sampling (SRS) from our
#   length distribution of fish in a lake.

# We will pretend we have a lake with N=1,000 fish, each having a different length
#   and we sample a subset of these fish and measure them (n).


N <- 1000

set.seed(101)
lengths <- rnorm(n=N, mean=100, sd=20)

lengths

# Here is our distribution of length for these 1,000 fish
hist(lengths, main="Histogram of Fish Length",
     col="royalblue")

# We can take our sample of n=100 fish
n <- 100

# Samp is again our sample of fish IDs that our included in our simple random
#   sample of n=100
samp.ID <- sample(x=1:N, size=n, replace=FALSE)
samp.ID


# So our lengths for this sample of 100 fish are:
samps <- lengths[samp.ID]

# And of course, we can calculate our key quantities from our sample under SRS
# Sample mean: y_bar
samp.mean <- (1/n)*sum(samps)
samp.mean

# Sample variance: s^2
samp.var <- (1/(n-1))*sum((samps-samp.mean)^2)
samp.var

# The UNBIASED ESTIMATOR of 
#   the variance in our estimate: var_hat(y_bar)
est.var.y_bar <- ((N-n)/N)*(samp.var/n)
est.var.y_bar

# Standard error (SE) of our sample mean
se.y_bar <- sqrt(est.var.y_bar)
se.y_bar

# Our approximate 95% Confidence Interval:
ci.low <- samp.mean - 1.96*se.y_bar
ci.up <- samp.mean + 1.96*se.y_bar

# So then, our estimate of the mean length of our fish population from 
#   this single sample is
samp.mean

# With a 95% confidence interval of
paste(ci.low, "-", ci.up)


# OK, now that we have all the pieces in place, let's simulated repeated samples
#   from our length distribution and plot their point estimates and CI's

# First, let's define the number of experiments we want to repeat,
#   we will start with 12
n.exp <- 12

# Second, we need an object to hold our point estimates and upper/low bounds
#   for the 95% CI
srs.outs <- matrix(nrow=n.exp, ncol=3, 
                   dimnames=list(c(1:n.exp), c("est","ci.low","ci.up")))

srs.outs

# Now we can loop through our 12 experiments, repeating the sampling process
#   from our length distribution from the lake, calculating our summary statistics
#     and then plotting our point estimates and 95% CI's from each SRS
#       relative to our true mean length for the population we simulated (100 mm)

set.seed(108)

i <- 1
for(i in 1:n.exp) {
  
  # Sample from our population of N=1,000 fish
  samp.ID <- sample(x=1:N, size=n, replace=FALSE)
  samps <- lengths[samp.ID]
  # Sample mean: y_bar
  samp.mean <- (1/n)*sum(samps)
  # Sample variance: s^2
  samp.var <- (1/(n-1))*sum((samps-samp.mean)^2)
  # The UNBIASED ESTIMATOR of 
  #   the variance in our estimate: var_hat(y_bar)
  est.var.y_bar <- ((N-n)/N)*(samp.var/n)
  # Standard error (SE) of our sample mean
  se.y_bar <- sqrt(est.var.y_bar)
  # Our approximate 95% Confidence Interval:
  ci.low <- samp.mean - 1.96*se.y_bar
  ci.up <- samp.mean + 1.96*se.y_bar
  
  # Save everything to our output object
  srs.outs[i,1] <- samp.mean
  srs.outs[i,2] <- ci.low
  srs.outs[i,3] <- ci.up
}

# OK, we have repeated our experiment 12 times
srs.outs

# We should expect that about 95% of our confidence intervals encompass
#   the true value for the mean length (100mm)...
# IF, AND ONLY IF, OUR UNCERTAINTY IS WELL QUANTIFIED. THAT IS ONLY IF OUR 
#   ESTIMATOR FOR THE VARIANCE IN OUR ESTIMATE AND THEREFORE CI'S ARE CORRECT !!!!

# Let's plot these and see.

y.lim <- c(min(srs.outs), max(srs.outs))
y.lim

# Plot point estimates
plot(x=1:n.exp, y=srs.outs[,1], type="p", pch=21, bg="blue",
       xlab="Sampling Event", ylab="Estimated Average Length",
       ylim=y.lim)
# Error bars representing the 95% CI
segments(x0=1:n.exp, y0=srs.outs[,2], x1=1:n.exp, y1=srs.outs[,3])

# Add horizontal line with true value
abline(h=100, col="red", lwd=2)

# We can determine the proportion of CI's that overlap the true value

length(which(srs.outs[,2]<100 & srs.outs[,3]>100))/n.exp

# REPEAT THEIS EXPERIMENT ABOVE SETTING THE SEED TO DIFFERENT VALUES 
#   TO CHECK THIS RESULT


# OK, lets expand this exercise to conduct 100 experiments and plot the results

n.exp <- 100

srs.outs <- matrix(nrow=n.exp, ncol=3, 
                   dimnames=list(c(1:n.exp), c("est","ci.low","ci.up")))

# Now we can loop through sampling experiments

set.seed(102)

i <- 1
for(i in 1:n.exp) {
  # Sample from our population of N=1,000 fish
  samp.ID <- sample(x=1:N, size=n, replace=FALSE)
  samps <- lengths[samp.ID]
  # Sample mean: y_bar
  samp.mean <- (1/n)*sum(samps)
  # Sample variance: s^2
  samp.var <- (1/(n-1))*sum((samps-samp.mean)^2)
  # The UNBIASED ESTIMATOR of 
  #   the variance in our estimate: var_hat(y_bar)
  est.var.y_bar <- ((N-n)/N)*(samp.var/n)
  # Standard error (SE) of our sample mean
  se.y_bar <- sqrt(est.var.y_bar)
  # Our approximate 95% Confidence Interval:
  ci.low <- samp.mean - 1.96*se.y_bar
  ci.up <- samp.mean + 1.96*se.y_bar
  
  # Save everything to our output object
  srs.outs[i,1] <- samp.mean
  srs.outs[i,2] <- ci.low
  srs.outs[i,3] <- ci.up
}

# Again, we should expect that about 95/100 of our confidence intervals encompass
#   the true value for the mean length (100mm)...
# IF, AND ONLY IF, OUR UNCERTAINTY IS WELL QUANTIFIED. THAT IS ONLY IF OUR 
#   ESTIMATOR FOR THE VARIANCE IN OUR ESTIMATE AND THEREFORE CI'S ARE CORRECT !!!!

# Let's plot these and see.

y.lim <- c(min(srs.outs), max(srs.outs))
y.lim

# Plot point estimates
plot(x=1:n.exp, y=srs.outs[,1], type="p", pch=21, bg="blue",
     xlab="Sampling Event", ylab="Estimated Average Length",
     ylim=y.lim)
# Error bars representing the 95% CI
segments(x0=1:n.exp, y0=srs.outs[,2], x1=1:n.exp, y1=srs.outs[,3])

# Add horizontal line with true value
abline(h=100, col="red", lwd=2)

# We can determine the proportion of CI's that overlap the true value

length(which(srs.outs[,2]<100 & srs.outs[,3]>100))/n.exp

#Petty close!

# CHALLENGE: CONFIDENCE INTERVALS ============================================

# Please repeat the experiment as above with 100 sampling events, but do so with
#   sample sizes of 25, 50, 100, and 200

# Please plot these side by side and be prepared to share your figure with the
#   class and discuss. 

N <- 1000
n.exp <- 100
n <- c(25,50,100,200)

srs.outs <- matrix(nrow=n.exp, ncol=12, 
                   dimnames=list(c(1:n.exp), 
                                 c("25.est","25.ci.low","25.ci.up","50.est","50.ci.low","50.ci.up","100.est","100.ci.low","100.ci.up","200.est","200.ci.low","200.ci.up")))
srs.outs

set.seed(102)

i <- 1
j <- 1

for (j in 1:length(n)) {
  for(i in 1:n.exp) {
    # Sample from our population of N=1,000 fish
    samp.ID <- sample(x=1:N, size=n[j], replace=FALSE)
    samps <- lengths[samp.ID]
    # Sample mean: y_bar
    samp.mean <- (1/n[j])*sum(samps)
    # Sample variance: s^2
    samp.var <- (1/(n[j]-1))*sum((samps-samp.mean)^2)
    # The UNBIASED ESTIMATOR of 
    #   the variance in our estimate: var_hat(y_bar)
    est.var.y_bar <- ((N-n[j])/N)*(samp.var/n[j])
    # Standard error (SE) of our sample mean
    se.y_bar <- sqrt(est.var.y_bar)
    # Our approximate 95% Confidence Interval:
    ci.low <- samp.mean - 1.96*se.y_bar
    ci.up <- samp.mean + 1.96*se.y_bar
    
    # Save everything to our output object
    srs.outs[i,j*3 - 2] <- samp.mean
    srs.outs[i,j*3 - 1] <- ci.low
    srs.outs[i,j*3] <- ci.up
  }
}

# Let's plot these and see.

y.lim <- c(min(srs.outs), max(srs.outs))
y.lim

par(mfrow=c(2,2), mar=c(2,3,3,1))

plot(x=1:n.exp, y=srs.outs[,1], type="p", pch=21, bg="blue",
     xlab="Sampling Event", ylab="Estimated Average Length",
     ylim=y.lim,
     main = "Sample size = 25")
segments(x0=1:n.exp, y0=srs.outs[,2], x1=1:n.exp, y1=srs.outs[,3])
abline(h=100, col="red", lwd=2)

plot(x=1:n.exp, y=srs.outs[,4], type="p", pch=21, bg="blue",
     xlab="Sampling Event", ylab="Estimated Average Length",
     ylim=y.lim,
     main = "Sample size = 50")
segments(x0=1:n.exp, y0=srs.outs[,5], x1=1:n.exp, y1=srs.outs[,6])
abline(h=100, col="red", lwd=2)

plot(x=1:n.exp, y=srs.outs[,7], type="p", pch=21, bg="blue",
     xlab="Sampling Event", ylab="Estimated Average Length",
     ylim=y.lim,
     main = "Sample size = 100")
segments(x0=1:n.exp, y0=srs.outs[,8], x1=1:n.exp, y1=srs.outs[,9])
abline(h=100, col="red", lwd=2)

plot(x=1:n.exp, y=srs.outs[,10], type="p", pch=21, bg="blue",
     xlab="Sampling Event", ylab="Estimated Average Length",
     ylim=y.lim,
     main = "Sample size = 200")
segments(x0=1:n.exp, y0=srs.outs[,11], x1=1:n.exp, y1=srs.outs[,12])
abline(h=100, col="red", lwd=2)

dev.off()

length(which(srs.outs[,2]<100 & srs.outs[,3]>100))/n.exp # Sample size = 25 -> 93% true
length(which(srs.outs[,5]<100 & srs.outs[,6]>100))/n.exp # Sample size = 50 -> 95% true
length(which(srs.outs[,8]<100 & srs.outs[,9]>100))/n.exp # Sample size = 100 -> 95% true
length(which(srs.outs[,11]<100 & srs.outs[,12]>100))/n.exp # Sample size = 200 -> 89% true

# Confirming confidence intervals from Petersen experiment =====================

# We can continue our exploration of confidence intervals using our
#   simple single-release single-recapture Petersen experiment

# Here we can assume our "true" but unknown population size is N=2500 individuals
N <- 2500

# We will further assume that during our first sampling event we capture
#   and mark n1=200 individuals
n1 <- 200

# And during our second sampling event we capture n2=500 individuals
#  and inspect them for marks
n2 <- 500

# Our expected number of marked individuals capture at time t=2 is therefore
#   the product of the proportion marked p1 and our number of fish observed at
#     time t=2 (n2)
p1 <- n1/N
p1

p1*n2

# We can use our hypergeometric distribution to simulate different realizations
#   of the number of marked individuals observed at t=2 (i.e. m2), assuming
#     sampling at time t=2 is without replacement
?rhyper

# Let's pretend we repeat this experiment 100 times
sim.recaps <- rhyper(nn=100, m=n1, n=N-n1, k=n2)

# And plot this distribution of our number of recaptures (m2) across these
#   100 experiments
hist(sim.recaps, col="royalblue")

# To calculate our Chapman estimator based on the data from our 100 experiments
#   we can create a simple function taking the number of marked individuals (n1),
#     the number of individuals captured and inspected for marks (n2), and
#       the number of observed recaptures (m2)

calc_chap <- function(n1, n2, m2) {
  N <- ((n1+1)*(n2+1))/(m2+1) - 1
  var <- ((n1+1)*(n2+1)*(n1-m2)*(n2-m2)) / ((m2+1)^2 * (m2+2))
  cv <- sqrt(var)/N
  # Output list
  out <- NULL
  out$N <- N
  out$var <- var
  out$cv <- cv
  return(out)
}

# Let's feed our data from the 100 experiments into this estimator
out.chap <- calc_chap(n1=n1, n2=n2, m2=sim.recaps)

out.chap

# Next, lets calculate lower and upper bounds for our approximate 95% CI
out.chap$ci.low <- out.chap$N - 1.96*sqrt(out.chap$var)
out.chap$ci.up <- out.chap$N + 1.96*sqrt(out.chap$var)

# Ok, now that we have done so, lets plot our predictions relative to our true
#   abundance across these 100 experiments

y.lim <- c(min(out.chap$ci.low), max(out.chap$ci.up))
y.lim

# Plot point estimates
plot(x=1:100, y=out.chap$N, type="p", pch=21, bg="blue",
     xlab="Sampling Event", ylab="Estimated Abundance (N",
     ylim=y.lim)
# Error bars representing the 95% CI
segments(x0=1:100, y0=out.chap$ci.low, x1=1:100, y1=out.chap$ci.up)

# Add horizontal line with true value
abline(h=N, col="red", lwd=2)

# CHALLENGE: Simple Random Sampling Approximate Confidence Intervals ===========

# Please repeat this exercise under the assumption that sampling for recaptures
#   is with replacement
N
n1
n2

trial.m2 <- rbinom(n=100, size=n2, prob=n1/N)

calc_bailey <- function(n1, n2, m2) {
  N <- (n1*(n2+1)) / (m2+1)
  var <- (n1^2 * (n2+1) * (n2-m2)) / ((m2+1)^2 * (m2+2))
  cv <- sqrt(var)/N
  # Output list
  out <- NULL
  out$N <- N
  out$var <- var
  out$cv <- cv
  return(out)
}

# Let's feed our data from the 100 experiments into this estimator
out.bailey <- calc_bailey(n1=n1, n2=n2, m2=trial.m2)
out.bailey

# Next, lets calculate lower and upper bounds for our approximate 95% CI
out.bailey$ci.low <- out.bailey$N - 1.96*sqrt(out.bailey$var)
out.bailey$ci.up <- out.bailey$N + 1.96*sqrt(out.bailey$var)

# Ok, now that we have done so, lets plot our predictions relative to our true
#   abundance across these 100 experiments

y.lim <- c(min(out.bailey$ci.low), max(out.bailey$ci.up))
y.lim

# Plot point estimates
plot(x=1:100, y=out.bailey$N, type="p", pch=21, bg="blue",
     xlab="Sampling Event", ylab="Estimated Abundance (N",
     ylim=y.lim)
# Error bars representing the 95% CI
segments(x0=1:100, y0=out.bailey$ci.low, x1=1:100, y1=out.bailey$ci.up)

# Add horizontal line with true value
abline(h=N, col="red", lwd=2)

length(which(out.bailey$ci.low<N & out.bailey$ci.up>N))/100
length(which(out.chap$ci.low<N & out.chap$ci.up>N))/100

# Exercise 3: Spatial State Models =============================================
# The state model is a statistical representation of the relevant biological,
#   demographic, and other processes that govern the distribution of animals
#   with respect to location, sex, size, ect. 

# We can explore different state models for the distribution of animals
#   in a field. 

# Each animal's location is defined by its distance east (u) and north (v)

# Let's assume we have N=200 individual animals
N <- 200

# Next let's generate locations locations for each of our 200 animals
#   assuming each animal's location is defined in x-y space
#     as an easting (u) is across the range 0-100 km, and a northing (v) across
#       the range 0-50 km

set.seed(101)

u <- runif(n=N, min=0, max=100)

v <- runif(n=N, min=0, max=50)

# The total area of our spatial domain is: (range of u) x (range of v)
A <- 100*50
A

# 5,000 km^2

# Combine these into locations
locs <- data.frame(u,v)

# And calculation the probability of occupying each location
#   as the product of the probability of a location (u) east and a location (v) north

probs <- dunif(x=locs$u, min=0, max=100) * dunif(x=locs$v, min=0, max=50)
probs

# This is equivalent to 1/A, for all animals given that there is uniform
#   probability of being anywhere in u and v directions

1/A



# And add this to our data frame
locs <- data.frame(locs, probs)

head(locs)

# Not that there is equal probability occupying each location

# Next we can a single realization of our uniform spatial field

plot(x=u, y=v, type="p", pch=21, bg=rgb(1,0,0, alpha=0.2),
     main="Uniform Spatial Field",
     xlab="Eastings (u)", ylab="Northings (v)")
grid(col="blue")

# From this we can calculated the joint likelihood of all N=200 individuals
#   as the product of the individual probabilities for observing individuals 
#   at specific locations, given our uniform field

prod(locs$probs)

# Lets compare several spatial fields
par(mfrow=c(2,2), mar=c(4,4,2,1))

for(i in 1:4) {
  set.seed(i) # Ensures we all get the same state fields
  
  # Simulate random locations from uniform field
  u <- runif(n=N, min=0, max=100)
  v <- runif(n=N, min=0, max=50)
  
  plot(x=u, y=v, type="p", pch=21, bg=rgb(1,0,0, alpha=0.2),
       main="Uniform Spatial Field",
       xlab="Eastings (u)", ylab="Northings (v)")
  grid(col="blue")
}

# While the spatial state models can be more complex, reflecting heterogeneity 
#   in abundance across space (i.e. the landscape) potentially in response
#     to habitat conditions or some other value, the underlying idea is that 
#       we must either (1) assume a fixed spatial state field, or (2) attempt
#         to estimate the spatial state field from our data.

# The assumption of a uniform spatial state is, as we have seen, fairly common
#   across many of our estimators related to simple random sampling.



# Exercise 4: Removal Estimator - Two Event ====================================

# We can simulate our simple removal estimator under ideal conditions,
#   that is the proportion of individuals observed and removed at each time point
#     is exactly equal

# We will start by assuming a population of 250 individuals at the start of 
#   the experiment.
N <- 250
N

# We will assume the probability of observing an individuals is independent,
#   and equal to 0.256
p <- 64/N
p

# We can calculate the number observed and removed at time t=1
n1 <- N*p
n1

# The number of individuals at the start of time t=2 is therefore the number
#   of individuals before the first removal event (N), less the number observed
#     and removed
N2 <- N - N*p
N2

# Or, equivalently...
N - n1

# The number captured (and potentially removed at time 2) is
n2 <- N2*p
n2

# And the number remaining 


# Our our abundance estimate is therefore...

N_hat <- n1^2/(n1-n2)
N_hat

# Let's continue this experiment across 

# Number in population at the start of time 3
N3 <- N2 - n2
# Number removed in event 3
n3 <- N3*p

# Number in population at the start of time 4
N4 <- N3 - n3
# Number removed in event 4
n4 <- N4*p

# Number in population at the start of time 5
N5 <- N4 - n4
# Number removed in event 5
n5 <- N5*p

# Number in population at the start of time 6
N6 <- N5 - n5
# Number removed in event 6
n6 <- N6*p

# Number in population at the start of time 7
N7 <- N6 - n6
# Number removed in event 7
n7 <- N7*p

# Number in population at the start of time 8
N8 <- N7 - n7
# Number removed in event 8
n8 <- N8*p

# Number in population at the start of time 9
N9 <- N8 - n8
# Number removed in event 9
n9 <- N9*p

# Number in population at the start of time 10
N10 <- N9 - n9


dev.off()

# Let's plot the number in the population at the start of each event (time point)
Nt <- c(N, N2, N3, N4, N5, N6, N7, N8, N9, N10)
Nt

plot(x=1:10, y=Nt,
       xlab="Sampling Events", ylab="Abundance Remaining",
     type="l", col="black")
points(x=1:10, y=Nt,
         pch=21, bg="red")
grid(col="dark gray")
abline(h=N, col="blue", lty=2, lwd=2)

# As expected the number in the population declines from the starting value
#   in the population of N=250, toward zero

# Conversely, we can also plot the total (cumulative) removals at each 
#   time point, again assuming the probability of observation, and therefore removal,
#     remains constant
p


# Our vector of removals during each of our 9 removal events, is the number observed at each 
#   time point
nt <- c(n1,n2,n3,n4,n5,n6,n7,n8,n9)
nt

# Calculate the cumulative removals
cum.remove <- cumsum(nt)
cum.remove

plot(x=1:9, y=cum.remove,
     xlab="Sampling Events", ylab="Cumulative Removals",
     type="l", col="black", ylim=c(0,250))
points(x=1:9, y=cum.remove,
       pch=21, bg="red")
grid(col="dark gray")
abline(h=N, col="blue", lty=2, lwd=2)

# What do we observe?
#   Our cumulative removals approach the initial population size as the 
#     population declines

# What about our estimates 

# Adjust obs prob and see how estimates between n1 and n2 and N_hat change

n2^2/(n2-n3)
n3^2/(n3-n4)

n2^2/(n2-n4)


# But, what happens if we don't EXACTLY observe and/or remove the same
#   proportion of individuals at each time point, that is to say
#    our observers have equal detection probability ON AVERAGE but it can vary



# Exercise 5: Change in Ratio Estimator ========================================

# Change in Ratio estimators are useful under conditions where we have 
#   two discrete categories within our population (i.e. males/females, 
#     juveniles/adults, ect. ), where we have an estimate of ratio of the
#       abundance in these categories at two time points and we know the number
#         of removals (or additions) to the population by category between
#           the time points. 
#   From this information we can calculate an estimate of abundance at either
#     time point and for either category.
#         

# Deer Example ==============================

# In the deer example from lecture we had several pieces of information:
# 1) At time t=1, a sample of the population indicated X1=83 fawns and Y1=100 adults
# 2) At time t=2, a sample of the population indicated X1=53 fawns and Y1=100 adults
# 3) A complete census of the area found 248 dead fawns and 60 dead adults

# From this we can calculate (defining fawns as our X reference category):

# The proportion of fawns at time t=1
P1 <- 83/(83+100)

# The proportion of fawns at time t=2
P2 <- 53/(53+100)

# Removals were:
Rx <- 248 # fawns
Ry <- 60 # adults

# Our total removals were
R <- Rx + Ry
R

# From this our estimate of abundance at time t=1 is:
N1_hat <- (Rx - R*P2) / (P1-P2)
N1_hat

# OR...
(248-308*(53/153)) / ((83/183) - (53/153))

# We can also calculate our predicted abundance at time t=2:
N2_hat <- N1_hat - R
N2_hat

# Based on the observed ratio of fawns to adults we can also calculate
#   the number of fawns (our reference category X) at time t=1, as:
X1_hat <- N1_hat*P1
X1_hat

# And our estimated number of fawns at time t=2
X2_hat <- X1_hat - Rx
X2_hat

# Likewise, we can calculate our expected number of adults (category Y) at time
#  t=1, as:
Y1_hat <- N1_hat*(1-P1)
Y1_hat

# And our number of adults at time t=2 as:
Y2_hat <- Y1_hat - Ry
Y2_hat

# Or equivalently...
N2_hat*(1-P2)


# Rainbow Trout Example ===============================
# In the second example we saw in lecture, we considered a pond with two classes
#   of trout large (catchable) trout and small (sub-catchable) trout.
# We define our large catchable trout as our reference category X.

# The data we observe are:
# 1) 500 small sub-catchable trout (Y category) are stocked at time t=1
Ry <- -500

# Note Ry is negative because these are additions to the population rather
#   than removals

# 2) The same day (t\1) 160 big catchable trout (X category) are caught, and
#      removed from the population
Rx <- 160

# 3) Surveys showed 40% of the population were of catchable size at time t=1
P1 <- 0.4

# and 10% of the population were of catchable size at time t=2
P2 <- 0.1

# From this we can calculate our total removals as:
R <- Rx + Ry
R

# Estimate of total trout abundance at time t=1:
N1_hat <- (Rx-R*P2)/(P1-P2)
N1_hat

# Estimate for total trout abundance at time t=2 is:
N2_hat <- N1_hat - R
N2_hat

# Our estimate of the number of large catchable trout at time t=1 is:
X1_hat <- N1_hat*P1
X1_hat

# And at time t=2:
X2_hat <- X1_hat - Rx
X2_hat

# Or, equivalently ...
N2_hat * P2

# We can calcualte the proptirion of the catchable population that was removed
#   between t=1 and t=2, as:
Rx/X1_hat

# Our estimate of the number of small sub-catchable trout at time t=1 is:
Y1_hat <- N1_hat*(1-P1)
Y1_hat

# And our number of small trout at time t=2 as:
Y2_hat <- Y1_hat - Ry
Y2_hat

# Or equivalently...
N2_hat*(1-P2)

# Exercise 6: Leslie Depletion Estimator =======================================

# First let's load an example dataset for exploring our depletion estimator, from
# Polovina, J.J. 1985. "A variable catchability version of the Leslie model 
#    with application to an intensive fishing experiment on a multispecies stock." 
data(Pathfinder)
str(Pathfinder)
head(Pathfinder)


# First lets visualize our data
list.pf <- Pathfinder %>% pivot_longer(-c(date,effort), 
                                       names_to="species", values_to="catch")
str(list.pf)

# Plot catches
ggplot(data=list.pf, aes(x=date, y=catch, color=species)) +
  geom_line() +
  geom_point() +
  facet_wrap(~species, ncol=1, scales="free_y")

# Next, lets calculate catch-per-unit-effort
list.pf <- list.pf %>% mutate("cpue"=catch/effort)
head(list.pf)

# Now we can plot CPUE across time
ggplot(data=list.pf, aes(x=date, y=cpue, color=species)) +
  geom_line() +
  geom_point() +
  facet_wrap(~species, ncol=1, scales="free_y")

# And the cumulative catch (K) across sampling events
list.pf <- list.pf %>% group_by(species) %>% mutate("K"=cumsum(catch))
head(list.pf)

# Let's double-check this worked correctly
list.pf %>% filter(species=="Pzonatus")
list.pf %>% filter(species=="Pauricilla")
list.pf %>% filter(species=="Ecarbunculus")

# But what we need for the depletion estimator is the relationship:
#   cpue_t ~ K_t-1

# So we need to create a lagged K variable
?lag
list.pf <- list.pf %>% group_by(species) %>% mutate("K_lag"=K-catch)
head(list.pf)

# Let's double-check this worked correctly, yes cpue is now aligned with the
#   K from the prior time step
list.pf %>% filter(species=="Pzonatus")
list.pf %>% filter(species=="Pauricilla")
list.pf %>% filter(species=="Ecarbunculus")

# Now lets plot cpue_t ~ K_t-1
ggplot(data=list.pf, aes(x=K_lag, y=cpue, color=species)) +
  geom_line() +
  geom_point() +
  facet_wrap(~species, ncol=1, scales="free_y")

# Let's take Pzonatus as the first example

# In order to use our Leslie depletion estimator we must fit our regression model
dat.pz <- list.pf[list.pf$species=="Pzonatus",]
lm.pz <- lm(cpue ~ K_lag, data=dat.pz)

summary(lm.pz)
coef(lm.pz)

# Lets plot our fit

plot(x=dat.pz$K_lag, y=dat.pz$cpue, type="p", 
     pch=21, bg="blue",
     xlab="Cumulative Catch (K t-1)",
     ylab="CPUE",
     main="Pzonatus")
abline(lm.pz, col=rgb(1,0,0, alpha=0.5), lwd=2)

# We will recall from lecture that our slope is our estimate of catchability (q)
q.pz <- abs(coef(lm.pz)[2])
q.pz

# And our intercept is equal to q*N0, where N0 is our initial population size
int.pz <- coef(lm.pz)[1]
int.pz

# We can calculate our estimate of initial abundance at the beginning of the
#   depletion experiment by dividing our intercept (q*N0) by q

N0.pz <- int.pz/q.pz
N0.pz

# CHALLENGE: Leslie Depletion for Ecarbunculus =================================

# Please use the Leslie Depletion estimator to calculate the initial N0 abundance
#   of Ecarbunculus

# In order to use our Leslie depletion estimator we must fit our regression model
dat.ec <- list.pf[list.pf$species=="Ecarbunculus",]
head(dat.ec)

lm.ec <- lm(cpue ~ K_lag, data=dat.ec)

summary(lm.ec)
coef(lm.ec)

# Lets plot our fit

plot(x=dat.ec$K_lag, y=dat.ec$cpue, type="p", 
     pch=21, bg="blue",
     xlab="Cumulative Catch (K t-1)",
     ylab="CPUE",
     main="Ecarbunculus")
abline(lm.ec, col=rgb(1,0,0, alpha=0.5), lwd=2)

# We will recall from lecture that our slope is our estimate of catchability (q)
q.ec <- abs(coef(lm.ec)[2])
q.ec

# And our intercept is equal to q*N0, where N0 is our initial population size
int.ec <- coef(lm.ec)[1]
int.ec

# We can calculate our estimate of initial abundance at the beginning of the
#   depletion experiment by dividing our intercept (q*N0) by q

N0.ec <- int.ec/q.ec
N0.ec










