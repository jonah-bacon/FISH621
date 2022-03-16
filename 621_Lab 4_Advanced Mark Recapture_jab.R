#==================================================================================================
#Project Name: FISH 621 Estimation of Fish Abundance - Lab 4
#Creator: Curry James Cunningham, College of Fisheries and Ocean Sciences, UAF
#Date: 2.18.22
#
#Purpose: Explore Schnabel estimators
#
#
#
#==================================================================================================

# Exercise 1: Loading Required Packages ========================================
library(tidyverse)
library(ggthemes)
library(dplyr)
library(fishmethods)

# Exercise 2: Schnabel Experiment ==============================================

# Load the Simple Schnabel dataset 
schnab.dat <- read.csv("Simple Schnabel.csv")

# Inspect the object
head(schnab.dat)

# In this dataframe object, each sampling period is a separate row from 1 to
nrow(schnab.dat)

# Therefore, our total number of sampling periods "s" is 
s <- nrow(schnab.dat)

# First, let's assume that during each sampling period i=1, 2, ..., s=10, any
#   UNMARKED fish in our RECAPTURE sample is marked before release, thus
#     joining our population of MARKED individuals within the population

# We can update our "schnab.dat" dataframe to include a new column containing
#   the NEW MARKS during each sampling period i

schnab.dat$New_Marks <- schnab.dat$Captures - schnab.dat$Recaptures

# Let's look at our resulting object
schnab.dat

# Second, let's calculate the TOTAL MARKS in the population AT THE START
#   of the current sampling period i

# We can make use of the cumsum() function to do so, which calculates the cumulative
#   sum of a numerical vector

?cumsum

x <- 1:4
x

cumsum(x)

schnab.dat$Total_Marks <- cumsum(schnab.dat$New_Marks) - schnab.dat$New_Marks

# Lets see if this is correct
schnab.dat

# We will recall the standard notation for our objects:
i <- 1:s

ni <- schnab.dat$Captures
ni

mi <- schnab.dat$Recaptures
mi

ui <- schnab.dat$New_Marks
ui

# Or equivalently, ui = 
ni - mi

Mi <- schnab.dat$Total_Marks
Mi

# As we saw in lecture we have several alternative methods for calcualting
#   abundance based on our Schnabel mark-recapture experiment, with observations
#     of marks and recaptures across periods

# The first is of course the Schnabel estimator which is:
#    the sum of CAPTURES X TOTAL MARKS, across the sampling periods (i)
#      divided by the sum of RECAPTURES across these same periods i = 1:s

# Schnabel estimate:
schnabel.est <- sum(ni*Mi) / sum(mi)
schnabel.est

# or using the columns from our dataframe
sum(schnab.dat$Captures * schnab.dat$Total_Marks) / sum(schnab.dat$Recaptures)

# Note, we would usually round this after the fact to the nearest individual.

# You will also recall from lecture that Chapman suggested an improvement to
#   reduce bias
schnabel_chap.est <- sum(ni*Mi) / ( sum(mi) + 1 )
schnabel_chap.est

# or using the columns from our dataframe
sum(schnab.dat$Captures * schnab.dat$Total_Marks) / ( sum(schnab.dat$Recaptures) + 1)


# Mean Petersen Estimator

# We can also use the results from our Schnabel experiment to calculate 
#   an abundance estimate as the average (mean) of Peterson estiamtes
#     across sampling stages i=2, 3, ..., s
# This works because each individual Petersen estimate should be UNBIASED
#   so the mean of these unbiased estimates should also be unbiased

# NOTE: We only use Petersen estimates for periods 2 - s because Mi or the 
#         total number of marks in the population at the start of the first 
#           sampling periods i=1, is zero: M(i=1)=0

# First we calculate the VECTOR individual Petersen estimates
pete_ests <- (Mi[2:s] * ni[2:s]) / mi[2:s]
pete_ests

# Mean Petersen estimate:
pete.est <- mean(pete_ests)
pete.est

# or using the columns from our dataframe
mean( (schnab.dat$Total_Marks[2:s] * schnab.dat$Captures[2:s]) / 
          schnab.dat$Recaptures[2:s] )

# Mean Chapman Estimator 

# As we did for the Mean Petersen Estimator we can take the average of 
#   Chapman estimates for abundance at each sampling stage i=2, 3, ..., s
#     to create an overall estimate

# First we calculate the VECTOR of individual Chapman abundance estimates
chap_ests <- ( (Mi[2:s]+1)*(ni[2:s]+1) / (mi[2:s] + 1) ) -1
chap_ests

# Mean Chapman estimate:
chap.est <- mean(chap_ests)
chap.est

# or using the columns from our dataframe
mean( ( (schnab.dat$Total_Marks[2:s]+1)*(schnab.dat$Captures[2:s]+1) / 
            (schnab.dat$Recaptures[2:s] + 1) ) -1 )


# Confidence Intervals for Schnabel Experiment estimators ======================

# As we have discussed before an abundance estimate is NOT very useful without
#   some associated estimate of uncertainty, often in the form of a 95% CI

# For our Mean Chapman Estimator can calculate confidence intervals in 
#   several ways 

# To calculate the overall variance for our Mean Chapman estimator we have 
#   two options:

# (1) Theoretical (uses estimated variances):

# For this method we start by calculating the Chapman variance for each of the sampling
#   periods i=2,3,...,s

# Our vector of Chapman variance estimates for esch sampling stage is:
chap_vars <- ((Mi[2:s]+1) * (ni[2:s]+1) * (Mi[2:s]-mi[2:s]) * (ni[2:s]-mi[2:s]) ) / 
               ((mi[2:s]+1)^2 * (mi[2:s]+2) )
chap_vars

# We then take the mean of these variance estiamtes i = 2,3,...,s
chap.var_1 <- sum(chap_vars)/(s-1)^2
chap.var_1

# The standard error is thus
chap.se_1 <- sqrt(chap.var_1)
chap.se_1

# (2) Empirical (uses squared deviations)
# Under option #2 we we look at the difference between individual 
#   Chapman abundance estimates across sampling periods 2-s (chap_ests) and the 
#     Mean Chapman estimate (chap.est)
chap_ests
chap.est

chap.var_2 <- sum( (chap_ests-chap.est)^2 / ((s-1)*(s-2)) )
chap.var_2

# The standard error is thus
chap.se_2 <- sqrt(chap.var_2)
chap.se_2

# Our 95% confidence intervals can be calculated based on a t-statistic
#   with s-2 degrees of freedom
?qt

qt(p=0.975, df = s-2)

# So then, our confidence interval for our estimate using variance estimation
#   Method #1
chap.lb_1 <- chap.est - qt(p=0.975, df = s-2)*chap.se_1
chap.lb_1

chap.ub_1 <- chap.est + qt(p=0.975, df = s-2)*chap.se_1
chap.ub_1

# So then, our confidence interval for our estimate using variance estimation
#   Method #2
chap.lb_2 <- chap.est - qt(p=0.975, df = s-2)*chap.se_2
chap.lb_2

chap.ub_2 <- chap.est + qt(p=0.975, df = s-2)*chap.se_2
chap.ub_2


# Our 95% for the Mean Chapman estimator from our Schnabel experiment,
#   using variance estimation Method #1 is:
paste("Estimate:", round(chap.est,0), 
      "(", round(chap.lb_1,0), "-", round(chap.ub_1,0), ")")

# Our 95% for the Mean Chapman estimator from our Schnabel experiment,
#   using variance estimation Method #2 is:
paste("Estimate:", round(chap.est,0), 
      "(", round(chap.lb_2,0), "-", round(chap.ub_2,0), ")")


# We can calculate an approximate (1-alpha)x100% confidence interval for our
#   Schnabel Estimator using a normal approximation to the Poisson Distribution

# First we need to calculate lambda, which is the sum of:
#   number of individuals sampled in each period X the number of marks in the 
#     population at the start of the sampling period
# OR, the sum of ni*Mi

lambda <- sum(ni*Mi)
lambda

# We will also need a Z score for alpha/2, z_0.025 ~ 1.96

# From this we can calculate the lower and upper bounds of our 95% CI 
#   for our Schnabel Estimator as 

schnabel_chap.est

schnabel.lb <- lambda*( (2*sum(mi) + 1.96^2 - 1.96*sqrt(4*sum(mi+1.96^2))) / 
                          (2*sum(mi)^2) )

schnabel.ub <- lambda*( (2*sum(mi) + 1.96^2 + 1.96*sqrt(4*sum(mi+1.96^2))) / 
                          (2*sum(mi)^2) )

# Our 95% for the Schnabel estimator from our Schnabel experiment is:
paste("Estimate:", round(schnabel_chap.est,0), 
      "(", round(schnabel.lb,0), "-", round(schnabel.ub,0), ")")

# Compared with our Mean Chapman 95% CIs...

# Method #1
paste("Estimate:", round(chap.est,0), 
      "(", round(chap.lb_1,0), "-", round(chap.ub_1,0), ")")

# Method #2
paste("Estimate:", round(chap.est,0), 
      "(", round(chap.lb_2,0), "-", round(chap.ub_2,0), ")")

# CHALLENGE Schnabel Estimators ================================================

# The Gerking data within the fishmethods library contains mark-recapture data
#   for sunfish in an Indiana Lake from 1953

library(fishmethods)
data(Gerking)

# Inspect the structure
str(Gerking)

?Gerking

# Each row represents a sampling period in the Schnabel-type experiment

# The C column is the number of captures

# The R column is the number of recaptures

# The nM column is the number of new marks released during a sampling event.

# Please do the following:

# (1) Determine the total number of sampling periods (s)

s <- nrow(Gerking)

# (2) Extract columns of the Gerking data frame into vectors using our standardized
#       notation (ni, mi, ui, Mi)

ni <- Gerking$C
ni

mi <- Gerking$R
mi

ui <- Gerking$nM
ui

Gerking$TM <- cumsum(Gerking$nM) - Gerking$nM
Mi <- Gerking$TM
Mi

# (3) Determine the proportion of unmarked fish that were captured, that were
#       marked prior to release

proportion.new.marked <- ui/(ni - mi)
proportion.new.marked

# (4) Calculate the MEAN CHAPMAN estimator for abundance from these data

gerk.chap.ests <- ( (Mi[2:s] + 1)*(ni[2:s] + 1) / (mi[2:s] + 1) ) - 1
gerk.chap.ests

gerk.mean.chap.est <- mean(gerk.chap.ests)
gerk.mean.chap.est

# (5) Calculate the SCHNABEL estimator for abundance from these data

gerk.schnabel.est <- sum(ni*Mi) / ( sum(mi) + 1 ) # Question
gerk.schnabel.est

# (6) Calculate an approximate 95% CI for the SCHNABEL estimator for abundance

gerk.lambda <- sum(ni*Mi)
gerk.lambda

gerk.schnabel.lb <- gerk.lambda*( (2*sum(mi) + 1.96^2 - 1.96*sqrt(4*sum(mi+1.96^2))) / 
                          (2*sum(mi)^2) )
gerk.schnabel.lb

gerk.schnabel.ub <- gerk.lambda*( (2*sum(mi) + 1.96^2 + 1.96*sqrt(4*sum(mi+1.96^2))) / 
                          (2*sum(mi)^2) )
gerk.schnabel.ub

paste("Estimate:", round(gerk.schnabel.est,0), 
      "(", round(gerk.schnabel.lb,0), "-", round(gerk.schnabel.ub,0), ")")

# (7) Calculate an approximate 95% CI for the MEAN CHAPMAN estimator for abundance
#       using the Theoretical (estimated variances) approach

gerk.chap.vars <- ((Mi[2:s]+1) * (ni[2:s]+1) * (Mi[2:s]-mi[2:s]) * (ni[2:s]-mi[2:s]) ) / 
  ((mi[2:s]+1)^2 * (mi[2:s]+2) )
gerk.chap.vars

gerk.chap.var_1 <- sum(gerk.chap.vars)/(s-1)^2
gerk.chap.var_1

gerk.chap.se_1 <- sqrt(gerk.chap.var_1)
gerk.chap.se_1

gerk.chap.lb_1 <- gerk.mean.chap.est - qt(p=0.975, df = s-2)*gerk.chap.se_1
gerk.chap.lb_1

gerk.chap.ub_1 <- gerk.mean.chap.est + qt(p=0.975, df = s-2)*gerk.chap.se_1
gerk.chap.ub_1

paste("Estimate:", round(gerk.mean.chap.est,0), 
      "(", round(gerk.chap.lb_1,0), "-", round(gerk.chap.ub_1,0), ")")

# (8) Calculate an approximate 95% CI for the MEAN CHAPMAN estimator for abundance
#       using the Empirical (squared deviations) approach

gerk.chap.var_2 <- sum( (gerk.chap.ests-gerk.mean.chap.est)^2 / ((s-1)*(s-2)) )
gerk.chap.var_2

gerk.chap.se_2 <- sqrt(gerk.chap.var_2)
gerk.chap.se_2

gerk.chap.lb_2 <- gerk.mean.chap.est - qt(p=0.975, df = s-2)*gerk.chap.se_2
gerk.chap.lb_2

gerk.chap.ub_2 <- gerk.mean.chap.est + qt(p=0.975, df = s-2)*gerk.chap.se_2
gerk.chap.ub_2

paste("Estimate:", round(gerk.mean.chap.est,0), 
      "(", round(gerk.chap.lb_2,0), "-", round(gerk.chap.ub_2,0), ")")

# Exercise 3: Schumacher-Eschmeyer Regression Method ===========================

# As discussed in lecutre an alternative estimator for abundance for
#   data collected as part of a Schnabel-type mark-recpature experiment
#     is available from the Schumacher-Eschmeyer Regression Method

# We will explore this method using the example Schnabel dataset we used in
#   Exercise #1

ni <- schnab.dat$Captures
ni

mi <- schnab.dat$Recaptures
mi

ui <- schnab.dat$New_Marks
ui

Mi <- schnab.dat$Total_Marks
Mi

# Number of sampling periods
s <- nrow(schnab.dat)

# The basis of the Schumacher-Eschmeyer Regression Method is
#   that if we treat the proportion of recaptures in each sample
#     as a response variable (yi=mi/ni) we can we can use a weighted
#       regression against total marks in the population (Mi),
#         with fixed intercept, to estimate abundance.

# Regression equation: yi = beta*Mi

# Let's begin by calculating yi
yi <- mi/ni
yi

barplot(yi, names.arg=1:s, xlab="Sampling Period", ylab="Proportion Recaptures")

# As we saw in lecture, in an ideal world we would weight our regression as
#   the inverse of the variance

# sigma_i^2 = pi(1-pi)/ni

# Weights_i = 1/sigma_i^2

# Where pi = Mi/N, is the true proportion of marks in the population.

# However, as we don't know the true abundance (N) this isn't possible
#   but we can use do a PRETTY GOOD job by approximating weights_i with ni

# WHY DOES THIS MAKE SENSE?

# In each of our sampling periods we get an estimate of the true proportion
#   marked in the population (Mi/N) based on our sample (mi/ni).
# And we should expect that this estimate should be IMPROVE as ni increases,
#   and we get a more representative sample of the mark fraction within
#     the population. 

# Let's begin by plotting the relationship between yi and Mi

plot(x=Mi, y=yi, pch=21, bg="blue",
       xlab="Total Marks in Population",
       ylab="Mark Fraction in Sample")

# We can derive an empirical estimator for our slope of our weighted regression
#   as:
#  The sum, across periods i=2 to i=s, of the product of:
#     a) the sample size (ni)
#     b) the mark fraction in the sample (yi)
#     c) the total marks in the population (Mi)

# Divided by the sum, across periods i=2 to i=s, of the product of:
#     a) the sample size (ni)
#     b) the total marks in the population (Mi) SQUARED

beta <- sum(ni[2:s]*yi[2:s]*Mi[2:s]) / sum(ni[2:s]*Mi[2:s]^2)
beta

# Or equivalently...
sum(mi[2:s]*Mi[2:s]) / sum(ni[2:s]*Mi[2:s]^2)


# In this context our slope coefficient (beta) is equal to 1/N_hat our abundance
#   estimate, so our abundance estimate is therefore:

N_hat <- 1/beta
N_hat

# We can also do this same procedure using our lm() function for a standard
#   regression in R

lm.se <- lm(yi~ -1 + Mi, weights=ni)
summary(lm.se)

beta.lm  <- coef(lm.se)

N_hat.lm <- 1/beta.lm
N_hat.lm


# CHALLENGE: Schumacher-Eschmeyer Regression for Gerking Data ==================

# Please attempt to use the Schumacher-Eschmeyer for the Gerking data
#   What do you find?

s <- nrow(Gerking)

ni <- Gerking$C
ni

mi <- Gerking$R
mi

ui <- Gerking$nM
ui

Mi <- Gerking$TM
Mi

yi <- mi/ni
yi

barplot(yi, names.arg=1:s, xlab="Sampling Period", ylab="Proportion Recaptures")

plot(x=Mi, y=yi, pch=21, bg="blue",
     xlab="Total Marks in Population",
     ylab="Mark Fraction in Sample")

gerk.beta <- sum(ni[2:s]*yi[2:s]*Mi[2:s]) / sum(ni[2:s]*Mi[2:s]^2)
gerk.beta

gerk.N_hat <- 1/gerk.beta
gerk.N_hat

gerk.lm.se <- lm(yi~ -1 + Mi, weights=ni)
summary(gerk.lm.se)

gerk.beta.lm  <- coef(gerk.lm.se)

gerk.N_hat.lm <- 1/gerk.beta.lm
gerk.N_hat.lm

