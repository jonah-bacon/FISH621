#==================================================================================================
#Project Name: FISH 621 Estimation of Fish Abundance - Lab 7
#Creator: Curry James Cunningham, College of Fisheries and Ocean Sciences, UAF
#Date: 3.24.22
#
#Purpose: Explore implementation of spatio-temporal models for index standarization
#
#
#
#==================================================================================================


# setwd("/Users/curryc2/Documents/Students/2022/622 - Estimation of Fish Abundance -2022/Content/Week 7/Lab")

# Exercise 1: Loading Required Packages ========================================

# Please load the following required packages, if you do not have any of these
#   installed please install them now.

library(tidyverse)
library(dplyr)
library(ggthemes)
library(VAST)

# Exercise 2: Define workflow standard workflow ================================

# In order to make our lives easier we will define a set of standardized 
#   relative path extensions for later use. 

# For those of you who have taken my R course (FISH/MSL 627) in the past
#   this should be review, but for others I will demonstrate now. 

# Begin by setting your working directory to the directory (folder) either
#   from the drop-down menu or with setwd() to the directory with this script
#     and containing the data files included with this lab.

# Double check you are in the right place...

getwd()

# Next we will extract our working directory as a path object (wd)

wd <- getwd() #

wd

# Now, we will use this in conjunction with the file.path() function 
#   to create new paths in which we will fit specific VAST models in our 
#     case studies.

?file.path

# Walleye pollock on the Eastern Bering Sea shelf

dir.ebs.pollock <- file.path(wd, "VAST-EBS-pollock")

dir.ebs.pollock

# Once we define the path extension, we can create a subdirectory using
#   the path extension that we have defined above

?dir.create

dir.create(dir.ebs.pollock)

# Walleye pollock in Gulf of Alaska
dir.goa.pollock <- file.path(wd, "VAST-GOA-pollock")
dir.create(dir.goa.pollock)

# Northern rockfish in the Gulf of Alaska
dir.goa.nrock <- file.path(wd, "VAST-GOA-northernRockfish")
dir.create(dir.goa.nrock)

# Exercise 3: VAST Case Studies - EBS Pollock ================================================

# In this section of the lab we will explore several species-region case 
#   studies based on the NOAA-AFSC RACE Program Bottom Trawl Survey data

# Let's load the data now:

# Individual bottom trawl survey datasets

bts.dat <- read.csv("race_cpue_by_haul.csv")

summary.bts.dat <- bts.dat %>% group_by(Survey, Year , Common.Name) %>% summarize("n"=n())
summary.bts.dat

# Next, we will load data associated with the survey strata
strata.dat <- read.csv("Stratum Descriptions.csv")


# First let's begin by subsetting our survey observations for our EBS pollock
#   example.

# At the same time we will make sure that effort (km^2) in area swept
#   is not an NA, nor is it equal to zero.

unique(bts.dat$Survey)
unique(bts.dat$Common.Name)


ebs.pol.dat <- bts.dat %>% dplyr::filter(Survey=="EBS_SHELF", 
                                         Common.Name=="walleye pollock",
                                         !is.na(Effort..km2.),
                                         Effort..km2.>0)

str(ebs.pol.dat)

# As we saw in lecture the first step in fitting a VAST model is to 
#   define our settings with the make_settings() function

?make_settings

# We will define several important elements:
#   1) n_x = our number of knots (level of spatial complexity)
#   2) Region = the extrapolation grid we will use the built in extrapolation 
#        grid for the Eastern Bering Sea Shelf
#   3) purpose = this defines the type of model we want to fit. VAST has been extended
#                   to several different applications, but for index standardization
#                   we should use "index2"
#   4) fine_scale = Whether we want to interpolate fine scale spatial heterogeneity
#         in encounter probability (EP) and positive catch rate (PCR)
#   5) FieldConfig = How/if we want to estimate our spatial and spatio-temporal components
#   6) RhoConfig = Whether we want to estimate any association among our years.
#                    NOTE: We will want to not assume any autocorrelation if our 
#                             purpose is index standardization as we want to treat each year
#                             as independent.
#   7) ObsModel = The distribution to assume for [1] our positive catch rates
#                     and [2] the structure of how we link together our EP and PCR 
#                       components.
#   8) bias.correct = Whether we should correct for retransformation bias in
#                       our random effects. For a final model we will want to do
#                       so, but it is computationally expensive so we will keep that
#                       OFF (or FALSE) for now


# Better descriptions of some of these components can be found in the
#   documentation for the make_data function, which received many of these
#     inputs as well as the make_model function
#    
?make_data

?make_model

?make_settings

# FieldConfig is a bit complicated, so lets define it here
FieldConfig <- array("IID", dim=c(3,2), 
                     dimnames=list(c("Omega","Epsilon","Beta"),
                                   c("Component_1", "Component_2")))
FieldConfig

settings = make_settings( n_x = 100, 
                          Region = "eastern_bering_sea", 
                          purpose = "index2", 
                          fine_scale = FALSE,
                          FieldConfig = FieldConfig,
                          RhoConfig=c("Beta1"=0,"Beta2"=0,
                                      "Epsilon1"=0,"Epsilon2"=0),
                          ObsModel=c(2,0),  # Delta-model with gamma distribution for PCR
                          bias.correct = FALSE
                          )

# What is the structure of the object produced by the make_settings function?

typeof(settings)

# It is a list!

# Let's inspect what it contains...

str(settings)

names(settings)

settings


# Next, we call the fit_model() function to build, compile, and run our VAST model
# The first argument is the settings list we created above with make_settings()

# The subsequent arguments are our data
# 1) Lat_i = The latitude for each observation
# 2) Lon_i = The longitude for each observation
# 3) t_i = The time reference for each observation (here a year)
# 4) b_i = The biomass or abundance for each sampling event (here kg)
# 5) a_i = The effort metric (here km^2)

# Each of these data elements expects a vector of equal length.

# We can extract those data from our bts.dat dataframe, by referencing the 
#   appropriate columns.

names(bts.dat)

ebs.pol.dat[,"Starting.Latitude..dd."]
ebs.pol.dat[,"Effort..km2."]
ebs.pol.dat[,"Weight..kg."]

min(ebs.pol.dat[,"Effort..km2."])
min(ebs.pol.dat[,"Weight..kg."])

# Let's quickly look at the histogram of our effort
hist(ebs.pol.dat[,"Effort..km2."])

# And catch weight
hist(ebs.pol.dat[,"Weight..kg."])

# But first let's set our working directory to the directory we previously
#   created so we don't clutter up our lab folder

setwd(dir.ebs.pollock)

# Several things are going to happen, it will:
#   1) Check our input values to make sure everything is OK
#   2) Build the VAST model in TMB (Template Model Builder)
#   3) Compile the TMB model in to C++ code that can be used to do th estimation
#   4) Fit the model to our data

# NOTE: This should take several minutes

fit = fit_model( settings = settings, 
                 Lat_i = ebs.pol.dat[,"Starting.Latitude..dd."], 
                 Lon_i = ebs.pol.dat[,"Starting.Longitude..dd."], 
                 t_i = ebs.pol.dat[,"Year"], 
                 b_i = ebs.pol.dat[,"Weight..kg."], 
                 a_i = ebs.pol.dat[,"Effort..km2."]
               )

# The result is a fitted VAST model object "fit", which contains A LOT 
#   of information, but importantly our parameter estimates

fit



# There are two things we want to check to see if our model has converged
# The first is the "Convergence_check"

fit$parameter_estimates$Convergence_check

# To ensure it says: "There is no evidence that the model is not converged"

# The second is our maximum gradient 
#   And ensure it is <0.001

fit$parameter_estimates$max_gradient

# Next we will call the plot our diagnostic plots, spatial predictions, 
#   and create some other useful objects

plot(fit)

# We can plot a few additional/useful figures using the plot_results() function

?plot_results 

# Predictions for encounter probability across space and time:
plot_results(fit, plot_set=1)

# Finally, let's reset our working directory
setwd(wd)



# Now please open the "VAST-EBS-pollock" folder and inspect the figures that 
#   you have created from you fitted model




# Exercise 4: VAST Case Studies - GOA Norther Rockfish ================================

# Now that we are familiar with the operations involved in fitting a VAST
#   model, let's try it for Northern Rockfish in the GOA

unique(bts.dat$Survey)
unique(bts.dat$Common.Name)

goa.nr.dat <- bts.dat %>% dplyr::filter(Survey=="GOA", 
                                         Common.Name=="northern rockfish",
                                         !is.na(Effort..km2.),
                                         Effort..km2.>0)

# Let's check the distribution of effort
hist(goa.nr.dat$Effort..km2.)

# And our catches in kg
hist(goa.nr.dat$Weight..kg.)

# More zeros maybe?

# Let's calculate the proportion of hauls with zero weight (no northerns caught)
nrow(goa.nr.dat[goa.nr.dat$Weight..kg.==0, ])/nrow(goa.nr.dat)

# A VERY HIGH PROPORTION OF SURVEY TOWS DIDN'T HAVE NORTHERN ROCKFISH
#   That is OK we are modelling our encounter probability separately

# We will use the same settings as before, except we need to use the extrapolation
#   grid for the gulf of alaska

?make_data
?make_model
?make_settings

# FieldConfig is a bit complicated, so lets define it here
FieldConfig <- array("IID", dim=c(3,2), 
                     dimnames=list(c("Omega","Epsilon","Beta"),
                                   c("Component_1", "Component_2")))
FieldConfig

settings = make_settings( n_x = 100, 
                          Region = "gulf_of_alaska", 
                          purpose = "index2", 
                          fine_scale = FALSE,
                          FieldConfig = FieldConfig,
                          RhoConfig=c("Beta1"=0,"Beta2"=0,
                                      "Epsilon1"=0,"Epsilon2"=0),
                          ObsModel=c(2,0),  # Delta-model with gamma distribution for PCR
                          bias.correct = FALSE)

# Let's inspect our settings
settings


# Next, we call the fit_model() function to build, compile, and run our VAST model

# But first don't forget to set your working directory

setwd(dir.goa.nrock)



# NOTE: This should take several minutes

fit = fit_model( settings = settings, 
                 Lat_i = goa.nr.dat[,"Starting.Latitude..dd."], 
                 Lon_i = goa.nr.dat[,"Starting.Longitude..dd."], 
                 t_i = goa.nr.dat[,"Year"], 
                 b_i = goa.nr.dat[,"Weight..kg."], 
                 a_i = goa.nr.dat[,"Effort..km2."])


fit

# Check convergence

fit$parameter_estimates$Convergence_check

# Check maximum gradient

fit$parameter_estimates$max_gradient

# Next we will call the plot our diagnostic plots, spatial predictions, 
#   and create some other useful objects

plot(fit)

# We can plot a few additional/useful figures using the plot_results() function

?plot_results 

# Predictions for encounter probability across space and time:
plot_results(fit, plot_set=1)

# Finally, let's reset our working directory
setwd(wd)

# PLEASE INSPECT THE PLOTS YOU HAVE CREATED:
#   WHAT DO YOU NOTICE ABOUT THE Aniso.png FIGURE?
#   

# Challenge A: GOA Pollock =====================================================

# Please implement a VAST model for walleye pollock in the Gulf of Alaska














