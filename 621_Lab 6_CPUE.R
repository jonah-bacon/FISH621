#==================================================================================================
#Project Name: FISH 621 Estimation of Fish Abundance - Lab 6
#Creator: Curry James Cunningham, College of Fisheries and Ocean Sciences, UAF
#Date: 3.18.22
#
#Purpose: Explore methods for working with and standardizing CPUE data
#
#
#
#==================================================================================================

# Exercise 1: Loading Required Packages ========================================

# Please install the following packages, if they will not load when you
#   call library() as below. 


library(tidyverse)
library(ggthemes)
library(dplyr)
library(rstan)
library(visreg)
library(performance)
library(equatiomatic)
library(coefplot)
library(rnaturalearth)
library(see)
library(patchwork)

# Next week we will begin exploring Vector Autoregressive Spatio-temporal (VAST)
#   models in R.
# To avoid any hiccups during next week's lab, I would like everyone
#   to start by installing the software.

# GitHub site:
# https://github.com/James-Thorson-NOAA/VAST


# This package depends on R version >=3.1.1 and a variety of other tools,
#   and a series of inter-related R packages.

# As VAST is a custom package we will be installing it from GitHub Jim Thorson's
#   GitHub repository rather than from CRAN. 
# The devtools package allows us to install packages from GitHub

# First, install the "devtools" package from CRAN

# Install and load devtools package

# install.packages("devtools") # UNCOMMENT ME AS NECESSARY

library(devtools)

# Next, please install the VAST package from this GitHub repository using a 
# function in the "devtools" package. 

# Install package

# install_github("james-thorson/VAST") # UNCOMMENT ME AS NECESSARY

# This may require using the INSTALL_opts option depending upon your version of R:
  
# install_github("james-thorson/VAST", INSTALL_opts="--no-staged-install") # UNCOMMENT ME AS NECESSARY

# Try loading the VAST package
library(VAST)

# If you are having problems with installation, please consider installing 
#   dependencies individually, e.g. using:
  
# Install TMB from CRAN

# install.packages("TMB") # UNCOMMENT ME AS NECESSARY

# Install INLA using currently recommended method

# install.packages("INLA", repos=c(getOption("repos"), # UNCOMMENT ME AS NECESSARY
                                 # INLA="https://inla.r-inla-download.org/R/stable"), 
                 # dep=TRUE)
# Install FishStatsUtils from CRAN
# install_github("james-thorson/FishStatsUtils", INSTALL_opts="--no-staged-install") # UNCOMMENT ME AS NECESSARY

# Exercise 2: Halibut CPUE Standarization ======================================

# In this example we will explore fishery-dependent CPUE data from from 
#   the longline fishery for Pacific halibut. 
# Vessels participating in this fishery historically used two kinds of gear:
#   a) fixed-hook: hooks tied directly onto th elongline at a fixed hook spacing
#   b) snap-on: hooks snapped onto the gear as the line goes out.

# Historically the International Pacific Halibut commission used only data
#   from fixed-hook vessels to calculate a CPUE index. 
# However, over the years an increasing number of vessels began using snap-on gear



# Load CPUE Data ==============================================
cpue.dat <- read.csv(file=file.path("Halibut CPUE.csv"), header=TRUE)

# Lets check it was read in correctly..
head(cpue.dat)
str(cpue.dat)

cpue.dat

# Inspect the available data, what do you see?
#   Based on CPUE (U) what is your initial sense of the relative fishing power
#     of Snap vs. Fixed gears?

# The next step in our process will be to organize our data so we can fit
#   a simple GLM to conduct our standardization.

# Define Factors ==============================================
# Given that our Area and Gear predictors were read in as character strings, and
#   our year reference was read in as an integer, we will need to convert all of these to factors.

cpue.dat$fArea <- factor(cpue.dat$Area)
cpue.dat$fGear <- factor(cpue.dat$Gear)
cpue.dat$fYear <- factor(cpue.dat$Year, levels=sort(unique(cpue.dat$Year)))


# Let's check this worked
str(cpue.dat)

# Let's plot our CPUE by gear type
ggplot(cpue.dat, aes(x=Area, y=U, fill=Gear)) +
  theme_linedraw() +
  geom_boxplot()

# What can you infer about relative abundance among regions from these CPUE data?
# What can you infer about the relative catch efficency (fishing power) of 
#   our two gear tyeps?

# Next let's plot the actual timeseries of CPUE to get a sense of trends

ggplot(cpue.dat, aes(x=Year, y=U, color=Gear)) +
  theme_linedraw() +
  geom_line() +
  geom_point() +
  facet_wrap(~ fArea)
  
# What is the trend in CPUE? What does this say about the tend in our stock?

# Define reference level for factors ==========================
# Next we need to define the reference level for our factors, this will be:
#   Year: 1980
#   Area: SE-AK-Inside (US)
#   Gear: Fixed

# We can define the reference level for factors in R using the relevel() function
?relevel

cpue.dat$fYear <- relevel(cpue.dat$fYear, ref="1980")
cpue.dat$fArea <- relevel(cpue.dat$fArea, ref="SE-AK-Inside (US)")
cpue.dat$fGear <- relevel(cpue.dat$fGear, ref="Fixed")

# Fit GLM =====================================================
# Next we can fit our GLM to estimate our year effects (as factors) to 
#   discern our index, while control for our areas and gear types.

# By estimating factor effects for our different gears, we can estimate 
#   fishing power coefficient describing the relative catch efficiency of our 
#     snap gear relative to our reference level (fixed gear)

fit.cpue <- glm(ln_U ~ fYear + fGear + fArea, data=cpue.dat, family=gaussian(link="identity"))

# Check Structure of Model against what we intend

# A fun and easy way to extract a LaTex format equation for our equation
#   can be found using the extract_eq() function in the equatiomatic package.

# This can be useful for embedding this equation in a document
#   or Rmarkdown script, however it is rather difficult for us to interpret
#     as human readers

equatiomatic::extract_eq(fit.cpue)

# Here is a more human-friendly version.
formula(fit.cpue)

# Lets see the summary of our estimated coefficients
summary(fit.cpue)

# How would you interpret these coefficients?

# Well, the intercept becomes the expected CPUE in 1980 for SE-AK-Inside waters,
#   for Fixed gear.

# All other effects are relative to this reference level:
#   1) Snap gear has a SIGNIFICANTLY lower CPUE on average compared to fixed gear
#   2) The Kodiak area has significantly higher CPUE compared with SEAK.
#   3) Some years have significantly lower CPUE compared with 1980, some significantly higher
#       with a general trend toward increasing CPUE

# As always, let's check our regression assumptions...
performance::check_model(fit.cpue)

# Next let's visualize the effects
visreg(fit.cpue, type="conditional")

# Or alternatively 
coefplot(fit.cpue)

# Plot Resulting Fit =========================================
# To plot the resulting fit, lets add a new column to our data frame with the predicted ln(CPUE) i.e. pred_ln_U

# Predicted values
pred_ln_U <- predict(fit.cpue)

# Attach to data frame
cpue.dat <- data.frame(cpue.dat, pred_ln_U)

str(cpue.dat)

# Extract Coefficients
coef(fit.cpue)

# Our fishing power coefficients are Pi = exp(Bi)
exp(coef(fit.cpue)["fGearSnap"])
exp(coef(fit.cpue)["fAreaKodiak (US)"])

# Now that we have both observed and predicted values, lets plot the fit

ggplot(data=cpue.dat, aes(x=Year, y=ln_U, color=fGear)) +
  theme_linedraw() +
  geom_point() +
  facet_wrap(~fArea) +
  geom_line(aes(y=pred_ln_U))

# Or alternatively we can just plot predicted ln_CPUE ~ observed ln_CPUE
plot(pred_ln_U~ln_U, data=cpue.dat,
       xlab="Observed CPUE", ylab="Predicted CPUE",
       pch=21, bg="blue")
abline(a=0, b=1, col="red")

# Not bad!

# Write out Predicted CPUE ====================================
write.csv(cpue.dat, "Updated Halibut CPUE.csv")


# Generating Index of Abundance =======================

# In order to get our index of abundance (in log space) and uncertainty (approximate 95% CI)
#   we can back predict our year effects based on our fitted model 

# By setting se=TRUE we will get the standard error for our prediction

?predict.glm

predict(fit.cpue, newdata=list(fYear=factor(1976), 
                                  fGear="Fixed", 
                                  fArea="SE-AK-Inside (US)"), se=TRUE)

predict(fit.cpue, newdata=list(fYear=factor(1977), 
                               fGear="Fixed", 
                               fArea="SE-AK-Inside (US)"), se=TRUE)

predict(fit.cpue, newdata=list(fYear=factor(1978), 
                               fGear="Fixed", 
                               fArea="SE-AK-Inside (US)"), se=TRUE)

predict(fit.cpue, newdata=list(fYear=factor(1979), 
                               fGear="Fixed", 
                               fArea="SE-AK-Inside (US)"), se=TRUE)

predict(fit.cpue, newdata=list(fYear=factor(1980), 
                               fGear="Fixed", 
                               fArea="SE-AK-Inside (US)"), se=TRUE)



# Or alternatively we can generate our index of abundance and uncertainty
#   for all years in the sequence

# First, we will identify the unique set of years
years <- sort(unique(cpue.dat$Year))
n.years <- length(years)

# Then create an object to hold the predicted CPUE for Fixed Gear, in Kodiak
cpue.SEAK.fixed <- array(dim=c(n.years, 6), dimnames=list(years, c("Year", "lnCPUE", "SE", "CV", "low95","up95")))

# Loop through years and predict lnCPUE for fixed gear 
y <- 1
for(y in 1:n.years) {
  # Predict lnCPUE
  temp.pred <-  predict(fit.cpue, newdata=list(fYear=factor(years[y]), 
                                               fGear="Fixed", 
                                               fArea="SE-AK-Inside (US)"), se=TRUE)

  # Populate our output array
  cpue.SEAK.fixed[y,1] <- years[y] # Year
  cpue.SEAK.fixed[y,2] <- temp.pred$fit # lnCPUE
  cpue.SEAK.fixed[y,3] <- temp.pred$se # Standard Error
  cpue.SEAK.fixed[y,4] <- temp.pred$se/temp.pred$fit # CV
  cpue.SEAK.fixed[y,5] <- temp.pred$fit - 1.96*temp.pred$se # Approximate lower bound of 95% CI
  cpue.SEAK.fixed[y,6] <- temp.pred$fit + 1.96*temp.pred$se # Approximate upper bound of 95% CI
}

cpue.SEAK.fixed


# Convert cpue array with year effects to a data frame and plot
#   index and uncertainty
cpue.SEAK.fixed <- data.frame(cpue.SEAK.fixed)

ggplot(cpue.SEAK.fixed, aes(x=Year, y=lnCPUE)) + 
                      geom_line() +
                      geom_point(pch=21, fill="red") +
                      geom_ribbon(aes(ymin=low95, ymax=up95), fill="red", alpha=0.2)



# However, we have one additional step before we can utilize our CPUE index,
#   we need to convert it back into normal CPUE space from where it is currently
#     predicted in ln(CPUE) space

# You will recall from your prior statistics courses that a "lognormal correction"
#   must be applied so that the expected value to the exponentiated variable
#    is equivalent to the expected value in normal space

# So in order to get the expected value for our index E(CPUE) from E(ln(CPUE)) 

# We need to exponentiate our expected CPUE and add the variance/2 or 
#   the residual standard deviation squared, divided by two

# First, lets extract the standard deviation of our normally distributed residuals
hist(fit.cpue$residuals)

sigma <- sd(fit.cpue$residuals)
sigma

# Ok now we can generate our expected CPUE index in  exponentiate our Predicted CPUE Index
cpue.SEAK.fixed$CPUE <- exp(cpue.SEAK.fixed$lnCPUE + (sigma^2)/2)


# Now lets again plot our CPUE Index back-transformed into Normal Space
ggplot(cpue.SEAK.fixed, aes(x=Year, y=CPUE)) + 
         geom_line() +
         geom_point(pch=21, fill="red") 

# Huzzah! We did it!

# Log link ===================

# An alternative way to approach this CPUE standardization problem,
#   rather than specifying log(CPUE) as the response, we can 

fit.cpue_link <- glm(U ~ fYear + fGear + fArea, data=cpue.dat, family=gaussian(link="log"))
summary(fit.cpue_link)

predict(fit.cpue_link, newdata=list(fYear=factor(1977), 
                               fGear="Fixed", 
                               fArea="SE-AK-Inside (US)"), se=TRUE,
        type="response")

cpue.SEAK.fixed_link <- array(dim=c(n.years, 6), dimnames=list(years, c("Year", "CPUE", "SE", "CV", "low95","up95")))

# Loop through years and predict lnCPUE for fixed gear 
y <- 1
for(y in 1:n.years) {
  # Predict lnCPUE
  temp.pred <-  predict(fit.cpue_link, newdata=list(fYear=factor(years[y]), 
                                               fGear="Fixed", 
                                               fArea="SE-AK-Inside (US)"), se=TRUE,
                        type="response")
  
  # Populate our output array
  cpue.SEAK.fixed_link[y,1] <- years[y] # Year
  cpue.SEAK.fixed_link[y,2] <- temp.pred$fit # lnCPUE
  cpue.SEAK.fixed_link[y,3] <- temp.pred$se # Standard Error
  cpue.SEAK.fixed_link[y,4] <- temp.pred$se/temp.pred$fit # CV
  cpue.SEAK.fixed_link[y,5] <- temp.pred$fit - 1.96*temp.pred$se # Approximate lower bound of 95% CI
  cpue.SEAK.fixed_link[y,6] <- temp.pred$fit + 1.96*temp.pred$se # Approximate upper bound of 95% CI
}
cpue.SEAK.fixed_link <- data.frame(cpue.SEAK.fixed_link)

ggplot(cpue.SEAK.fixed_link, aes(x=Year, y=CPUE)) + 
  geom_line() +
  geom_point(pch=21, fill="red") +
  geom_ribbon(aes(ymin=low95, ymax=up95), fill="red", alpha=0.2)

# If we compare our coefficeints we will see that the results are not 
#   exactly the same, but close
coef(fit.cpue)
coef(fit.cpue_link)

# Next we can compare the performance of this CPUE standardization model with 
#   alternatives that do not control for gear effects or area effects
fit.cpue.noGear <- glm(ln_U ~ fYear + fArea, data=cpue.dat, family=gaussian(link="identity"))
fit.cpue.noArea <- glm(ln_U ~ fGear + fYear, data=cpue.dat, family=gaussian(link="identity"))
fit.cpue.onlyYear <- glm(ln_U ~ fYear, data=cpue.dat, family=gaussian(link="identity"))

# Compare performance of these models
(comp.cpue <- compare_performance(fit.cpue, fit.cpue.noGear, fit.cpue.noArea, fit.cpue.onlyYear))

plot(comp.cpue)

# We can clearly see across a variety of metrics that our fit.cpue model
#   that has gear and area effects is preferred. 

# Exercise 3: Bottom Trawl Survey Data =========================================

# In this exercise we will explore bottom trawl survey data collected by
#   the NOAA-AFSC RACE program in the Gulf of Alaska and Eastern Bering Sea Shelf

# Please load the two associated files into R

# The catch observations, where each row is the catch from a particular haul
#   (i.e. sample in space and time), for a set of species.
bts.dat <- read.csv("race_cpue_by_haul.csv")

str(bts.dat)

# The common name for each species is provided
unique(bts.dat$Common.Name)

# Along with the identifier for the spatial region of each survey
unique(bts.dat$Survey)

# We can quickly calculate the total number of observations for a single species
#   

summary.bts.dat <- bts.dat %>% group_by(Survey, Year , Common.Name) %>% summarize("n"=n())
summary.bts.dat

View(summary.bts.dat)

# Our first step should be to check that we have the same number of observations
#   for each species in each year-survey combination.

# We can plot for a single species the number of observations across time
summary.pollock <- summary.bts.dat %>% filter(Common.Name=="walleye pollock")
summary.pollock

ggplot(summary.pollock, aes(x=Year, y=n, fill=Survey)) +
  theme_linedraw() +
  scale_fill_colorblind() +
  geom_col() +
  facet_wrap(~Survey) +
  ylab("Number of Survey Hauls")

# Next, we will load data associated with the survey strata
strata.data <- read.csv("Stratum Descriptions.csv")

str(strata.data)

# From these data we know that area (Area..km2.) for each strata within each
#   Survey

# Plotting BTS Data ======================================
# First we will begin by visualizing the survey data

# We will start by subsetting out data for a specific species, walleye pollock

pol.dat <- bts.dat %>% filter(Common.Name=="walleye pollock")

head(pol.dat)

names(pol.dat)

# We can easily plot a single year for the Eastern Bering Sea in 2019

# Both the starting and ending latitude and longitude are recorded for each
#   survey haul
# We will use the starting coordinates as our reference location
# "Starting.Longitude..dd." and "Starting.Latitude..dd."

pol.dat %>% filter(Survey=="EBS_SHELF", Year==2019) %>% 
  ggplot(aes(x=Starting.Longitude..dd., y=Starting.Latitude..dd.)) +
  geom_point() +
  ggtitle("Walleye Pollock", subtitle=2019)

# These are our individual survey locations.

# Next, lets color these points proportional to log(CPUE),
#   or the natural log ow weight CPUE: The number of kg of pollock caught per square
#     kilometer "Weight.CPUE..kg.km2."
pol.dat %>% filter(Survey=="EBS_SHELF", Year==2019) %>% 
  ggplot(aes(x=Starting.Longitude..dd., y=Starting.Latitude..dd.,
             color=log(Weight.CPUE..kg.km2.))) +
  geom_point() +
  scale_color_viridis_c() +
  ggtitle("EBS Walleye Pollock", subtitle=2019)

# Higher log CPUE is in yellow, lower log CPUE is in blue

# WHAT DO YOU INFER ABOUT THE DISTRIBUTION OF POLLOCK BIOMASS ACROSS SPACE IN 
#   THIS YEAR?

# We could of course re-size our points based on the log(CPUE),but this is a bit 
#   less helpful
pol.dat %>% filter(Survey=="EBS_SHELF", Year==2019) %>% 
  ggplot(aes(x=Starting.Longitude..dd., y=Starting.Latitude..dd.,
             color=log(Weight.CPUE..kg.km2.), size=log(Weight.CPUE..kg.km2.))) +
  geom_point() +
  scale_color_viridis_c() +
  ggtitle("EBS Walleye Pollock", subtitle=2019)

# It might be nice to fill in land forms around our survey to orient ourselves.

# To create a map, we usually need base layers describing the spatial extent of
#   landmasses. To obtain these data easily for plotting purposes
#     we can use the ne_countries() function in rnaturalearth package

?ne_countries

world <- ne_countries(scale = "medium", returnclass = "sf")

# What is the structure of the object that was retuned
str(world)

# This is a data frame of the class "sf" for simple features

# Ok, lets overlay our CPUE observations over our the base map we have 
#   downloaded...
g <- ggplot(data = world) +
  theme_linedraw() +  
  geom_sf() +
  coord_sf(xlim = c(-180, -155), ylim = c(52, 63), expand = TRUE)  +
  geom_point(data=pol.dat[pol.dat$Year==years[i] & pol.dat$Survey=="EBS_SHELF",],
             aes(x=Starting.Longitude..dd., y=Starting.Latitude..dd., 
                 color=log(Weight.CPUE..kg.km2.)), alpha=0.5) +
  scale_color_viridis_c() +
  ggtitle("EBS Walleye Pollock", subtitle=2019)
g


# Challenge A: Plotting BTS Data ===============================================

# In order to continue exploring the BTS data, please:
# 1) Find a reasonable way to plot multiple years of log pollock CPUE
# 2) Please explore data from the Gulf of Alaska BTS for pollock and other species
#   We will share our figures in class, as well as what inference we derive from them.
str(pol.dat)

## Question - I don't know how to index the Year variable in sequential order

years <- sort(unique(pol.dat$Year))

plot.list <- list()
for (i in 1:length(unique(pol.dat$Year))) {
  g <- eval(substitute(
    ggplot(data = world) +
      theme_linedraw() +  
      geom_sf() +
      coord_sf(xlim = c(-180, -155), ylim = c(52, 63), expand = F)  +
      geom_point(data=pol.dat[pol.dat$Year==years[i] & pol.dat$Survey=="EBS_SHELF",],
                 aes(x=Starting.Longitude..dd., y=Starting.Latitude..dd., 
                     color=log(Weight.CPUE..kg.km2.)), alpha=0.7) +
      scale_color_viridis_c() +
      ggtitle("EBS Walleye Pollock", subtitle=years[i]) +
      xlab("Starting Latitude") +
      ylab("Starting Longitude")
    , list(i = i)))
  
  print(i)
  print(g)
  plot.list[[i]] <- g
}

pdf("all.pdf")
invisible(lapply(plot.list, print))
dev.off()


# ==============================================================================



# Design-based Abundance Indices ===============================================

# Here we will practice calculating area-weighted design-based biomass estimates
#   from our BTS data.

# This process involves considering CPUE by strata as well as the areas of the 
#   individual strata.

# Remember, as our unit of effort is km^2, we can easily scale this up to a total 
#   biomass estimate for the entire region if we know the area of our individual
#   strata (km^2)

# Lets start by using pollock from the Gulf of Alaska as an example
unique(bts.dat$Survey)
load.data <- bts.dat %>% filter(Common.Name=="walleye pollock", Survey=="GOA")

# There will be several steps involved in calculating our area-weighted
#   biomass estimates, they are as follows:

# 1) Calculate sum and variance in cpue by year and stratum
cstrat <- data.frame(load.data %>% group_by(Year, Stratum) %>% 
                       summarize(CPUE=sum(Weight.CPUE..kg.km2.), CPUEvar=var(Weight.CPUE..kg.km2.))) #dplyr

# Lets inspect this new object
str(cstrat)
head(cstrat)

# Challenge B: Visualizing Stratum-specific CPUE ===============================

# Please visualize the stratum-specific cpue variance in CPUE for this species across time

ggplot(data = cstrat, aes(x=Year, y=CPUE, color = factor(Stratum))) +
  geom_point() +
  geom_line() +
  ggtitle("Stratum-specific CPUE", subtitle="GOA Walleye Pollock") +
  xlab("Year") +
  ylab("CPUE")

# ==============================================================================

# 2) Calculate number of hauls in each year and stratum
#     In order to determine y_bar_h the average biomass per stratum,
#       we will need to know the number of hauls that occurred within each stratum in 
#         each year.
#   We can do this by summarizing our data by Year and Stratum and counting the number
#      of unique HAUL.JOIN.IDs
#   These represent unique identifiers for each sampling event

hstrat <- data.frame(load.data %>% group_by(Year, Stratum) %>% summarize(n_sta=length(unique(Haul.Join.ID)))) #dplyr

str(hstrat)
head(hstrat)


# 3) Next we will join our prior table of average CPUE and variance in CPUE
#      by strata and year with our number of survey observations per for each stratum and year
biomvar <- left_join(cstrat, hstrat, by=c("Year", "Stratum")) #dplyr
colnames(biomvar) <- c("YEAR","STRATUM","CPUE","VAR","n_stations")

# Inspect the resulting object
str(biomvar)
head(biomvar)


# 4) Next need our data for the individual strata to calculate the area of each
#      strata so we can scale up our biomass estimates for each stratm

# We already loaded it, so we can re-familiarize ourselves here...
head(strata.data)

# 

# 5) We will then filter the strata data for the correct survey area
#      and just extract the variables (columns) we need

strata.area <- strata.data %>% filter(Survey=="GOA") %>% select("Stratum", "Area..km2.", 
                                                                "Stratum.INPFC.Area","Stratum.Regulatory.Area.Name")
names(strata.area) <- c("STRATUM","AREA","INPFC_AREA","REGULATORY.AREA")

# What is the average size in km^2 for our strata, and the distribution
summary(strata.area$AREA)

hist(strata.area$AREA)

# 6) We will need to join our strata area data to the CPUE data by year and stratum
#      we previously calculated.

# A left_join() will allow us to attach the strata area info to our CPUE object
biomvar <- left_join(biomvar, strata.area, by=c("STRATUM")) #dplyr

head(biomvar)

# 7) Next we will calculate our key summary information by year and strata

# We can calculate the biomass (kg) per stratum by:
#   a) calculating the average cpue/station within each stratum, and multiplying that 
#        by the area of each stratum

biomvar$BIOMASS<-(biomvar$CPUE/biomvar$n_stations)*biomvar$AREA

# The variance in biomass per stratum is the (area)^2 * (variance/number of stations)
biomvar$VAR2<-biomvar$AREA^2*(biomvar$VAR/biomvar$n_stations)

# 8) Finally, we can calculate our total biomass and variance in biomass
#      by summing across strata


biomass <- data.frame(biomvar %>% group_by(YEAR) %>%
                        summarize(Biomass=sum(BIOMASS,na.rm=TRUE)/1e3,
                                  Variance=sum(VAR2,na.rm=TRUE)/(1e3^2),
                                  SD=sqrt(sum(VAR2,na.rm=TRUE))/1e3,
                                  CV=SD/(sum(BIOMASS,na.rm=TRUE)/1e3)))
biomass

# The result is the estimated total biomass and uncertainty by year
#   aggregated across strata.

# Challenge C: Design-based Estimates ==========================================

# Please calculate design-based biomass estimates from the bottom trawl survey data
#   for northern rockfish and yellowfin sole in the Gulf of Alaska

goa.data <- bts.dat %>% filter(Common.Name %in% c("northern rockfish","yellowfin sole"), Survey=="GOA")

# 1) Calculate sum and variance in cpue by year and stratum
cstrat1 <- data.frame(goa.data %>% group_by(Year, Stratum, Common.Name) %>% 
                       summarize(CPUE=sum(Weight.CPUE..kg.km2.), CPUEvar=var(Weight.CPUE..kg.km2.))) #dplyr

# Lets inspect this new object
str(cstrat1)
head(cstrat1)

# 2) Calculate number of hauls in each year and stratum
#     In order to determine y_bar_h the average biomass per stratum,
#       we will need to know the number of hauls that occurred within each stratum in 
#         each year.
#   We can do this by summarizing our data by Year and Stratum and counting the number
#      of unique HAUL.JOIN.IDs
#   These represent unique identifiers for each sampling event

hstrat1 <- data.frame(goa.data %>% group_by(Year, Stratum, Common.Name) %>% summarize(n_sta=length(unique(Haul.Join.ID)))) #dplyr

str(hstrat1)
head(hstrat1)


# 3) Next we will join our prior table of average CPUE and variance in CPUE
#      by strata and year with our number of survey observations per for each stratum and year
biomvar1 <- left_join(cstrat1, hstrat1, by=c("Year", "Stratum", "Common.Name")) #dplyr
colnames(biomvar1) <- c("YEAR","STRATUM","COMMON.NAME","CPUE","VAR","n_stations")

# Inspect the resulting object
str(biomvar1)
head(biomvar1)


# 4) Next need our data for the individual strata to calculate the area of each
#      strata so we can scale up our biomass estimates for each stratm

# We already loaded it, so we can re-familiarize ourselves here...
head(strata.data)

# 

# 5) We will then filter the strata data for the correct survey area
#      and just extract the variables (columns) we need

strata.area <- strata.data %>% filter(Survey=="GOA") %>% select("Stratum", "Area..km2.", 
                                                                "Stratum.INPFC.Area","Stratum.Regulatory.Area.Name")
names(strata.area) <- c("STRATUM","AREA","INPFC_AREA","REGULATORY.AREA")

# What is the average size in km^2 for our strata, and the distribution
summary(strata.area$AREA)

hist(strata.area$AREA)

# 6) We will need to join our strata area data to the CPUE data by year and stratum
#      we previously calculated.

# A left_join() will allow us to attach the strata area info to our CPUE object
biomvar1 <- left_join(biomvar1, strata.area, by=c("STRATUM")) #dplyr

head(biomvar1)

# 7) Next we will calculate our key summary information by year and strata

# We can calculate the biomass (kg) per stratum by:
#   a) calculating the average cpue/station within each stratum, and multiplying that 
#        by the area of each stratum

biomvar1$BIOMASS<-(biomvar1$CPUE/biomvar1$n_stations)*biomvar1$AREA

# The variance in biomass per stratum is the (area)^2 * (variance/number of stations)
biomvar1$VAR2<-biomvar1$AREA^2*(biomvar1$VAR/biomvar1$n_stations)

# 8) Finally, we can calculate our total biomass and variance in biomass
#      by summing across strata


biomass.northern.rockfish <- data.frame(biomvar1 %>% 
                                          filter(COMMON.NAME == "northern rockfish") %>% 
                                          group_by(YEAR) %>%
                                          summarize(Biomass=sum(BIOMASS,na.rm=TRUE)/1e3,
                                                    Variance=sum(VAR2,na.rm=TRUE)/(1e3^2),
                                                    SD=sqrt(sum(VAR2,na.rm=TRUE))/1e3,
                                                    CV=SD/(sum(BIOMASS,na.rm=TRUE)/1e3)))
biomass.northern.rockfish

biomass.yellowfin.sole <- data.frame(biomvar1 %>% 
                                          filter(COMMON.NAME == "yellowfin sole") %>% 
                                          group_by(YEAR) %>%
                                          summarize(Biomass=sum(BIOMASS,na.rm=TRUE)/1e3,
                                                    Variance=sum(VAR2,na.rm=TRUE)/(1e3^2),
                                                    SD=sqrt(sum(VAR2,na.rm=TRUE))/1e3,
                                                    CV=SD/(sum(BIOMASS,na.rm=TRUE)/1e3)))
biomass.yellowfin.sole

# ==============================================================================

# Model-based CPUE Indices ====================================================
# Now that we have calculated our design-based absolute biomass indices
#   for pollock in the GOA, let's compare these

# With our model-based GLM approach
load.dat <- bts.dat %>% filter(Common.Name=="walleye pollock", Survey=="GOA")
str(load.dat)

# Recall that we the absolutely necessary piece of our GLM for CPUE standardization
#   is the factor year effect

# Lets convert year to a factor
load.dat$fYear <- factor(load.dat$Year)

glm.pol.goa <- glm(log(Weight.CPUE..kg.km2.+1) ~ fYear, data=load.dat)
summary(glm.pol.goa)

visreg(glm.pol.goa)


# Challenge D: Model-based Pollock Biomass Index ===============================
# Please extract your CPUE index, transform it appropriately into normal space
#   and compare it with the design-based estimate.

# Please complete this process also for northern rockfish and yellowfin sole

nrrf.dat <- bts.dat %>% filter(Common.Name=="northern rockfish", Survey=="GOA")
ylfs.dat <- bts.dat %>% filter(Common.Name=="yellowfin sole", Survey=="GOA")

nrrf.dat$fYear <- factor(nrrf.dat$Year)
ylfs.dat$fYear <- factor(ylfs.dat$Year)

glm.nrrf.goa <- glm(log(Weight.CPUE..kg.km2.+1) ~ fYear, data=nrrf.dat)
glm.ylfs.goa <- glm(log(Weight.CPUE..kg.km2.+1) ~ fYear, data=ylfs.dat)


years <- sort(unique(load.dat$Year))
n.years <- length(years)

# Create output vectors for first set of GLM model predictions:
CPUE.glm.pol <- rep(NA,length(years))
CPUE.glm.nrrf <- rep(NA,length(years))
CPUE.glm.ylfs <- rep(NA,length(years))

# Calculate sigma from each model:
sigmas <- rep(NA,3)
sigmas[1] <- sd(glm.pol.goa$residuals)
sigmas[2] <- sd(glm.nrrf.goa$residuals)
sigmas[3] <- sd(glm.ylfs.goa$residuals)

# Loop through and create CPUE predictions:
y <- 1
for(y in 1:n.years) {
  
  # Predict lnCPUE from first set of GLM models:
  glm.pol.goa.pred <-  predict(glm.pol.goa, newdata=list(fYear=factor(years[y])), se=TRUE)
  glm.nrrf.goa.pred <-  predict(glm.pol.goa, newdata=list(fYear=factor(years[y])), se=TRUE)
  glm.ylfs.goa.pred <-  predict(glm.pol.goa, newdata=list(fYear=factor(years[y])), se=TRUE)
  
  # Populate our output vectors:
  CPUE.glm.pol[y] <- exp(glm.pol.goa.pred$fit + (sigmas[1]^2)/2)
  CPUE.glm.nrrf[y] <- exp(glm.nrrf.goa.pred$fit + (sigmas[2]^2)/2)
  CPUE.glm.ylfs[y] <- exp(glm.ylfs.goa.pred$fit + (sigmas[3]^2)/2)
  
}

basic.model.df <- data.frame(
  "model.walleye.pollock" = CPUE.glm.pol, 
  "model.northern.rockfish" = CPUE.glm.nrrf, 
  "model.yellowfin.sole" = CPUE.glm.ylfs
  )

basic.model.df


# Challenge E: Model-based Index with Covariates ===============================
# Please consider what other covariates you might want to include in your
#   model based GLM for index standardization.

# Please implement alternative models and compare their performance and their
#   resulting predictions for the CPUE index timeseries.

load.dat$fStratum <- factor(load.dat$Stratum)
nrrf.dat$fStratum <- factor(nrrf.dat$Stratum)
ylfs.dat$fStratum <- factor(ylfs.dat$Stratum)

glm.pol.goa1 <- glm(log(Weight.CPUE..kg.km2.+1) ~ fYear + fStratum, data=load.dat)
glm.nrrf.goa1 <- glm(log(Weight.CPUE..kg.km2.+1) ~ fYear + fStratum, data=nrrf.dat)
glm.ylfs.goa1 <- glm(log(Weight.CPUE..kg.km2.+1) ~ fYear + fStratum, data=ylfs.dat)


years <- sort(unique(load.dat$Year))
n.years <- length(years)
stratum <- sort(unique(load.dat$Stratum))
n.stratum <- length(unique(load.dat$Stratum))

# Create output vectors for second set of GLM model predictions:
CPUE.glm.pol1 <- rep(NA,length(years))
CPUE.glm.nrrf1 <- rep(NA,length(years))
CPUE.glm.ylfs1 <- rep(NA,length(years))

# Calculate sigma from each model:
sigmas1 <- rep(NA,3)
sigmas1[1] <- sd(glm.pol.goa1$residuals)
sigmas1[2] <- sd(glm.nrrf.goa1$residuals)
sigmas1[3] <- sd(glm.ylfs.goa1$residuals)

y <- 1
x <- 1
for (x in 1:n.stratum) {
  for(y in 1:n.years) {
    
    # Predict lnCPUE from second set of GLM models:
    glm.pol.goa1.pred <-  predict(glm.pol.goa1, newdata=list(fYear=factor(years[y]),
                                                             fStratum=factor(stratum[x])), se=TRUE)
    glm.nrrf.goa1.pred <-  predict(glm.pol.goa1, newdata=list(fYear=factor(years[y]),
                                                              fStratum=factor(stratum[x])), se=TRUE)
    glm.ylfs.goa1.pred <-  predict(glm.pol.goa1, newdata=list(fYear=factor(years[y]),
                                                              fStratum=factor(stratum[x])), se=TRUE)
    
    # Populate our output vectors:
    CPUE.glm.pol1[y] <- exp(glm.pol.goa1.pred$fit + (sigmas1[1]^2)/2)
    CPUE.glm.nrrf1[y] <- exp(glm.nrrf.goa1.pred$fit + (sigmas1[2]^2)/2)
    CPUE.glm.ylfs1[y] <- exp(glm.ylfs.goa1.pred$fit + (sigmas1[3]^2)/2)
    
  }
}

stratum.model.df <- data.frame(
  "model.walleye.pollock" = CPUE.glm.pol1, 
  "model.northern.rockfish" = CPUE.glm.nrrf1, 
  "model.yellowfin.sole" = CPUE.glm.ylfs1
)

stratum.model.df



### GAM Models
library(mgcv)

gam.pol.goa <- gam(log(Weight.CPUE..kg.km2.+1) ~ fYear + fStratum + s(Gear.Depth), data=load.dat)
gam.nrrf.goa <- gam(log(Weight.CPUE..kg.km2.+1) ~ fYear + fStratum + s(Gear.Depth), data=nrrf.dat)
gam.ylfs.goa <- gam(log(Weight.CPUE..kg.km2.+1) ~ fYear + fStratum + s(Gear.Depth), data=ylfs.dat)

years <- sort(unique(load.dat$Year))
n.years <- length(years)
stratum <- sort(unique(load.dat$Stratum))
n.stratum <- length(unique(load.dat$Stratum))

# Create output vectors for GAM model predictions:
CPUE.gam.pol <- rep(NA,length(years))
CPUE.gam.nrrf <- rep(NA,length(years))
CPUE.gam.ylfs <- rep(NA,length(years))

# Calculate sigma from each model:
sigmas2 <- rep(NA,3)
sigmas2[1] <- sd(gam.pol.goa$residuals)
sigmas2[2] <- sd(gam.nrrf.goa$residuals)
sigmas2[3] <- sd(gam.ylfs.goa$residuals)

y <- 1
x <- 1
for (x in 1:n.stratum) {
  for(y in 1:n.years) {
  
    # Predict lnCPUE from GAM models:
    gam.pol.goa.pred <-  predict(gam.pol.goa, newdata=list(fYear=factor(years[y]),
                                                           fStratum=factor(stratum[x])), se=TRUE)
    gam.nrrf.goa.pred <-  predict(gam.pol.goa, newdata=list(fYear=factor(years[y]),
                                                            fStratum=factor(stratum[x])), se=TRUE)
    gam.ylfs.goa.pred <-  predict(gam.pol.goa, newdata=list(fYear=factor(years[y]),
                                                            fStratum=factor(stratum[x])), se=TRUE)
    
    # Populate our output vectors:
    CPUE.gam.pol[y] <- exp(gam.pol.goa.pred$fit + (sigmas2[1]^2)/2)
    CPUE.gam.nrrf[y] <- exp(gam.nrrf.goa.pred$fit + (sigmas2[2]^2)/2)
    CPUE.gam.ylfs[y] <- exp(gam.ylfs.goa.pred$fit + (sigmas2[3]^2)/2)
    
  }
}

GAM.model.df <- data.frame(
  "model.walleye.pollock" = CPUE.gam.pol, 
  "model.northern.rockfish" = CPUE.gam.nrrf, 
  "model.yellowfin.sole" = CPUE.gam.ylfs
)

GAM.model.df
