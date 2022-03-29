# =====================================================================
# Homework 3
# Jonah Bacon
# 22 March 2022
# FISH 621
# =====================================================================


# Load packages -----------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(visreg)

# Problem 1 ---------------------------------------------------------------

n.h <- c(50,50,50)
y.bar.h <- c(10,20,30)
s.h <- c(2800,700,600)
N.h <- c(100,50,300)

# Part 1

tau.bar.h <- N.h*y.bar.h
tau.bar.h

# Part 2

tau.bar.st <- sum(tau.bar.h)
tau.bar.st

# Part 3

var.hat_tau.bar.st <- sum(N.h*(N.h - n.h)*s.h/n.h)
var.hat_tau.bar.st

# Part 4

CV.total.abund <- sqrt(var.hat_tau.bar.st)/tau.bar.st
CV.total.abund

# Part 5

ub.total.abund <- tau.bar.st + 1.96*sqrt(var.hat_tau.bar.st)
lb.total.abund <- tau.bar.st - 1.96*sqrt(var.hat_tau.bar.st)

ub.total.abund
lb.total.abund

# Part 6

y.bar.st <- (1/sum(N.h))*sum(tau.bar.h)
y.bar.st

# Part 7

var.hat_y.bar.st <- sum( (N.h/sum(N.h))^2 * ((N.h - n.h)/N.h) * (s.h/n.h) )
var.hat_y.bar.st

# Part 8

ub.average.density <- y.bar.st + 1.96*sqrt(var.hat_y.bar.st)
lb.average.density <- y.bar.st - 1.96*sqrt(var.hat_y.bar.st)

ub.average.density
lb.average.density


# Problem 2 ---------------------------------------------------------------

moose.dat <- read.csv("Yukon Moose Survey.csv")
head(moose.dat)
str(moose.dat)

moose.dat$str <- as.factor(moose.dat$str)

moose.N.h <- c(122,57,22)
moose.h <- c(1,2,3)

# Part 1

moose.n.h <- moose.dat %>% 
  group_by(str) %>% 
  summarise("n.h" = length(str))
moose.n.h
moose.n.h <- moose.n.h$n.h
moose.n.h

# Part 2

moose.y.bar.h <- moose.dat %>% 
  group_by(str) %>% 
  summarise(
    "n.h" = length(str),
    "y.bar.h" = (1/n.h * sum(moose)))
moose.y.bar.h
moose.y.bar.h <- moose.y.bar.h$y.bar.h
moose.y.bar.h

# Part 3

moose.y.bar.st <- (1/sum(moose.N.h)) * sum(moose.N.h * y.bar.h)
moose.y.bar.st

# Part 4

moose.s.h <- moose.dat %>% 
  group_by(str) %>% 
  summarize("s.h" = ( 1/(length(str) - 1) ) * sum( (moose - (1/length(str) * sum(moose)) )^2 ) )
moose.s.h
moose.s.h <- moose.s.h$s.h
moose.s.h

# Part 5

moose.var.hat_y.bar.st <- sum( (moose.N.h/sum(moose.N.h))^2 * ((moose.N.h - n.h)/moose.N.h) * (s.h/n.h) )
moose.var.hat_y.bar.st

# Part 6

moose.ub.average.density <- moose.y.bar.st + 1.96*sqrt(moose.var.hat_y.bar.st)
moose.lb.average.density <- moose.y.bar.st - 1.96*sqrt(moose.var.hat_y.bar.st)

moose.ub.average.density
moose.lb.average.density

# Part 7

moose.tau.hat.h <- moose.N.h * moose.y.bar.h
moose.tau.hat.h

# Part 8

moose.tau.hat.st <- sum(moose.tau.hat.h)
moose.tau.hat.st

# Part 9

moose.var.hat_tau.hat.st <- sum(moose.N.h * (moose.N.h - moose.n.h) * moose.s.h/moose.n.h)
moose.var.hat_tau.hat.st

# Part 10

moose.ub.total.abund <- moose.tau.hat.st + 1.96*sqrt(moose.var.hat_tau.hat.st)
moose.lb.total.abund <- moose.tau.hat.st - 1.96*sqrt(moose.var.hat_tau.hat.st)

moose.ub.total.abund
moose.lb.total.abund



# Problem 3 ---------------------------------------------------------------

perch.dat <- read.csv("race_cpue_by_haul.csv")
strat.dat <- read.csv("Stratum Descriptions.csv")

str(perch.dat)
head(perch.dat)

str(strat.dat)
head(strat.dat)

strata.area <- strat.dat %>% 
  filter(Survey == "GOA") %>% 
  select("Stratum", "Area..km2.")
names(strata.area) <- c("Stratum","Area")

perch.dat <- perch.dat %>% filter(Common.Name == "Pacific ocean perch", Survey == "GOA")
perch.dat <- left_join(perch.dat, strata.area, by = c("Stratum"))

# Part 1

yearly.total.stratum.biomass <- perch.dat %>% 
  group_by(Year) %>% 
  summarize(
    "Total.Stratum.Biomass" = sum(Weight.CPUE..kg.km2.),
  )
print(yearly.total.stratum.biomass)

ggplot(data = yearly.total.stratum.biomass, aes(x = Year, y = Total.Stratum.Biomass)) +
  geom_point() +
  geom_line()

# Part 2

yearly.n.stations <- perch.dat %>% 
  group_by(Year) %>% 
  summarize(
    "Total.Stratum.Biomass" = sum(Weight.CPUE..kg.km2.),
    "N.stations" = length(unique(Haul.Join.ID)))
yearly.n.stations

ggplot(data = yearly.n.stations, aes(x = Year, y = N.stations)) +
  geom_point() +
  geom_line()

# Part 3

area.weighted.biomass <- perch.dat %>% 
  group_by(Year,Stratum) %>% 
  summarize(
    "Total.Stratum.Biomass" = sum(Weight.CPUE..kg.km2.),
    "Total.Stratum.Biomass.Var" = var(Weight.CPUE..kg.km2.),
    "N.stations" = length(unique(Haul.Join.ID)),
    "Area" = unique(Area),
    "Area.Weighted.Biomass" = (Total.Stratum.Biomass/N.stations) * Area,
    "Area.Weighted.Biomass.Var" = Area^2 * (Total.Stratum.Biomass.Var/N.stations)
  )
area.weighted.biomass

design.based.biomass.est <- data.frame(area.weighted.biomass %>% group_by(Year) %>%
                        summarize(Biomass=sum(Area.Weighted.Biomass, na.rm=TRUE)/1e3, # Question - Why do we divide by 1000?
                                  Variance=sum(Area.Weighted.Biomass.Var, na.rm=TRUE)/(1e3^2),
                                  SD=sqrt(sum(Area.Weighted.Biomass.Var, na.rm=TRUE))/1e3,
                                  CV=SD/(sum(Area.Weighted.Biomass, na.rm=TRUE)/1e3)))
design.based.biomass.est

design.preds <- design.based.biomass.est %>% 
  select(Year, Biomass) %>% 
  mutate("method" = rep("design",length(Year)), "preds" = Biomass)
design.preds

ggplot(data = design.based.biomass.est, aes(x = Year, y = Biomass)) +
  geom_point() +
  geom_line() +
  ylab("Biomass") +
  xlab("Year") +
  scale_x_continuous(limits=c(1984,2021),breaks=c(unique(design.based.biomass.est$Year)), expand = c(0,1)) +
  theme(
    panel.background = element_blank(),
    axis.title.x = element_text(size=16, vjust = 0),
    axis.title.y = element_text(size =16),
    axis.text = element_text(size=13, color="black"), 
    axis.line=element_line()
  )

# Part 4
## Model-based estimates w/ year
perch.dat$fYear <- factor(perch.dat$Year)

biomass.index <- glm(log(Weight.CPUE..kg.km2. + 1) ~ fYear, data = perch.dat)
summary(biomass.index)

visreg(biomass.index)

performance::check_model(biomass.index)

sigma <- sd(biomass.index$residuals)                                           
sigma

glm.model.preds1 <- perch.dat %>% 
  group_by(Year) %>% 
  summarise(
    "log.CPUE.preds" = predict(biomass.index, newdata = list(fYear = factor(unique(Year)))),
    "CPUE.preds" = exp(log.CPUE.preds + (sigma^2)/2) - 1)
glm.model.preds1

ggplot(data = glm.model.preds1, aes(x = Year, y = CPUE.preds)) +
  geom_point() +
  geom_line() +
  ylab("CPUE predictions") +
  xlab("Year") +
  scale_x_continuous(limits=c(1984,2021),breaks=c(glm.model.preds1$Year), expand = c(0,1)) +
  theme(
    panel.background = element_blank(),
    axis.title.x = element_text(size=16, vjust = 0),
    axis.title.y = element_text(size =16),
    axis.text = element_text(size=13, color="black"), 
    axis.line=element_line()
  )

## Model-based estimates w/ year + stratum

perch.dat$fStratum <- factor(perch.dat$Stratum)

biomass.index2 <- glm(log(Weight.CPUE..kg.km2. + 1) ~ fYear + fStratum, data = perch.dat)
summary(biomass.index2)

visreg(biomass.index2)

performance::check_model(biomass.index2)


sigma2 <- sd(biomass.index2$residuals)                                           
sigma2

glm.model.preds2 <- perch.dat %>% 
  group_by(Year) %>% 
  summarise(
    "log.CPUE.preds" = predict(biomass.index2, newdata = list(fYear = factor(unique(Year)), fStratum = rep(factor(10)))),
    "CPUE.preds" = exp(log.CPUE.preds + (sigma2^2)/2) - 1)
glm.model.preds2


### Visualize and compare between the three methods:

design.preds <- design.based.biomass.est %>% 
  select(Year, Biomass) %>% 
  mutate("method" = rep("design",length(Year)), "preds" = Biomass) %>% 
  select(Year, method, preds)
design.preds

glm.model.preds1 <- glm.model.preds1 %>% 
  select(Year, CPUE.preds) %>% 
  mutate("method" = rep("model.year", length(Year)), "preds" = CPUE.preds) %>% 
  select(Year, method, preds)
glm.model.preds1

glm.model.preds2 <- glm.model.preds2 %>% 
  select(Year, CPUE.preds) %>% 
  mutate("method" = rep("model.year+stratum", length(Year)), "preds" = CPUE.preds) %>% 
  select(Year, method, preds)
glm.model.preds2

catch.preds.df <- rbind(
  design.preds,
  glm.model.preds1,
  glm.model.preds2
)
catch.preds.df



id.labs <- c("Design-based est.","Model-based. est (w/ year)","Model-based. est (w/ year + strata)")
names(id.labs) <- c("design","model.year","model.year+stratum")

ggplot(data = catch.preds.df, aes(x = Year, y = preds, fill = method)) +
  geom_area(alpha=0.5, show.legend = F) +
  facet_wrap(~method, scales="free_y", labeller = labeller(method = id.labs)) +
  ylab("Relative population prediction") +  
  theme(
    panel.background = element_blank(),
    axis.title.x = element_text(size=16, vjust = 0),
    axis.title.y = element_text(size =16),
    axis.text = element_text(size=13, color="black"),
    axis.line=element_line(),
    strip.text = element_text(size = 14)
  )
