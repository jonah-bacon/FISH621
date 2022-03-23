# =====================================================================
# Homework 3
# Jonah Bacon
# 22 March 2022
# FISH 621
# =====================================================================


# Load packages -----------------------------------------------------------

library(tidyr)
library(tidyverse)
library(ggplot2)

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

str.1 <- moose.dat %>% filter(str == 1)
str.2 <- moose.dat %>% filter(str == 2)
str.3 <- moose.dat %>% filter(str == 3)

# Part 1

moose.n.h <- moose.dat %>% 
  group_by(str) %>% 
  summarise("n.h" = length(str))
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

# Part 1

yearly.total.biomass.cpue <- perch.dat %>% 
  filter(Common.Name == "Pacific ocean perch") %>% 
  group_by(Year) %>% 
  summarize("Total.Biomass.CPUE" = sum(Weight.CPUE..kg.km2.))
print(yearly.total.biomass.cpue)

ggplot(data = yearly.total.biomass.cpue, aes(x = Year, y = Total.Biomass.CPUE)) +
  geom_point() +
  geom_line()

# Part 2

yearly.n.stations <- perch.dat %>% 
  filter(Common.Name == "Pacific ocean perch") %>% 
  group_by(Year) %>% 
  summarize("n.stations" = length(unique(Haul.Join.ID)))
yearly.n.stations

# Part 3

