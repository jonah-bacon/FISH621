# =====================================================================
# Homework 4
# Jonah Bacon
# 12 April 2022
# FISH 621
# =====================================================================


# Load packages -----------------------------------------------------------

library(ggplot2)
library(tidyverse)
library(Distance)

# Problem 1 ---------------------------------------------------------------

wren.df <- read.csv("wren.csv")
head(wren.df)
str(wren.df)

# Part 1

hist(wren.df$distance,
     xlab = "Detection distance", ylab = "Count of wrens",
     main = "Histogram of detection distances")

# Part 2

boxplot(wren.df$distance ~ wren.df$Sample.Label,
        xlab = "Transect", ylab = "Detection distance")

# Part 3

wren.df %>% 
  group_by(Sample.Label) %>% 
  summarise("wren.detects" = n()) %>% 
  ggplot(aes(x = Sample.Label, y = wren.detects)) +
  geom_col() +
  scale_x_continuous(limits = c(0.4,19.6), breaks = seq(1,19,1), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,14), breaks = seq(0,14,1), expand = c(0,0)) +
  xlab("Transect") +
  ylab("Number of wren detections") +
  theme(
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color = "black", size = 0.5),
    axis.title.x = element_text(size=16, vjust = 0),
    axis.title.y = element_text(size =16),
    axis.text = element_text(size=13, color="black"), 
    axis.line=element_line()
  )

# Part 4

hn.model <- ds(wren.df, key = "hn", adjustment = NULL, convert_units = 0.001) # AIC = 1418.188
hr.model <- ds(wren.df, key = "hr", adjustment = NULL, convert_units = 0.001) # AIC = 1412.133
uni.model <- ds(wren.df, key = "unif", adjustment = "cos", convert_units = 0.001) # AIC = 1416.433

# Part 5

par(mfrow=c(1,3))
plot(hn.model, nc=10)
plot(hr.model, nc=10)
plot(uni.model, nc=10)
dev.off()

gof_ds(hn.model) # p-value = 0.077
gof_ds(hr.model) # p-value = 0.189
gof_ds(uni.model) # p-value = 0.200

# Part 6

density.estimate.table <- data.frame(
  "Sighting_model" = c("Half-normal", "Hazard-rate", "Uniform"),
  "Density_estimates" = c(hn.model$dht$individuals$D$Estimate, hr.model$dht$individuals$D$Estimate, uni.model$dht$individuals$D$Estimate),
  "CV" = c(hn.model$dht$individuals$D$cv, hr.model$dht$individuals$D$cv, uni.model$dht$individuals$D$cv),
  "Lower_CI" = c(hn.model$dht$individuals$D$lcl, hr.model$dht$individuals$D$lcl, uni.model$dht$individuals$D$lcl),
  "Upper_CI" = c(hn.model$dht$individuals$D$ucl, hr.model$dht$individuals$D$ucl, uni.model$dht$individuals$D$ucl))
density.estimate.table

# Part 7

ggplot(data = density.estimate.table, aes(x = Sighting_model, y = Density_estimates, fill = Sighting_model)) +
  geom_col() +
  geom_errorbar(aes(ymin=Lower_CI, ymax=Upper_CI), width=.2,position=position_dodge(.9)) +
  scale_y_continuous(limits = c(0,165), breaks = seq(0,150,25), expand = c(0,0)) +
  xlab("Sighting model") +
  ylab("Density (wrens/km2)") +
  theme(
    panel.background = element_blank(),
    axis.title.x = element_text(size=16, vjust = 0),
    axis.title.y = element_text(size =16),
    axis.text = element_text(size=13, color="black"), 
    axis.line=element_line(),
    legend.position = "none"
  )

# Part 8

abundance.estimate.table <- data.frame(
  "Sighting_model" = c("Half-normal", "Hazard-rate", "Uniform"),
  "Abundance_estimates" = c(hn.model$dht$individuals$N$Estimate, hr.model$dht$individuals$N$Estimate, uni.model$dht$individuals$N$Estimate),
  "CV" = c(hn.model$dht$individuals$N$cv, hr.model$dht$individuals$N$cv, uni.model$dht$individuals$N$cv),
  "Lower_CI" = c(hn.model$dht$individuals$N$lcl, hr.model$dht$individuals$N$lcl, uni.model$dht$individuals$N$lcl),
  "Upper_CI" = c(hn.model$dht$individuals$N$ucl, hr.model$dht$individuals$N$ucl, uni.model$dht$individuals$N$ucl))
abundance.estimate.table

# Part 9

ggplot(data = abundance.estimate.table, aes(x = Sighting_model, y = Abundance_estimates, fill = Sighting_model)) +
  geom_col() +
  geom_errorbar(aes(ymin=Lower_CI, ymax=Upper_CI), width=.2,position=position_dodge(.9)) +
  scale_y_continuous(limits = c(0,55), breaks = seq(0,50,10), expand = c(0,0)) +
  xlab("Sighting model") +
  ylab("Total wren abundance estimate") +
  theme(
    panel.background = element_blank(),
    axis.title.x = element_text(size=16, vjust = 0),
    axis.title.y = element_text(size =16),
    axis.text = element_text(size=13, color="black"), 
    axis.line=element_line(),
    legend.position = "none"
  )

