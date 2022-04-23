# =====================================================================
# Homework 4
# Jonah Bacon
# 15 April 2022
# FISH 621
# =====================================================================


# Load packages -----------------------------------------------------------

library(ggplot2)
library(tidyverse)
library(Distance)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Problem 1 ---------------------------------------------------------------

wren.df <- read.csv("wren.csv")
head(wren.df)
str(wren.df)

# Part 1

hist(wren.df$distance,
     xlab = "Detection distance (m)", ylab = "Count of wrens",
     main = "Histogram of detection distances")

# Part 2

boxplot(wren.df$distance ~ wren.df$Sample.Label,
        xlab = "Transect", ylab = "Detection distance (m)")

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

hn.model
# Part 5

par(mfrow=c(2,3))
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


# Problem 2 ---------------------------------------------------------------

nest.df <- read.csv("ducks-area-effort.csv")
head(nest.df)
str(nest.df)

# Part 1

nest.hn.model <- ds(nest.df, key = "hn", adjustment = NULL, convert_units = 0.001) # AIC = 928.134
plot(nest.hn.model, nc=12)
gof_ds(nest.hn.model) # p-value = 0.955

nest.density.estimates <- data.frame(nest.hn.model$dht$individuals$D)
nest.density.estimates

nest.abundance.estimate <- data.frame(nest.hn.model$dht$individuals$N)
nest.abundance.estimate

# Part 2

area.sampled <- nest.df %>% 
  group_by(Sample.Label) %>% 
  summarise("Area.Sampled" = 0.00125*2*unique(Effort))
area.sampled

# Part 3

nest.estimates <- nest.df %>% 
  filter(distance <= 1.25) %>% 
  summarise("count" = n(), "density" = count/(0.00125*2*unique(Effort)*length(unique(Sample.Label))), "abundance" = density*unique(Area))
nest.estimates

# Part 4

N <- (40.47)/(0.00125*2*128.75)
N

# Part 5

n <- 20

nests.per.sample <- nest.df %>% 
  group_by(Sample.Label) %>% 
  filter(distance <= 1.25) %>% 
  summarise("count" = n())
nests.per.sample

## Sample mean: 
samp.mean <- mean(nests.per.sample$count)
samp.mean

## Variance of the sample:
samp.var <- (1/(n-1))*sum((nests.per.sample$count-samp.mean)^2)
samp.var

## Estimate of the total nest abundance:
(N/n)*nest.estimates$count

## Estimate of the variance of the sample mean:
est.var.y_bar <- ((N-n)/N)*(samp.var/n)
est.var.y_bar

## Estimate of the variance for the total abundance estimate:
est.var.total <- N^2*est.var.y_bar
est.var.total

## Coefficient of variation for the total abundance estimate:
sqrt(est.var.total) / (N*samp.mean)

# Part 6

area.sampled2 <- nest.df %>% 
  group_by(Sample.Label) %>% 
  summarise("Area.Sampled" = 0.001*2*unique(Effort))
area.sampled2

nest.estimates2 <- nest.df %>% 
  filter(distance <= 1) %>% 
  summarise("count" = n(), "density" = count/(0.001*2*unique(Effort)*length(unique(Sample.Label))), "abundance" = density*unique(Area))
nest.estimates2

N2 <- (40.47)/(0.001*2*128.75)
N2

n <- 20

nests.per.sample2 <- nest.df %>% 
  group_by(Sample.Label) %>% 
  filter(distance <= 1) %>% 
  summarise("count" = n())
nests.per.sample2

## Sample mean: 
samp.mean2 <- mean(nests.per.sample2$count)
samp.mean2

## Variance of the sample:
samp.var2 <- (1/(n-1))*sum((nests.per.sample2$count-samp.mean2)^2)
samp.var2

## Estimate of the total nest abundance:
(N2/n)*nest.estimates2$count

## Estimate of the variance of the sample mean:
est.var.y_bar2 <- ((N2-n)/N2)*(samp.var2/n)
est.var.y_bar2

## Estimate of the variance for the total abundance estimate:
est.var.total2 <- N2^2*est.var.y_bar2
est.var.total2

## Coefficient of variation for the total abundance estimate:
sqrt(est.var.total2) / (N2*samp.mean2)


# Problem 3 ---------------------------------------------------------------

scallop.df <- read.csv("Scallop CPUE.csv")
head(scallop.df)
tail(scallop.df)
str(scallop.df)

# Part 1

cpue <- scallop.df %>% 
  group_by(Region, Season) %>% 
  summarise("CPUE.meat" = Catch_Meat/Dredge_Hours, "CPUE.round" = Catch_Round/Dredge_Hours)
cpue

# Part 2

ggplot(data = cpue, aes(x = Season)) +
  geom_line(aes(y=CPUE.meat*10), color = cbPalette[3], cex = 1.2) + 
  geom_line(aes(y=CPUE.round), color = cbPalette[2], cex = 1.2) +
  geom_point(aes(y=CPUE.meat*10), color = cbPalette[3], cex = 3) + 
  geom_point(aes(y=CPUE.round), color = cbPalette[2], cex = 3) +
  scale_y_continuous(name = "Scallop round CPUE", sec.axis = sec_axis(trans = ~./10, name="Scallop meat CPUE")) +
  scale_x_continuous(limits = c(2000,2020), breaks = seq(2000,2020,1), expand = c(0,0.2)) +
  facet_wrap(~ Region, ncol = 1) +
  theme(
    panel.background = element_blank(),
    axis.title.x = element_text(size=16),
    axis.title.y.left = element_text(size=16, color=cbPalette[2]),
    axis.title.y.right = element_text(size=16, color=cbPalette[3]),
    axis.text.x = element_text(size=10, color="black"), 
    axis.text.y.left = element_text(size=10, color=cbPalette[2]),
    axis.text.y.right = element_text(size=10, color=cbPalette[3]),
    strip.text = element_text(size = 13),
    axis.line=element_line()
  )

# Part 3

meat.glm <- glm(log(CPUE.meat+1) ~ factor(Season), data=cpue)
summary(meat.glm)

years <- unique(cpue$Season)
meat.glm_ln.preds <- predict(meat.glm, newdata=list(Season=years))
meat.glm_preds <- exp(meat.glm_ln.preds + sd(meat.glm$residuals)^2/2)-1

meat.glm_preds

# Part 4

meat.glm2 <- glm(log(CPUE.meat+1) ~ factor(Season) + factor(Region), data=cpue)
summary(meat.glm2)

meat.glm2_ln.preds <- predict(meat.glm2, newdata=list(Season=years, Region = rep("Shelikof", length(years))))
meat.glm2_preds <- exp(meat.glm2_ln.preds + sd(meat.glm2$residuals)^2/2)-1

meat.glm2_preds

# Part 5

meat.model.preds <- data.frame(
  "year" = c(years,years),
  "model" = c(rep("Without region effect", length(meat.glm_preds)), rep("With region effect", length(meat.glm2_preds))),
  "prediction" = c(meat.glm_preds,meat.glm2_preds)
)
meat.model.preds

ggplot(data = meat.model.preds, aes(x = year, y = prediction, color = model)) +
  geom_line(cex = 1.2) +
  geom_point(cex = 4) +
  xlab("Year") +
  ylab("Model-based meat weight CPUE prediction") +
  scale_x_continuous(limits = c(2000,2020), breaks = seq(2000,2020,1), expand = c(0,0.2)) +
  scale_color_manual(values=cbPalette[c(6,2)], name = "Model") +
  theme(
    panel.background = element_blank(),
    axis.title.x = element_text(size=16, vjust = 0.5),
    axis.title.y = element_text(size =16, vjust = 1.5),
    axis.text = element_text(size=9, color="black"), 
    axis.line=element_line()
  )

# Part 6

round.glm <- glm(log(CPUE.round+1) ~ factor(Season), data=cpue)
summary(round.glm)

years <- unique(cpue$Season)
round.glm_ln.preds <- predict(round.glm, newdata=list(Season=years))
round.glm_preds <- exp(round.glm_ln.preds + sd(round.glm$residuals)^2/2)-1

round.glm_preds

# Part 7

round.glm2 <- glm(log(CPUE.round+1) ~ factor(Season) + factor(Region), data=cpue)
summary(round.glm2)

round.glm2_ln.preds <- predict(round.glm2, newdata=list(Season=years, Region = rep("Shelikof", length(years))))
round.glm2_preds <- exp(round.glm2_ln.preds + sd(round.glm2$residuals)^2/2)-1

round.glm2_preds

# Part 8

round.model.preds <- data.frame(
  "year" = c(years, years),
  "model" = c(rep("Without region effect", length(round.glm_preds)), rep("With region effect", length(round.glm2_preds))),
  "prediction" = c(round.glm_preds,round.glm2_preds)
)
round.model.preds

ggplot(data = round.model.preds, aes(x = year, y = prediction, color = model)) +
  geom_line(cex = 1.2) +
  geom_point(cex = 4) +
  xlab("Year") +
  ylab("Model-based round weight CPUE prediction") +
  scale_x_continuous(limits = c(2000,2020), breaks = seq(2000,2020,1), expand = c(0,0.2)) +
  scale_color_manual(values=cbPalette[c(6,2)], name = "Model") +
  theme(
    panel.background = element_blank(),
    axis.title.x = element_text(size=16, vjust = 0.5),
    axis.title.y = element_text(size =16, vjust = 1.5),
    axis.text = element_text(size=9, color="black"), 
    axis.line=element_line()
  )

# Part 9

combined.cpue.preds <- data.frame(
  "year" = years,
  "meat.preds" = meat.glm2_preds,
  "round.preds" = round.glm2_preds
)
combined.cpue.preds

ggplot(data = combined.cpue.preds, aes(x = year)) +
  geom_line(aes(y=meat.preds*10), color = cbPalette[4], cex = 1.2) + 
  geom_line(aes(y=round.preds), color = cbPalette[7], cex = 1.2) +
  geom_point(aes(y=meat.preds*10), color = cbPalette[4], cex = 3) + 
  geom_point(aes(y=round.preds), color = cbPalette[7], cex = 3) +
  scale_y_continuous(name = "Round Weight CPUE model prediction", sec.axis = sec_axis(trans = ~./10, name="Meat Weight CPUE model prediction")) +
  scale_x_continuous(limits = c(2000,2020), breaks = seq(2000,2020,1), expand = c(0,0.2)) +
  xlab("Year") +
  theme(
    panel.background = element_blank(),
    axis.title.x = element_text(size=16),
    axis.title.y.left = element_text(size=16, color=cbPalette[7]),
    axis.title.y.right = element_text(size=16, color=cbPalette[4]),
    axis.text.x = element_text(size=10, color="black"), 
    axis.text.y.left = element_text(size=10, color=cbPalette[7]),
    axis.text.y.right = element_text(size=10, color=cbPalette[4]),
    axis.line=element_line()
  )
