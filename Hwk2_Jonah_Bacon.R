# =====================================================================
# Homework 2
# Jonah Bacon
# 22 Feb 2022
# FISH 621
# =====================================================================


# Load packages -----------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(ggsci)

# Problem 1 ---------------------------------------------------------------

field <- c(1,2,3,4,5)
n1 <- c(100,160,160,50,10)
n2 <- c(90,90,50,15,8)

## Part 1

N1_hat <- n1^2/(n1-n2)
N1_hat.df <- data.frame(field,round(N1_hat,0))
names(N1_hat.df) <- c("field","N1_hat")
print(N1_hat.df)

## Part 2

N2_hat <- round(N1_hat,0) - n1
N2_hat.df <- data.frame(N1_hat.df,N2_hat)
print(N2_hat.df)

## Part 3

p_hat <- n1/N1_hat
N_hat.p_hat.df <- data.frame(N2_hat.df,p_hat)
print(N_hat.p_hat.df)


# Problem 2 ---------------------------------------------------------------

pike.df <- read.csv("pike.csv")
head(pike.df)
str(pike.df)

pike.df$Pond <- as.factor(pike.df$Pond)
pike.df <- pike.df %>% 
  mutate("CPUE"=Catch/Hours.Fished) %>% 
  group_by(Pond) %>% 
  mutate("K" = cumsum(Catch)) %>% 
  mutate("K_lag"=K-Catch)
head(pike.df)

## Part 1

PondA <- pike.df %>% filter(Pond == "A")
lm.A <- lm(CPUE ~ K_lag, data = PondA)  
summary(lm.A)  
coef(lm.A)
q_hat.A <- abs(coef(lm.A)[2])
q_hat.A

PondB <- pike.df %>% filter(Pond == "B")
lm.B <- lm(CPUE ~ K_lag, data = PondB)  
summary(lm.B)
coef(lm.B)
q_hat.B <- abs(coef(lm.B)[2])
q_hat.B

## Part 2

N0.A <- round(coef(lm.A)[1]/q_hat.A,0)
N0.A

N0.B <- round(coef(lm.B)[1]/q_hat.B,0)
N0.B

## Part 3

ggplot(data = pike.df, aes(x=K_lag, y=CPUE, color=Pond)) +
  geom_point(cex = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Cumulative catch (K t-1)") +
  ylab("CPUE") +
  # facet_wrap(~Pond, ncol=1, scales="free_y") +
  scale_y_continuous(limits=c(0,4), breaks=seq(0,4,1), expand=c(0,0)) +
  scale_x_continuous(limits = c(0,140), breaks = seq(0,140,20), expand = c(0,0.5)) +
  scale_color_jco() +
  theme(
    panel.background = element_blank(),
    axis.title.x = element_text(size=16, face="bold"),
    axis.title.y = element_text(size =16, face="bold"),
    axis.text = element_text(size=10, color="black"), 
    legend.text = element_text(size=8), 
    legend.title = element_text(size=12), 
    axis.line=element_line()
  )

ggplot(data = pike.df, aes(x=K_lag, y=CPUE, color=Pond)) +
  geom_point(cex = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Cumulative catch (K t-1)") +
  ylab("CPUE") +
  facet_wrap(~Pond, ncol=2, scales="free_y") +
  scale_y_continuous(limits=c(0,4), breaks=seq(0,4,1), expand=c(0,0)) +
  scale_x_continuous(limits = c(0,140), breaks = seq(0,140,20), expand = c(0,0.5)) +
  scale_color_jco() +
  theme(
    panel.background = element_blank(),
    axis.title.x = element_text(size=16, face="bold"),
    axis.title.y = element_text(size =16, face="bold"),
    axis.text = element_text(size=10, color="black"), 
    legend.text = element_text(size=8), 
    legend.title = element_text(size=12), 
    axis.line=element_line()
  )


# Problem 3 ---------------------------------------------------------------

month <- c("Sep","Oct","Nov","Dec","Jan","Feb")
harvest.males <- c(279,51,330,210,74)
harvest.females <- c(15,180,140,149,18)
harvest.total <- harvest.males + harvest.females

ratio.males <- c(27,24,26,22,19,17) / 50
ratio.females <- c(23,26,24,28,31,33) / 50

## Part 1

N_hat <- rep(NA,6)
N_hat[1] <- (harvest.males[1] - harvest.total[1]*ratio.males[2]) / (ratio.males[1]-ratio.males[2])
N_hat[2] <- (harvest.males[2] - harvest.total[2]*ratio.males[3]) / (ratio.males[2]-ratio.males[3])
N_hat[3] <- (harvest.males[3] - harvest.total[3]*ratio.males[4]) / (ratio.males[3]-ratio.males[4])
N_hat[4] <- (harvest.males[4] - harvest.total[4]*ratio.males[5]) / (ratio.males[4]-ratio.males[5])
N_hat[5] <- (harvest.males[5] - harvest.total[5]*ratio.males[6]) / (ratio.males[5]-ratio.males[6])
N_hat[6] <- N_hat[5] - harvest.total[5]

total.abund.df <- data.frame(month, round(N_hat,0))
names(total.abund.df) <- c("month", "N_hat")
total.abund.df

## Part 2

N.males_hat <- rep(NA,6)
N.males_hat[1] <- round(N_hat[1]*ratio.males[1], 0)
N.males_hat[2] <- round(N_hat[2]*ratio.males[2], 0)
N.males_hat[3] <- round(N_hat[3]*ratio.males[3], 0)
N.males_hat[4] <- round(N_hat[4]*ratio.males[4], 0)
N.males_hat[5] <- round(N_hat[5]*ratio.males[5], 0)
N.males_hat[6] <- round(N_hat[6]*ratio.males[6], 0)

males.abund.df <- data.frame(month, N.males_hat)
males.abund.df

## Part 3

N.females_hat <- rep(NA,6)
N.females_hat[1] <- round(N_hat[1]*ratio.females[1], 0)
N.females_hat[2] <- round(N_hat[2]*ratio.females[2], 0)
N.females_hat[3] <- round(N_hat[3]*ratio.females[3], 0)
N.females_hat[4] <- round(N_hat[4]*ratio.females[4], 0)
N.females_hat[5] <- round(N_hat[5]*ratio.females[5], 0)
N.females_hat[6] <- round(N_hat[6]*ratio.females[6], 0)

females.abund.df <- data.frame(month, N.females_hat)
females.abund.df

## Complete table

complete.df <- data.frame(total.abund.df, N.males_hat, N.females_hat)
complete.df

# Problem 4 ---------------------------------------------------------------

sch.df <- read_csv("Schnabel Data.csv")
str(sch.df)
sch.df

s <- length(sch.df$Period)
ni <- sch.df$Captures
mi <- sch.df$Recaptures

## Part 1

sch.df <- sch.df %>% mutate("New_Marks" = Captures - Recaptures)
ui <- sch.df$New_Marks
ui

## Part 2

sch.df <- sch.df %>% mutate("Total_Marks" = cumsum(New_Marks) - New_Marks)
Mi <- sch.df$Total_Marks
Mi

## Part 3

schnabel.est <- sum(ni*Mi) / sum(mi)
schnabel.est

## Part 4

schnabel_chap.est <- sum(ni*Mi) / ( sum(mi) + 1 )
schnabel_chap.est

## Part 5

lambda <- sum(ni*Mi)
lambda

schnabel_chap.lb <- lambda*( (2*sum(mi) + 1.96^2 - 1.96*sqrt(4*sum(mi+1.96^2))) / (2*sum(mi)^2) )
schnabel_chap.ub <- lambda*( (2*sum(mi) + 1.96^2 + 1.96*sqrt(4*sum(mi+1.96^2))) / (2*sum(mi)^2) )

schnabel_chap.95CI <- paste("Estimate:", round(schnabel_chap.est,0), "(", round(schnabel_chap.lb,0), "-", round(schnabel_chap.ub,0), ")")
schnabel_chap.95CI

## Part 6

chap_ests <- ( (Mi[2:s]+1)*(ni[2:s]+1) / (mi[2:s] + 1) ) -1
chap_ests

## Part 7

chap_vars <- ((Mi[2:s]+1) * (ni[2:s]+1) * (Mi[2:s]-mi[2:s]) * (ni[2:s]-mi[2:s]) ) / ((mi[2:s]+1)^2 * (mi[2:s]+2) )
chap_vars

## Part 8

mean.chap.est <- mean(chap_ests)
mean.chap.est

## Part 9

chap.var_1 <- sum(chap_vars)/(s-1)^2
chap.var_1

chap.se_1 <- sqrt(chap.var_1)
chap.se_1

chap.lb_1 <- mean.chap.est - qt(p=0.975, df = s-2)*chap.se_1
chap.lb_1

chap.ub_1 <- mean.chap.est + qt(p=0.975, df = s-2)*chap.se_1
chap.ub_1

chap_var1.95CI <- paste("Estimate:", round(mean.chap.est,0), "(", round(chap.lb_1,0), "-", round(chap.ub_1,0), ")")
chap_var1.95CI

## Part 10

chap.var_2 <- sum( (chap_ests-mean.chap.est)^2 / ((s-1)*(s-2)) )
chap.var_2

chap.se_2 <- sqrt(chap.var_2)
chap.se_2

chap.lb_2 <- mean.chap.est - qt(p=0.975, df = s-2)*chap.se_2
chap.lb_2

chap.ub_2 <- mean.chap.est + qt(p=0.975, df = s-2)*chap.se_2
chap.ub_2

chap_var2.95CI <- paste("Estimate:", round(mean.chap.est,0), "(", round(chap.lb_2,0), "-", round(chap.ub_2,0), ")")
chap_var2.95CI

## Part 11

chap_cv <- sqrt(chap_vars)/chap_ests

chap_abund.df <- data.frame(
  Period = seq(1:15), 
  ni = sch.df$Captures,
  mi = sch.df$Recaptures,
  ui = sch.df$New_Marks,
  Mi = sch.df$Total_Marks,
  Chapman_ests = c(NA,chap_ests), 
  Chapman_vars = c(NA,chap_vars),
  Chapman_CV = c(NA,chap_cv))
chap_abund.df

## Plots: ni,mi,ui,Mi vs Chapman estimate

plot.df <- chap_abund.df[-1,]

ggplot(data = plot.df, aes(x = ni, y = Chapman_ests)) +
  geom_point(cex = 4, color = "red") +
  xlab("Captures (ni)") +
  ylab("Chapman estimate") +
  scale_y_continuous(limits=c(1400,2800), breaks=seq(1400,2800,200), expand=c(0,0)) +
  scale_x_continuous(limits = c(0,160), breaks = seq(0,160,20), expand = c(0,1.6)) +
  theme(
    panel.background = element_blank(),
    axis.title.x = element_text(size=16, face="bold"),
    axis.title.y = element_text(size =16, face="bold"),
    axis.text = element_text(size=10, color="black"), 
    legend.text = element_text(size=8), 
    legend.title = element_text(size=12), 
    axis.line=element_line()
  )

ggplot(data = plot.df, aes(x = mi, y = Chapman_ests)) +
  geom_point(cex = 4, color = "orange") +
  xlab("Recaptures (mi)") +
  ylab("Chapman estimate") +
  scale_y_continuous(limits=c(1400,2800), breaks=seq(1400,2800,200), expand=c(0,0)) +
  scale_x_continuous(limits = c(0,60), breaks = seq(0,60,10), expand = c(0,0)) +
  theme(
    panel.background = element_blank(),
    axis.title.x = element_text(size=16, face="bold"),
    axis.title.y = element_text(size =16, face="bold"),
    axis.text = element_text(size=10, color="black"), 
    legend.text = element_text(size=8), 
    legend.title = element_text(size=12), 
    axis.line=element_line()
  )

ggplot(data = plot.df, aes(x = ui, y = Chapman_ests)) +
  geom_point(cex = 4, color = "blue") +
  xlab("New captures (ui)") +
  ylab("Chapman estimate") +
  scale_y_continuous(limits=c(1400,2800), breaks=seq(1400,2800,200), expand=c(0,0)) +
  scale_x_continuous(limits = c(0,100), breaks = seq(0,100,10), expand = c(0,1)) +
  theme(
    panel.background = element_blank(),
    axis.title.x = element_text(size=16, face="bold"),
    axis.title.y = element_text(size =16, face="bold"),
    axis.text = element_text(size=10, color="black"), 
    legend.text = element_text(size=8), 
    legend.title = element_text(size=12), 
    axis.line=element_line()
  )

ggplot(data = plot.df, aes(x = Mi, y = Chapman_ests)) +
  geom_point(cex = 4, color = "dark green") +
  xlab("Total captures (Mi)") +
  ylab("Chapman estimate") +
  scale_y_continuous(limits=c(1400,2800), breaks=seq(1400,2800,200), expand=c(0,0)) +
  scale_x_continuous(limits = c(0,900), breaks = seq(0,900,100), expand = c(0,10)) +
  theme(
    panel.background = element_blank(),
    axis.title.x = element_text(size=16, face="bold"),
    axis.title.y = element_text(size =16, face="bold"),
    axis.text = element_text(size=10, color="black"), 
    legend.text = element_text(size=8), 
    legend.title = element_text(size=12), 
    axis.line=element_line()
  )


## Plots: ni,mi,ui,Mi vs CV

ggplot(data = plot.df, aes(x = ni, y = Chapman_CV)) +
  geom_point(cex = 4, color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = c("black"), cex = 2) +
  xlab("Captures (ni)") +
  ylab("CV") +
  scale_y_continuous(limits=c(0,0.6), breaks=seq(0,0.6,0.1), expand=c(0,0)) +
  scale_x_continuous(limits = c(0,160), breaks = seq(0,160,20), expand = c(0,1.6)) +
  theme(
    panel.background = element_blank(),
    axis.title.x = element_text(size=16, face="bold"),
    axis.title.y = element_text(size =16, face="bold"),
    axis.text = element_text(size=10, color="black"), 
    legend.text = element_text(size=8), 
    legend.title = element_text(size=12), 
    axis.line=element_line()
  )

ggplot(data = plot.df, aes(x = mi, y = Chapman_CV)) +
  geom_smooth(se = FALSE, color = c("black"), cex = 2) +
  geom_point(cex = 4, color = "orange") +
  xlab("Recaptures (mi)") +
  ylab("CV") +
  scale_y_continuous(limits=c(0,0.6), breaks=seq(0,0.6,0.1), expand=c(0,0)) +
  scale_x_continuous(limits = c(0,60), breaks = seq(0,60,10), expand = c(0,0)) +
  theme(
    panel.background = element_blank(),
    axis.title.x = element_text(size=16, face="bold"),
    axis.title.y = element_text(size =16, face="bold"),
    axis.text = element_text(size=10, color="black"), 
    legend.text = element_text(size=8), 
    legend.title = element_text(size=12), 
    axis.line=element_line()
  )

ggplot(data = plot.df, aes(x = ui, y = Chapman_CV)) +
  geom_point(cex = 4, color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = c("black"), cex = 2) +
  xlab("New captures (ui)") +
  ylab("CV") +
  scale_y_continuous(limits=c(0,0.6), breaks=seq(0,0.6,0.1), expand=c(0,0)) +
  scale_x_continuous(limits = c(0,100), breaks = seq(0,100,10), expand = c(0,1)) +
  theme(
    panel.background = element_blank(),
    axis.title.x = element_text(size=16, face="bold"),
    axis.title.y = element_text(size =16, face="bold"),
    axis.text = element_text(size=10, color="black"), 
    legend.text = element_text(size=8), 
    legend.title = element_text(size=12), 
    axis.line=element_line()
  )

ggplot(data = plot.df, aes(x = Mi, y = Chapman_CV)) +
  geom_point(cex = 4, color = "dark green") +
  geom_smooth(method = "lm", se = FALSE, color = c("black"), cex = 2) +
  xlab("Total captures (Mi)") +
  ylab("CV") +
  scale_y_continuous(limits=c(0,0.6), breaks=seq(0,0.6,0.1), expand=c(0,0)) +
  scale_x_continuous(limits = c(0,900), breaks = seq(0,900,100), expand = c(0,10)) +
  theme(
    panel.background = element_blank(),
    axis.title.x = element_text(size=16, face="bold"),
    axis.title.y = element_text(size =16, face="bold"),
    axis.text = element_text(size=10, color="black"), 
    legend.text = element_text(size=8), 
    legend.title = element_text(size=12), 
    axis.line=element_line()
  )
