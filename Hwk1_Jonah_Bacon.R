# =====================================================================
# Homework 1
# Jonah Bacon
# 01 Feb 2022
# FISH 621
# =====================================================================

# Problem 1 ---------------------------------------------------------------

# Load data. *Make sure .csv data files are in your working directory
df.50 <- read.csv("Length Sample 50.csv", header = TRUE)
df.250 <- read.csv("Length Sample 250.csv", header = TRUE)

# Visualize data structures
head(df.50)
head(df.250)
## I don't like the column names, so I'll rename them with more intuitive labels:
colnames(df.50) <- c("ID","length")
colnames(df.250) <- c("ID","length")

  ## Sample size = 50 ----------------------------------------------------
y.bar.50 <- mean(df.50$length)
var.50 <- var(df.50$length)
CV.50 <- sqrt(var.50)/y.bar.50
var.hat_y.bar.50 <- ( (1000-50)/1000 )* var.50/length(df.50$length)

print(y.bar.50); print(var.50); print(CV.50); print(var.hat_y.bar.50)

  ## Sample size = 250 --------------------------------------------------
y.bar.250 <- mean(df.250$length)
var.250 <- var(df.250$length)
CV.250 <- sqrt(var.250)/y.bar.250
var.hat_y.bar.250 <- ( (1000-250)/1000 ) * var.250/length(df.250$length)

print(y.bar.250); print(var.250); print(CV.250); print(var.hat_y.bar.250)

# Problem 2 ---------------------------------------------------------------

# Create data frame
samp.ID <- seq(1,15,1)
flower.count <- c(10,15,1,23,18,12,19,9,5,8,3,26,20,12,31)
flower.df <- cbind(samp.ID,flower.count)

  ## Sample statistics -----------------------------------------------------
flwr.y.bar <- mean(flower.count)
flwr.var <- var(flower.count)
flwr.CV <- sqrt(flwr.var)/flwr.y.bar
var.hat_flwr.y.bar <- ( (6000 - 15)/6000) * flwr.var/length(flower.count) # For this, we need N, which is equal to the total number of 1x1 m quadrats in the field. This is equal to 50*120 = 6000

print(flwr.y.bar); print(flwr.var); print(flwr.CV); print(var.hat_flwr.y.bar)

  ## Population estimates ------------------------------------------------
samp.space <- 50*120 # Determine the total sample space (i.e. N, or how many 1x1 quadrats are in the field) = 6000
flower.pop.est <- samp.space * flwr.y.bar
flower.pop.est.var <- samp.space^2 * var.hat_flwr.y.bar
flower.pop.est.CV <- sqrt(flower.pop.est.var)/flower.pop.est 

print(flower.pop.est); print(flower.pop.est.var); print(flower.pop.est.CV)

# Problem 3 ---------------------------------------------------------------

  ## Sampling event 1 -------------------------------------------------------
n.detect.1 <- 12 # Number of voles detected in this sample event
p <- 0.15 # Probability of detection = proportion of apartment visible

N_hat.est.1 <- n.detect.1/p
N_hat.est.1
N_hat.trial <- 1:350
probs.N_hat.1 <- dbinom(x=n.detect.1, size=N_hat.trial, prob=p)

  ## Sampling event 2 -------------------------------------------------------
n.detect.2 <- 22 # Number of voles detected in this sample event

N_hat.est.2 <- n.detect.2/p
N_hat.est.2
probs.N_hat.2 <- dbinom(x=n.detect.2, size=N_hat.trial, prob=p)

  ## Sampling event 3 ------------------------------------------------------
n.detect.3 <- 19 # Number of voles detected in this sample event

N_hat.est.3 <- n.detect.3/p
N_hat.est.3
probs.N_hat.3 <- dbinom(x=n.detect.3, size=N_hat.trial, prob=p)

  ## Plot the 3 profiles ----------------------------------------------------
plot(x=N_hat.trial, y=probs.N_hat.1, type="l",
     xlab="Possible Total Population Sizes (N_hat)",
     ylab="Probability of Population Sizes") +
  lines(x=N_hat.trial, y=probs.N_hat.2) +
  lines(x=N_hat.trial, y=probs.N_hat.3)

legend(x = 200, y = 0.12, legend = c("#1: Initial number detected = 12", "#2: Initial number detected = 22", "#3: Initial number detected = 19"),
       fill = c(rgb(1,0,0, alpha = 0.5),rgb(0,0,1, alpha = 0.5),rgb(0,1,0, alpha = 0.5)), title = "Sampling event")
polygon(x=N_hat.trial, y=probs.N_hat.1, col=rgb(1,0,0, alpha = 0.5))
polygon(x=N_hat.trial, y=probs.N_hat.2, col=rgb(0,0,1, alpha = 0.5))
polygon(x=N_hat.trial, y=probs.N_hat.3, col=rgb(0,1,0, alpha = 0.5))

# Problem 4 ---------------------------------------------------------------

N <- 1000
n1 <- 50
n2 <- 100

m2_hat <- n1*n2 / N
m2_hat

trial.m2 <- 0:20
trial.prob.m2 <- dhyper(x=trial.m2, m=50, n=950, k=100)

plot(x=trial.m2, y=trial.prob.m2, type="l",
     xlab="Possible m2 values",
     ylab="Probability of m2 values")
grid()
polygon(x=trial.m2, y=trial.prob.m2, col=rgb(0,1,1, alpha=0.25))
points(x=trial.m2, y=trial.prob.m2, pch=21, bg="red")

est.m2 <- sum(trial.m2*trial.prob.m2)
est.m2

  ## Varying n2 ------------------------------------------------------------
n2 <- 50
m2_hat.2 <- n1*n2 / N
m2_hat.2

n2 <- 200
m2_hat.3 <- n1*n2 / N
m2_hat.3

trial.m2 <- 0:20
trial.prob.m2.2 <- dhyper(x=trial.m2, m=50, n=950, k=50) # Change k (n2) to 50
trial.prob.m2.3 <- dhyper(x=trial.m2, m=50, n=950, k=200) # Change k (n2) to 200

plot(x=trial.m2, y=trial.prob.m2.2, type="l", lwd = 5,
     xlab="Possible m2 values",
     ylab="Probability of m2 values") +
  lines(x=trial.m2, y=trial.prob.m2, col="red", lwd = 5) +
  lines(x=trial.m2, y=trial.prob.m2.3, col = "blue", lwd = 5)
legend(x = 10, y = 0.25, legend = c("100", "50", "200"),
       fill = c("red", "black", "blue"), title = "n2 value")

  ## Varying N ------------------------------------------------------------
n2 <- 100

N <- 500
m2_hat.4 <- n1*n2 / N
m2_hat.4

N <- 2000
m2_hat.5 <- n1*n2 / N
m2_hat.5

trial.m2 <- 0:20
trial.prob.m2.4 <- dhyper(x=trial.m2, m=50, n=450, k=100) # Change n (= N-m) to 450
trial.prob.m2.5 <- dhyper(x=trial.m2, m=50, n=1950, k=100) # Change n (= N-m) to 1950

plot(x=trial.m2, y=trial.prob.m2.5, type="l", lwd = 5, col = "blue",
     xlab="Possible m2 values",
     ylab="Probability of m2 values") +
  lines(x=trial.m2, y=trial.prob.m2, col="red", lwd = 5) +
  lines(x=trial.m2, y=trial.prob.m2.4, col = "black", lwd = 5)
legend(x = 10, y = 0.25, legend = c("1000", "500", "2000"),
       fill = c("red", "black", "blue"), title = "True N value")



# Problem 5 ---------------------------------------------------------------

  ## Chapman abundance estimation (sample w/o replacement) ------------------
### May

n1 = 65
n2 = 120
m2 = 19

chap.may.est <- ((n1+1)*(n2+1))/(m2+1) - 1
chap.may.var <- ((n1+1)*(n2+1)*(n1-m2)*(n2-m2)) / ((m2+1)^2 * (m2+2))
chap.may.CV <- sqrt(chap.may.var)/chap.may.est

trial.N <- 0:800
may.trial.N.prob <- dhyper(x=m2, m=n1, n=trial.N-n1, k=n2)

plot(x=trial.N, y=may.trial.N.prob, type="l",
     xlab="Trial Population Sizes (N_hat)",
     ylab="Probability of True Value",
     main="Hypergeometric")

### June

n1 = 25
n2 = 150
m2 = 12

chap.june.est <- ((n1+1)*(n2+1))/(m2+1) - 1
chap.june.var <- ((n1+1)*(n2+1)*(n1-m2)*(n2-m2)) / ((m2+1)^2 * (m2+2))
chap.june.CV <- sqrt(chap.june.var)/chap.june.est

june.trial.N.prob <- dhyper(x=m2, m=n1, n=trial.N-n1, k=n2)

plot(x=trial.N, y=june.trial.N.prob, type="l",
     xlab="Trial Population Sizes (N_hat)",
     ylab="Probability of True Value",
     main="Hypergeometric")

### July

n1 = 40
n2 = 110
m2 = 15

chap.july.est <- ((n1+1)*(n2+1))/(m2+1) - 1
chap.july.var <- ((n1+1)*(n2+1)*(n1-m2)*(n2-m2)) / ((m2+1)^2 * (m2+2))
chap.july.CV <- sqrt(chap.july.var)/chap.july.est

july.trial.N.prob <- dhyper(x=m2, m=n1, n=trial.N-n1, k=n2)

plot(x=trial.N, y=july.trial.N.prob, type="l",
     xlab="Trial Population Sizes (N_hat)",
     ylab="Probability of True Value",
     main="Hypergeometric")

### August

n1 = 45
n2 = 80
m2 = 35

chap.august.est <- ((n1+1)*(n2+1))/(m2+1) - 1
chap.august.var <- ((n1+1)*(n2+1)*(n1-m2)*(n2-m2)) / ((m2+1)^2 * (m2+2))
chap.august.CV <- sqrt(chap.august.var)/chap.august.est

august.trial.N.prob <- dhyper(x=m2, m=n1, n=trial.N-n1, k=n2)

plot(x=trial.N, y=august.trial.N.prob, type="l",
     xlab="Trial Population Sizes (N_hat)",
     ylab="Probability of True Value",
     main="Hypergeometric")

### September

n1 = 60
n2 = 70
m2 = 42

chap.september.est <- ((n1+1)*(n2+1))/(m2+1) - 1
chap.september.var <- ((n1+1)*(n2+1)*(n1-m2)*(n2-m2)) / ((m2+1)^2 * (m2+2))
chap.september.CV <- sqrt(chap.september.var)/chap.september.est

september.trial.N.prob <- dhyper(x=m2, m=n1, n=trial.N-n1, k=n2)

plot(x=trial.N, y=september.trial.N.prob, type="l",
     xlab="Trial Population Sizes (N_hat)",
     ylab="Probability of True Value",
     main="Hypergeometric")


### Combined table & plot

chapman_table <- cbind(N_hat <- c(chap.may.est, chap.june.est, chap.july.est, chap.august.est, chap.september.est), var <- c(chap.may.var, chap.june.var, chap.july.var, chap.august.var, chap.september.var), CV <- c(chap.may.CV, chap.june.CV, chap.july.CV, chap.august.CV, chap.september.CV))
rownames(chapman_table) <- c("May", "June", "July", "August", "September")
colnames(chapman_table) <- c("N_hat", "var", "CV")
chapman_table

plot(x=trial.N, y=august.trial.N.prob, type="l", col = "red", lwd = 3,
     xlab="Possible Population Sizes (N_hat)",
     ylab="Probability of True Value",
     main="PDF for Population Size Estimates (N_hat) by month") +
  lines(x=trial.N, y=may.trial.N.prob, col="blue", lwd = 3) +
  lines(x=trial.N, y=june.trial.N.prob, col="green", lwd = 3) +
  lines(x=trial.N, y=july.trial.N.prob, col="orange", lwd = 3) +
  lines(x=trial.N, y=september.trial.N.prob, col="black", lwd = 3)
legend(x = 500, y = 0.17, legend = c("May", "June", "July", "August", "September"),
       fill = c("blue", "green", "orange", "red", "black"), title = "Month")


  ## Bailey abundance estimation (sample w/ replacement) ---------------------------------------
### May

n1 = 65
n2 = 120
m2 = 19

bailey.may.est <- (n1*(n2+1)) / (m2+1)
bailey.may.var <- (n1^2 * (n2+1) * (n2-m2)) / ((m2+1)^2 * (m2+2))
bailey.may.CV <- sqrt(bailey.may.var)/bailey.may.est

trial.N <- 0:800
may.trial.N.prob_bailey <- dbinom(x=m2, size=n2, prob=n1/trial.N)

plot(x=trial.N, y=may.trial.N.prob_bailey, type="l", lwd = 3,
     xlab="Trial Population Sizes (N_hat)",
     ylab="Probability of True Value",
     main="Bailey's Binomial Model")
abline(v=bailey.may.est, col="red")

### June

n1 = 25
n2 = 150
m2 = 12

bailey.june.est <- (n1*(n2+1)) / (m2+1)
bailey.june.var <- (n1^2 * (n2+1) * (n2-m2)) / ((m2+1)^2 * (m2+2))
bailey.june.CV <- sqrt(bailey.june.var)/bailey.june.est

june.trial.N.prob_bailey <- dbinom(x=m2, size=n2, prob=n1/trial.N)

plot(x=trial.N, y=june.trial.N.prob_bailey, type="l", lwd = 3,
     xlab="Trial Population Sizes (N_hat)",
     ylab="Probability of True Value",
     main="Bailey's Binomial Model")
abline(v=bailey.june.est, col="red")

### July

n1 = 40
n2 = 110
m2 = 15

bailey.july.est <- (n1*(n2+1)) / (m2+1)
bailey.july.var <- (n1^2 * (n2+1) * (n2-m2)) / ((m2+1)^2 * (m2+2))
bailey.july.CV <- sqrt(bailey.july.var)/bailey.july.est

july.trial.N.prob_bailey <- dbinom(x=m2, size=n2, prob=n1/trial.N)

plot(x=trial.N, y=july.trial.N.prob_bailey, type="l", lwd = 3,
     xlab="Trial Population Sizes (N_hat)",
     ylab="Probability of True Value",
     main="Bailey's Binomial Model")
abline(v=bailey.july.est, col="red")

### August

n1 = 45
n2 = 80
m2 = 35

bailey.august.est <- (n1*(n2+1)) / (m2+1)
bailey.august.var <- (n1^2 * (n2+1) * (n2-m2)) / ((m2+1)^2 * (m2+2))
bailey.august.CV <- sqrt(bailey.august.var)/bailey.august.est

august.trial.N.prob_bailey <- dbinom(x=m2, size=n2, prob=n1/trial.N)

plot(x=trial.N, y=august.trial.N.prob_bailey, type="l", lwd = 3,
     xlab="Trial Population Sizes (N_hat)",
     ylab="Probability of True Value",
     main="Bailey's Binomial Model")
abline(v=bailey.august.est, col="red")

### September

n1 = 60
n2 = 70
m2 = 42

bailey.september.est <- (n1*(n2+1)) / (m2+1)
bailey.september.var <- (n1^2 * (n2+1) * (n2-m2)) / ((m2+1)^2 * (m2+2))
bailey.september.CV <- sqrt(bailey.september.var)/bailey.september.est

september.trial.N.prob_bailey <- dbinom(x=m2, size=n2, prob=n1/trial.N)

plot(x=trial.N, y=september.trial.N.prob_bailey, type="l", lwd = 3,
     xlab="Trial Population Sizes (N_hat)",
     ylab="Probability of True Value",
     main="Bailey's Binomial Model")
abline(v=bailey.september.est, col="red")

### Combined table & plot

bailey_table <- cbind(N_hat <- c(bailey.may.est, bailey.june.est, bailey.july.est, bailey.august.est, bailey.september.est), var <- c(bailey.may.var, bailey.june.var, bailey.july.var, bailey.august.var, bailey.september.var), CV <- c(bailey.may.CV, bailey.june.CV, bailey.july.CV, bailey.august.CV, bailey.september.CV))
rownames(bailey_table) <- c("May", "June", "July", "August", "September")
colnames(bailey_table) <- c("N_hat", "var", "CV")
bailey_table

plot(x=trial.N, y=june.trial.N.prob_bailey, type="l", col = "green", lwd = 3,
     xlab="Possible Population Sizes (N_hat)",
     ylab="Probability of True Value",
     main="PDF for Population Size Estimates (N_hat) by month") +
  lines(x=trial.N, y=may.trial.N.prob_bailey, col="blue", lwd = 3) +
  lines(x=trial.N, y=august.trial.N.prob_bailey, col="red", lwd = 3) +
  lines(x=trial.N, y=july.trial.N.prob_bailey, col="orange", lwd = 3) +
  lines(x=trial.N, y=september.trial.N.prob_bailey, col="black", lwd = 3)
legend(x = 600, y = 0.11, legend = c("May", "June", "July", "August", "September"),
       fill = c("blue", "green", "orange", "red", "black"), title = "Month")

  ## Summary --------------------------------------------------------------------

plot(x = 1:5 - 0.05, y = chapman_table[,1], pch = 16, col = "red", cex = 1.5,
     xlab = "Month",
     ylab = "N_hat Population Estimate",
     ylim = c(0,500),
     xaxt = "n") +
  points(x = 1:5 + 0.05, y = bailey_table[,1], pch = 18, col = "black", cex = 1.5) +
  axis(side = 1, at = c(1:5), labels = c("May", "June", "July", "August", "September"))
legend(x = 2.5, y = 500, legend = c("Chapman (sample w/o replacement)", "Bailey's (sample w/ replacement)"), col = c("red", "black"), pch = c(16, 18), title = "N_hat estimate method")
