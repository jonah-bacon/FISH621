// Simple Linear Regression

data {
  int<lower=0> N;
  vector[N] obs;
  vector[N] x;
}

parameters {
  real alpha; // Intercept
  real beta;  // Slope
  real<lower=0> sigma; // Observation Error
}

transformed parameters {
  // Define transformed parameters at the top
  vector[N] pred;
  // Calculate predictions
  pred = alpha + x*beta;
}

model {
  // PRIORS
  alpha ~ normal(0,100);
  beta ~ normal(0,100);
  sigma ~ normal(0,100);
  // LIKELIHOODS
  obs ~ normal(pred, sigma);
}

