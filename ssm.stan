data {
  int<lower=0> T;
  vector[T] y;
}
parameters {
  real<lower=0, upper=10> mean_lambda; // Mean growth rate
  real<lower=0, upper=10> sigma_proc; // SD of state process
  real<lower=0, upper=100> sigma_obs; // SD of observation process
  vector<lower=0>[T - 1] lambda;
  real<lower=0, upper=500> N_est1; // Initial population size
}
transformed parameters {
  vector<lower=0>[T] N_est;
  
  N_est[1] = N_est1;
  // State process
  for (t in 1 : (T - 1)) {
    N_est[t + 1] = N_est[t] * lambda[t];
  }
}
model {
  // PRIORS
   // N_est1 ~ uniform(0, 500);
   // Informative
   N_est1 ~ normal(y[1], 100);
   mean_lambda ~ uniform(0, 10);
   sigma_proc ~ uniform(0, 10);
   sigma_obs ~ uniform(0, 100);
  
  // LIKELIHOODS
  // Process Variation
  lambda ~ normal(mean_lambda, sigma_proc);
  
  // Observation process
  y ~ normal(N_est, sigma_obs);
}
generated quantities {
  real<lower=0> cv_obs;
  real<lower=0> cv_proc;
  
  cv_obs = sigma_obs/mean(N_est);
  cv_proc = sigma_proc/mean_lambda;
}
