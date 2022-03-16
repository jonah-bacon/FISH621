// Bristol Bay stock recruitment

data {
  int nyears; // Number of Years
  real<lower=0> spawn[nyears]; // Spawner counts
  real ln_rps[nyears];
}

parameters {
  real alpha;
  real<lower=0> beta;
  real<lower=0> sigma;
}

transformed parameters {
  vector[nyears] pred_ln_rps;
  
  // Long Form
  for(i in 1:nyears) {
    pred_ln_rps[i] = alpha + beta * spawn[i];
  } // next i
  
}
model {
    // PRIORS
  alpha ~ normal(0, 100);
  beta ~ normal(0, 100);
  sigma ~ normal(0,1);
  
  // LIKELIHOODS
  // Distribution for random part
  ln_rps ~ normal(pred_ln_rps, sigma);
}
