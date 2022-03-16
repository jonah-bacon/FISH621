data {
  int nyears; // Number of Years
  int<lower=0> C[nyears]; // Counts
  int<lower=0> N[nyears]; // Binomial Totals
  vector[nyears] year; // Year covariates
}
transformed data {
  vector[nyears] year_squared;
  // Long Form
  for(i in 1:nyears) {
    year_squared[i] = year[i] * year[i];
  }
  // Vectorized Form
  // year_squared = year .* year;
}
parameters {
  real alpha;
  real beta1;
  real beta2;
}
transformed parameters {
  vector[nyears] logit_p;
  
  // Long Form
  for(i in 1:nyears) {
    logit_p[i] = alpha + beta1 * year[i] + beta2 * year_squared[i];
  } // next i
  
  // Vectorized form
  // logit_p = alpha + beta1 * year + beta2 * year_squared;
}
model {
  // PRIORS
  alpha ~ normal(0, 100);
  beta1 ~ normal(0, 100);
  beta2 ~ normal(0, 100);
  
  // LIKELIHOODS
  // Distribution for random part
  C ~ binomial_logit(N, logit_p);
}
generated quantities {
  real<lower=0, upper=1> p[nyears];
  
  for (i in 1 : nyears) {
    p[i] = inv_logit(logit_p[i]);
  }
}
