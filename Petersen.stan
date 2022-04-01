// Simple Petersen Mark-Recapture Estimator

data {
  int<lower=0> n1; // Number marked at t1
  int<lower=0> n2; // Number captured at t2
  int<lower=0,upper=min(n1,n2)> m2; // Number of marked animals in second capture
}
parameters {
  real<lower=(n2 - m2 + n1)> N;
}
model {
  m2 ~ binomial(n2, n1 / N);
}
