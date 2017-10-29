data{
  real logit_usage[100];
  real x[100];
}
parameters{
  real b0;
  real b1;
  real<lower =0> sigma;
  real error[100];
}
model{
  error ~ normal(0,sigma);
  for (i in 1:100)
  logit_usage[i] ~ normal(b0 + b1*x[i] + error[i], sigma);
}
generated quantities{
  real logit_usage_rep[100];
  for (i in 1:100){
    logit_usage_rep[i] = normal_rng(b0 + b1*i, sigma);
  }
}
