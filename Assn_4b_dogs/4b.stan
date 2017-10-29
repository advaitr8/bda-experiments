data {
  int<lower=0> N; 
  int<lower=0> S; 
  int<lower=0> id[N]; 
  int y[N];
  real avoid[N];
  real shock[N];
}

parameters {
  real b0;
  real b1[30];
  real b2[30];
  real mu1;
  real<lower=0> tau1;
  real mu2;
  real<lower=0> tau2;
  }

model {
  b1 ~ normal(mu1,tau1);
  b2 ~ normal(mu2,tau2);
  for(n in 1: N){
    for(i in 1:S){
  y[n] ~ bernoulli_logit(b0 + b1[id[i]]*trials[n] + b2[id[i]]*shock[n]);
    }
  }
}

