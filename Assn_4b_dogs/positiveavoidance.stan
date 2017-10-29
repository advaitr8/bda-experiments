data{
  int y[750];
  int x1[750];
  int x2[750];
  int id[750];
  }
parameters{
  real<lower = 0> b1[30];
  real<lower = 0> b2[30];
  real mu1;
  real mu2;
  real<lower = 0> tau1;
  real<lower = 0> tau2;
}
model{
  b1 ~ normal(mu1,tau1);
  b2 ~ normal(mu2,tau2);
  mu1 ~ normal(0,5);
  mu2 ~ normal(0,5);
  for (i in 1:30){
  for (n in 1:750){
   target += bernoulli_lpmf(y[n]|exp(b1[id[i]]*x1[n] + b2[id[i]]*x2[n]));
}
}
}
