data{
  int y[750];
  real x1[750];
  real x2[750];
  int id[750];
  }
parameters{
  real b1[30];
  real b2[30];
  real mu1;
  real mu2;
  real tau1;
  real tau2;
}
transformed parameters{
  for (i in 1:30){
  for (n in 1:750){
    a[n] = exp(b1[id[i]]*x1[n] + b2[id[i]]*x2[n]);
  }  
  }
  
}
model{
  b1 ~ normal(mu1,tau1);
  b2 ~ normal(mu2,tau2);
  for n in 1:750{
  target += bernoulli_lpmf(y[n]|a[n]);
  }
}
