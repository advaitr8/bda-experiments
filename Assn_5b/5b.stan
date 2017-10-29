data{
  int x1[100];
  int x2[100];
  real y[100];
  #real error[100];
  }

parameters{
  real b0;
  real b1;
  real b2;
  real <lower = 0> sigma;
}
model{
  for (i in 1: 100){
  y[i] ~ normal(b0 + b1*x1[i] + b2*x2[i], sigma);
  }
  }
