data{
  int nr;
  int nc;
  matrix[nr,nc] X;
  vector[nr] y;
}
parameters{
  vector[nc] beta;
  vector<lower = 0>[nc] lambda;
  real <lower = 0> tau;
  real<lower = 0> sigma;
}
transformed parameters{
  real<lower = 0> tau01;
  // real<lower = 0> tau02;
 // real<lower = 0> tau06;
  tau01 = sigma/sqrt(50);
  // tau02 = 0.67*(sigma/sqrt(50));
 // tau06 = 3*(sigma/sqrt(500));
}
model{
  y ~ normal(X*beta, sigma);
  beta ~ normal(0,lambda*tau);
  tau ~ cauchy(0,tau01);
  // tau ~ cauchy(0,tau02);
 // tau ~ cauchy(0,tau06);
  lambda ~ cauchy(0,1);
}
