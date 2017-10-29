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
model{
  y ~ normal(X*beta, sigma);
  beta ~ normal(0,lambda*tau);
   tau ~ cauchy(0,1);
  // tau ~ cauchy(0,sigma);
  lambda ~ cauchy(0,1);
}

