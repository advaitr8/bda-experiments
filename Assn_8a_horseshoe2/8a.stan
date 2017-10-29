data{
  int nr;
  int nc;
  matrix[nr,nc] X;
  int y[nr];
}
parameters{
  vector[nc] beta;
  vector<lower = 0>[nc] lambda;
  real <lower = 0> tau;
}
transformed parameters{
  matrix[nr,nc]theta;
  vector[nr] theta2;
  for (j in 1:nc){
  for(i in 1:nr){
    theta[i,j] = X[i,j]*beta[j];
}
}
  for(i in 1:nr){
    theta2[i] = sum(theta[i,]);
  }
}
model{
  for(i in 1:nr){
    y[i] ~ bernoulli_logit(theta2[i]);
  }
  for (i in 1:nc){
  beta[i] ~ normal(1,lambda[i]*tau);
  }
  tau ~ cauchy(0,1);
  lambda ~ cauchy(0,1);
}
