data{
  vector[192]post;
  vector[192]pre;
  vector[192]treat;
  int grade[192];
  int city[192];
}
parameters{
  real b0;
  matrix [4,2] beta;
  matrix [4,2] theta;
  real <lower = 0> sigma;
  real mu_F;
  real<lower = 0> tau_F;
  real mu_Y;
  real<lower = 0> tau_Y;
  real mu_beta;
  real<lower = 0> tau_beta;
}
model{
  for(i in 1:192){
    post[i] ~ normal(b0 + beta[grade[i], city[i]]*pre[i] + 
                          theta[grade[i], city[i]]*treat[i] ,
                          sigma);
  }
  theta[, 1] ~ normal(mu_F, tau_F);
  theta[, 2] ~ normal(mu_Y, tau_Y);
  mu_F ~ normal(5,5);
  mu_Y ~ normal(5,5);
  tau_F ~ normal(3,2);
  tau_Y ~ normal(3,2);
  for(i in 1:4){
    beta[i,] ~ normal(mu_beta, tau_beta);
  }
}
