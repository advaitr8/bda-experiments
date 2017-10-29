data{
  int y[750];
  int x1[750];
  int x2[750];
  #int id[750];
}
parameters{
  real b0;
  real b1;
  real b2;
  #real<upper = 0> b1[30];
  #real<upper = 0> b2[30];
  // real mu1;
  // real mu2;
  // real<lower = 0> tau1;
  // real<lower = 0> tau2;
}
model{
  // b1 ~ normal(mu1,tau1);
  // b2 ~ normal(mu2,tau2);
  // mu1 ~ normal(0,5);
  // mu2 ~ normal(0,5);
//   for (i in 1:30){
//   for (n in 1:750){
//   y[n] ~ bernoulli_logit(b0 + b1[id[i]]*x1[n] + b2[id[i]]*x2[n]);
for(i in 1:750){
  y[i] ~ bernoulli_logit(b0 + b1*x1[i] + b2*x2[i]);
}
// }
// }
// }
// generated quantities{
//   real<lower = 0, upper=1> y_rep[750];
//   for (i in 1:30){
//   for (n in 1:750){
//     y_rep[n] = bernoulli_rng(inv_logit(b0 + b1[id[i]]*x1[n] + b2[id[i]]*x2[n])); 
//   }
// }
// }
}
