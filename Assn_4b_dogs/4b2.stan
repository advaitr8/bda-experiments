data {
  int<lower=0> N; 
  #int<lower=0> S; 
  int<lower=0> id[N]; 
  int y[N];
  vector[N] avoid;
  vector[N] shock;
}

parameters {
  real<upper=0> b1[30];
  real<upper=0> b2[30];
  real mu1;
  real<lower=0> tau1;
  real mu2;
  real<lower=0> tau2;
  
  }
transformed parameters{
  real a[N];
for(i in 1:30){
  for(n in 1: N){
  a[n] = exp(b1[id[i]]*avoid[n] + b2[id[i]]*shock[n]);
  }}
}

model {
  b1 ~ normal(mu1,tau1);
  b2 ~ normal(mu2,tau2);
  for(n in 1:N){
  target += bernoulli_lpmf(y[n] | a[n]);
  }
  #y[n] ~  bernoulli_lpmf(a[n]);

//   real<lower=0, upper=1> a[N];
//   for(i in 1:S){
//   for(n in 1: N){
//   a[n] = b1[id[i]]*avoid[n] + b2[id[i]]*shock[n];}}
// }
  // for(n in 1: N)
  //   increment_log_prob(bernoulli_log(y[n],a[n]));
  //   #y[n] ~ exp(bernoulli_log(25,a[n]));
     
//}
// generated quantities{
//   int<lower = 0, upper=1> avoid_pred[N];
//   real foo[N];
//   for(i in 1:N){
//   foo[i] = exp(b1[id[i]]*avoid[i] + b2[id[i]]*shock[i]);
//   avoid_pred[i] = bernoulli_rng(foo[i]);}
// }
}
