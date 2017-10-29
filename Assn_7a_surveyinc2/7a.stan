data{
  real y[101];
  int incentive[101];
  real mod[101];
  real burden[101];
  real V[101];
  int id[101];
}
parameters{
  real b0;
  real b1;
  real b2;
  real b3;
  real <lower = 0> sigma;
  real alpha[39];
  real <lower = 0> tau;
}
model{
for (i in 1:101){
    y[i] ~ normal(b0 + b1*incentive[i] + b2*mod[i]
                  + b3*burden[i] + alpha[id[i]] , sqrt(pow(sigma,2) + V[i]));
  }
for (i in 1:39){
  alpha[i] ~ normal(0,tau);
}
}
