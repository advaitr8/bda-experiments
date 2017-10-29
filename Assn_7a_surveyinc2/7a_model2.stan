data{
  real y[101];
  int incentive[101];
  real mod[101];
  real burden[101];
  real v[101];
  real time[101];
  real form[101];
  real V[101];
  int id[101];
}
parameters{
  real b0;
  real b1;
  real b2;
  real b3;
  real b4;
  real b5;
  real b6;
  real b7;
  real b8;
  real b9;
  real <lower = 0> sigma;
  real alpha[39];
  real <lower = 0> tau;
}
model{
  for (i in 1:101){
    y[i] ~ normal(b0 + b1*incentive[i] + b2*mod[i]
                  + b3*burden[i] + b4*(mod[i]*burden[i])  
                  + b5*(incentive[i]*v[i])
                  + b6*(incentive[i]*time[i])
                  + b7*(incentive[i]*form[i])
                  + b8*(incentive[i]*mod[i])
                  + b9*(incentive[i]*burden[i])
                  + alpha[id[i]] , sqrt(pow(sigma,2) + V[i]));
}
  for(i in 1:39)
alpha[i] ~ normal(0, tau);
}


