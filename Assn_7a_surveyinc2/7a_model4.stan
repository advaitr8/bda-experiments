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
  real b10;
  real b11;
  real b12;
  real b13;
  real b14;
  real b15;
  real b16;
  real b17;
  real b18;
  real b19;
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
                  + b10*(incentive[i]*v[i]*time[i])
                  + b11*(incentive[i]*v[i]*burden[i])
                  + b12*(incentive[i]*time[i]*burden[i])
                  + b13*(incentive[i]*v[i]*form[i])
                  + b14*(incentive[i]*v[i]*mod[i])
                  + b15*(incentive[i]*time[i]*form[i])
                  + b16*(incentive[i]*time[i]*mod[i])
                  + b17*(incentive[i]*form[i]*mod[i])
                  + b18*(incentive[i]*form[i]*burden[i])
                  + b19*(incentive[i]*mod[i]*burden[i])
                  + alpha[id[i]] , sqrt(pow(sigma,2) + V[i]));
b1 ~ normal(0.06,.015);
b2 ~ normal(0.018,.015);
b3 ~ normal(-.099,.015);
b4 ~ normal(-.049,.015);
b5 ~ normal(.0026,.015);
b6 ~ normal(-.002,.015);
b7 ~ normal(-.012,.015);
b8 ~ normal(.078,.015);
b9 ~ normal(-.052,.015);
b10 ~ normal(.0058,.015);
b11 ~ normal(.011,.015);
b12 ~ normal(.11,.015);
b13 ~ normal(.003,.015);
b14 ~ normal(-.012,.015);
b15 ~ normal(.099,.015);
b16 ~ normal(-.174,.015);
b17 ~ normal(-.003,.015);
b18 ~ normal(.059,.015);
b19 ~ normal(-.058,.015);
}
for(i in 1:39){
    alpha[i] ~ normal(0, tau);}
}
