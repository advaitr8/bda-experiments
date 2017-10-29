data{
  #int id[101];
  real rdif[62];
  real vdif[62];
  real m[62];
  real time[62];
  real f[62];
  real b[62];
  }
parameters{
  real b0;
  real b1;
  real b2;
  real b3;
  real b4;
  real b5;
  real <lower = 0> sigma;
}
model{
for(i in 1:62){
  rdif[i] ~ normal(b0 + b1*vdif[i] + b2*m[i] + b3*time[i] + b4*f[i] + b5*b[i], sigma);
    }
  
}
