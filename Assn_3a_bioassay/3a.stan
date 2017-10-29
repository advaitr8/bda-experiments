data{
int N;
#int n;
int y[N];
real x[N]; 
}
parameters{
real b1; 
real b2; 
}
transformed parameters{
  real theta[N];
  for (i in 1:N){
    theta[i] = inv_logit(b1 + b2*x[i]);
  }
}
model{
y ~ binomial(5,theta);
}
