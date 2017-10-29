data{
int n;
int y[n];
real x[n]; 
real u[n];
}
parameters{
real alpha; 
real beta; 
}
transformed parameters{
  real lambda[n];
  for (i in 1:n)
 lambda[i] = u[i]*(exp(alpha + beta*(x[i])));
 }
model{
y ~ poisson(lambda);
}

