data{
int N;  
int y[N];
real x[N];
}
parameters{
  real alpha;
  real beta;
}
model{
  for(i in 1:N)
  y[i] ~ bernoulli(Phi(alpha + beta * x[i]));
// }
// generated quantities{
//   real y_rep[N];
//   for(i in 1:N)
//   y_rep[i] = bernoulli_rng(Phi(alpha + beta * x[i]));
}
