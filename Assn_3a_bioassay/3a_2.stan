data{
int N;
int y[N];
real x[N]; 
matrix[2,2] sigma;
vector[2] ab;
}
parameters{
vector[2] b; 
}
transformed parameters{
  real theta[N];
  for (i in 1:N){
    theta[i] = inv_logit(b[1] + b[2]*x[i]);
  }
}
model{
y ~ binomial(5,theta);
b ~ multi_normal(ab,sigma);
}