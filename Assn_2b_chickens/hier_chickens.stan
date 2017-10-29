data{
int N;
real y[N];
real<lower=0> sigma[N]; 
real tau;
}
parameters{
real theta [N] ; 
real mu; 
#real<lower=0> tau;
}
model{
theta ~ normal(mu, tau); 
y ~ normal(theta , sigma);
}
