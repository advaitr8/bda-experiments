data{
int nr; 
int nc;
matrix[nr, nc] Xmat; 
int y[nr];
} 
parameters{
vector[nc] betas; 
}
model{
 y ~ poisson(exp(Xmat*betas));
  for(i in 1:nc){
    betas[i] ~ cauchy(0, 2.5);
  }
}
