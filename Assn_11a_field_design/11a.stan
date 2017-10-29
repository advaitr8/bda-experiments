data{
  int y[25];
  int treat[25];
  int row_level[25];
  int col_level[25];
}
parameters{
  real alpha;
  real beta_treat[5];
  real beta_row[5];
  real beta_col[5];
  real <lower = 0> sigma;
  real <lower = 0> sig_treat;
  real <lower = 0> sig_row_level;
  real <lower = 0> sig_col_level;
  real <lower = 0> sig_alpha;
}
model{
  for(i in 1:25){
  y[i] ~ normal(alpha + beta_treat[treat[i]] 
                      + beta_row[row_level[i]] 
                      + beta_col[col_level[i]], sigma);
beta_treat ~ normal(0,sig_treat);
beta_row ~ normal(0, sig_row_level);
beta_col ~ normal(0, sig_col_level);
  alpha ~ normal(252.16, sig_alpha);
 }
 }

generated quantities{
  real y_rep[25];
  for(i in 1:25){
    
      y_rep[i] = normal_rng(alpha + beta_treat[treat[i]]
                      + beta_row[row_level[i]]
                      + beta_col[col_level[i]], sigma);
    }
}
