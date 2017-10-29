rm(list = ls())
setwd("/Users/Advait/Desktop/New School/Fall16/BDA/Class21")
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library("arm")
install.packages("LearnBayes")
library("LearnBayes")
install.packages("MASS")
library("MASS")
##################
# Program HMC in R for the bioassay logistic regression example from Chapter 3.
# 
# (a)  Code the gradients analytically and numerically and check that the two programs give the same result.
# 
# (b)  Pick reasonable starting values for the mass matrix, step size, and number of steps.
# 
# (c)  Tune the algorithm to an approximate 65% acceptance rate.
# 
# (d)  Run 4 chains long enough so that each has an effective sample size of at least 100. How many iterations did you need?
# 
# (e)  Check that your inferences are consistent with those from the direct approach in Chapter 3. 


x <- c(-0.86, -0.3, -0.05, 0.73)
n <- rep(5,4)
y <- c(0,1,3,5)

##1Posterior
log_post <- function(param, x , y){
  param_prior <- dunif(param,-100,100, log = T)
  log_prior <- sum(param_prior)
  log_likelihood <- sum(dbinom(y, n, invlogit(param[1] + param[2]*x),log = T))
  return(log_prior + log_likelihood)
}

##PART A
#Analytical gradient
gradient_an <- function(param, x,y){
  d_alpha <- sum(y - n*exp(param[1] + param[2]*x)/(1 + exp(param[1] + param[2]*x)))
  d_beta <- sum(y*x - n*x*exp(param[1] + param[2]*x)/(1 + exp(param[1] + param[2]*x)))
  return(c(d_alpha,d_beta))
}

gradient_an(c(0.1,4),x,y)

#Numerical gradient
gradient_num <- function(param, x, y){
  d <- length(param)
  e <- 0.0001
  diff <- rep(NA,d)
 for(k in 1:d){
   th_hi <- param
   th_lo <- param
   th_hi[k] <- param[k] + e
   th_lo[k] <- param[k] - e
   diff[k] <- (log_post(th_hi,x,y) - log_post(th_lo,x,y))/(2*e)
  }
  return(diff)
}


#Histogram
checking <-matrix(NA ,ncol=2,nrow =1000)
for(i in 1:1000){
  random_unif <- runif(2, -10,10)
  checking[i,]<-gradient_num(random_unif ,x,y)-gradient_an(random_unif ,x,y)
}
options(scipen =999)
checking <- checking[complete.cases(checking),]
as.matrix(checking)
par(mfrow = c(1, 2),
    mar = c(3, 3, 1, 1),
    oma = c(.5, .5, .5, .5),
    mgp = c(2,1,0))
hist(checking[,1], breaks = 100, col = "gray",
     main = "Differences in alpha",
     xlab = "Differences",
     yaxt = "n",
     ylab = NA,
     cex.main = 1)
abline(v = 0, col = "red")
#
hist(checking[,2], breaks = 100, col = "gray",
     main = "Differences in beta",
     xlab = "Differences",
     yaxt = "n",
     ylab = NA,
     cex.main = 1)
abline(v = 0, col = "red")
###########################################################
##PART B
log_post_interim <- function(param){
  log_prior <- 0
  log_likelihood <- sum(dbinom(y, n, invlogit(param[1] + param[2]*x),log = T))
  return(log_prior + log_likelihood)
}
lat <- laplace(log_post_interim, c(1,15))
M <- ginv(lat$var)

#HMC iter
hmc_iteration <- function(param,x,y, epsilon,L,M){
  M_inv <- 1/M
  d <- length(param)
  # Sample 10 points randomly from a normal distribution with mean = 0 and standard deviation = sqrt(M)
  phi <- rnorm(d , 0, sqrt(M))
  param_old <- param
  log_p_old <- log_post(param,x,y) - 0.5*sum(M_inv*phi^2)
  phi <- phi + 0.5*epsilon*gradient_num(c(param), x, y)
  for (l in 1:L){
    param <- param + epsilon*M_inv*phi
    phi <- phi + (if (l==L)0.5 else 1)*epsilon*gradient_num(c(param),x,y)
  }
  phi <- -phi
  log_p_star <- log_post(c(param),x,y) - 0.5*sum(M_inv*phi^2)
  r <- exp(log_p_star - log_p_old)
  if(is.nan(r)) r <- 0
  p_jump <- min(r,1)
  param_new <- if(runif(1) < p_jump) param else param_old
  return (list (param = param_new, p_jump = p_jump))
}

##HMC run
hmc_run <- function (starting_values, iter, epsilon_0, L_0, M) {
# Get the number of rows and store in chains
chains <- nrow (starting_values)
# The number of parameters that you have in the starting values
d <- ncol (starting_values)
# Create space to store iterations of the parameters 
sims <- array (NA, c(iter, chains, d),
               dimnames=list (NULL, NULL, colnames (starting_values)))
warmup <- 0.5*iter
p_jump <- array (NA, c(iter, chains))
for (j in 1:chains){
  param <- starting_values[j,]
  for (t in 1:iter){
    epsilon <- runif (1, 0, 2*epsilon_0)
    L <- ceiling (2*L_0*runif(1))
    temp <- hmc_iteration (param,x,y, epsilon,L,M)
    p_jump[t,j] <- temp$p_jump
    sims[t,j,] <- temp$param
    param <- temp$param
  } }
monitor (sims, warmup)
cat ("Avg acceptance probs:",
     fround(colMeans(p_jump[(warmup+1):iter,]),2),"\n")
return (list (sims=sims, p_jump=p_jump))
}

##RUN it
parameter_names <- c (paste ("param[",1:2,"]",sep=""))
d <- 2
chains <- 4

#
mass_vector <- c(1.9648937, 0.08595404)

#Starts
starts <- array (NA,c(chains,d),dimnames=list(NULL,parameter_names))
for (j in 1:chains){
  starts[j,1] <- rnorm (1,0,15)
  starts[j,2] <- rnorm (1,0,15)
}


###PART C 
#65% accuracy
M2 <- hmc_run (starting_values=starts, iter=100,
                 epsilon_0=1, L_0=3, M=mass_vector)
##PART D
#100 n_eff
M1 <- hmc_run (starting_values=starts, iter=500,
               epsilon_0=.1, L_0=10, M=mass_vector)
#PART E
grid_sim <- function(alpha, beta){
  x <- x
  y <- y
  n <- n
  grid_length = length(alpha)
  grid = matrix(NA, grid_length, grid_length) 
  for(i in 1:grid_length){
    for(j in 1:grid_length){ 
      grid[i,j] <- exp(log_post(c(alpha[i], beta[j]), x, y))
    }
  }
  return(grid/sum(grid)) #normalizing
}

sampling <- function(S=1000, grid, alpha, beta){
  marginal_alpha <- apply(grid,1,sum) 
  cdf_alpha <- cumsum(marginal_alpha)
  alpha_sim = rep(0,S) 
  beta_sim = rep(0,S) 
  for(s in 1:S){
    random_unif <- runif(1,0,1)
    f.alpha <- max(cdf_alpha[cdf_alpha <= random_unif])
    alpha_sim[s] <- alpha[cdf_alpha == f.alpha]
    beta_p <- length(alpha[alpha <= alpha_sim[s]]) 
    grid[beta_p, ] <- grid[beta_p,]/sum(grid[beta_p,]) 
    cdf_beta_con <- cumsum(grid[beta_p,]) 
    random_unif<-runif(1,0,1)
    f.beta <- max(cdf_beta_con[cdf_beta_con <= random_unif ])
    beta_sim[s] <- beta[cdf_beta_con == f.beta] }
  return(list(alpha_sim = alpha_sim,
              beta_sim = beta_sim)) }

alpha_dir <- seq(-2,8,length.out = 300) 
beta_dir <- seq(-10,39,length.out = 300)

grid <- grid_sim(alpha_dir, beta_dir)
draws <- sampling(S=1000, grid, alpha_dir, beta_dir) 
alpha_draws<-draws[[1]]
beta_draws<-draws[[2]]
mean(alpha_draws) 
mean(beta_draws) 
sd(alpha_draws) 
sd(beta_draws)

###
par(mfcol = c(1,2), mar = c(3, 3, 1, 1), 
    oma = c(.5, .5, .5, .5), 
    mgp = c(2,1,0))
#alpha grid scatter
plot(alpha_draws, beta_draws, pch = 16, cex = 0.4, main = "Direct approach", cex.main = 0.8, xlim = c(-4,10), ylim = c(0,40),
     xlab = "alpha", ylab = "beta")
plot(M1$sims[250:500,,1], M1$sims[250:500,,2], pch = 16, cex = 0.4, main = "HMC", cex.main = 0.8, xlim = c(-4,10), ylim = c(0,40),
     xlab = "alpha", ylab = "beta")
plot(density(alpha_draws))
abline(v = mean(alpha_draws))
plot(density(M1$sims[,,1]))
str(M1$sims)
###
#LD50
DA <- -(alpha_draws/beta_draws)
hist(DA, main = "LD 50 Direct",
     ylab = NA, yaxt = "n", breaks = 50,
     col = "gray")
abline(v = mean(DA), col = "red")

HMCA <- -(M1$sims[250:500,,1]/M1$sims[250:500,,2])
hist(HMCA, main = "LD 50 HMC",
     ylab = NA, yaxt = "n", breaks = 50,
     col = "gray")
abline(v = mean(HMCA), col = "red")
###############
foo <- c(10,20,30)
sd(foo)
foo1 <- foo/100
sd(foo1)
100^2
