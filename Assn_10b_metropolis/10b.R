rm(list = ls())
setwd("/Users/Advait/Desktop/New School/Fall16/BDA/Class19")
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
# Consider the following discrete-data regression model: yi ∼ Poisson(eXiβ),i = 1,...,n, with independent Cauchy prior distributions with location 0 and scale 2.5 on the elements of β.
# 
# (a)  Write a program in R to apply the Metropolis algorithm for β given data X,y. Your program should work with any number of predictors (that is, X can be any matrix with the same number of rows as the length of y).
# 
# (b)  Simulate fake data from the model for a case with 50 data points and 3 predictors and run your program. Plot the posterior simulations from multiple chains and monitor convergence.
# 
# (c)  Fit the model in Stan and check that you get the same results. 


#############################################################################
##PART A
###
metropolis.func <- function(n.pred, length.data){
  #generate data
  generate <- function(n.pred, length.data){
    beta.mat <<- matrix(data = rnorm(3,1), nrow = n.pred, ncol = 1)
    X.mat <<- matrix(data = NA, nrow = length.data ,ncol = n.pred)
    for(i in 1:length.data){
      for(j in 1:n.pred){
        X.mat[i,j] <<- (rnorm(1))
      }
    }
    y <<- rpois(length.data, exp(X.mat %*% beta.mat))
  }
 #Generate some data
generate(n.pred, length.data)
 ####Prior
prior <- function(param){
  prb <- NULL
  for(i in 1:length(param)){
    prb[i] <- dcauchy(param[i], 
                      location = 0, 
                      scale = 2.5, 
                      log = TRUE)
  }
  return(sum(prb))
}
####Likelihood
likelihood <- function(param){
  pred = exp(X.mat %*% param)
  lk = dpois(y, pred, log = T)
  return(sum(lk))
}
####Posterior
posterior <- function(param){
 return (likelihood(param) + prior(param))
}
####Proposal function
proposalfunction <- function(param){
  return(rnorm(length(param),
               mean = param,
               sd = rep(0.05,length(param))))
}
####Metropolis algorithm
run_metropolis_MCMC <- function(startvalue, iterations){
  chain = array(dim = c(iterations+1,length(startvalue)))
  chain[1,] = startvalue
  for (i in 1:iterations){
    proposal = proposalfunction(chain[i,])
    
    probab = exp(posterior(proposal) - posterior(chain[i,]))
    if (runif(1) < probab){
      chain[i+1,] = proposal
    }else{
      chain[i+1,] = chain[i,]
    }
  }
  return(chain)
}
metropolis.func.fit <<- (run_metropolis_MCMC(rep(mean(c(beta.mat)), 
                                             length(c(beta.mat))), 20000))
warmup <<- 10000
acceptance.rate <- 1 - mean(duplicated(metropolis.func.fit[-(1:warmup),]))
return(paste("acceptance rate =", round(acceptance.rate, digits = 3)))
}

###PART B
metropolis.func(3,50)

#Histograms of parameter estimates
par(mfrow = c(1, 3), 
    mar = c(3, 3, 1, 1), 
    oma = c(.5, .5, .5, .5), 
    mgp = c(2,1,0))
parameters <- c(beta.mat)

hist((metropolis.func.fit[-(1:warmup),1]), 
     freq = FALSE, 
     xlab = "Beta1",
     col = "grey",
     main = NULL)
abline(v = parameters[1], 
       col = "red",
       lwd = 2)
abline(v = mean(metropolis.func.fit[-(1:warmup),1]), 
       col = "green",
       lwd = 2)
hist((metropolis.func.fit[-(1:warmup),2]), 
     freq = FALSE,
     xlab = "Beta2",
     col = "grey",
     main = NULL)
abline(v = parameters[2], 
       col = "red",
       lwd = 2)
abline(v = mean(metropolis.func.fit[-(1:warmup),2]), 
       col = "green",
       lwd = 2)
hist((metropolis.func.fit[-(1:warmup),3]), 
     freq = FALSE,
     xlab = "Beta3",
     col = "grey",
     main = NULL,
     lwd = 2)
abline(v = parameters[3], 
       col = "red",
       lwd = 2)
abline(v = mean(metropolis.func.fit[-(1:warmup),3]), 
       col = "green",
       lwd = 2)
#Check convergence
par(mfrow = c(1, 3), 
    mar = c(3, 3, 1, 1), 
    oma = c(.5, .5, .5, .5), 
    mgp = c(2,1,0))

plot(metropolis.func.fit[-(1:warmup), 1], 
     type = "l",
     xlab = "Sampling Iteration",
     ylab = "Beta 1",
     main = NULL)
abline(h = parameters[1], 
       col = "red",
       lwd = 2)
abline(h = mean(metropolis.func.fit[-(1:warmup),1]), 
       col = "green",
       lwd = 2)
plot(metropolis.func.fit[-(1:warmup), 2], 
     type = "l",
     xlab = "Sampling Iteration",
     ylab = "Beta 2",
     main = NULL)
abline(h = parameters[2], 
       col = "red",
       lwd = 2)
abline(h = mean(metropolis.func.fit[-(1:warmup),2]), 
       col = "green",
       lwd = 2)
plot(metropolis.func.fit[-(1:warmup), 3], 
     type = "l",
     xlab = "Sampling Iteration",
     ylab = "Beta 3",
     main = NULL)
abline(h = parameters[3], 
       col = "red",
       lwd = 2)
abline(h = mean(metropolis.func.fit[-(1:warmup),3]), 
       col = "green",
       lwd = 2)
###
##PART C
Xmat <- X.mat
y
nr <- 50
nc <- 3

stanc("10b.stan")
fit <- stan("10b.stan", 
            data = list("Xmat", "y"),
            iter = 1000,
            chains = 3)
print(fit)
extract(fit)
par(mfrow = c(2, 3), 
    mar = c(3, 3, 1, 1), 
    oma = c(.5, .5, .5, .5), 
    mgp = c(2,1,0))
hist(extract(fit)$betas[,1], 
      freq = FALSE, 
      xlab = "Beta1",
      col = "grey",
      main = "Stan Beta 1")
abline(v = parameters[1], 
       col = "red",
       lwd = 2)
abline(v = mean(extract(fit)$betas[,1]),
       col = "green",
       lwd = 2)
hist(extract(fit)$betas[,2], 
     freq = FALSE, 
     xlab = "Beta2",
     col = "grey",
     main = "Stan Beta 2")
abline(v = parameters[2], 
       col = "red",
       lwd = 2)
abline(v = mean(extract(fit)$betas[,2]),
       col = "green",
       lwd = 2)
hist(extract(fit)$betas[,3], 
     freq = FALSE, 
     xlab = "Beta3",
     col = "grey",
     main = "Stan Beta 3")
abline(v = parameters[3], 
       col = "red",
       lwd = 2)
abline(v = mean(extract(fit)$betas[,3]),
       col = "green",
       lwd = 2)

####
hist((metropolis.func.fit[-(1:warmup),1]), 
     freq = FALSE, 
     xlab = "Beta1",
     col = "grey",
     main = "Met Beta1")
abline(v = parameters[1], 
       col = "red",
       lwd = 2)
abline(v = mean(metropolis.func.fit[-(1:warmup),1]), 
       col = "green",
       lwd = 2)
hist((metropolis.func.fit[-(1:warmup),2]), 
     freq = FALSE,
     xlab = "Beta2",
     col = "grey",
     main = "Met Beta2")
abline(v = parameters[2], 
       col = "red",
       lwd = 2)
abline(v = mean(metropolis.func.fit[-(1:warmup),2]), 
       col = "green",
       lwd = 2)
hist((metropolis.func.fit[-(1:warmup),3]), 
     freq = FALSE,
     xlab = "Beta3",
     col = "grey",
     main = "Met Beta3",
     lwd = 2)
abline(v = parameters[3], 
       col = "red",
       lwd = 2)
abline(v = mean(metropolis.func.fit[-(1:warmup),3]), 
       col = "green",
       lwd = 2)
####

