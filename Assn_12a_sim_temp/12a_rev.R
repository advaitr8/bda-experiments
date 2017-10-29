rm(list = ls())
setwd("/Users/Advait/Desktop/New School/Fall16/BDA/Class22")
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
#################################################
#Part A
post <- function(theta){
  ((1 + (1.3 - theta)^2)^-1) + ((1 + (15 - theta)^2)^-1)
}
arg <- seq(-20,30,0.001)
post(arg)

plot(arg, post(arg),type = "l", xlim = c(-15,30),
     yaxt = "n",
     ylab = NA,
     bty = "n",
     xlab = "theta",
     main = "posterior density",
     cex.main = 0.9)
#################################################
#Part B
#log posterior function
y <- c(1.3,15)
scl <- 10
#
log_post <- function(theta){
  lpost = dcauchy(y, 
                  theta, 
                  1, log = T)
  return(sum(lpost))
}

#proposal function
proposal <- function(theta){
  initial = rcauchy(1,theta, scl)
  return(initial)
}

#Metropolis algorithm
metropolis <- function(startvalue, iterations, n_chains){
  chains <- list()
  for (j in 1:n_chains){
   chain = NULL
   chain = array(dim = c(iterations+1,length(startvalue)))
   chain[1,] = startvalue
 for(i in 1:iterations){
   prop = proposal(chain[i,])
   probab = exp(log_post(prop) - log_post(chain[i,]))
   if (runif(1) < probab){
     chain[i+1,] = prop
   }else{
     chain[i+1,] = chain[i,]
   }
   chains[[j]] <- chain
   }
  }
 return(chains)
}

fit <- metropolis(50,10000, 3)
warmup <- 5000
acceptance.rate <- NULL
for (i in 1:3){
  acceptance.rate[i] <- 1 - mean(duplicated(fit[[i]][-(1:warmup),]))
}
acceptance.rate
par (mfrow = c ( 1 , 2) ,
     mar = c ( 3 , 3 , 1 , 1) ,
     oma = c ( 0.5 , 0.5 , 0.5 , 0.5 ) ,
     mgp = c ( 2 , 1 , 0 ) )
plot(arg, post(arg),type = "l", 
     main = "posterior density",
     cex.main = 0.9,
     xlab = "theta",
     bty = "n",
     yaxt = "n",
     ylab = NA)
hist(c(fit[[1]][-(1:warmup),], fit[[2]][-(1:warmup),], fit[[3]][-(1:warmup),]), breaks = 50, freq = F,
     col = "gray",
     main = "Untempered",
     xlab = "theta",
     cex.main = 0.9, ylab = NA, yaxt = "n")
###############################################
#Part C

#run metropolis for simulated tempering
metropolis_temp <- function(startvalue, iterations, n_chains){
  chains <- list()
  for (j in 1:n_chains){
    chain = NULL
    chain = array(dim = c(iterations+1,length(startvalue)))
    chain[1,] = startvalue
    temp  = NULL
    for(i in 1:iterations){
      temp[i] <- 1/sample(seq(0.1,1,0.1),1, replace = T)
      proposal1 <- function(theta) {
      rcauchy (1,theta ,
                scale = temp[i] )
      }
      prop = proposal1(chain[i,])
      probab = exp(log_post(prop) - log_post(chain[i,]))
      if (runif(1) < probab){
        chain[i+1,] = prop
      }else{
        chain[i+1,] = chain[i,]
      }
      chains[[j]] <- chain
    }
  }
  return(chains)
}

fit_2 <- metropolis_temp(50,10000, 3)
warmup <- 5000
acceptance.rate <- NULL
for (i in 1:3){
  acceptance.rate[i] <- 1 - mean(duplicated(fit_2[[i]][-(1:warmup),]))
}
acceptance.rate
par (mfrow = c ( 1 , 3) ,
     mar = c ( 3 , 3 , 1 , 1) ,
     oma = c ( 0.5 , 0.5 , 0.5 , 0.5 ) ,
     mgp = c ( 2 , 1 , 0 ) )
plot(arg, post(arg),type = "l", 
     main = "posterior density",
     cex.main = 0.9,
     xlab = "theta",
     bty = "n",
     yaxt = "n",
     ylab = NA)
hist(c(fit[[1]][-(1:warmup),], fit[[2]][-(1:warmup),], fit[[3]][-(1:warmup),]), breaks = 50, freq = F,
     col = "gray",
     main = "Untempered",
     xlab = "theta",
     cex.main = 0.9, ylab = NA, yaxt = "n")

hist(c(fit_2[[1]][-(1:warmup),],
       fit_2[[2]][-(1:warmup),],
       fit_2[[3]][-(1:warmup),]),
     xlab = "theta",
     ylab = " ",
     yaxt = 'n',
     xlim = c(-20, 30),
     main = "Simulated tempering",
     breaks = 50,
     col = "grey",
     freq = FALSE,
     cex.main = 0.9)
########
##Checking convergence
par (mfrow = c ( 2 , 3) ,
     mar = c ( 3 , 3 , 1 , 1) ,
     oma = c ( 0.5 , 0.5 , 0.5 , 0.5 ) ,
     mgp = c ( 2 , 1 , 0 ) )
for(i in 1:3){
  plot(fit[[i]],
       cex = 0.5,
       pch = 16,
       main = paste("Untempered Chain", i),
       ylab = "theta",
       xlab = "iteration",
       ylim = c(-10,30),
       cex.main = 0.9)
       }
for(i in 1:3){
  plot(fit_2[[i]],
       cex = 0.5,
       pch = 16,
       main = paste("Tempered Chain", i),
       ylab = "theta",
       xlab = "iteration",
       ylim = c(-10,30),
       cex.main = 0.9)
}
