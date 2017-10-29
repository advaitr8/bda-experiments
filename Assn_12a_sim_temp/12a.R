rm(list = ls())
setwd("/Users/Advait/Desktop/New School/Fall16/BDA/Class22")
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
####
# Consider a simple one-parameter model of independent data, yi ∼ Cauchy(θ, 1), i = 1, . . . , n, with uniform prior density on θ and two data points, y1 = 1.3, y2 = 15.0.
# 
# (a)  Graph the posterior density.
# 
# (b)  Program the Metropolis algorithm for this problem using a symmetric Cauchy jumping distribution. Tune the scale parameter of the jumping distribution appropriately.
# 
# (c)  Program simulated tempering with a ladder of 10 inverse-temperatures, 0.1, . . . , 1.
# 
# (d)  Compare your answers in (b) and (c) to the graph in (a). 
#####
##PART A
##Posterior
post <- function(theta){
 ((1 + (1.3 - theta)^2)^-1) + ((1 + (15 - theta)^2)^-1)
}
arg <- seq(-10,30,0.001)
post(arg)
plot(arg, post(arg),type = "l")
abline(v = 1.3 , col = "red")
abline(v = 15 , col = "red")
####
####PART B
#Log Posterior
log_post <- function(theta){
  post = dcauchy(data,theta, 1, log = T)
  return(sum(post))
}
# #Cauchy proposal function
# proposalfunction <- function(theta){
#   return(rcauchy(1, location = theta, scale = 1))
# }

##Metropolis

run_metropolis_MCMC <- function(startvalue, iterations){
  chain = array(dim = c(iterations+1,length(startvalue)))
  chain[1,] = startvalue
  for (i in 1:iterations){
    proposalfunction <- function(theta){
      return(rcauchy(1, location = theta, scale = 1))
    }
    proposal = proposalfunction(chain[i,])
    probab = exp(log_post(proposal) - log_post(chain[i,]))
    if (runif(1) < probab){
      chain[i+1,] = proposal
    }else{
      chain[i+1,] = chain[i,]
    }
  }
  return(chain)
}

fit <- run_metropolis_MCMC(0, 20000)
warmup <- 10000
acceptance.rate <- 1 - mean(duplicated(fit[-(1:warmup),]))

####PART C
##PROPOSAL
temp <- sample(seq(0.1,1,0.1),1, replace = T)
proposalfunction <- function(theta){
  return(rcauchy(1, location = theta, scale = temp))
}

data <- c(1.3,15.0)
###POSTERIOR
log_post <- function(theta){
  post = dcauchy(data,theta, temp, log = T)
  return(sum(post))
}


#met <- function(startvalue, iterations){
  run_metropolis_MCMC <- function(startvalue, iterations){
  chain = array(dim = c(iterations+1,length(startvalue)))
  chain[1,] = startvalue
  for (i in 1:iterations){
    proposalfunction <- function(theta){
      return(rcauchy(1, location = theta, scale = temp))
    }
    proposal = proposalfunction(chain[i,])
probab = exp(log_post(proposal) - log_post(chain[i,]))
if (runif(1) < probab){
  chain[i+1,] = proposal
}else{
  chain[i+1,] = chain[i,]
}
  }
  return(chain)
}

run_metropolis_MCMC(0, 20000)
warmup <- 10000
acceptance.rate <- 1 - mean(duplicated(metropolis.func.fit[-(1:warmup),]))


###
met(0,20000)
print(metropolis.func.fit)
hist(metropolis.func.fit, breaks = 50, col = "gray",freq = F)

########

