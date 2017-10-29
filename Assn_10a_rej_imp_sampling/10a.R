rm(list = ls())
install.packages("arm", dependencies = T)
install.packages("numDeriv", dependencies = T)
library("arm")
library(LearnBayes) 
library(mvtnorm) 
library(numDeriv) 
library(stats) 
setwd("/Users/Advait/Desktop/New School/Fall16/BDA/Class18")
curve(dbeta(x, 3,6),0,1)

# To do this, we first create a data.frame with 100000 random values between 0 and 1, and calculate their beta density values

sampled <- data.frame(proposal = runif(100000,0,1))
str(sampled)
sampled$targetDensity <- dbeta(sampled$proposal, 3,6)

# Now, accept proportional to the targetDensity. It’s easiest if we calculate the highest density value, and then accept the others in relation to that
maxDens = max(sampled$targetDensity, na.rm = T)
sampled$accepted = ifelse(runif(100000,0,1) < sampled$targetDensity / maxDens, TRUE, FALSE)
# Plot the result

hist(sampled$proposal[sampled$accepted], freq = F, col = "grey", breaks = 100)
curve(dbeta(x, 3,6),0,1, add =T, col = "red")
#########################################
#NORM
#########################################
curve(dnorm(x,10,2), -3,20)
sampled <- data.frame(proposal = runif(100000,0,20))
sampled$targetDensity <- dnorm(sampled$proposal , 10 ,2)
maxDens = max(sampled$targetDensity, na.rm = T)
sampled$accepted = ifelse(runif(100000,0,20) < sampled$targetDensity / maxDens, TRUE, FALSE)
hist(sampled$proposal[sampled$accepted], freq = F, col = "grey", breaks = 100)
curve(dnorm(x,10,2), -3,20, add = T, col = "red")
######################################################
# Consider the model, yj ∼ Binomial(nj,θj), where θj = logit−1(α + βxj), for j = 1,...,J, and with independent prior distributions, α ∼ t4(0,22) and β ∼ t4(0,1). Suppose J = 10, the xj values are randomly drawn from a U(0, 1) distribution, and nj ∼ Poisson+(5), where Poisson+ is the Poisson distribution restricted to positive values.
# 
# (a)  Sample a dataset at random from the model
# 
# (b)  Use rejection sampling to get 1000 independent posterior draws from (α,β).
# 
# (c)  Approximate the posterior density for (α, β) by a normal centered at the posterior mode with covariance matrix fit to the curvature at the mode.
# 
# (d)  Take 1000 draws from the two-dimensional t4 distribution with that center and scale matrix and use importance sampling to estimate E(α|y) and E(β|y). 
##
set.seed(8)
N <- 10
rtpois <- function(N1, lambda){
        qpois(runif(N1, dpois(0, lambda), 1), lambda)
}
n <- rtpois(N, 5)

alpha <- 4*rt(1,4)
beta <- rt(1, 4)
x <- runif(N,0,1)
theta <- invlogit(alpha + beta*x)
#Part A
y <- rbinom(N, n,theta)

#Part B

log.prior <- function(ab){
  dt(ab[1],4, log = T) + dt(ab[2],4,log = T) + log(4)
}

log.lik <- function(ab){
  sum(y*log(invlogit(ab[1] + ab[2]*x)) 
               + (n - y)*log(1 - invlogit(ab[1] + ab[2]*x)))
}

post <- function(ab){
  exp(log.prior(ab) + log.lik(ab))
}

ab.mat <- NULL

ab.mat <- data.frame(
  a.grid = 4*rt(10^4, 4), 
  b.grid = rt(10^4, 4)
)
str(ab.mat)

post.density <- apply(ab.mat,1,post)
prior <- function(ab){
   exp(log.prior(ab))
 }
accept <- function(ab){
  ifelse(
    post(ab)/(prior(ab)) >= M, 
    1, 0
  )
}
########
n
y
M <- NULL
for(i in 1:10){
  M[i] = 
}
M <- choose(n,y)
M <- prod(M)
##########
attempt <- apply(cbind(ab.mat$a.grid, ab.mat$b.grid), 1, accept)
summary(attempt)

ab.mat$attempt <- attempt
thing1 <- sample(ab.mat$a.grid[ab.mat$attempt==1],1000 )
mean(thing1, na.rm = T)
thing2 <- sample(ab.mat$b.grid[ab.mat$attempt==1],1000 )
mean(thing2, na.rm = 2)

par(mfcol = c(1,2))
hist(thing1, breaks = 50, freq = F, 
     main = "Marginal posterior density of alpha",
     cex.main = 0.8, xlab = "a.grid", col = "gray")
lines(density(4*rt(10^4, 4)), col = "red")
text(x = 12, y = 0.09,
     labels = paste("mean = ", 
                    round(mean(mean(thing1, na.rm = T)),2)))
text(x = 12, y = 0.08,
     labels = paste("sd = ", 
                    round(sd(thing1, na.rm = T),2)))

hist(sample(ab.mat$b.grid[ab.mat$attempt==1],1000), breaks = 50, freq = F, 
     main = "Marginal posterior density of beta",
     cex.main = 0.8, xlab = "b.grid", col = "gray")
lines(density(rt(10^4, 4)), col = "red")
text(x = 6, y = 0.3,
     labels = paste("mean = ", 
                    round(mean(mean(thing2, na.rm = T)),2)))
text(x = 6, y = 0.27,
     labels = paste("sd = ", 
                    round(sd(thing2, na.rm = T),2)))
######
##Part C
log.post <- function(ab){ 
  sum(y*log(invlogit(ab[1] + ab[2]*x)) 
      + (n - y)*log(1- invlogit(ab[1] + ab[2]*x)) 
      + log(dt(ab[1], 4)) 
      + log(4) 
      + log(dt(ab[2], 4)))
}

la_t <- laplace(log.post, c(0, -1)) 

alpha.sim <- seq(-5, 5, length.out = 300) 
beta.sim <- seq(-5, 5, length.out = 300) 
la <- length(alpha.sim) 
lb <- length(beta.sim)
mode1 <- c(la_t$mode[1], la_t$mode[2]) 
sigma <- la_t$var
approx <- matrix(0, la, lb)

for(i in 1:la){ 
  for (j in 1:lb){
    approx[i,j] <- dmvnorm(c(alpha.sim[i],beta.sim[j]), mode1, sigma) 
  }
}

#contour
par(mfrow = c(1, 1), 
    mar = c(3, 3, 1, 1), 
    oma = c(.5, .5, .5, .5), 
    mgp=c(2,1,0))

contour(alpha.sim, beta.sim, approx, nlevels = 5, 
        xlim=c(-0.4, 0.6),
        ylim=c(-0.5, .7),
        xlab="Alpha", 
        ylab="Beta",
        main=NULL)
points(la_t$mode[1],
       la_t$mode[2],
       pch=20,
       cex=1,
       col="blue") 
legend(0.44, 0.55,
       legend=c("mode"),
       col=c("blue"),
       cex=1,
       pch=c(16),
       bty="n")
##
#Part D
#Sample from the multi-t
draws <- rmvt(1000, 
              delta = mode1, 
              sigma = sigma,
              df = 4) 
alphas <- draws[,1]
betas <- draws[,2]

#Importance sampling algorithm
nsims <- 1000
post.dens <- NULL
for(i in 1:nsims){
  post.dens[i] <- exp(log.post(c(alphas[i], betas[i])))
}

g <- NULL
for(i in 1:nsims){
  g[i] <- dmvt(c(alphas[i], betas[i]),
               delta = mode1,
               sigma = sigma,
               df = 4,
               log = FALSE) 
}

#Generate Expectations
e_alpha <- sum(alphas*(post.dens/sum(post.dens))/
                 (g/sum(g)))/nsims
e_alpha

e_beta <- sum(betas*(post.dens/sum(post.dens))/
                (g/sum(g)))/nsims
e_beta

################
# jitt
#############
t <- c(0,0)
sdx <- 1
sdy <- 1
rho <- 0.5
sigma1 <- matrix(data = 
                 c(sdx^2, rho*sdx*sdy,rho*sdx*sdy, sdy^2), 
               nrow =2, ncol = 2)
corrs <- NULL
for (i in 1:1000){
  draws <- NULL
  draws <- rmvnorm(1000, mean = t, sigma = sig1)
  corrs[i] <- cor((draws[,1])[draws[,1] >0 & draws[,2]>0],
                  (draws[,2])[draws[,1] >0 &draws[,2]>0 ])
}

length(corrs[corrs>rho])
