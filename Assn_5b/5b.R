# Computing problem:
# Suppose you have 100 data points that arose from the 
# following model: y = 3 + 0.1 x1 + 0.5 x2 + error, 
# with errors having a t distribution with mean 0, 
# scale 5, and 4 degrees of freedom. We shall explore the 
# implications of fitting a standard linear regression to 
# these data.
# a.Simulate data from this model. For simplicity, suppose 
# the values of x1 are simply the integers from 1 to 100, and 
# that the values of x2 are random and equally likely to be 0 
# or 1. Use Stan to fit a linear regression (with normal errors)
# to these data and see if the 50% posterior intervals for the 
# regression coefficients cover the true values.
# b.Put the above step in a loop and repeat 100 times. 
# Calculate the confidence coverage for the 50% intervals for 
# each of the three coefficients in the model.
rm(list = ls())
getwd()
setwd("/Users/Advait/Desktop/New School/Fall16/BDA/Class9")
install.packages("beepr")
library(beepr)

x1 <- c(1:100)
x2 <- rbinom(100, 1, 0.5)
error <- 5*rt(100, 4)
y <- NULL
for (i in 1:100){
  y[i] = 3 + 0.1*x1[i] + 0.5*x2[i] + error[i]
}
plot(density(y))
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
stanc("5b.stan")
fit1 <- stan("5b.stan", data = list("x1", "x2", "y"),
             iter = 1000,
             chains = 3)
ext1 <- extract(fit1)
ext1$sigma
par(mfcol = c(1,1))
print(fit1)
plot(density(error),main = "Error distributed with very wide tails")
abline(v=(mean(error)),lty  =2, col = "red")

meanextb0 <- NULL
meanextb1 <- NULL
meanextb2 <- NULL
q1extb0 <- NULL
q1extb1 <- NULL
q1extb2 <- NULL
q3extb0 <- NULL
q3extb1 <- NULL
q3extb2 <- NULL

for(i in 1:100){
  fit <- stan("5b.stan", data = list("x1", "x2", "y"),
                 iter = 1000,
                 chains = 3)
  meanextb0[i] <- quantile(extract(fit)$b0, 
                        probs = 0.5)
  meanextb1[i] <- quantile(extract(fit)$b1,
                        probs = 0.5)
  meanextb2[i] <- quantile(extract(fit)$b2,
                       probs = 0.5)
  q1extb0[i] <- quantile(extract(fit)$b0,
                         probs = 0.25)
  q1extb1[i] <- quantile(extract(fit)$b1,
                         probs = 0.25)
  q1extb2[i] <- quantile(extract(fit)$b2,
                         probs = 0.25)
  q3extb0[i] <- quantile(extract(fit)$b0,
                         probs = 0.75)
  q3extb1[i] <- quantile(extract(fit)$b1,
                         probs = 0.75)
  q3extb2[i] <- quantile(extract(fit)$b2,
                         probs = 0.75)
}
beep(8)
summary(meanextb0)
summary(meanextb1)
summary(meanextb2)
summary(q1extb0)
summary(q1extb1)
summary(q1extb2)
summary(q3extb0)
summary(q3extb1)
summary(q3extb2)

