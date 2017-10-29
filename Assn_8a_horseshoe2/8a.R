##Question 1
rm(list = ls())
setwd("/Users/Advait/Desktop/New School/Fall16/BDA/Class14")
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

##Question 1  - India
library(plyr)

#Read in Data
ind <- read.csv("paneldata_csv.csv",header = TRUE)
str(ind)

#Plot Health Exp by state over time
inf.state.time <- ddply(ind, .(State.no., Time), summarise,
                        inf = mean(IMR),
                        exp = mean(state_exp))
str(inf.state.time)
###TRY1
par(mfcol = c(1,1))
x <- inf.state.time$exp
y <- inf.state.time$inf
length(x)
stanc("8atrial.stan")
projfit <- stan("8atrial.stan", data = list("x", "y"),
                iter = 1000, chains = 3)
ext_proj <- extract(projfit)
y_alex <- colMeans(ext_proj$y_alex)
bar1 <- NULL
bar2 <- NULL
for (i in 1:270){
  bar1[i] <- quantile(ext_proj$y_alex[,i],probs = 0.025 )
  bar2[i] <- quantile(ext_proj$y_alex[,i],probs = 0.975 ) }

plot(inf.state.time$exp,inf.state.time$inf,
     pch = 16,
     cex = .6, ylim = c(5,100))
points(inf.state.time$exp, y_alex, col = "red", pch = 16,
       cex = 0.6)
points(inf.state.time$exp, bar1, col = "gray", pch = 16,
       cex = 0.6)
points(inf.state.time$exp, bar2, col = "gray", pch = 16,
       cex = 0.6)
legend(8,80, legend = c("data","predicted","95% interval"),
       col = c("black", "red","gray"),lwd = 1, bty = "n")

orderx <- inf.state.time$inf[order(inf.state.time$inf)]
ordery_alex <- y_alex[order(y_alex)]
plot(ind$State.no.,orderx)
points(ind$State.no.,ordery_alex, col = "red")




#############
###Question2
#############
rm(list = ls())
setwd("/Users/Advait/Desktop/New School/Fall16/BDA/Class14")
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
################
# Simulate a fake-data scenario of a linear regression with n data points and D predictors, of which D − p are close to zero and p are far from zero. Then fit the horseshoe model and compare your inferences to the true parameter values.
# 
# Do this for a range of values of n, D, and p. 
# Repeat Exercise 2 from previous assignment with logistic instead of linear regression. 
#################
random_gen <- function(lengthb0, lengthb1, numberx){
  b0 <- NULL
  b1 <- NULL
  y <- NULL
  noise <- NULL
  coeff <- NULL
  snoopx <<- matrix(NA, nrow = numberx, ncol = (lengthb0 + lengthb1))
  for(i in 1:numberx){
    for(j in 1:(lengthb0+lengthb1)){
      snoopx[i,j] <<- rnorm(1)
    }
  } 
  step2 <- for(i in 1:lengthb0){
    b0[i] <- rnorm(1,0,0.001 )
  }
  
  step3 <- for (i in 1:lengthb1){
    b1[i] <- rnorm(1,8,5)
  }
  
  step4 <- for(i in 1:numberx){
    noise[i] <- rnorm(1)
  }
  beta <- matrix(c(rep(0.01,lengthb0), rep(3,lengthb1)),
                 nrow = (lengthb0 + lengthb1) , 
                 ncol = 1)
  trial <<- snoopx %*% beta
  prob <<- exp(snoopx %*% beta)/ (1+ exp(snoopx %*% beta))
  coeff <<- beta
  y <<- rbinom(numberx, 1, prob)
}


###TRY 1
# n = 50
# numberx => D = 6
# far from 0 => p = 3
# close to 0 => D-p = 3
random_gen(3,3,50)
nc <- 6
nr <- 50
X <- snoopx
y <- as.vector(y)
coeff1 <- as.vector(coeff)
fit1 <- stan("8a.stan", data = list("X", "y", "nc", "nr"),
             iter = 2000, chains = 3)
print(fit1)
ext1 <- extract(fit1)
est1 <- colMeans(ext1$beta)

###TRY 2
# n = 50
# numberx => D = 10
# far from 0 => p = 4
# close to 0 => D-p = 6
random_gen(6,4,50)
nc <- 10
nr <- 50
X <- snoopx
y <- as.vector(y)
coeff2 <- as.vector(coeff)
stanc("8a.stan")
fit2 <- stan("8a.stan", data = list("X", "y", "nc", "nr"),
             iter = 1000, chains = 3)
print(fit2)
ext2 <- extract(fit2)
est2 <- colMeans(ext2$beta)

###TRY 3
# n = 500
# numberx => D = 20
# far from 0 => p = 15
# close to 0 => D-p = 5
random_gen(5,15,500)
nc <- 20
nr <- 500
X <- snoopx
y <- as.vector(y)
coeff3 <- as.vector(coeff)
fit3 <- stan("8a.stan", data = list("X", "y", "nc", "nr"),
             iter = 1000, chains = 3)
print(fit3)
ext3 <- extract(fit3)
est3 <- colMeans(ext3$beta)

###TRY 4
# n = 500
# numberx => D = 20
# far from 0 => p = 5
# close to 0 => D-p = 15
random_gen(15,5,500)
nc <- 20
nr <- 500
X <- snoopx
y <- as.vector(y)
coeff4 <- as.vector(coeff)
fit4 <- stan("8a.stan", data = list("X", "y", "nc", "nr"),
             iter = 1000, chains = 3)
print(fit4)
ext4 <- extract(fit4)
est4 <- colMeans(ext4$beta)



###
par(mfcol = c(2,2),mar = c(2.5,2.5,2.5,2.5))
plot(c(1:6),coeff1, 
     xlab = "Number of predictors",
     ylab = "Original/Predicted values", pch = 16,
     cex = .6,
     ylim = c(-0.5,6),
     main = "Model I",
     cex.main = 0.8)
points(c(1:6), est1, pch = 16, cex = .6, col = "red")
lines(c(1:6), coeff1, lty = 2)
lines(c(1:6), est1, lty = 2, col = "pink")
#
plot(c(1:10),coeff2, 
     xlab = "Number of predictors",
     ylab = "Original/Predicted values", pch = 16,
     cex = .6,
     ylim = c(-0.5,6),
     main = "Model II",
     cex.main = 0.8)
points(c(1:10), est2, pch = 16, cex = .6, col = "red")
lines(c(1:10), coeff2, lty = 2)
lines(c(1:10), est2, lty = 2, col = "pink")
#
plot(c(1:20),coeff3, 
     xlab = "Number of predictors",
     ylab = "Original/Predicted values", pch = 16,
     cex = .6,
     ylim = c(-0.5,6),
     main = "Model III",
     cex.main = 0.8)
points(c(1:20), est3, pch = 16, cex = .6, col = "red")
lines(c(1:20), coeff3, lty = 2)
lines(c(1:20), est3, lty = 2, col = "pink")
#
plot(c(1:20),coeff4, 
     xlab = "Number of predictors",
     ylab = "Original/Predicted values", pch = 16,
     cex = .6,
     ylim = c(-0.5,6),
     main = "Model IV",
     cex.main = 0.8)
points(c(1:20), est4, pch = 16, cex = .6, col = "red")
lines(c(1:20), coeff4, lty = 2)
lines(c(1:20), est4, lty = 2, col = "pink")

coeff1
par(mfcol = c(2,3))
plot(density(ext1$beta[,1]), main = "Beta1")
abline(v = 0.01, col = "red")
plot(density(ext1$beta[,2]), main = "Beta2")
abline(v = 0.01, col = "red")
plot(density(ext1$beta[,3]), main = "Beta3")
abline(v = 0.01, col = "red")
plot(density(ext1$beta[,4]), main = "Beta4")
abline(v = 3, col = "red")
plot(density(ext1$beta[,5]), main = "Beta5")
abline(v = 3, col = "red")
plot(density(ext1$beta[,6]), main = "Beta6")
abline(v = 3, col = "red")
##
coeff2
par(mfcol = c(2,5))
plot(density(ext2$beta[,1]), main = "Beta1")
abline(v = 0.01, col = "red")
plot(density(ext2$beta[,2]), main = "Beta2")
abline(v = 0.01, col = "red")
plot(density(ext2$beta[,3]), main = "Beta3")
abline(v = 0.01, col = "red")
plot(density(ext2$beta[,4]), main = "Beta4")
abline(v = 0.01, col = "red")
plot(density(ext2$beta[,5]), main = "Beta5")
abline(v = 0.01, col = "red")
plot(density(ext2$beta[,6]), main = "Beta6")
abline(v = 0.01, col = "red")
plot(density(ext2$beta[,7]), main = "Beta7")
abline(v = 3, col = "red")
plot(density(ext2$beta[,8]), main = "Beta8")
abline(v = 3, col = "red")
plot(density(ext2$beta[,9]), main = "Beta9")
abline(v = 3, col = "red")
plot(density(ext2$beta[,10]), main = "Beta10")
abline(v = 3, col = "red")
#
coeff3
par(mfcol = c(4,5))
plot(density(ext3$beta[,1]), main = "Beta1")
abline(v = 0.01, col = "red")
plot(density(ext3$beta[,2]), main = "Beta2")
abline(v = 0.01, col = "red")
plot(density(ext3$beta[,3]), main = "Beta3")
abline(v = 0.01, col = "red")
plot(density(ext3$beta[,4]), main = "Beta4")
abline(v = 0.01, col = "red")
plot(density(ext3$beta[,5]), main = "Beta5")
abline(v = 0.01, col = "red")
plot(density(ext3$beta[,6]), main = "Beta6")
abline(v = 3, col = "red")
plot(density(ext3$beta[,7]), main = "Beta7")
abline(v = 3, col = "red")
plot(density(ext3$beta[,8]), main = "Beta8")
abline(v = 3, col = "red")
plot(density(ext3$beta[,9]), main = "Beta9")
abline(v = 3, col = "red")
plot(density(ext3$beta[,10]), main = "Beta10")
abline(v = 3, col = "red")
plot(density(ext3$beta[,11]), main = "Beta11")
abline(v = 3, col = "red")
plot(density(ext3$beta[,12]), main = "Beta12")
abline(v = 3, col = "red")
plot(density(ext3$beta[,13]), main = "Beta13")
abline(v = 3, col = "red")
plot(density(ext3$beta[,14]), main = "Beta14")
abline(v = 3, col = "red")
plot(density(ext3$beta[,15]), main = "Beta15")
abline(v = 3, col = "red")
plot(density(ext3$beta[,16]), main = "Beta16")
abline(v = 3, col = "red")
plot(density(ext3$beta[,17]), main = "Beta17")
abline(v = 3, col = "red")
plot(density(ext3$beta[,18]), main = "Beta18")
abline(v = 3, col = "red")
plot(density(ext3$beta[,19]), main = "Beta19")
abline(v = 3, col = "red")
plot(density(ext3$beta[,20]), main = "Beta20")
abline(v = 3, col = "red")
#
coeff4
par(mfcol = c(4,5))
plot(density(ext4$beta[,1]), main = "Beta1")
abline(v = 0.01, col = "red")
plot(density(ext4$beta[,2]), main = "Beta2")
abline(v = 0.01, col = "red")
plot(density(ext4$beta[,3]), main = "Beta3")
abline(v = 0.01, col = "red")
plot(density(ext4$beta[,4]), main = "Beta4")
abline(v = 0.01, col = "red")
plot(density(ext4$beta[,5]), main = "Beta5")
abline(v = 0.01, col = "red")
plot(density(ext4$beta[,6]), main = "Beta6")
abline(v = 0.01, col = "red")
plot(density(ext4$beta[,7]), main = "Beta7")
abline(v = 0.01, col = "red")
plot(density(ext4$beta[,8]), main = "Beta8")
abline(v = 0.01, col = "red")
plot(density(ext4$beta[,9]), main = "Beta9")
abline(v = 0.01, col = "red")
plot(density(ext4$beta[,10]), main = "Beta10")
abline(v = 0.01, col = "red")
plot(density(ext4$beta[,11]), main = "Beta11")
abline(v = 0.01, col = "red")
plot(density(ext4$beta[,12]), main = "Beta12")
abline(v = 0.01, col = "red")
plot(density(ext4$beta[,13]), main = "Beta13")
abline(v = 0.01, col = "red")
plot(density(ext4$beta[,14]), main = "Beta14")
abline(v = 0.01, col = "red")
plot(density(ext4$beta[,15]), main = "Beta15")
abline(v = 0.01, col = "red")
plot(density(ext4$beta[,16]), main = "Beta16")
abline(v = 3, col = "red")
plot(density(ext4$beta[,17]), main = "Beta17")
abline(v = 3, col = "red")
plot(density(ext4$beta[,18]), main = "Beta18")
abline(v = 3, col = "red")
plot(density(ext4$beta[,19]), main = "Beta19")
abline(v = 3, col = "red")
plot(density(ext4$beta[,20]), main = "Beta20")
abline(v = 3, col = "red")



