rm(list = ls())
setwd("/Users/Advait/Desktop/New School/Fall16/BDA/Class13")
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

par(mfrow = c(5, 6), mar = c(1, 1, 1, 1))
for(i in 1:30){
  plot(inf.state.time$Time[inf.state.time$State.no. == i], 
       inf.state.time$exp[inf.state.time$State.no == i],
       type = "l",
       ylim = c(0, 11), 
       yaxt = "n", xaxt = "n",
       main = paste("state", i), cex.main = 0.9)
}

#Plot IMR by state over time
par(mfrow = c(5, 6), mar = c(1, 1, 1, 1))
for(i in 1:30){
  plot(inf.state.time$Time[inf.state.time$State.no. == i], 
       inf.state.time$inf[inf.state.time$State.no == i],
       type = "l",
       ylim = c(0, 100), 
       yaxt = "n", xaxt = "n",
       main = paste("state", i), cex.main = 0.9)
}

#Plot IMR and health exp by state
inf.state <- ddply(ind, .(State.no.), summarise,
                   inf = mean(IMR),
                   exp = mean(state_exp),
                   se.imr = sqrt(sd(IMR)^2/length(State.no.)),
                   se.exp = sqrt(sd(state_exp)^2/length(State.no.)),
                   lit = mean(lit_rate_I),
                   se.lit = sqrt(sd(lit_rate_I)^2/length(State.no.)))

inf.state <- inf.state[order(inf.state$inf),]
inf.state$State.no. <- c(30:1)

par(mfrow = c(1, 3), mar = c(4, 3.9, 3.8, 2), cex = 0.9)
plot(inf.state$State.no., inf.state$inf,
     pch = 16, cex = 0.6, 
     ylab = "Deaths per Thousand Births",
     xlab = "State", 
     main = "Infant Mortality Rate",
     ylim = c(0, 75),
     cex.main = 0.8)
arrows(inf.state$State.no., inf.state$inf + 2*inf.state$se.imr,
       inf.state$State.no., inf.state$inf - 2*inf.state$se.imr,
       length = 0)
abline(h = mean(ind$IMR),  lty = 2, col = "darkgrey")

plot(inf.state$State.no., inf.state$exp,
     pch = 16, cex = 0.6, 
     ylab = "Budget Exp on Health (%)",
     xlab = "State", 
     main = "Public Health Expenditure", 
     ylim = c(0, 10),
     cex.main = 0.8)
arrows(inf.state$State.no., inf.state$exp + 2*inf.state$se.exp,
       inf.state$State.no., inf.state$exp - 2*inf.state$se.exp,
       length = 0)
abline(h = mean(ind$state_exp),  lty = 2, col = "darkgrey")

#Plot Literacy Rate by State
#par(mfrow = c(1, 1), mar = c(4, 4, 3, 2))
plot(inf.state$State.no., inf.state$lit,
     pch = 16, cex = 0.6, 
     ylab = "Literacy Race",
     xlab = "State", 
     main = "Wide Variation in Literacy Rate", type = "l",
     cex.main = 0.8)
points(inf.state$State.no., inf.state$lit, pch = 16)
arrows(inf.state$State.no., inf.state$lit + 2*inf.state$se.lit,
       inf.state$State.no., inf.state$lit - 2*inf.state$se.lit,
       length = 0)
abline(h = mean(ind$lit_rate_I),  lty = 2, col = "darkgrey")

# #Overall IMR and Public Exp Over Time
# all.time <- ddply(ind, .(Time), summarise, 
#                   imr = mean(IMR),
#                   exp = mean(state_exp))
# 
# par(mfrow = c(1, 2), mar = c(4, 3.9, 3.8, 2), cex = 0.9)
# plot(all.time$Time, all.time$imr,
#      type = "l", cex = 0.6, 
#      ylab = "Deaths per Thousand Births",
#      xlab = "Year", 
#      main = "Infant Mortality Rate",
#      ylim = c(30, 50))
# 
# plot(all.time$Time, all.time$exp,
#      type = "l", cex = 0.6, 
#      ylab = "Budget Exp on Health (%)",
#      xlab = "Year", 
#      main = "Public Health Expenditure",
#      ylim = c(3, 5))

################
# Simulate a fake-data scenario of a linear regression with n data points and D predictors, of which D − p are close to zero and p are far from zero. Then fit the horseshoe model and compare your inferences to the true parameter values.
# 
# Do this for a range of values of n, D, and p. 
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
beta <- matrix(c(b0,b1),nrow = (lengthb0 + lengthb1) , ncol = 1 )
y <<- (snoopx) %*% beta + noise
coeff <<- beta
return(beta)
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
coeff <- as.vector(coeff)
stanc("7b.stan")
## MLE Estimate
fit1_mle <- stan("7b.stan", data = list("X", "y", "nr", "nc"), 
            iter = 1000, chains = 3)
print(fit1_mle)
ext1_mle <- extract(fit1_mle)
## Horseshoe prior
fit1_h <- stan("7b.stan", data = list("X", "y", "nr", "nc"), 
                 iter = 1000, chains = 3)
print(fit1_h)
ext1_h <- extract(fit1_h)
## Horseshoe prior - scale tau
fit1_h2 <- stan("7b.stan", data = list("X", "y", "nr", "nc"), 
               iter = 1000, chains = 3)
print(fit1_h2)
ext1_h2 <- extract(fit1_h2)
## Horseshoe tau0
fit1_h3 <- stan("7b2.stan", data = list("X", "y", "nr", "nc"), 
                iter = 1000, chains = 3)
print(fit1_h3)
ext1_h3 <- extract(fit1_h3)
##Plot1
par(mfcol = c(2,3), mar = c(2,2,2,2))
#
plot(density(ext1_mle$beta[,1]), main = "Beta_1", ylim = c(0,6))
lines(density(ext1_h$beta[,1]), col = "red")
lines(density(ext1_h2$beta[,1]), col = "blue")
lines(density(ext1_h3$beta[,1]), col = "green")
abline(v = coeff[1], lty = 2 )

plot(density(ext1_mle$beta[,2]), main = "Beta_2", ylim = c(0,6))
lines(density(ext1_h$beta[,2]), col = "red")
lines(density(ext1_h2$beta[,2]), col = "blue")
lines(density(ext1_h3$beta[,2]), col = "green")
abline(v = coeff[2], lty = 2 )

plot(density(ext1_mle$beta[,3]), main = "Beta_3", ylim = c(0,6))
lines(density(ext1_h$beta[,3]), col = "red")
lines(density(ext1_h2$beta[,3]), col = "blue")
lines(density(ext1_h3$beta[,3]), col = "green")
abline(v = coeff[3], lty = 2 )

plot(density(ext1_mle$beta[,4]), main = "Beta_4", ylim = c(0,6))
lines(density(ext1_h$beta[,4]), col = "red")
lines(density(ext1_h2$beta[,4]), col = "blue")
lines(density(ext1_h3$beta[,4]), col = "green")
abline(v = coeff[4], lty = 2 )

plot(density(ext1_mle$beta[,5]), main = "Beta_5", ylim = c(0,6))
lines(density(ext1_h$beta[,5]), col = "red")
lines(density(ext1_h2$beta[,5]), col = "blue")
lines(density(ext1_h3$beta[,5]), col = "green")
abline(v = coeff[5], lty = 2 )

plot(density(ext1_mle$beta[,6]), main = "Beta_6", ylim = c(0,6))
lines(density(ext1_h$beta[,6]), col = "red")
lines(density(ext1_h2$beta[,6]), col = "blue")
lines(density(ext1_h3$beta[,6]), col = "green")
abline(v = coeff[6], lty = 2 )


     
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
coeff <- as.vector(coeff)
stanc("7b.stan")
## MLE Estimate
fit2_mle <- stan("7b.stan", data = list("X", "y", "nr", "nc"), 
                 iter = 1000, chains = 3)
print(fit2_mle)
ext2_mle <- extract(fit2_mle)
## Horseshoe prior
fit2_h <- stan("7b.stan", data = list("X", "y", "nr", "nc"), 
               iter = 1000, chains = 3)
print(fit2_h)
ext2_h <- extract(fit2_h)
## Horseshoe prior - scale tau
fit2_h2 <- stan("7b.stan", data = list("X", "y", "nr", "nc"), 
                iter = 1000, chains = 3)
print(fit2_h2)
ext2_h2 <- extract(fit2_h2)
## Horseshoe tau0
fit2_h3 <- stan("7b2.stan", data = list("X", "y", "nr", "nc"), 
                iter = 1000, chains = 3)
print(fit2_h3)
ext2_h3 <- extract(fit2_h3)
##Plot2
par(mfcol = c(2,5), mar = c(2,2,2,2))
#
plot(density(ext2_mle$beta[,1]), main = "Beta_1", ylim = c(0,4))
lines(density(ext2_h$beta[,1]), col = "red")
lines(density(ext2_h2$beta[,1]), col = "blue")
lines(density(ext2_h3$beta[,1]), col = "green")
abline(v = coeff[1], lty = 2 )

plot(density(ext2_mle$beta[,2]), main = "Beta_2", ylim = c(0,4))
lines(density(ext2_h$beta[,2]), col = "red")
lines(density(ext2_h2$beta[,2]), col = "blue")
lines(density(ext2_h3$beta[,2]), col = "green")
abline(v = coeff[2], lty = 2 )

plot(density(ext2_mle$beta[,3]), main = "Beta_3", ylim = c(0,4))
lines(density(ext2_h$beta[,3]), col = "red")
lines(density(ext2_h2$beta[,3]), col = "blue")
lines(density(ext2_h3$beta[,3]), col = "green")
abline(v = coeff[3], lty = 2 )

plot(density(ext2_mle$beta[,4]), main = "Beta_4", ylim = c(0,4))
lines(density(ext2_h$beta[,4]), col = "red")
lines(density(ext2_h2$beta[,4]), col = "blue")
lines(density(ext2_h3$beta[,4]), col = "green")
abline(v = coeff[4], lty = 2 )

plot(density(ext2_mle$beta[,5]), main = "Beta_5", ylim = c(0,4))
lines(density(ext2_h$beta[,5]), col = "red")
lines(density(ext2_h2$beta[,5]), col = "blue")
lines(density(ext2_h3$beta[,5]), col = "green")
abline(v = coeff[5], lty = 2 )

plot(density(ext2_mle$beta[,6]), main = "Beta_6", ylim = c(0,4))
lines(density(ext2_h$beta[,6]), col = "red")
lines(density(ext2_h2$beta[,6]), col = "blue")
lines(density(ext2_h3$beta[,6]), col = "green")
abline(v = coeff[6], lty = 2 )

plot(density(ext2_mle$beta[,7]), main = "Beta_7", ylim = c(0,4))
lines(density(ext2_h$beta[,7]), col = "red")
lines(density(ext2_h2$beta[,7]), col = "blue")
lines(density(ext2_h3$beta[,7]), col = "green")
abline(v = coeff[7], lty = 2 )

plot(density(ext2_mle$beta[,8]), main = "Beta_8", ylim = c(0,4))
lines(density(ext2_h$beta[,8]), col = "red")
lines(density(ext2_h2$beta[,8]), col = "blue")
lines(density(ext2_h3$beta[,8]), col = "green")
abline(v = coeff[8], lty = 2 )

plot(density(ext2_mle$beta[,9]), main = "Beta_9", ylim = c(0,4))
lines(density(ext2_h$beta[,9]), col = "red")
lines(density(ext2_h2$beta[,9]), col = "blue")
lines(density(ext2_h3$beta[,9]), col = "green")
abline(v = coeff[9], lty = 2 )

plot(density(ext2_mle$beta[,10]), main = "Beta_10", ylim = c(0,4))
lines(density(ext2_h$beta[,10]), col = "red")
lines(density(ext2_h2$beta[,10]), col = "blue")
lines(density(ext2_h3$beta[,10]), col = "green")
abline(v = coeff[10], lty = 2 )



# ###TRY 3
# # n = 200
# # numberx => D = 6
# # far from 0 => p = 3
# # close to 0 => D-p = 3
# 
# random_gen(3,3,200)
# nc <- 6
# nr <- 200
# X <- snoopx
# y <- as.vector(y)
# coeff <- as.vector(random_gen(3,3,200))
# stanc("7b.stan")
# ## MLE Estimate
# fit3_mle <- stan("7b.stan", data = list("X", "y", "nr", "nc"), 
#                  iter = 1000, chains = 3)
# print(fit3_mle)
# ext3_mle <- extract(fit3_mle)
# ## Horseshoe prior
# fit3_h <- stan("7b.stan", data = list("X", "y", "nr", "nc"), 
#                iter = 1000, chains = 3)
# print(fit3_h)
# ext3_h <- extract(fit3_h)
# ## Horseshoe prior - scale tau
# fit3_h2 <- stan("7b.stan", data = list("X", "y", "nr", "nc"), 
#                 iter = 1000, chains = 3)
# print(fit3_h2)
# ext3_h2 <- extract(fit3_h2)
# 
# ###TRY 4
# # n = 200
# # numberx => D = 10
# # far from 0 => p = 4
# # close to 0 => D-p = 6
# 
# random_gen(6,4,200)
# nc <- 10
# nr <- 200
# X <- snoopx
# y <- as.vector(y)
# coeff <- as.vector(random_gen(3,3,200))
# stanc("7b.stan")
# ## MLE Estimate
# fit4_mle <- stan("7b.stan", data = list("X", "y", "nr", "nc"), 
#                  iter = 1000, chains = 3)
# print(fit4_mle)
# ext4_mle <- extract(fit4_mle)
# ## Horseshoe prior
# fit4_h <- stan("7b.stan", data = list("X", "y", "nr", "nc"), 
#                iter = 1000, chains = 3)
# print(fit4_h)
# ext4_h <- extract(fit4_h)
# ## Horseshoe prior - scale tau
# fit4_h2 <- stan("7b.stan", data = list("X", "y", "nr", "nc"), 
#                 iter = 1000, chains = 3)
# print(fit4_h2)
# ext4_h2 <- extract(fit4_h2)

##TRY 5
# n = 500
# numberx => D = 20
# far from 0 => p = 5
# close to 0 => D-p = 15
# 
# random_gen(15,5,500)
# nc <- 20
# nr <- 500
# X <- snoopx
# y <- as.vector(y)
# coeff <- as.vector(random_gen(3,3,200))
# stanc("7b.stan")
# ## MLE Estimate
# fit5_mle <- stan("7b.stan", data = list("X", "y", "nr", "nc"),
#                  iter = 1000, chains = 3)
# print(fit5_mle)
# ext5_mle <- extract(fit5_mle)
# ## Horseshoe prior
# fit5_h <- stan("7b.stan", data = list("X", "y", "nr", "nc"),
#                iter = 1000, chains = 3)
# print(fit5_h)
# ext5_h <- extract(fit5_h)
# ## Horseshoe prior - scale tau
# fit5_h2 <- stan("7b.stan", data = list("X", "y", "nr", "nc"),
#                 iter = 1000, chains = 3)
# print(fit5_h2)
# ext5_h2 <- extract(fit5_h2)

###TRY 6
# n = 500
# numberx => D = 20
# far from 0 => p = 15
# close to 0 => D-p = 5

random_gen(5,15,500)
nc <- 20
nr <- 500
X <- snoopx
y <- as.vector(y)
coeff <- as.vector(coeff)
stanc("7b.stan")
## MLE Estimate
fit6_mle <- stan("7b.stan", data = list("X", "y", "nr", "nc"), 
                 iter = 1000, chains = 3)
print(fit6_mle)
ext6_mle <- extract(fit6_mle)
## Horseshoe prior
fit6_h <- stan("7b.stan", data = list("X", "y", "nr", "nc"), 
               iter = 1000, chains = 3)
print(fit6_h)
ext6_h <- extract(fit6_h)
## Horseshoe prior - scale tau
fit6_h2 <- stan("7b.stan", data = list("X", "y", "nr", "nc"), 
                iter = 1000, chains = 3)
print(fit6_h2)
ext6_h2 <- extract(fit6_h2)
# Horseshoe tau0
fit6_h3 <- stan("7b2.stan", data = list("X", "y", "nr", "nc"), 
                iter = 1000, chains = 3)
print(fit6_h3)
ext6_h3 <- extract(fit6_h3)
##
par(mfcol = c(4,5), mar = c(2,2,2,2))

plot(density(ext6_mle$beta[,1]), main = "Beta_1", ylim = c(0,10))
lines(density(ext6_h$beta[,1]), col = "red")
lines(density(ext6_h2$beta[,1]), col = "blue")
lines(density(ext6_h3$beta[,1]), col = "green")
abline(v = coeff[1], lty = 2 )

plot(density(ext6_mle$beta[,2]), main = "Beta_2", ylim = c(0,10))
lines(density(ext6_h$beta[,2]), col = "red")
lines(density(ext6_h2$beta[,2]), col = "blue")
lines(density(ext6_h3$beta[,2]), col = "green")
abline(v = coeff[2], lty = 2 )

plot(density(ext6_mle$beta[,3]), main = "Beta_3", ylim = c(0,10))
lines(density(ext6_h$beta[,3]), col = "red")
lines(density(ext6_h2$beta[,3]), col = "blue")
lines(density(ext6_h3$beta[,3]), col = "green")
abline(v = coeff[3], lty = 2 )

plot(density(ext6_mle$beta[,4]), main = "Beta_4", ylim = c(0,10))
lines(density(ext6_h$beta[,4]), col = "red")
lines(density(ext6_h2$beta[,4]), col = "blue")
lines(density(ext6_h3$beta[,4]), col = "green")
abline(v = coeff[4], lty = 2 )

plot(density(ext6_mle$beta[,5]), main = "Beta_5", ylim = c(0,10))
lines(density(ext6_h$beta[,5]), col = "red")
lines(density(ext6_h2$beta[,5]), col = "blue")
lines(density(ext6_h3$beta[,5]), col = "green")
abline(v = coeff[5], lty = 2 )

plot(density(ext6_mle$beta[,6]), main = "Beta_6", ylim = c(0,10))
lines(density(ext6_h$beta[,6]), col = "red")
lines(density(ext6_h2$beta[,6]), col = "blue")
lines(density(ext6_h3$beta[,6]), col = "green")
abline(v = coeff[6], lty = 2 )

plot(density(ext6_mle$beta[,7]), main = "Beta_7", ylim = c(0,10))
lines(density(ext6_h$beta[,7]), col = "red")
lines(density(ext6_h2$beta[,7]), col = "blue")
lines(density(ext6_h3$beta[,7]), col = "green")
abline(v = coeff[7], lty = 2 )

plot(density(ext6_mle$beta[,8]), main = "Beta_8", ylim = c(0,10))
lines(density(ext6_h$beta[,8]), col = "red")
lines(density(ext6_h2$beta[,8]), col = "blue")
lines(density(ext6_h3$beta[,8]), col = "green")
abline(v = coeff[8], lty = 2 )

plot(density(ext6_mle$beta[,9]), main = "Beta_9", ylim = c(0,10))
lines(density(ext6_h$beta[,9]), col = "red")
lines(density(ext6_h2$beta[,9]), col = "blue")
lines(density(ext6_h3$beta[,9]), col = "green")
abline(v = coeff[9], lty = 2 )

plot(density(ext6_mle$beta[,10]), main = "Beta_10", ylim = c(0,10))
lines(density(ext6_h$beta[,10]), col = "red")
lines(density(ext6_h2$beta[,10]), col = "blue")
lines(density(ext6_h3$beta[,10]), col = "green")
abline(v = coeff[10], lty = 2 )

plot(density(ext6_mle$beta[,11]), main = "Beta_11", ylim = c(0,10))
lines(density(ext6_h$beta[,11]), col = "red")
lines(density(ext6_h2$beta[,11]), col = "blue")
lines(density(ext6_h3$beta[,11]), col = "green")
abline(v = coeff[11], lty = 2 )

plot(density(ext6_mle$beta[,12]), main = "Beta_12", ylim = c(0,10))
lines(density(ext6_h$beta[,12]), col = "red")
lines(density(ext6_h2$beta[,12]), col = "blue")
lines(density(ext6_h3$beta[,12]), col = "green")
abline(v = coeff[12], lty = 2 )

plot(density(ext6_mle$beta[,13]), main = "Beta_13", ylim = c(0,10))
lines(density(ext6_h$beta[,13]), col = "red")
lines(density(ext6_h2$beta[,13]), col = "blue")
lines(density(ext6_h3$beta[,13]), col = "green")
abline(v = coeff[13], lty = 2 )

plot(density(ext6_mle$beta[,14]), main = "Beta_14", ylim = c(0,10))
lines(density(ext6_h$beta[,14]), col = "red")
lines(density(ext6_h2$beta[,14]), col = "blue")
lines(density(ext6_h3$beta[,14]), col = "green")
abline(v = coeff[14], lty = 2 )

plot(density(ext6_mle$beta[,15]), main = "Beta_15", ylim = c(0,10))
lines(density(ext6_h$beta[,15]), col = "red")
lines(density(ext6_h2$beta[,15]), col = "blue")
lines(density(ext6_h3$beta[,15]), col = "green")
abline(v = coeff[15], lty = 2 )

plot(density(ext6_mle$beta[,16]), main = "Beta_16", ylim = c(0,10))
lines(density(ext6_h$beta[,16]), col = "red")
lines(density(ext6_h2$beta[,16]), col = "blue")
lines(density(ext6_h3$beta[,16]), col = "green")
abline(v = coeff[16], lty = 2 )

plot(density(ext6_mle$beta[,17]), main = "Beta_17", ylim = c(0,10))
lines(density(ext6_h$beta[,17]), col = "red")
lines(density(ext6_h2$beta[,17]), col = "blue")
lines(density(ext6_h3$beta[,17]), col = "green")
abline(v = coeff[17], lty = 2 )

plot(density(ext6_mle$beta[,18]), main = "Beta_18", ylim = c(0,10))
lines(density(ext6_h$beta[,18]), col = "red")
lines(density(ext6_h2$beta[,18]), col = "blue")
lines(density(ext6_h3$beta[,18]), col = "green")
abline(v = coeff[18], lty = 2 )

plot(density(ext6_mle$beta[,19]), main = "Beta_19", ylim = c(0,10))
lines(density(ext6_h$beta[,19]), col = "red")
lines(density(ext6_h2$beta[,19]), col = "blue")
lines(density(ext6_h3$beta[,19]), col = "green")
abline(v = coeff[19], lty = 2 )

plot(density(ext6_mle$beta[,20]), main = "Beta_20", ylim = c(0,10))
lines(density(ext6_h$beta[,20]), col = "red")
lines(density(ext6_h2$beta[,20]), col = "blue")
lines(density(ext6_h3$beta[,20]), col = "green")
abline(v = coeff[20], lty = 2 )


