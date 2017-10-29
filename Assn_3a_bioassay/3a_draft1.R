setwd("/Users/Advait/Desktop/New School/Fall16/BDA/Class4")
N <- 4
n <- 5
y <- c(0,1,3,5)
x  <- c(-0.86, -0.30, -0.05, 0.73)
table <- cbind(y,x)
table <- as.data.frame(table)
str(table)
y <- table$y
x <- table$x
##Bayesian estimation
#With a uniform prior on b1 and b2
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
stanc("3a.stan")
fit1 <- stan("3a.stan", data = list("N", "y", "x"),iter = 1000, chains = 3)
print(fit1)
ext1 <- extract(fit1)
b1 <- ext1$b1
b2 <- ext1$b2
theta11 <- ext1$theta[,1]
theta12 <- ext1$theta[,2]
theta13 <- ext1$theta[,3]
theta14 <- ext1$theta[,4]
####
#With a multivariate normal prior on b1 and b2
sigma <- matrix(data =c(4,0.5*sqrt(4)*sqrt(100),0.5*sqrt(4)*sqrt(100),100),nrow=2,ncol=2)
ab <- c(0,10)
fit2 <- stan("3a_2.stan", data = list("N", "y", "x","sigma","ab"),iter = 1000, chains = 3)
print(fit2)
ext2 <- extract(fit2)
b1new <- ext2$b[,1]
b2new <- ext2$b[,2]
theta21 <- ext2$theta[,1]
theta22 <- ext2$theta[,2]
theta23 <- ext2$theta[,3]
theta24 <- ext2$theta[,4]

####
#Scatter plots
par(mfcol = c(1,2))
plot(b1,b2,pch = 16, main = "Flat prior estimates of b1 and b2", 
     xlim = c(-4,10),ylim = c(-10 ,40) , cex = .5 )
abline(v = mean(b1), lty = 2,col = "red")
abline(h = mean(b2), lty = 2,col = "red")
plot(b1new, b2new, pch  = 16, main = "MVN prior for b1 and b2",
     xlim = c(-4,10),ylim = c(-10 ,40) , cex = .5)
abline(v = mean(b1new), lty = 2 , col = "red")
abline(h = mean(b2new), lty = 2,col = "red")

#Plot densities of b1 and b2
par(mfcol = c(2,2))
plot(density(b1), main = "Density of b1 flat prior")
abline(v = mean(b1), col = "red")


plot(density(b2), main = "Density of b2 flat prior")
abline(v = mean(b2), col = "red")

plot(density(b1new), main = "Density of b1 MVN prior")
abline(v = mean(b1new), col = "red")

plot(density(b2new), main = "Density of b2 MVN prior")
abline(v = mean(b2new), col = "red")
#
##Plot flat prior theta densities
par(mfcol = c(2,4))
plot(density(theta11), main = "Density of theta1, flat prior")
abline(v = mean(theta11), col = "red")
plot(density(theta21), main = "Density of theta1, mvn prior")
abline(v = mean(theta21), col = "red")
plot(density(theta12), main = "Density of theta2, flat prior")
abline(v = mean(theta12), col = "red")
plot(density(theta22), main = "Density of theta2, mvn prior")
abline(v = mean(theta22), col = "red")
plot(density(theta13), main = "Density of theta3, flat prior")
abline(v = mean(theta13), col = "red")
plot(density(theta23), main = "Density of theta3, mvn prior")
abline(v = mean(theta23), col = "red")
plot(density(theta14), main = "Density of theta4, flat prior")
abline(v = mean(theta14), col = "red")
plot(density(theta24), main = "Density of theta4, mvn prior")
abline(v = mean(theta24), col = "red")

print(fit2)
par(mfcol=c(1,1))

ld50 <- -(b1new/b2new)
hist(ld50,breaks=65,xlab = "LD50",
     main = NULL, yaxt = "n", ylab = NULL)


