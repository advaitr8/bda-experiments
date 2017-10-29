#rm(list = ls())
setwd("/Users/Advait/Desktop/New School/Fall16/BDA/Class6")
####
#part A
theta <- seq(0,1,0.001)
n <- length(theta)
y <- c(-2,-1,0,1.5,2.5)
b <- 1
for(i in 1:5){ 
  #b <- (1/(1 + (y[j] - theta)^2))
  q <- b*(1/(1 + (y[i] - theta)^2))
  b <- q
}
q
posterior_density_norm <- (q/(0.001*sum(q)))
par(mfcol = c(1,1))
plot(theta,posterior_density_norm ,
     main = "Normalized density of theta",
     ylim = c(0,1.3*max(posterior_density_norm)),
     lwd = "3",
     type = "l")

#########
#part B
#log posterior first derivative
dlog_dtheta <- function(x) {
  sum(2*(y - x)/(1 + (y - x)^2))
}
#log posterior second derivative
d2log_dtheta2 <- function(x){
  sum(2*(-1 + (y - x)^2)/(1 + (y - x)^2)^2)
}
#part C
theta.guess <- seq(-1.0,1,0.001)
#Plot derivatives to see where 0 is
d1 = rep(0,length(theta.guess))
for (i in 1:length(theta.guess)){
  d1[i] = dlog_dtheta(theta.guess[i])  
}    
length(theta.guess)
plot(theta.guess, d1,
     main = "Plot of first derivative and possible mode",
     xlab = "theta.guess",
     ylab = "first derivatives",
     type = "l",
     lwd = 3)
abline(h=0,col = "red",lty = 2)

#Find the mode by iteration
abs_min <- abs(dlog_dtheta(theta[1]))
location <- 1
for (i in 2:length(theta.guess)){
  if(abs_min > abs(dlog_dtheta(theta.guess[i])))
  {abs_min <- abs(dlog_dtheta(theta.guess[i]))
   location <- i
  }
}
theta.guess[863]
print(theta.guess[location])


par(mfcol = c(1,2))
plot(theta.guess, d1,
     main = "Plot of first derivative and possible mode",
     xlab = "theta.guess",
     ylab = "first derivatives",
     type = "l",
     lwd = 3)
abline(h=0,col = "red",lty = 2)
plot(theta.guess, d1,
     main = "True mode shown by blue line",
     xlab = "theta.guess",
     ylab = "first derivatives",
     type = "l",
     lwd = 3)
abline(h=0,col = "red",lty = 2)
abline(v=-0.138, col = "blue", lty = 2)

#part D
stdev <- sqrt(1/-d2log_dtheta2(-0.138))
norm_density <- dnorm(theta, 0 , stdev)
normalize <- norm_density/(0.001*sum(norm_density))
par(mfcol = c(1,1))
plot(theta,posterior_density_norm ,
     main = "Normal approximation is a good fit",
     ylim = c(0,1.3*max(posterior_density_norm)),
     lwd = "3",
     type = "l")
lines(theta, normalize, col = "red",lwd = 2)
legend(0.7, 1.2, legend = c("True density", "Normal Approx"), col = c("black", "red"),
       bty = "n", cex = 1, lty = c(1, 1), lwd = c(2, 2))








#########################################
###Alternate Approach with function###
########################################

#lik <- NULL
#lik <- function(theta){
# (1/(1 + (y[1] - theta)^2))*
#(1/(1 + (y[2] - theta)^2))*
#  (1/(1 + (y[3] - theta)^2))*
# (1/(1 + (y[4] - theta)^2))*
#  (1/(1 + (y[5] - theta)^2))
#}
pdawg <- function(theta){
  
}
posterior_density_norm <- lik(theta)/(0.001*sum(lik(theta)))
plot(theta,posterior_density_norm, 
     main = "Normalized posterior density vs Theta",
     ylim = c(0,1.5*max(posterior_density_norm)),
     lwd = "3",
     type = "l")
######
dlog_dtheta(theta)
