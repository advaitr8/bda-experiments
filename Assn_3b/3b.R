#Consider the following Poisson regression model:
 # yi ∼ Poisson uieα+βxi , for data i = 1,...,n.
 # Set n = 10, x to the values 1, 2, . . . , 10, u 
 # to the values created by rep(c(0.1,0.5),c(5,5)), and set (α, β) to the true values of 
#(1.0, 0.3). 
#(a)  Simulate data y from the model and make a scatterplot of yi/ui vs. xi. 
#(b)  Now forget the true values of α,β. 
#Write the posterior density for (α,β), assuming a noninformative uniform prior distribution.
#(c)  Using R, compute the posterior mode and the covariance matrix around the mode. 
  #Make a plot showing the mode and an ellipse showing the normal approximation. 
  #Also on this plot indicate the true value of (α,β). 
  #(d)  Fit the model in Stan and then plot random draws from the joint posterior distribution
#of (α,β) by drawing from a grid, adding these dots to the graph above. 
-----------
#rm(list = ls())
setwd("/Users/Advait/Desktop/New School/Fall16/BDA/Class5")
getwd()
#set.seed(102)
#Part A
n <- 10
x <- c(1:10)
u <- rep(c(0.1,0.5),c(5,5))
atrue <- 1
btrue <- 0.3
y <- rpois(10, u*(exp(atrue + btrue*x)))
lm(y~x)
plot(density(y))
plot(x, (y/u), pch = 16, xlab = "xvalues", ylab = "y_i/u_i", main = "Plot y/u vs x")
######################
library(rstan)
stanc("3b.stan")
#stanc("3b_trial.stan")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
fit1 <- stan("3b.stan", data = list("y","x","n","u"), iter = 1000, chains = 3)
print(fit1)
ext1 <- extract(fit1)
atrial <- ext1$alpha
btrial <- ext1$beta
plot(atrial,btrial,pch =20, cex = .4)
#fit2 <- stan("3b_trial.stan", data = list("y","x","n","u"), iter = 1000, chains = 3)
#print(fit2)
######

#posterior <- NULL
#for(i in n){
#for (j in 1:1500){
 #   posterior[j] = y[i]*log(u[i]) + y[i]*atrial[j] + y[i]*btrial[j]*x[i] 
  #                 - u[i]*exp(atrial[j] + btrial[j]*x[i]) - log(factorial(y[i]))
  #}
#}
##approximation
install.packages("LearnBayes",dependencies = T)
library(LearnBayes)
install.packages("mvtnorm", dependencies = T)
library(mvtnorm)

######
logpost <-function(theta){ 
  pf = 0
a<-theta[1]
b<-theta[2]
for (i in (1:length(x))){
  pf2 = (a+b*x[i])
  pf = pf + y[i]*(log(u[i]) + pf2) - u[i]*exp(pf2) - log(factorial(y[i]))
}                                           
pf}
summary <- laplace(logpost,c(1,0.3))
summary$mode
summary$var
##contour
#Creating incremental a and b values based on mode and standard deviation
alphanew <- seq(summary$mode[1] - 3*sqrt((summary$var[1,1])),
                summary$mode[1] + 3*sqrt((summary$var[1,1])),length.out = 500)
betanew <- seq(summary$mode[2] - 3*sqrt((summary$var[2,2])),
               summary$mode[2] + 3*sqrt((summary$var[2,2])),length.out = 500)
postmod <- c(summary$mode)
cov_matrix <- summary$var
zmatrix <- matrix(0,500,500)
length(postmod)
length(cov_matrix)
for (i in 1:500){
  for(j in 1:500){
  zmatrix[i,j] =  dmvnorm(c(alphanew[i], betanew[j]), postmod, cov_matrix)}}



#plot1
contour(alphanew, betanew, zmatrix, nlevels = 8, #xlim = c(-0.013705,2.9557),
        #ylim = c(0.07065,0.43065), 
        xlab = "alpha", ylab = "beta", 
        main = "Contours with normal approximations of alpha and beta")
points(1,0.3, pch = 16, cex = 1, col = "yellow")
points(postmod[1],postmod[2],pch = 16, cex = 1, col = "red" )
legend(2,.35,legend=c("mode","true value"),
col=c("red","yellow"),cex=1,pch=c(16),bty="n")

#Plot2
contour(alphanew, betanew, zmatrix, nlevels = 8, #xlim = c(-0.013705,2.9557),
        #ylim = c(0.07065,0.43065), 
        xlab = "alpha", ylab = "beta", 
        main = "Scatterplot of posterior draws of alpha and beta")
for (i in 1:1500){
  points(atrial[i], btrial[i], cex = .1)
}
points(1,0.3, pch = 16, cex = 1, col = "yellow")
points(postmod[1],postmod[2],pch = 16, cex = 1, col = "red" )
legend(2,.35,legend=c("mode","true value"),
       col=c("red","yellow"),cex=1,pch=c(16),bty="n")
#############################



