setwd("/Users/Advait/Desktop/New School/Fall16/BDA/Class17")
x <- rnorm(10^5,0,1)
plot(density(x))
y <- x[x>-1.96]
length(y)/length(x)
z <- x[x>1.96]
length(z)/length(x)

x <- rnorm(10^5, 0,1)
plot(density(x))
y <- 0.5*x
lines(density(y), col = "red")

?pnorm
dnorm(1.96,0,1)
#######
f.alpha <-dnorm(qnorm(0.025,0,1), 0, 1) 
n <-(0.025*0.975/f.alpha^2)/0.01 
sqrt(0.025*0.975/(714*f.alpha^2))
###2-b
set.seed(8)
theta.gen1 <- replicate(1000,rnorm(714)) 
rep_fun1 <-function(x){
  abs(qnorm(0.025) - quantile(x, c(0.025))) 
}
rep1 <- apply(theta.gen1,2,rep_fun1)
dim(theta.gen1)
mean(rep1)
hist(rep1 )
abline(v = mean(rep1))
#
set.seed(8)
theta.gen2 <- replicate(1000,rnorm(714)) 
rep_fun2 <-function(x){
  abs(qnorm(0.975) - quantile(x, c(0.975))) 
}
rep2 <- apply(theta.gen2,2,rep_fun2)
mean(rep2)
hist(rep2 )
abline(v = mean(rep2))

par(mfcol = c(1,2))
hist(rep1, col = "gray", breaks  = 20, main = "Absvalue (alpha_hat - alpha) for 2.5%", cex.main = 0.8, xlab = "Distance" )
abline(v = mean(rep1), col = "red")
#
hist(rep2, col = "gray", breaks  = 20, main = "Absvalue (alpha_hat - alpha) for 97.5%", cex.main = 0.8 , xlab = "Distance")
abline(v = mean(rep2), col = "red")

#
hist(theta.gen1, xlim = c(-2.3,-1.8))
abline(v=qnorm(0.025))
abline(v = quantile(theta.gen1, c(0.025)), col = "red")
       
