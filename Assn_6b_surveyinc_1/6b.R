#####
# Question 1
#####
data <- c(1,1,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,0,0)
data <- table(data)
class(data)
data[names(data==0)]

##Switches function
switches <- function(x)
{ switches <- sign(x)
sum(switches[-1] != switches[-length(x)])
}
switches(data)
#Replicating with original protocol
nswitch <- NULL
for ( i in 1:10000){
  theta <- rbeta(10000, 8,14)
  ygen <- rbinom(20, 1, mean(theta))
  nswitch[i] <- switches(ygen)
}
#Simulating y_rep with new protocol
y_rep_gen<-function(){
  theta <- rbeta (10000 ,8 ,14) 
  y_rep_each <- rbinom (1,1,mean(theta)) 
  while (sum(y_rep_each==0) < 13)
    y_rep_each<- c(y_rep_each, rbinom(1,1,mean(theta))) 
  return(y_rep_each)
}
y_rep_all <- NULL
for(i in 1:10000){
  y_rep_all[[i]]<-y_rep_gen() }
y_switch<-c() 
for(i in 1:10000){
  y_switch[i]<-length(rle(y_rep_all[[i]])$values)-1 
}
#Plotting histograms
par(mfcol = c(1,2))
hist(nswitch, main="Original switches", 
     yaxt = "n", ylab=NULL, breaks = 20)
abline(v = 3, col = "red")
hist(y_switch,main="Replicated switches", 
     yaxt = "n", ylab=NULL, breaks = 50)
abline(v = 3, col = "red")

#####
# Question 2
#####
# #Read in data
rm(list = ls())
setwd("/Users/Advait/Desktop/New School/Fall16/BDA/Class11")
incentive <- read.table("http://www.stat.columbia.edu/~gelman/bda.course/incentives_data_clean.txt",
                        skip = 12)
str(incentive)
incentive$id <- c(as.factor(incentive$sid))
id <- incentive$id
unique(sid)
length(unique(sid))
#Initial plots
par(mfcol = c(2,2),  mar = c(4,4,1,1))
plot(incentive$v[incentive$m==-0.5],
     incentive$r[incentive$m==-0.5], pch = 16, cex = 0.5,
     xlab = "Dollar incentive",
     ylab = "Response rate", main = "Mode of survey",
     ylim = c(-0.1,1))
points(incentive$v[incentive$m==0.5],
       incentive$r[incentive$m==0.5], pch = 16, cex = 0.5, col = "red")
legend(80,0.6, legend =  c("Phone", "Face to face"), 
       col = c("black", "red"), pch = 16, bty = "n")
##
plot(incentive$v[incentive$t==-0.5],
     incentive$r[incentive$t==-0.5], pch = 16, cex = 0.5,
     xlab = "Dollar incentive",
     ylab = "Response rate", main = "Timing",
     ylim = c(-0.1,1))
points(incentive$v[incentive$t==0.5],
       incentive$r[incentive$t==0.5], pch = 16, cex = 0.5, col = "red")
legend(80,0.6, legend =  c("After survey", "Before survey"), 
       col = c("black", "red"), pch = 16, bty = "n")
##
plot(incentive$v[incentive$f==-0.5],
     incentive$r[incentive$f==-0.5], pch = 16, cex = 0.5,
     xlab = "Dollar incentive",
     ylab = "Response rate", main = "Form",
     ylim = c(-0.1,1))
points(incentive$v[incentive$f==0.5],
       incentive$r[incentive$f==0.5], pch = 16, cex = 0.5, col = "red")
legend(80,0.6, legend =  c("Gift", "Cash"), 
       col = c("black", "red"), pch = 16, bty = "n")
##
plot(incentive$v[incentive$b==-0.5],
     incentive$r[incentive$b==-0.5], pch = 16, cex = 0.5,
     xlab = "Dollar incentive",
     ylab = "Response rate", main = "Burden",
     ylim = c(-0.1,1))
points(incentive$v[incentive$b==0.5],
       incentive$r[incentive$b==0.5], pch = 16, cex = 0.5, col = "red")
legend(80,0.6, legend =  c("Low burden", "High burden"), 
       col = c("black", "red"), pch = 16, bty = "n")
####################################################################
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
incentiveprime <- incentive[complete.cases(incentive),]
dim(incentiveprime)

rdif <- incentiveprime$r.dif
vdif <- incentiveprime$v.dif
time <- incentiveprime$t
f <- incentiveprime$f
b <- incentiveprime$b
m <- incentiveprime$m
stanc("6b.stan")
fit <- stan("6b.stan", data = list("rdif", "vdif","m", "time", "f", "b"),
            iter = 1000, chains = 3)
print(fit)
ext1 <- extract(fit)
sd(ext1$b1)
###
stanc("6b2.stan")
fit2 <- stan("6b2.stan", data = list("rdif", "vdif","m", "time", "f", "b"),
             iter = 1000, chains = 3)
print(fit2)
ext2 <- extract(fit2)
mean(ext2$b1)
sd(ext2$b1)
mean(ext2$b9)
############################
par(mfcol = c(2,2), mar = c(4,4,1,1))
##Mode
plot(incentiveprime$v.dif[incentiveprime$m==-0.5],
     incentiveprime$r.dif[incentiveprime$m==-0.5], pch = 16, cex = 0.5,
     xlab = "Dollar incentive",
     ylab = "Response rate",
     ylim = c(-0.1,0.2))
points(incentiveprime$v.dif[incentiveprime$m==0.5],
       incentiveprime$r.dif[incentiveprime$m==0.5], pch = 16, cex = 0.5, col = "red")
lines(incentiveprime$v.dif, 0.05 + 0.001*incentiveprime$v.dif)
lines(incentiveprime$v.dif, 0.03 + 0.001*incentiveprime$v.dif, col = "red")
legend(70,0, legend =  c("Phone", "Face to face"), 
       col = c("black", "red"), pch = 16, bty = "n")
##Time
plot(incentiveprime$v.dif[incentiveprime$t==-0.5],
     incentiveprime$r.dif[incentiveprime$t==-0.5], pch = 16, cex = 0.5,
     xlab = "Dollar incentive",
     ylab = "Response rate",
     ylim = c(-0.1,0.2))
points(incentiveprime$v.dif[incentiveprime$t==0.5],
       incentiveprime$r.dif[incentiveprime$t==0.5], pch = 16, cex = 0.5, col = "red")
lines(incentiveprime$v.dif, 0 + 0.001*incentiveprime$v.dif)
lines(incentiveprime$v.dif, 0.03 + 0.001*incentiveprime$v.dif, col = "red")
legend(70,0, legend =  c("After Survey", "Before Survey"), 
       col = c("black", "red"), pch = 16, bty = "n")
##Form
plot(incentiveprime$v.dif[incentiveprime$f==-0.5],
     incentiveprime$r.dif[incentiveprime$f==-0.5], pch = 16, cex = 0.5,
     xlab = "Dollar incentive",
     ylab = "Response rate",
     ylim = c(-0.1,0.2))
points(incentiveprime$v.dif[incentiveprime$f==0.5],
       incentiveprime$r.dif[incentiveprime$f==0.5], pch = 16, cex = 0.5, col = "red")
lines(incentiveprime$v.dif, 0.03 + 0.001*incentiveprime$v.dif)
lines(incentiveprime$v.dif, 0.08 + 0.001*incentiveprime$v.dif, col = "red")
legend(70,0, legend =  c("Gift", "Cash"), 
       col = c("black", "red"), pch = 16, bty = "n")
##Burden
plot(incentiveprime$v.dif[incentiveprime$b==-0.5],
     incentiveprime$r.dif[incentiveprime$b==-0.5], pch = 16, cex = 0.5,
     xlab = "Dollar incentive",
     ylab = "Response rate",
     ylim = c(-0.1,0.2))
points(incentiveprime$v.dif[incentiveprime$b==0.5],
       incentiveprime$r.dif[incentiveprime$b==0.5], pch = 16, cex = 0.5, col = "red")
lines(incentiveprime$v.dif, -0.01 + 0.001*incentiveprime$v.dif)
lines(incentiveprime$v.dif, 0.03 + 0.001*incentiveprime$v.dif, col = "red")
legend(70,0, legend =  c("Low burden", "High burden"), 
       col = c("black", "red"), pch = 16, bty = "n")

#######################












