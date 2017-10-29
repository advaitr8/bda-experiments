rm(list = ls())
library(foreign)
library(plyr)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
getwd()
setwd("/Users/Advait/Desktop/New School/Fall16/BDA/Class10")
list.files()
df <- read.csv("supercharger.csv")

df <- arrange(df, by = Remoteness)

x <- df$Remoteness
usage <- df$Usage
range(usage)
logit_usage <- NULL
for(i in 1:100){
logit_usage[i] <- log(usage[i]/(1-usage[i]))
}
range(logit_usage)

###
##Plot1
par(mfcol = c(1,2))
plot(x,usage,xlab = "Remoteness",ylab = "Usage", 
     pch = 16, main = "Initial usage vs remoteness")
plot(x, logit_usage, xlab = "Remoteness", ylab = "logit(Usage)", 
     pch = 16, main = "logit transform usage vs remoteness")
abline(lm(logit_usage ~ x), col = "red", lty = 2, lwd = 2)
legend(100,0, legend = "regression", 
       col = "red", lty = 2)
###

stanc("final.stan")
finalfit <- stan("final.stan", data = list("logit_usage", "x"), 
                 iter = 1000,
                 chains = 3)
print(finalfit)

ext <- extract(finalfit)
inter_usage <- colMeans(ext$logit_usage_rep)
usage_pred <- NULL
for (i in 1:100){
usage_pred[i] <- exp(inter_usage[i])/(1+exp(inter_usage[i]))
}
plot(c(1:100),usage_pred)

plot(x,usage, pch = 16, main = "The model fits the data well",
     xlab = "Remoteness", ylab = "Usage")
lines(c(1:200),usage_pred, col = "red", pch = 16, cex = .7, lwd = 2)
legend(100,0.5, legend = "Logistic normal", 
       col = "red", lwd = 2)

bar1 <- NULL
bar2 <- NULL
for (i in 1:100){
  bar1[i] <- quantile(ext$logit_usage_rep[,i],probs = 0.025 )
  bar2[i] <- quantile(ext$logit_usage_rep[,i],probs = 0.975 )
}
for (i in 1:100){
  bar1[i] <- exp(bar1[i])/(1+exp(bar1[i]))
  bar2[i] <- exp(bar2[i])/(1+exp(bar2[i]))
}

moop <- c(1:200)
plot(x,usage, pch = 16, main = "The 95% Interval in gray",
     xlab = "Remoteness", ylab = "Usage",cex = 0.7)
lines(moop,usage_pred, col = "red", pch = 16, cex = .7, lwd = 2)
lines(moop,bar1, col = "blue", pch = 16, cex = .7)
lines(moop,bar2, col = "blue", pch = 16, cex = .7)
polygon(c(moop, rev(moop)), c(bar2, rev(bar1)),
        col = "lightgray", border = NA)
points(x,usage,pch = 16,cex = 0.6)
lines(moop,usage_pred, col = "red", pch = 16, cex = .7, lwd = 2)
lines(moop,bar1, col = "gray30", pch = 16, cex = .7)
lines(moop,bar2, col = "gray30", pch = 16, cex = .7)
####
plot(x,bar1, pch = 16, main = "Usage above 5%",
     xlab = "Remoteness", ylab = "Usage", type ="l")
abline(h = 0.05, col = "red", lty = 2)
abline(v = 95, col = "red", lty = 2)
#####
par(mfcol = c(1,1))
plot(df$Usage,usage_pred, pch = 16, cex = 0.7,
     xlab = "Raw usage",
     ylab = "Predicted Usage",
     main = "Comparing the raw and generated usage values",
     xlim = c(0,1),
     ylim = c(0,1))
abline(0,1, col = "red", lty = 2, lwd = 2)


plot(x, df$Usage, type = "l", xlab = "Remoteness", 
     ylab = "Usage", main = "Simulating Usage")
legend(130,0.6, legend = c("data","predicted","95% interval"),
       col = c("black", "red","gray"),lwd = 1, bty = "n")
lines(x, usage_pred, type = "l", col = "red")
lines(x,bar1, col = "gray", pch = 16, cex = .7)
lines(x,bar2, col = "gray", pch = 16, cex = .7)
######################
par(mfcol = c(1,1))
usage <- sort(df$Usage, decreasing = T)
plot(log(usage/(1-usage)), col = "red",pch = 16, cex = 0.7)
points(inter_usage, pch = 16, cex = 0.7)
usage <- df[order(df$Usage),]

newdata <- mtcars[order(mpg),]
plot(df$Remoteness, df$usage)
