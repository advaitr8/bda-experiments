getwd()
setwd("/Users/Advait/Desktop/New School/Fall16/BDA/Week3")

df <- read.table("data_chickens", header=FALSE)
str(df)
colnames(df) <- c("frequency","N1","ratio_sham","se_sham","N2","ratio_exp","se_exp")
str(df)
plot(df$frequency,df$ratio_sham)
abline(lm (df$ratio_sham ~ df$frequency))
plot(df$frequency, df$ratio_exp)
abline(lm (df$ratio_exp ~ df$frequency))
summary(lm (df$ratio_exp ~ df$frequency))
mean(df$ratio_sham)

#Plotting the sham values for frequencies
plot(df$frequency, df$ratio_sham,
     ylim = range(c(df$ratio_sham - 2.5*df$se_sham, df$ratio_sham + 2.5*df$se_sham)),
     pch = 16, col = "black", xlab = "Frequency", ylab = "Ratio of Sham to Control",
     main = "Ratio of sham treatments to control vs. Frequency")
arrows(df$frequency, df$ratio_sham - 2*df$se_sham, df$frequency, df$ratio_sham
       + 2*df$se_sham, length=0, angle=90, code=2, col = "black")
abline(h=1, lty = 2, col = "green", lwd = 1.5)

#Plotting the treatment values for frequencies
plot(df$frequency, df$ratio_exp,
     ylim = range(c(df$ratio_exp - 2.5*df$se_exp, df$ratio_exp + 2.5*df$se_exp)),
     pch = 16, col = "black", xlab = "Frequency", ylab = "Ratio of Treatments to Control",
     main = "Ratio of treatments to control vs. Frequency")
arrows(df$frequency, df$ratio_exp - 2*df$se_exp, df$frequency, df$ratio_exp
       + 2*df$se_exp, length=0, angle=90, code=2, col = "black")
abline(h=1, lty = 2, col = "green", lwd = 1.5)
####
#Fit a multilevel model for ratio of treatments to control vs frequency
N <- length(df$frequency)
y <- df$ratio_exp
tau <- 0.01
sigma <- df$se_exp
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
stanc("hier_chickens.stan")
fit1 <- stan("hier_chickens.stan", data = list("N", "y", "sigma"), iter = 1000, chains = 3)
print(fit1)
ext1 <- extract(fit1)
thetaval <- colMeans(ext1$theta)
points(df$frequency, thetaval, col = "blue")
#####
#Partial pooling with tau = 0.07
fit2 <- stan("hier_chickens.stan", data = list("N", "y", "sigma","tau"), iter = 1000, chains = 3)
print(fit2)
ext2 <- extract(fit2)
thetaval2 <- colMeans(ext2$theta)


##Plotting raw estimates and multilevel estimates
lowerint <- sapply(as.data.frame(fit2), FUN = quantile, probs = 0.025)
upperint <- sapply(as.data.frame(fit2), FUN = quantile, probs = 0.975)

par(mfcol = c(1,2))
plot(df$frequency, df$ratio_exp,
     ylim = range(c(df$ratio_exp - 2.5*df$se_exp, df$ratio_exp + 2.5*df$se_exp)),
     pch = 16, col = "black", xlab = "Frequency", ylab = "Ratio of Treatments to Control",
     main = "Ratio of treatments to control vs. Frequency")
arrows(df$frequency, df$ratio_exp - 2*df$se_exp, df$frequency, df$ratio_exp
       + 2*df$se_exp, length=0, angle=90, code=2, col = "black")
abline(h=1, lty = 2, col = "green", lwd = 1.5)

plot(df$frequency, thetaval2,
     ylim = range(c(thetaval2 - 2.5*tau, thetaval2 + 2.5*tau)),
     pch = 16, col = "black", xlab = "Frequency", ylab = "Multilevel estimates of theta",
     main = "Multilevel estimates of theta vs Frequency")
for (i in 1:38){
arrows(df$frequency[i], lowerint[i], df$frequency[i], upperint[i], length=0, angle=90, code=2, col = "black")
}
abline(h=1, lty = 2, col = "green", lwd = 1.5)

##Plot for correlation between A1/A2 and B1/B2
plot(df$ratio_sham,df$ratio_exp, xlab = "B1/B2", ylab = "A1/A2", 
     main = "Scatter plot for the A1/A2 and B1/B2 over all frequencies",
     pch = 16)
abline (lm(df$ratio_exp ~ df$ratio_sham), col = "green")



