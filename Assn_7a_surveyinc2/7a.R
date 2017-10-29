# #Read in data
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
rm(list = ls())
setwd("/Users/Advait/Desktop/New School/Fall16/BDA/Class12")
inc <- read.table("http://www.stat.columbia.edu/~gelman/bda.course/incentives_data_clean.txt",
                        skip = 12)
str(inc)
inc$id <- c(as.factor(inc$sid))
#Model1 - AG
incentive <- inc$I
mod <- inc$m
burden <- inc$b
y <- inc$r
id <- inc$id
V <- NULL
for (i in 1:101){
  V[i] <- (inc$r[i]*(1-inc$r[i]))/inc$basen[i]
}
V

##fit model 1
stanc("7a.stan")
fit1 <- stan("7a.stan", data = list("y", "incentive", "mod",
                                    "burden","V","id" ), 
             iter = 5000, chains = 3)
print(fit1)
##Model2 - AG
str(inc)
v <- inc$v
time <- inc$t
form <- inc$f
stanc("7a_model2.stan")
fit2 <- stan("7a_model2.stan", data = list("y","incentive","mod","burden","v","time","form","V","id"), iter = 1000, chains=3)
print(fit2, digits_summary = 3)

##Model3 - AG
stanc("7a_model3.stan")
fit3 <- stan("7a_model3.stan", data = list("y","incentive","mod","burden","v","time","form","V","id"), iter = 1000, chains=3)
print(fit3, digits_summary = 3)
ext.3 <- extract(fit3)
##Model4 - AG
stanc("7a_model4.stan")
fit4 <- stan("7a_model4.stan", data = list("y","incentive","mod","burden","v","time","form","V","id"), iter = 5000, chains=3)
print(fit4, digits_summary = 4)
####
#Plot Model 2
par(mfcol = c(1,1))
money <- c(0:120)
plot(money, 0.6995 + 0.0015*money, lty =3, col = "green", type = "l",
     ylim = c(0.4,0.9), main = "Incentive interacting with other variables",
     cex.main = 0.8, ylab = "Response rate", xlab = "Dollar incentive",
     cex.lab = 0.7, lwd = 2)
points(inc$v[inc$I==1], inc$r[inc$I==1], pch = 16, cex = .4, col = "black")

lines(0.6895 + 0.0015*money, lty =3, col = "blue", lwd = 2)#FHBG
lines(0.6465 + 0.0015*money, lty =3, col = "brown", lwd = 2)#FHAG
lines(0.5975 + 0.0015*money, lty =3, col = "darkgray", lwd = 2)#FLAG

lines(0.6405 + 0.0015*money, lty =3, col = "yellow", lwd = 2)#FLBG
lines(0.6075 + 0.0015*money, lty =3, col = "maroon", lwd = 2)#FLAC
lines(0.6695 + 0.0015*money, lty =3, col = "darkgreen", lwd = 2)#THAG
lines(0.6635 + 0.0015*money, lty =3, col = "darkblue", lwd = 2)#TLBG

lines(0.6505 + 0.0015*money, lty =3, col = "gray", lwd = 2)#FLBC
lines(0.6565 + 0.0015*money, lty =3, col = "purple", lwd = 2)#FHAC
lines(0.7125 + 0.0015*money, lty =3, col = "aquamarine3", lwd = 2)#THBG
lines(0.6795 + 0.0015*money, lty =3, col = "blueviolet", lwd = 2)#THAC

lines(0.7225 + 0.0015*money, lty =3, col = "brown1", lwd = 2)#THBC
lines(0.6735 + 0.0015*money, lty =3, col = "darkgoldenrod", lwd = 2)#TLBC
lines(0.6305 + 0.0015*money, lty =3, col = "cornflowerblue", lwd = 2)#TLAC
lines(0.6205 + 0.0015*money, lty =3, col = "chocolate", lwd = 2)#TLAG

legend(100,0.73,legend = c("fhbc", "fhbg","fhag","flag",
                           "flbg", "flac","thag", "tlbg",
                           "flbc", "fhac", "thbg","thac",
                           "thbc", "tlbc", "tlac", "tlag"),
        lty = 3, col = c("green", "blue", "brown", "darkgray",
                        "yellow", "maroon", "darkgreen", "darkblue",
                        "gray", "purple", "aquamarine3", "blueviolet",
                        "brown1", "darkgoldenrod", "cornflowerblue","chocolate"), bty = "n", cex = 1)

# ##################
# Plots model 3
# #####
x <- seq(0, 120, 1)

par(mfrow = c(2, 2), mar = c(2, 2, 2, 2))

#Plot Low Burden telephone surveys
plot(x, mean(ext.3$b0) + 
       mean(ext.3$b1)*1 + 
       mean(ext.3$b3)*(-0.5) +
       mean(ext.3$b2)*(-0.5) +
       mean(ext.3$b4)*(-0.5)*(-0.5) +
       mean(ext.3$b5)*x +
       mean(ext.3$b6)*(0.5) +
       mean(ext.3$b7)*0.5 +
       mean(ext.3$b8)*(-0.5) +
       mean(ext.3$b9)*(-0.5) +
       mean(ext.3$b10)*x*(0.5) +
       mean(ext.3$b11)*x*(-0.5) -
       (mean(ext.3$b0) + 
          mean(ext.3$b1)*0 + 
          mean(ext.3$b3)*(-0.5) +
          mean(ext.3$b2)*(-0.5) +
          mean(ext.3$b4)*(-0.5)*(-0.5)),
     ylab = "Diff in Response Rate", xlab = "Incentive",
     type = "l", col = "red",
     ylim = c(-0.1, 0.5), 
     main = "Low Burden Phone",
     cex.main = 0.8)
abline(h = 0, col = "grey", lty = 2)

lines(x, mean(ext.3$b0) + 
        mean(ext.3$b1)*1 + 
        mean(ext.3$b3)*(-0.5) +
        mean(ext.3$b2)*(-0.5) +
        mean(ext.3$b4)*(-0.5)*(-0.5) +
        mean(ext.3$b5)*x +
        mean(ext.3$b6)*(0.5) +
        mean(ext.3$b7)*(-0.5) +
        mean(ext.3$b8)*(-0.5) +
        mean(ext.3$b9)*(-0.5) +
        mean(ext.3$b10)*x*(0.5) +
        mean(ext.3$b11)*x*(-0.5) -
        (mean(ext.3$b0) + 
           mean(ext.3$b1)*0 + 
           mean(ext.3$b3)*(-0.5) +
           mean(ext.3$b2)*(-0.5) +
           mean(ext.3$b4)*(-0.5)*(-0.5)),
      ylab = "Response Rate", xlab = "Incentive",
      type = "l", col = "red", lty = 2, 
      ylim = c(-0.1, 1))

lines(x, mean(ext.3$b0) + 
        mean(ext.3$b1)*1 + 
        mean(ext.3$b3)*(-0.5) +
        mean(ext.3$b2)*(-0.5) +
        mean(ext.3$b4)*(-0.5)*(-0.5) +
        mean(ext.3$b5)*x +
        mean(ext.3$b6)*(-0.5) +
        mean(ext.3$b7)*(0.5) +
        mean(ext.3$b8)*(-0.5) +
        mean(ext.3$b9)*(-0.5) +
        mean(ext.3$b10)*x*(-0.5) +
        mean(ext.3$b11)*x*(-0.5) -
        (mean(ext.3$b0) + 
           mean(ext.3$b1)*0 + 
           mean(ext.3$b3)*(-0.5) +
           mean(ext.3$b2)*(-0.5) +
           mean(ext.3$b4)*(-0.5)*(-0.5)),
      ylab = "Response Rate", xlab = "Incentive",
      type = "l", col = "blue", 
      ylim = c(0, 1))

lines(x, mean(ext.3$b0) + 
        mean(ext.3$b1)*1 + 
        mean(ext.3$b3)*(-0.5) +
        mean(ext.3$b2)*(-0.5) +
        mean(ext.3$b4)*(-0.5)*(-0.5) +
        mean(ext.3$b5)*x +
        mean(ext.3$b6)*(-0.5) +
        mean(ext.3$b7)*(-0.5) +
        mean(ext.3$b8)*(-0.5) +
        mean(ext.3$b9)*(-0.5) +
        mean(ext.3$b10)*x*(-0.5) +
        mean(ext.3$b11)*x*(-0.5) -
        (mean(ext.3$b0) + 
           mean(ext.3$b1)*0 + 
           mean(ext.3$b3)*(-0.5) +
           mean(ext.3$b2)*(-0.5) +
           mean(ext.3$b4)*(-0.5)*(-0.5)),
      ylab = "Response Rate", xlab = "Incentive",
      type = "l", col = "blue", lty = 2, 
      ylim = c(0, 1))

#Plot Low Burden face to face surveys
plot(x, mean(ext.3$b0) + 
       mean(ext.3$b1)*1 + 
       mean(ext.3$b3)*(-0.5) +
       mean(ext.3$b2)*(0.5) +
       mean(ext.3$b4)*(0.5)*(-0.5) +
       mean(ext.3$b5)*x +
       mean(ext.3$b6)*(0.5) +
       mean(ext.3$b7)*0.5 +
       mean(ext.3$b8)*(0.5) +
       mean(ext.3$b9)*(-0.5) +
       mean(ext.3$b10)*x*(0.5) +
       mean(ext.3$b11)*x*(-0.5)-
       (mean(ext.3$b0) + 
          mean(ext.3$b1)*0 + 
          mean(ext.3$b3)*(-0.5) +
          mean(ext.3$b2)*(0.5) +
          mean(ext.3$b4)*(0.5)*(-0.5)),
     ylab = "Diff in Response Rate", xlab = "Incentive",
     type = "l", col = "red",
     ylim = c(-0.1, 0.5),
     main = "Low Burden Face-to-Face",
     cex.main = 0.8)
abline(h = 0, col = "grey", lty = 2)

lines(x, mean(ext.3$b0) + 
        mean(ext.3$b1)*1 + 
        mean(ext.3$b3)*(-0.5) +
        mean(ext.3$b2)*(0.5) +
        mean(ext.3$b4)*(0.5)*(-0.5) +
        mean(ext.3$b5)*x +
        mean(ext.3$b6)*(0.5) +
        mean(ext.3$b7)*(-0.5) +
        mean(ext.3$b8)*(0.5) +
        mean(ext.3$b9)*(-0.5) +
        mean(ext.3$b10)*x*(0.5) +
        mean(ext.3$b11)*x*(-0.5) -
        (mean(ext.3$b0) + 
           mean(ext.3$b1)*0 + 
           mean(ext.3$b3)*(-0.5) +
           mean(ext.3$b2)*(0.5) +
           mean(ext.3$b4)*(0.5)*(-0.5)),
      ylab = "Response Rate", xlab = "Incentive",
      type = "l", col = "red", lty = 2, 
      ylim = c(-0.1, 1))

lines(x, mean(ext.3$b0) + 
        mean(ext.3$b1)*1 + 
        mean(ext.3$b3)*(-0.5) +
        mean(ext.3$b2)*(0.5) +
        mean(ext.3$b4)*(0.5)*(-0.5) +
        mean(ext.3$b5)*x +
        mean(ext.3$b6)*(-0.5) +
        mean(ext.3$b7)*(0.5) +
        mean(ext.3$b8)*(0.5) +
        mean(ext.3$b9)*(-0.5) +
        mean(ext.3$b10)*x*(-0.5) +
        mean(ext.3$b11)*x*(-0.5) -
        (mean(ext.3$b0) + 
           mean(ext.3$b1)*0 + 
           mean(ext.3$b3)*(-0.5) +
           mean(ext.3$b2)*(0.5) +
           mean(ext.3$b4)*(0.5)*(-0.5)),
      ylab = "Response Rate", xlab = "Incentive",
      type = "l", col = "blue", 
      ylim = c(0, 1))

lines(x, mean(ext.3$b0) + 
        mean(ext.3$b1)*1 + 
        mean(ext.3$b3)*(-0.5) +
        mean(ext.3$b2)*(0.5) +
        mean(ext.3$b4)*(0.5)*(-0.5) +
        mean(ext.3$b5)*x +
        mean(ext.3$b6)*(-0.5) +
        mean(ext.3$b7)*(-0.5) +
        mean(ext.3$b8)*(0.5) +
        mean(ext.3$b9)*(-0.5) +
        mean(ext.3$b10)*x*(-0.5) +
        mean(ext.3$b11)*x*(-0.5) -
        (mean(ext.3$b0) + 
           mean(ext.3$b1)*0 + 
           mean(ext.3$b3)*(-0.5) +
           mean(ext.3$b2)*(0.5) +
           mean(ext.3$b4)*(0.5)*(-0.5)),
      ylab = "Response Rate", xlab = "Incentive",
      type = "l", col = "blue", lty = 2, 
      ylim = c(0, 1))


#Plot High Burden telephone surveys
plot(x, mean(ext.3$b0) + 
       mean(ext.3$b1)*1 + 
       mean(ext.3$b3)*(0.5) +
       mean(ext.3$b2)*(0.5) +
       mean(ext.3$b4)*(0.5)*(0.5) +
       mean(ext.3$b5)*x +
       mean(ext.3$b6)*(0.5) +
       mean(ext.3$b7)*0.5 +
       mean(ext.3$b8)*(0.5) +
       mean(ext.3$b9)*(0.5) +
       mean(ext.3$b10)*x*(0.5) +
       mean(ext.3$b11)*x*(0.5) -
       (mean(ext.3$b0) + 
          mean(ext.3$b1)*0 + 
          mean(ext.3$b3)*(0.5) +
          mean(ext.3$b2)*(0.5) +
          mean(ext.3$b4)*(0.5)*(0.5)),
     ylab = "Diff in Response Rate", xlab = "Incentive",
     type = "l", col = "red",
     ylim = c(-0.1, 0.5),
     main = "High Burden Phone",
     cex.main = 0.8)
abline(h = 0, col = "grey", lty = 2)

lines(x, mean(ext.3$b0) + 
        mean(ext.3$b1)*1 + 
        mean(ext.3$b3)*(0.5) +
        mean(ext.3$b2)*(0.5) +
        mean(ext.3$b4)*(0.5)*(0.5) +
        mean(ext.3$b5)*x +
        mean(ext.3$b6)*(0.5) +
        mean(ext.3$b7)*(-0.5) +
        mean(ext.3$b8)*(0.5) +
        mean(ext.3$b9)*(0.5) +
        mean(ext.3$b10)*x*(0.5) +
        mean(ext.3$b11)*x*(0.5) -
        (mean(ext.3$b0) + 
           mean(ext.3$b1)*0 + 
           mean(ext.3$b3)*(0.5) +
           mean(ext.3$b2)*(0.5) +
           mean(ext.3$b4)*(0.5)*(0.5)),
      ylab = "Response Rate", xlab = "Incentive",
      type = "l", col = "red", lty = 2, 
      ylim = c(-0.1, 1))

lines(x, mean(ext.3$b0) + 
        mean(ext.3$b1)*1 + 
        mean(ext.3$b3)*(0.5) +
        mean(ext.3$b2)*(0.5) +
        mean(ext.3$b4)*(0.5)*(0.5) +
        mean(ext.3$b5)*x +
        mean(ext.3$b6)*(-0.5) +
        mean(ext.3$b7)*(0.5) +
        mean(ext.3$b8)*(0.5) +
        mean(ext.3$b9)*(0.5) +
        mean(ext.3$b10)*x*(-0.5) +
        mean(ext.3$b11)*x*(0.5) -
        (mean(ext.3$b0) + 
           mean(ext.3$b1)*0 + 
           mean(ext.3$b3)*(0.5) +
           mean(ext.3$b2)*(0.5) +
           mean(ext.3$b4)*(0.5)*(0.5)),
      ylab = "Response Rate", xlab = "Incentive",
      type = "l", col = "blue", 
      ylim = c(0, 1))

lines(x, mean(ext.3$b0) + 
        mean(ext.3$b1)*1 + 
        mean(ext.3$b3)*(0.5) +
        mean(ext.3$b2)*(0.5) +
        mean(ext.3$b4)*(0.5)*(0.5) +
        mean(ext.3$b5)*x +
        mean(ext.3$b6)*(-0.5) +
        mean(ext.3$b7)*(-0.5) +
        mean(ext.3$b8)*(0.5) +
        mean(ext.3$b9)*(0.5) +
        mean(ext.3$b10)*x*(-0.5) +
        mean(ext.3$b11)*x*(0.5) -
        (mean(ext.3$b0) + 
           mean(ext.3$b1)*0 + 
           mean(ext.3$b3)*(0.5) +
           mean(ext.3$b2)*(0.5) +
           mean(ext.3$b4)*(0.5)*(0.5)),
      ylab = "Response Rate", xlab = "Incentive",
      type = "l", col = "blue", lty = 2, 
      ylim = c(0, 1))

#Plot High Burden face to face surveys
plot(x, mean(ext.3$b0) + 
       mean(ext.3$b1)*1 + 
       mean(ext.3$b3)*(0.5) +
       mean(ext.3$b2)*(-0.5) +
       mean(ext.3$b4)*(-0.5)*(0.5) +
       mean(ext.3$b5)*x +
       mean(ext.3$b6)*(0.5) +
       mean(ext.3$b7)*0.5 +
       mean(ext.3$b8)*(-0.5) +
       mean(ext.3$b9)*(0.5) +
       mean(ext.3$b10)*x*(0.5) +
       mean(ext.3$b11)*x*(0.5)-
       (mean(ext.3$b0) + 
          mean(ext.3$b1)*0 + 
          mean(ext.3$b3)*(0.5) +
          mean(ext.3$b2)*(-0.5) +
          mean(ext.3$b4)*(-0.5)*(0.5)),
     ylab = "Diff in Response Rate", xlab = "Incentive",
     type = "l", col = "red",
     ylim = c(-0.1, 0.5),
     main = "High Burden Face-to-Face",
     cex.main = 0.8)
abline(h = 0, col = "grey", lty = 2)

lines(x, mean(ext.3$b0) + 
        mean(ext.3$b1)*1 + 
        mean(ext.3$b3)*(0.5) +
        mean(ext.3$b2)*(-0.5) +
        mean(ext.3$b4)*(-0.5)*(0.5) +
        mean(ext.3$b5)*x +
        mean(ext.3$b6)*(0.5) +
        mean(ext.3$b7)*(-0.5) +
        mean(ext.3$b8)*(-0.5) +
        mean(ext.3$b9)*(0.5) +
        mean(ext.3$b10)*x*(0.5) +
        mean(ext.3$b11)*x*(0.5) -
        (mean(ext.3$b0) + 
           mean(ext.3$b1)*0 + 
           mean(ext.3$b3)*(0.5) +
           mean(ext.3$b2)*(-0.5) +
           mean(ext.3$b4)*(-0.5)*(0.5)),
      ylab = "Response Rate", xlab = "Incentive",
      type = "l", col = "red", lty = 2, 
      ylim = c(-0.1, 1))

lines(x, mean(ext.3$b0) + 
        mean(ext.3$b1)*1 + 
        mean(ext.3$b3)*(0.5) +
        mean(ext.3$b2)*(-0.5) +
        mean(ext.3$b4)*(-0.5)*(0.5) +
        mean(ext.3$b5)*x +
        mean(ext.3$b6)*(-0.5) +
        mean(ext.3$b7)*(0.5) +
        mean(ext.3$b8)*(-0.5) +
        mean(ext.3$b9)*(0.5) +
        mean(ext.3$b10)*x*(-0.5) +
        mean(ext.3$b11)*x*(0.5) -
        (mean(ext.3$b0) + 
           mean(ext.3$b1)*0 + 
           mean(ext.3$b3)*(0.5) +
           mean(ext.3$b2)*(-0.5) +
           mean(ext.3$b4)*(-0.5)*(0.5)),
      ylab = "Response Rate", xlab = "Incentive",
      type = "l", col = "blue", 
      ylim = c(0, 1))

lines(x, mean(ext.3$b0) + 
        mean(ext.3$b1)*1 + 
        mean(ext.3$b3)*(0.5) +
        mean(ext.3$b2)*(-0.5) +
        mean(ext.3$b4)*(-0.5)*(0.5) +
        mean(ext.3$b5)*x +
        mean(ext.3$b6)*(-0.5) +
        mean(ext.3$b7)*(-0.5) +
        mean(ext.3$b8)*(-0.5) +
        mean(ext.3$b9)*(0.5) +
        mean(ext.3$b10)*x*(-0.5) +
        mean(ext.3$b11)*x*(0.5) -
        (mean(ext.3$b0) + 
           mean(ext.3$b1)*0 + 
           mean(ext.3$b3)*(0.5) +
           mean(ext.3$b2)*(-0.5) +
           mean(ext.3$b4)*(-0.5)*(0.5)),
      ylab = "Response Rate", xlab = "Incentive",
      type = "l", col = "blue", lty = 2,
      ylim = c(0, 1))









