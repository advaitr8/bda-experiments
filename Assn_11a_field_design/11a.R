rm(list = ls())
setwd("/Users/Advait/Desktop/New School/Fall16/BDA/Class20")
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
install.packages("dplyr")
library(dplyr)
############
# Consider a model for the crop yields data in Table 8.4 with a mean level, row effects, column effects, and treatment effects. Assign a hierarchical model with mean 0 and unknown variance to each of the three batches of effects.
# 
# (a)  Write the model in statistical notation, specifying all aspects of the model unambiguously. (Write the model directly, not as a regression.
#                                                                       (b)  Write the joint posterior density (up to an arbitrary multiplicative constant).
#                                                                       (c)  Fit the model in Stan.
#                                                                       (d)  Display the inferences from the model.
#                                                                       (e)  Make a graph showing the data and fitted model.                                                                               
############
y <- c(257, 245,182,203,231,
       230,283,252,204,271,
       279,245,280,227,266,
       287,280,246,193,334,
       202,260,250,259,338)

treat <- c(2,4,5,1,3,
           5,1,2,3,4,
           1,5,3,4,2,
           3,2,4,5,1,
           4,3,1,2,5)

row_level <- c(rep(1,5),rep(2,5),rep(3,5),rep(4,5),rep(5,5))
col_level <- (rep(seq(1:5),5))

df <- cbind(y,treat,row_level,col_level)   
df <- as.data.frame(df)
class(df)  
ylog <- log(y)
stanc("11a_trial.stan")
fit <- stan("11a_trial.stan", 
            data = list("ylog", "treat", "row_level", "col_level"),
            iter = 1000, chains = 3)
print(fit)        
ext <- extract(fit)

y_rep <- ext$y_rep_log
str(y_rep)
class(y_rep)

upper <- NULL
lower <- NULL
for(i in 1:25){
  upper[i] = quantile(y_rep[,i], probs = 0.75)
  lower[i] = quantile(y_rep[,i], probs = 0.25)
}
par(mfrow = c(1, 2),
    mar=c(3, 3, 2, 1),
    oma = c(0.5, 0.5, 0.5, 0.5), mgp=c(2, 1, 0))

plot(c(1:25), ylog, pch = 16, cex = 0.7, ylab = "Predicted yield",
     xlab = "Index", cex.lab = 0.8,
     main = "Predicted and Original yield",
     cex.main = 0.8,ylim = c(5,6))
points(c(1:25),colMeans(ext$y_rep_log), pch = 16, col = "darkgray", cex = 0.7 )
legend(4,5.9, legend = c("Pred", "Orig"),
       col = c("gray", "black"),
       bty = "n",
       cex = 0.8, pch = 16)

plot(ylog, colMeans(ext$y_rep_log),
     pch = 16,
     ylim = c(5, 6),
     xlim = c(5, 6),
     cex = 0.7, ylab = "Predicted yield",
     xlab = "Original yield", cex.lab = 0.8, 
     main = "Predicted vs. Original yield", cex.main = 0.8)
abline(0, 1, lty = 2)
arrows(ylog, colMeans(ext$y_rep_log), ylog, upper, col = "gray",length = 0 )
arrows(ylog, colMeans(ext$y_rep_log), ylog, lower, col = "gray",length = 0 )
points(ylog, colMeans(ext$y_rep_log),
       pch = 16, cex = 0.7)
legend(5, 5.9, legend = c("Data", "50% Interval"),
       col = c("black" , "darkgrey") , pch=c(16, 3),
       lty = c(0, 1),
       bty = 'n',
       cex = 0.8)
######
##Part D
par(mfrow = c(3, 5),
    mar=c(3, 3, 2, 1),
    oma = c(0.5, 0.5, 0.5, 0.5), mgp=c(2, 1, 0))
plot(density(ext$bt[,1]), main = "Treatment 1",
     xlab = "A")
abline(v = mean(ext$bt[,1]), col = "red")
#
plot(density(ext$bt[,2]), main = "Treatment 2",
     xlab = "B")
abline(v = mean(ext$bt[,2]), col = "red")
#
plot(density(ext$bt[,3]), main = "Treatment 3",
     xlab = "C")
abline(v = mean(ext$bt[,3]), col = "red")
#
plot(density(ext$bt[,4]), main = "Treatment 4",
     xlab = "D")
abline(v = mean(ext$bt[,4]), col = "red")
#
plot(density(ext$bt[,5]), main = "Treatment 5",
     xlab = "E")
abline(v = mean(ext$bt[,5]), col = "red")
#######
plot(density(ext$br[,1]), main = "Row 1", xlab = "Row effect")
abline(v = mean(ext$br[,1]), col = "red")
#
plot(density(ext$br[,2]), main = "Row 2", xlab = "Row effect")
abline(v = mean(ext$br[,2]), col = "red")
#
plot(density(ext$br[,3]), main = "Row 3", xlab = "Row effect")
abline(v = mean(ext$br[,3]), col = "red")
#
plot(density(ext$br[,4]), main = "Row 4", xlab = "Row effect")
abline(v = mean(ext$br[,4]), col = "red")
#
plot(density(ext$br[,5]), main = "Row 5", xlab = "Row effect")
abline(v = mean(ext$br[,5]), col = "red")
######
plot(density(ext$bc[,1]), main = "Col 1", xlab = "Col effect")
abline(v = mean(ext$bc[,1]), col = "red")
#
plot(density(ext$bc[,2]), main = "Col 2", xlab = "Col effect")
abline(v = mean(ext$bc[,2]), col = "red")
#
plot(density(ext$bc[,3]), main = "Col 3", xlab = "Col effect")
abline(v = mean(ext$bc[,3]), col = "red")
#
plot(density(ext$bc[,4]), main = "Col 4", xlab = "Col effect")
abline(v = mean(ext$bc[,4]), col = "red")
#
plot(density(ext$bc[,5]), main = "Col 5", xlab = "Col effect")
abline(v = mean(ext$bc[,5]), col = "red")














