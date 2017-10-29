##Question 1
rm(list = ls())
setwd("/Users/Advait/Desktop/New School/Fall16/BDA/Class15")
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
##
elec <- read.table("http://www.stat.columbia.edu/~gelman/bda.course/electric.txt", header = T,skip  = 1)
str(elec)
dim(elec)
elec[1:5,]
##
par(mfcol = c(4,2), mar = c(3,1,1,1))
hist(elec$Posttest.1[elec$Grade==1], col = "gray", 
     breaks = 10, xlim = c(0,120), 
     yaxt = "n", ylab = NULL,
     xlab = NULL, main = "Control Grade 1",
     cex.main = 0.8)
legend(10, 5, bty = "n",legend = paste("Mean = ", round(mean(elec$Posttest.1[elec$Grade==1]),2), "\n sd = ", round(sd(elec$Posttest.1[elec$Grade==1]))))

hist(elec$Posttest.1[elec$Grade==2], col = "gray", 
     breaks = 10, xlim = c(0,120), yaxt = "n", ylab = NULL, xlab = NULL, main = "Control Grade 2",
     cex.main = 0.8)
legend(10, 5, bty = "n",legend = paste("Mean = ", round(mean(elec$Posttest.1[elec$Grade==2]),2), "\n sd = ", round(sd(elec$Posttest.1[elec$Grade==2]))))

hist(elec$Posttest.1[elec$Grade==3], col = "gray", breaks = 10, xlim = c(0,120), yaxt = "n", ylab = NULL, xlab = NULL, main = "Control Grade 3",
     cex.main = 0.8)
legend(10, 5, bty = "n",legend = paste("Mean = ", round(mean(elec$Posttest.1[elec$Grade==3]),2), "\n sd = ", round(sd(elec$Posttest.1[elec$Grade==3]))))

hist(elec$Posttest.1[elec$Grade==4], col = "gray", breaks = 10, xlim = c(0,120), yaxt = "n", ylab = NULL, xlab = NULL, main = "Control Grade 4",
     cex.main = 0.8)
legend(10, 5, bty = "n",legend = paste("Mean = ", round(mean(elec$Posttest.1[elec$Grade==4]),2), "\n sd = ", round(sd(elec$Posttest.1[elec$Grade==4]))))

#
hist(elec$Posttest[elec$Grade==1], col = "gray", breaks = 10, xlim = c(0,120), yaxt = "n", ylab = NULL, xlab = NULL, main = "Treat Grade 1",
     cex.main = 0.8)
legend(10, 5, bty = "n",legend = paste("Mean = ", round(mean(elec$Posttest[elec$Grade==1]),2), "\n sd = ", round(sd(elec$Posttest[elec$Grade==1]))))

hist(elec$Posttest[elec$Grade==2], col = "gray", breaks = 10, xlim = c(0,120), yaxt = "n", ylab = NULL, xlab = NULL, main = "Treat Grade 2",
     cex.main = 0.8)
legend(10, 5, bty = "n",legend = paste("Mean = ", round(mean(elec$Posttest[elec$Grade==2]),2), "\n sd = ", round(sd(elec$Posttest[elec$Grade==2]))))

hist(elec$Posttest[elec$Grade==3], col = "gray", breaks = 10, xlim = c(0,120), yaxt = "n", ylab = NULL, xlab = NULL, main = "Treat Grade 3",
     cex.main = 0.8)
legend(10, 5, bty = "n",legend = paste("Mean = ", round(mean(elec$Posttest[elec$Grade==3]),2), "\n sd = ", round(sd(elec$Posttest[elec$Grade==3]))))

hist(elec$Posttest[elec$Grade==4], col = "gray", breaks = 10, xlim = c(0,120), yaxt = "n", ylab = NULL, xlab = NULL, main = "Treat Grade 4",
     cex.main = 0.8)
legend(10, 5, bty = "n",legend = paste("Mean = ", round(mean(elec$Posttest[elec$Grade==4]),2), "\n sd = ", round(sd(elec$Posttest[elec$Grade==4]))))

# ####
# PLOT 2
# ####
g1treat <- elec$Posttest[elec$Grade==1]
g1treat.pre <- elec$Pretest[elec$Grade==1]
g1cont <- elec$Posttest.1[elec$Grade==1]
g1cont.pre <- elec$Pretest.1[elec$Grade==1]
g1post <- c(g1treat,g1cont)
g1pre <- c(g1treat.pre,g1cont.pre)
t1 <- c(rep(1,21),rep(0,21))
city1 <- c(rep("F",11),rep("Y",10),rep("F",11),rep("Y",10))
g1 <- data.frame(g1pre,g1post,t1,city1)
names(g1) <- c("Pre", "Post", "Treat", "City")

#
g2treat <- elec$Posttest[elec$Grade==2]
g2treat.pre <- elec$Pretest[elec$Grade==2]
g2cont <- elec$Posttest.1[elec$Grade==2]
g2cont.pre <- elec$Pretest.1[elec$Grade==2]
length(g2treat)
g2post <- c(g2treat,g2cont)
g2pre <- c(g2treat.pre,g2cont.pre)
t2 <- c(rep(1,34),rep(0,34))
city2 <- c(rep("F",14),rep("Y",20),rep("F",14),rep("Y",20))
g2 <- data.frame(g2pre,g2post,t2,city2)
names(g2) <- c("Pre", "Post", "Treat", "City")

#
g3treat <- elec$Posttest[elec$Grade==3]
g3treat.pre <- elec$Pretest[elec$Grade==3]
g3cont <- elec$Posttest.1[elec$Grade==3]
g3cont.pre <- elec$Pretest.1[elec$Grade==3]
length(g3treat)
g3post <- c(g3treat,g3cont)
g3pre <- c(g3treat.pre,g3cont.pre)
t3 <- c(rep(1,20),rep(0,20))
city3 <- c(rep("F",10),rep("Y",10),rep("F",10),rep("Y",10))
g3 <- data.frame(g3pre,g3post,t3,city3)
names(g3) <- c("Pre", "Post", "Treat", "City")

#
g4treat <- elec$Posttest[elec$Grade==4]
g4treat.pre <- elec$Pretest[elec$Grade==4]
g4cont <- elec$Posttest.1[elec$Grade==4]
g4cont.pre <- elec$Pretest.1[elec$Grade==4]
length(g4treat)
g4post <- c(g4treat,g4cont)
g4pre <- c(g4treat.pre,g4cont.pre)
t4 <- c(rep(1,21),rep(0,21))
city4 <- c(rep("F",11),rep("Y",10),rep("F",11),rep("Y",10))
g4 <- data.frame(g4pre,g4post,t4,city4)
names(g4) <- c("Pre", "Post", "Treat", "City")

##
snoop <- rbind(g1,g2,g3,g4)
snoop <- data.frame(snoop)
str(snoop)
names(snoop) <- c("pre", "post", "treat","city")
snoop$grade <- c(rep(1,42),rep(2,68),rep(3,40), rep(4,42))
###########
par(mfcol = c(1,4),
    mar=c(3, 3, 2, 1), 
    oma=c(.5, .5, .5, .5), 
    mgp=c(2, 1, 0))
for(i in 1:4){
plot(snoop$pre[snoop$grade==i & snoop$treat==1],
     snoop$post[snoop$grade==i & snoop$treat==1], 
     xlim = c(1,120),ylim = c(1,120),
     xlab = "Pre-test score",
     ylab = "Post-test score",
     main = paste("Grade",i), cex = 0.8, pch = 16)
points(snoop$pre[snoop$grade== i & snoop$treat==0],
       snoop$post[snoop$grade== i & snoop$treat==0], 
       xlim = c(1,120),ylim = c(1,120),cex = 0.8)
abline(lm(snoop$post[snoop$grade==i & snoop$treat==1]~snoop$pre[snoop$grade== i & snoop$treat==1]))
abline(lm(snoop$post[snoop$grade==i & snoop$treat==0]~snoop$pre[snoop$grade==i & snoop$treat==0]), lty = 2)
}
#
############################

snoop[1:5,]
str(snoop)

snoop$treat <- ifelse(snoop$treat==1, 0.5,-0.5)
snoop$city <- ifelse(snoop$city=="F", 1,2)
pre <- snoop$pre
post <- snoop$post
treat <- snoop$treat
city <- snoop$city
grade <- snoop$grade
stanc("8b.stan")
fit1 <- stan("8b.stan", 
             data = list("pre", "post", "treat", "city","grade"),
             iter = 1000,
             chains = 3)
print(fit1)
ext1 <- extract(fit1)
str(ext1)
thetas <- matrix(ext1$theta, nrow=1500, ncol=8)
lower <- NULL 
for(i in 1:8){
  lower[i] <- quantile(thetas[,i], 0.025) }
upper <- NULL 
for(i in 1:8){
  upper[i] <- quantile(thetas[,i], 0.975) }
lower


treatment_fx <- colMeans(ext1$theta)

par(mfcol = c(1,2))
plot(c(1,2,3,4), treatment_fx[,1], main = "Treatments within Fresno",
     cex.main = 0.8, xlab = "Grades", ylab = "Treatment coefficient",
     pch = 16, cex = 0.7, ylim = c(2*min(lower), 1.5*max(upper)))
abline(h = 0, lty = 2,col = "gray")
arrows(c(1:4), treatment_fx[,1],c(1:4), upper[1:4], col = "black",length = 0)
arrows(c(1:4), treatment_fx[,1],c(1:4), lower[1:4], col = "black",length = 0)
plot(seq(1,4, by = 1), treatment_fx[,2], main = "Treatments within Youngstown",
     cex.main = 0.8, xlab = "Grades", ylab = "Treatment coefficient",
     pch = 16, cex = 0.7, ylim = c(2*min(lower), 1.5*max(upper)))
abline(h = 0, lty = 2,col = "gray")
arrows(c(1:4), treatment_fx[,2],c(1:4), upper[5:8], col = "black",length = 0)
arrows(c(1:4), treatment_fx[,2],c(1:4), lower[5:8], col = "black",length = 0)
###
#Plot city wise treatment-control relationships
##
#Plot Relationship between pre and post for all grades and cities
par(mfrow = c(2,4),mar = c(3, 3, 1, 1), oma = c(.5, .5, .5, .5), mgp=c(2,1,0))
str(snoop)
for(i in 1:4){
  plot(snoop$pre[snoop$grade == i & snoop$treat==0.5 & snoop$city == 1],
       snoop$post[snoop$grade == i & snoop$treat==0.5 & snoop$city == 1],
       xlab = "Pre-test score",
       ylab = "Post-test score",
       cex = 0.8, pch = 16, main = paste("Fresno Grade",i),
       xlim = c(1,120),
       ylim = c(0,120),
       cex.main = 1)
  points(snoop$pre[snoop$grade == i & snoop$treat== -0.5 & snoop$city == 1],
         snoop$post[snoop$grade == i & snoop$treat== -0.5 & snoop$city == 1],
         cex = 0.8)
  lines(c(0,120), 
        mean(ext1$b0) 
        + colMeans(ext1$beta)[i,1]*c(0,120) 
        + colMeans(ext1$theta)[i,1])
  lines(c(0,120), 
        mean(ext1$b0) 
        + colMeans(ext1$beta)[i,1]*c(0,120) 
        ,lty = 2)
}
for(i in 1:4){
  plot(snoop$pre[snoop$grade == i & snoop$treat==0.5 & snoop$city == 2],
       snoop$post[snoop$grade == i & snoop$treat==0.5 & snoop$city == 2],
       xlab = "Pre-test score",
       ylab = "Post-test score",
       cex = 0.8, pch = 16, main = paste("Youngstown Grade",i),
       xlim = c(1,120),
       ylim = c(0,120),
       cex.main = 1)
  points(snoop$pre[snoop$grade == i & snoop$treat== -0.5 & snoop$city == 2],
         snoop$post[snoop$grade == i & snoop$treat== -0.5 & snoop$city == 2],
         cex = 0.8)
  lines(c(0,120), 
        mean(ext1$b0) 
        + colMeans(ext1$beta)[i,2]*c(0,120) 
        + colMeans(ext1$theta)[i,2])
  lines(c(0,120), 
        mean(ext1$b0) 
        + colMeans(ext1$beta)[i,2]*c(0,120) 
        ,lty = 2)
}

###Part B
str(snoop)
par(mfrow = c(1,2),mar = c(3, 3, 1, 1), oma = c(.5, .5, .5, .5), mgp=c(2,1,0))
plot(c(1:192), snoop$pre ,col = "white",
     main = "Pre test scores in all grades",
     cex.main = 0.9,
     xlab = "Individual Observation",
     ylab = "Pre test score")
points(c(1:42),snoop$pre[snoop$grade==1], col = "black", cex = 0.8, pch = 16)
points(c(43:110),snoop$pre[snoop$grade==2], col = "gray", cex = 0.8, pch = 16)
points(c(111:150),snoop$pre[snoop$grade==3], col = "blue", cex = 0.8, pch = 16)
points(c(151:192), snoop$pre[snoop$grade==4], col = "red", cex = 0.8, pch = 16)
legend(100,30, bty = "n",
       legend = c("Grade 1", "Grade 2", "Grade 3", "Grade 4"),
       col = c("black", "gray", "blue", "red"), pch = 16, cex = 0.8)
##
plot(c(1:192), snoop$pre ,col = "white",
     main = "Post test scores in all grades",
     cex.main = 0.9,
     xlab = "Individual Observation",
     ylab = "Post test score")
points(c(1:42),snoop$post[snoop$grade==1], col = "black", cex = 0.8, pch = 16)
points(c(43:110),snoop$post[snoop$grade==2], col = "gray", cex = 0.8, pch = 16)
points(c(111:150),snoop$post[snoop$grade==3], col = "blue", cex = 0.8, pch = 16)
points(c(151:192), snoop$post[snoop$grade==4], col = "red", cex = 0.8, pch = 16)
legend(100,30, bty = "n",
       legend = c("Grade 1", "Grade 2", "Grade 3", "Grade 4"),
       col = c("black", "gray", "blue", "red"), pch = 16, cex = 0.8)
###

####################################################

par(mfrow = c(2,2),mar = c(3, 3, 1, 1), oma = c(.5, .5, .5, .5), mgp=c(2,1,0))
plot(c(0:91), snoop$pre[snoop$city==1] ,col = "white",
     main = "Pre test Fresno",
     cex.main = 0.9,
     xlab = "Individual Observation",
     ylab = "Pre test score")
points(c(0:21),snoop$pre[snoop$grade==1 & snoop$city==1], col = "black", cex = 0.8, pch = 16)
points(c(22:49),snoop$pre[snoop$grade==2 & snoop$city==1], col = "gray", cex = 0.8, pch = 16)
points(c(50:69),snoop$pre[snoop$grade==3 & snoop$city==1], col = "blue", cex = 0.8, pch = 16)
points(c(69:90), snoop$pre[snoop$grade==4 & snoop$city==1], col = "red", cex = 0.8, pch = 16)
legend(60,50, bty = "n",
       legend = c("Grade 1", "Grade 2", "Grade 3", "Grade 4"),
       col = c("black", "gray", "blue", "red"), pch = 16, cex = 0.7)
##
plot(c(1:100), snoop$pre[snoop$city==2] ,col = "white",
     main = "Pre test Youngstown",
     cex.main = 0.9,
     xlab = "Individual Observation",
     ylab = "Post test score")
points(c(0:19),snoop$pre[snoop$grade==1 & snoop$city==2], col = "black", cex = 0.8, pch = 16)
points(c(20:59),snoop$pre[snoop$grade==2 & snoop$city==2], col = "gray", cex = 0.8, pch = 16)
points(c(60:79),snoop$pre[snoop$grade==3 & snoop$city==2], col = "blue", cex = 0.8, pch = 16)
points(c(80:99), snoop$pre[snoop$grade==4 & snoop$city==2], col = "red", cex = 0.8, pch = 16)
legend(70,50, bty = "n",
       legend = c("Grade 1", "Grade 2", "Grade 3", "Grade 4"),
       col = c("black", "gray", "blue", "red"), pch = 16, cex = 0.7)
######################
plot(c(0:91), snoop$post[snoop$city==1] ,col = "white",
     main = "Post test Fresno",
     cex.main = 0.9,
     xlab = "Individual Observation",
     ylab = "Pre test score")
points(c(0:21),snoop$post[snoop$grade==1 & snoop$city==1], col = "black", cex = 0.8, pch = 16)
points(c(22:49),snoop$post[snoop$grade==2 & snoop$city==1], col = "gray", cex = 0.8, pch = 16)
points(c(50:69),snoop$post[snoop$grade==3 & snoop$city==1], col = "blue", cex = 0.8, pch = 16)
points(c(69:90), snoop$post[snoop$grade==4 & snoop$city==1], col = "red", cex = 0.8, pch = 16)
legend(70,70, bty = "n",
       legend = c("Grade 1", "Grade 2", "Grade 3", "Grade 4"),
       col = c("black", "gray", "blue", "red"), pch = 16, cex = 0.7)
##
plot(c(1:100), snoop$post[snoop$city==2] ,col = "white",
     main = "Post test Youngstown",
     cex.main = 0.9,
     xlab = "Individual Observation",
     ylab = "Post test score")
points(c(0:19),snoop$post[snoop$grade==1 & snoop$city==2], col = "black", cex = 0.8, pch = 16)
points(c(20:59),snoop$post[snoop$grade==2 & snoop$city==2], col = "gray", cex = 0.8, pch = 16)
points(c(60:79),snoop$post[snoop$grade==3 & snoop$city==2], col = "blue", cex = 0.8, pch = 16)
points(c(80:99), snoop$post[snoop$grade==4 & snoop$city==2], col = "red", cex = 0.8, pch = 16)
legend(70,70, bty = "n",
       legend = c("Grade 1", "Grade 2", "Grade 3", "Grade 4"),
       col = c("black", "gray", "blue", "red"), pch = 16, cex = 0.7)




