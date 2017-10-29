##Question 1
rm(list = ls())
setwd("/Users/Advait/Desktop/New School/Fall16/BDA/Class16")
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library(plyr)
##
elec <- read.table("http://www.stat.columbia.edu/~gelman/bda.course/electric.txt", header = T,skip  = 1)
str(elec)
# ##
# CREATING THE SNOOP
# ##
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

snoop <- rbind(g1,g2,g3,g4)
snoop <- data.frame(snoop)
str(snoop)
names(snoop) <- c("pre", "post", "treat","city")
snoop$grade <- c(rep(1,42),rep(2,68),rep(3,40), rep(4,42))
str(snoop)
snoop$supp <- elec$Supplement.
snoop$supp <- ifelse(snoop$supp=="S",1,0)
str(snoop)
plot(c(1:192),snoop$pre, col = "black")

snoop2 <- snoop

snoop2 <- snoop2[order(snoop2$city,snoop2$grade,snoop$supp),]
snoop2$sno <- c(1:192)
snoop2$city <- ifelse(snoop2$city=="F",1,2 )
##
par(mfrow = c(2,2),
    mar = c(3, 3, 1, 1), 
    oma = c(.5, .5, .5, .5), 
    mgp=c(2,1,0))
plot(snoop2$pre, 
     xlab = "Indexes",
     ylab = "Pre test score",
     main = "Treated students in Fresno", cex.main = 0.8,
     col = "white", xlim = c(1,95), ylim = c(0,120))
# legend(75,30, bty = "n",
#        legend = c("G1 Rep", "G2 Rep", "G3 Rep", "G4 Rep"),
#        col = c("black", "gray", "blue", "red"),pch = 1 , cex = 0.8)
# legend(60,30, bty = "n",
#        legend = c("G1 Sup", "G2 Sup", "G3 Sup", "G4 Sup"),
#        col = c("black", "gray", "blue", "red"), pch = 16, cex = 0.8)
points(snoop2$sno[snoop2$city==1 & snoop2$grade==1 & snoop2$supp==0 & snoop2$treat==1],
       snoop2$pre[snoop2$city==1 & snoop2$grade==1 & snoop2$supp==0 & snoop2$treat==1],cex = 0.8)
points(snoop2$sno[snoop2$city==1 & snoop2$grade==1 & snoop2$supp==1 & snoop2$treat==1],
       snoop2$pre[snoop2$city==1 & snoop2$grade==1 & snoop2$supp==1 & snoop2$treat==1],pch = 16, cex = 0.8)

points(snoop2$sno[snoop2$city==1 & snoop2$grade==2 & snoop2$supp==0 & snoop2$treat==1],
       snoop2$pre[snoop2$city==1 & snoop2$grade==2 & snoop2$supp==0 & snoop2$treat==1],cex = 0.8, col = "gray")
points(snoop2$sno[snoop2$city==1 & snoop2$grade==2 & snoop2$supp==1 & snoop2$treat==1],
       snoop2$pre[snoop2$city==1 & snoop2$grade==2 & snoop2$supp==1 & snoop2$treat==1],pch = 16, cex = 0.8,
       col = "gray")

points(snoop2$sno[snoop2$city==1 & snoop2$grade==3 & snoop2$supp==0 & snoop2$treat==1],
       snoop2$pre[snoop2$city==1 & snoop2$grade==3 & snoop2$supp==0 & snoop2$treat==1],cex = 0.8, col = "blue")
points(snoop2$sno[snoop2$city==1 & snoop2$grade==3 & snoop2$supp==1 & snoop2$treat==1],
       snoop2$pre[snoop2$city==1 & snoop2$grade==3 & snoop2$supp==1 & snoop2$treat==1],pch = 16, cex = 0.8,
       col = "blue")

points(snoop2$sno[snoop2$city==1 & snoop2$grade==4 & snoop2$supp==0 & snoop2$treat==1],
       snoop2$pre[snoop2$city==1 & snoop2$grade==4 & snoop2$supp==0 & snoop2$treat==1],cex = 0.8, col = "red")
points(snoop2$sno[snoop2$city==1 & snoop2$grade==4 & snoop2$supp==1 & snoop2$treat==1],
       snoop2$pre[snoop2$city==1 & snoop2$grade==4 & snoop2$supp==1 & snoop2$treat==1],pch = 16, cex = 0.8,
       col = "red")

plot(snoop2$pre, 
     xlab = "Indexes",
     ylab = "Pre test score",
     main = "Treated students in Youngstown", cex.main = 0.8,
     col = "white", xlim = c(93,192),ylim = c(0,120))
# legend(170,30, bty = "n",
#        legend = c("G1 Rep", "G2 Rep", "G3 Rep", "G4 Rep"),
#        col = c("black", "gray", "blue", "red"),pch = 1 , cex = 0.7)
# legend(155,30, bty = "n",
#        legend = c("G1 Sup", "G2 Sup", "G3 Sup", "G4 Sup"),
#        col = c("black", "gray", "blue", "red"), pch = 16, cex = 0.7)
points(snoop2$sno[snoop2$city==2 & snoop2$grade==1 & snoop2$supp==0 & snoop2$treat==1],
       snoop2$pre[snoop2$city==2 & snoop2$grade==1 & snoop2$supp==0 & snoop2$treat==1],cex = 0.8)
points(snoop2$sno[snoop2$city==2 & snoop2$grade==1 & snoop2$supp==1 & snoop2$treat==1],
       snoop2$pre[snoop2$city==2 & snoop2$grade==1 & snoop2$supp==1 & snoop2$treat==1],pch = 16, cex = 0.8)

points(snoop2$sno[snoop2$city==2 & snoop2$grade==2 & snoop2$supp==0 & snoop2$treat==1],
       snoop2$pre[snoop2$city==2 & snoop2$grade==2 & snoop2$supp==0 & snoop2$treat==1],cex = 0.8, col = "gray")
points(snoop2$sno[snoop2$city==2 & snoop2$grade==2 & snoop2$supp==1 & snoop2$treat==1],
       snoop2$pre[snoop2$city==2 & snoop2$grade==2 & snoop2$supp==1 & snoop2$treat==1],pch = 16, cex = 0.8,
       col = "gray")

points(snoop2$sno[snoop2$city==2 & snoop2$grade==3 & snoop2$supp==0 & snoop2$treat==1],
       snoop2$pre[snoop2$city==2 & snoop2$grade==3 & snoop2$supp==0 & snoop2$treat==1],cex = 0.8, col = "blue")
points(snoop2$sno[snoop2$city==2 & snoop2$grade==3 & snoop2$supp==1 & snoop2$treat==1],
       snoop2$pre[snoop2$city==2 & snoop2$grade==3 & snoop2$supp==1 & snoop2$treat==1],pch = 16, cex = 0.8,
       col = "blue")

points(snoop2$sno[snoop2$city==2 & snoop2$grade==4 & snoop2$supp==0 & snoop2$treat==1],
       snoop2$pre[snoop2$city==2 & snoop2$grade==4 & snoop2$supp==0 & snoop2$treat==1],cex = 0.8, col = "red")
points(snoop2$sno[snoop2$city==2 & snoop2$grade==4 & snoop2$supp==1 & snoop2$treat==1],
       snoop2$pre[snoop2$city==2 & snoop2$grade==4 & snoop2$supp==1 & snoop2$treat==1],pch = 16, cex = 0.8,
       col = "red")

#################################

# par(mfrow = c(1,2),
#     mar = c(3, 3, 1, 1), 
#     oma = c(.5, .5, .5, .5), 
#     mgp=c(2,1,0))
plot(snoop2$post, 
     xlab = "Indexes",
     ylab = "Post test score",
     main = "Post scores in Fresno", cex.main = 0.8,
     col = "white", xlim = c(1,95),ylim = c(0,120))
# legend(75,30, bty = "n",
#        legend = c("G1 Rep", "G2 Rep", "G3 Rep", "G4 Rep"),
#        col = c("black", "gray", "blue", "red"),pch = 1 , cex = 0.8)
# legend(60,30, bty = "n",
#        legend = c("G1 Sup", "G2 Sup", "G3 Sup", "G4 Sup"),
#        col = c("black", "gray", "blue", "red"), pch = 16, cex = 0.8)
points(snoop2$sno[snoop2$city==1 & snoop2$grade==1 & snoop2$supp==0 & snoop2$treat==1],
       snoop2$post[snoop2$city==1 & snoop2$grade==1 & snoop2$supp==0 & snoop2$treat==1],cex = 0.8)
points(snoop2$sno[snoop2$city==1 & snoop2$grade==1 & snoop2$supp==1 & snoop2$treat==1],
       snoop2$post[snoop2$city==1 & snoop2$grade==1 & snoop2$supp==1 & snoop2$treat==1],pch = 16, cex = 0.8)

points(snoop2$sno[snoop2$city==1 & snoop2$grade==2 & snoop2$supp==0 & snoop2$treat==1],
       snoop2$post[snoop2$city==1 & snoop2$grade==2 & snoop2$supp==0 & snoop2$treat==1],cex = 0.8, col = "gray")
points(snoop2$sno[snoop2$city==1 & snoop2$grade==2 & snoop2$supp==1 & snoop2$treat==1],
       snoop2$post[snoop2$city==1 & snoop2$grade==2 & snoop2$supp==1 & snoop2$treat==1],pch = 16, cex = 0.8,
       col = "gray")

points(snoop2$sno[snoop2$city==1 & snoop2$grade==3 & snoop2$supp==0 & snoop2$treat==1],
       snoop2$post[snoop2$city==1 & snoop2$grade==3 & snoop2$supp==0 & snoop2$treat==1],cex = 0.8, col = "blue")
points(snoop2$sno[snoop2$city==1 & snoop2$grade==3 & snoop2$supp==1 & snoop2$treat==1],
       snoop2$post[snoop2$city==1 & snoop2$grade==3 & snoop2$supp==1 & snoop2$treat==1],pch = 16, cex = 0.8,
       col = "blue")

points(snoop2$sno[snoop2$city==1 & snoop2$grade==4 & snoop2$supp==0 & snoop2$treat==1],
       snoop2$post[snoop2$city==1 & snoop2$grade==4 & snoop2$supp==0 & snoop2$treat==1],cex = 0.8, col = "red")
points(snoop2$sno[snoop2$city==1 & snoop2$grade==4 & snoop2$supp==1 & snoop2$treat==1],
       snoop2$post[snoop2$city==1 & snoop2$grade==4 & snoop2$supp==1 & snoop2$treat==1],pch = 16, cex = 0.8,
       col = "red")

plot(snoop2$post, 
     xlab = "Indexes",
     ylab = "Post test score",
     main = "Post scores in Youngstown", cex.main = 0.8,
     col = "white", xlim = c(93,192),ylim = c(0,120))
# legend(170,30, bty = "n",
#        legend = c("G1 Rep", "G2 Rep", "G3 Rep", "G4 Rep"),
#        col = c("black", "gray", "blue", "red"),pch = 1 , cex = 0.7)
# legend(155,30, bty = "n",
#        legend = c("G1 Sup", "G2 Sup", "G3 Sup", "G4 Sup"),
#        col = c("black", "gray", "blue", "red"), pch = 16, cex = 0.7)
points(snoop2$sno[snoop2$city==2 & snoop2$grade==1 & snoop2$supp==0 & snoop2$treat==1],
       snoop2$post[snoop2$city==2 & snoop2$grade==1 & snoop2$supp==0 & snoop2$treat==1],cex = 0.8)
points(snoop2$sno[snoop2$city==2 & snoop2$grade==1 & snoop2$supp==1 & snoop2$treat==1],
       snoop2$post[snoop2$city==2 & snoop2$grade==1 & snoop2$supp==1 & snoop2$treat==1],pch = 16, cex = 0.8)

points(snoop2$sno[snoop2$city==2 & snoop2$grade==2 & snoop2$supp==0 & snoop2$treat==1],
       snoop2$post[snoop2$city==2 & snoop2$grade==2 & snoop2$supp==0 & snoop2$treat==1],cex = 0.8, col = "gray")
points(snoop2$sno[snoop2$city==2 & snoop2$grade==2 & snoop2$supp==1 & snoop2$treat==1],
       snoop2$post[snoop2$city==2 & snoop2$grade==2 & snoop2$supp==1 & snoop2$treat==1],pch = 16, cex = 0.8,
       col = "gray")

points(snoop2$sno[snoop2$city==2 & snoop2$grade==3 & snoop2$supp==0 & snoop2$treat==1],
       snoop2$post[snoop2$city==2 & snoop2$grade==3 & snoop2$supp==0 & snoop2$treat==1],cex = 0.8, col = "blue")
points(snoop2$sno[snoop2$city==2 & snoop2$grade==3 & snoop2$supp==1 & snoop2$treat==1],
       snoop2$post[snoop2$city==2 & snoop2$grade==3 & snoop2$supp==1 & snoop2$treat==1],pch = 16, cex = 0.8,
       col = "blue")

points(snoop2$sno[snoop2$city==2 & snoop2$grade==4 & snoop2$supp==0 & snoop2$treat==1],
       snoop2$post[snoop2$city==2 & snoop2$grade==4 & snoop2$supp==0 & snoop2$treat==1],cex = 0.8, col = "red")
points(snoop2$sno[snoop2$city==2 & snoop2$grade==4 & snoop2$supp==1 & snoop2$treat==1],
       snoop2$post[snoop2$city==2 & snoop2$grade==4 & snoop2$supp==1 & snoop2$treat==1],pch = 16, cex = 0.8,
       col = "red")

####################
#Stan Model
####################
snoop3 <- subset(snoop2, snoop2$treat==1)
pre <- snoop3$pre
post <- snoop3$post
city <- snoop3$city
grade <- snoop3$grade
supp <- snoop3$supp
length(supp)

stanc("9a.stan")
fit_sup <- stan("9a.stan", 
                data = list("pre", "post", "supp", 
                            "city", "grade"),
                iter = 1000,
                chains = 3)
print(fit_sup)
##########################
par(mfrow = c(2, 2), mar=c(3, 3, 2, 1), 
    oma=c(.5, .5, .5, .5), mgp=c(2, 1, 0))
for(i in 1:4){
  plot(snoop$pre[snoop$grade==i & snoop$treat==1 & snoop$supp=="S"],snoop$post[snoop$grade==i & snoop$treat==1 & snoop$supp=="S"], 
  pch = 16, cex = 0.8, xlab = "Pre test score", 
  ylab = "Post test score", main = paste("Grade",i), xlim = c(0,120), ylim = c(0,120))
  points(snoop$pre[snoop$grade==i & snoop$treat ==1 & snoop$supp=="R"],snoop$post[snoop$grade==i & snoop$treat ==1 & snoop$supp=="R"] ,cex = 0.8)

abline(lm(snoop$post[snoop$grade==i & snoop$treat==1 & 
                       snoop$supp=="S"] ~ snoop$pre[snoop$grade==i & snoop$treat==1 & snoop$supp=="S"]))

abline(lm(snoop$post[snoop$grade==i & snoop$treat==1 & 
                       snoop$supp=="R"] ~ snoop$pre[snoop$grade==i & snoop$treat==1 & snoop$supp=="R"]), lty = 2)
        
}










