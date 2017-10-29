# #Read in data
rm(list = ls())
setwd("/Users/Advait/Desktop/New School/Fall16/BDA/Class7")
dogs <- read.table("http://www.stat.columbia.edu/~gelman/bda.course/dogs.txt",
                  skip = 2)
 names(dogs) <- c("dog", 0:24)
 str(dogs)
dogs <- dogs[,-c(1)]
dogs1 <- dogs[,1:25] 
dogs1 <- ifelse(dogs1 == "S", 1, 0)
 
q <- matrix(data = NA, nrow = dim(dogs1)[1], ncol = dim(dogs1)[2])
for(i in 1:dim(dogs1)[1]){
 for(j in 1:dim(dogs1)[2]){
  q[i, j] <- sum(dogs1[i, 1:c(j - 1)])
   }
 }
q[,1] <- rep(0, 30)


id <- matrix(data = NA, nrow = dim(dogs1)[1], ncol = dim(dogs1)[2])
for(i in 1:dim(dogs1)[1]){
id[i,] <- rep(i,25)
}

grand_snoop_dogg <- as.data.frame(
  cbind(c(t(dogs1)), 
      c(t(q)), 
      rep(seq(from = 0, to = 24), 30),
      c(t(id))
      )
)
names(grand_snoop_dogg) <- c("bool.shock", "prev.shock", 
                             "trials","dog.id")

grand_snoop_dogg[1:10,]
y <- grand_snoop_dogg$bool.shock
trials <- grand_snoop_dogg$trials
shock <- grand_snoop_dogg$prev.shock

avoid <- trials - shock
grand_snoop_dogg$avoid <- avoid
N <- 750
S <- 30
id <- grand_snoop_dogg$dog.id


list.files()
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

stanc("4b.stan")
fit1 <- stan("4b.stan", 
             data = list("N","S","y","trials","shock","id"), 
             iter = 1000, chains = 3)

stanc("4b2.stan")
fit2 <- stan("4b2.stan", 
             data = list("N","S","y","avoid","shock","id"), 
             iter = 1000, chains = 1)
print(fit2)[1:5,]

