install.packages("msm", dependencies = T)
library(msm)
library(mvtnorm)
################
#Generate data

N <- 100
sd_a <- 1
sd_b <- 1
rho <- 0.05

beta <- rmvnorm(1,
                mean = c(1,0.5),
                sigma = matrix(
                  data =c(sd_a^2, sd_a*sd_b*rho,sd_a*sd_b*rho, sd_b^2),
                  nrow = 2))
X <-  matrix( c(rep (1 ,N) , (runif(N, -5, 2) ) ) , nrow = N, ncol = 2)
z <-  rnorm(N, X%*%c (beta ) , 1)
y <-  ifelse( z > 0 , 1 , 0)
#
#Lower bound
lower.bound2<- function(betas, X, stars, sigs, draws){

  ab<- t(X)%*%X%*%(betas%*%t(betas) + sigs)
  part1<- sum(diag(ab))/2
  part2<- t(betas)%*%betas + sum(diag(sigs))
  part2<- part2*(beta.mat.prior[1,1])
  part2<- part2/2 + (1/2)*log(det(solve(beta.mat.prior))) + (length(betas)/2)*log(2*pi)
  part3<- t(stars)%*%stars
  part4<- length(betas)/2 + (1/2)*log(det(sigs)) + (length(betas)/2)*log(2*pi)
  bounds<-  part1  +  part2 + part3/2 + part4
  parts<- c(-part1, -part2, part3/2, part4)
  ##we will return the lower bound and the constituent
  ##parts, useful for monitoring convergence of the algorithm
  bounds<- list(bounds, parts)
  names(bounds)<- c("bounds","parts")
  return(bounds)
}
#########
##we will use the lower-bound to monitor convergence,
##stopping the model when the lower-bound drops below 1e-8
func.reg<- function(X, draws){
  beta.mat.prior <- diag(1/100, ncol(X))
  beta.VA<- matrix(NA, nrow=1000, ncol=ncol(X))
  ##we begin with random values for the augmented data
  ystars<- rep(0, nrow(X))
  for(j in 1:nrow(X)){
    ystars[j]<- ifelse(draws[j]==1, rtnorm(1, mean=0.5, sd=1, lower=0, upper=Inf),
                       rtnorm(1, mean=-0.5, sd=1, lower=-Inf, upper=0) )
  }
  ##we will store the progress of the lower bound on the model
  bounds<- c()
  zz<- 0
  ##this stores the parts of the lower bound
  parts<- matrix(NA, nrow=1000,ncol=4)
  j<- 0
  ##creating a while loop
  while(zz==0){
    j<- j + 1
    ##updating the beta parameters
    beta.VA[j,]<- solve(t(X)%*%X + beta.mat.prior)%*%t(X)%*%ystars
    ##this does not need to be in the loop
    ##(it doesn’t change over observations)
    ##but is placed here for teaching purposes
    sigs<- solve(t(X)%*%X + beta.mat.prior)
    ##computing the inner product of the
    ##covariates current estimates of the coefficients
    stars<- X%*%beta.VA[j,]
    denom1<- pnorm(-stars)
    num1<- dnorm(-stars)
    ##now, computing the expected value for each
    ##individual’s augmented data, given
    ##current estimates of the approximating
    ##distribution on the coefficients
    ystars[which(draws==0)]<- stars[draws==0] +
      -num1[which(draws==0)]/denom1[which(draws==0)]
    ystars[which(draws==1)]<- stars[draws==1] +
      num1[which(draws==1)]/(1 - denom1[which(draws==1)])
    ##calculating the lower bound
    trial<- lower.bound2(beta.VA[j,], X, ystars, sigs, draws)
    bounds[j]<- trial$bounds
    parts[j,]<- trial$parts
    if(j>1){
      ##observing convergence
      ab<-abs(bounds[j]-  bounds[j-1])
      if(ab<1e-8){
        zz<-1}
    }
  }
  ##the information to be returned, after convergence
  stuff<- list(bounds, beta.VA[j,], sigs)
  names(stuff)<- c("bound","betas", "sigma")
  return(stuff)
}
example.run <- func.reg(X, y) 
##where X are the covariates and draws are the dep
