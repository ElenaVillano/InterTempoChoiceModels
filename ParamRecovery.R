#### 
#### Parameterrecovery
#### January, 2020 
#### By Elena Villalobos 
####

### Clear workspace

##### Trade-off ####
rm(list=ls())
### Question especifications
# Outcome of the smaller reward
x_ss <- c(5150,5300,5450,5150,5300,5150,
          6050,6200,6350,6050,6200,6050,
          5150,5600,6050,5150,5600,5150,
          1150,1250,1350,1150,1250,1150)
# Outcome of the larger reward
x_ll <- c(5300,5450,5600,5450,5600,5600,
          6200,6350,6500,6350,6500,6500,
          5600,6050,6500,6050,6500,6500,
          1250,1350,1450,1350,1450,1450)
# Delay of the sooner reward
t_ss <- c(1,2,3,1,2,1,
          7,8,9,7,8,7,
          1,4,7,1,4,1,
          1,4,7,1,4,1)
# Delay of the larger reward
t_ll <- c(2,3,4,3,4,4,
          8,9,10,9,10,10,
          4,7,10,7,10,10,
          4,7,10,7,10,10)

### Especifications
n_sub <- 1
n_que <- length(t_ll)
n_rep <- 1000

### Choice
t_choice <- array(NA,dim=c(n_sub,n_que,n_rep))

### Model
trademode <- function(gamma,tau,kappa,vartheta,epsilon){
 w_ss <- array(NA,dim=c(n_sub,n_que))
 w_ll <- array(NA,dim=c(n_sub,n_que))
 v_ss <- array(NA,dim=c(n_sub,n_que))
 v_ll <- array(NA,dim=c(n_sub,n_que))
 q_time <- array(NA,dim=c(n_sub,n_que))
 q_value <- array(NA,dim=c(n_sub,n_que))
 theta <- array(NA,dim=c(n_sub,n_que))
 
 for(i in 1:length(kappa)){
  for (j in 1:n_que){
   v_ll[i,j] <- (1/gamma[i])*log(1+gamma[i]*x_ll[j])        
   v_ss[i,j] <- (1/gamma[i])*log(1+gamma[i]*x_ss[j])
   w_ll[i,j] <- (1/tau[i])*log(1+tau[i]*t_ll[j])
   w_ss[i,j] <- (1/tau[i])*log(1+tau[i]*t_ss[j])
   
   q_value[i,j] <- v_ll[i,j]-v_ss[i,j]
   q_time[i,j] <- (kappa[i])*log(1+((w_ll[i,j]-w_ss[i,j])/vartheta[i])^vartheta[i])
   
   theta[i,j] <- (q_value[i,j]^(1/epsilon))/((q_value[i,j]^(1/epsilon))+(q_time[i,j]^(1/epsilon)))
  
   for (h in 1:n_rep){
    t_choice[i,j,h] <<- rbinom(1,1,theta[i,j])
   }
  }
 } 
}

ga <- 0.2
ta <- 0.1
ka <- 0.5
va <- 1.5
### Choice generator 
trademode(gamma = ga,
          tau = ta,
          kappa = ka,
          vartheta = va,
          epsilon = 0.5)

### Bayesian evaluation 

# Data that JAGS will use
data <- list("n_que","n_sub","n_rep","t_choice","x_ss","x_ll","t_ss","t_ll")

# Initial values
myinits = list(
 list(t_kappa=rep(0.5,n_sub),
      t_vartheta=rep(1.5,n_sub),
      t_gamma=rep(0.9,n_sub),
      t_tau=rep(0.3,n_sub),
      t_epsilon_trade=0.1))

# Model
write('model{

      # TRADE-OFF MODEL
      prec <- 1/(5^2)
      t_epsilon_trade ~ dlnorm(0,prec)

      for (i in 1:n_sub){

      # PARAMETERS TO ESTIMATE
      t_gamma[i] ~ dlnorm(0,prec)
      t_tau[i] ~ dlnorm(0,prec)
      t_kappa[i] ~ dlnorm(0,prec)
      t_vartheta[i] ~ dlnorm(1,prec)

      for (j in 1:n_que){

      # FUNCTIONS
      v_ll[i,j] <- (1/t_gamma[i])*log(1+t_gamma[i]*x_ll[j])
      v_ss[i,j] <- (1/t_gamma[i])*log(1+t_gamma[i]*x_ss[j])

      w_ll[i,j] <- (1/t_tau[i])*log(1+t_tau[i]*t_ll[j])
      w_ss[i,j] <- (1/t_tau[i])*log(1+t_tau[i]*t_ss[j])

      q_value[i,j] <- v_ll[i,j]-v_ss[i,j]
      q_time[i,j] <- (t_kappa[i])*log(1+((w_ll[i,j]-w_ss[i,j])/t_vartheta[i])^t_vartheta[i])

      # Probability of choosing the larger later
      theta[i,j] <- (q_value[i,j]^(1/t_epsilon_trade))/((q_value[i,j]^(1/t_epsilon_trade))+(q_time[i,j]^(1/t_epsilon_trade)))

      # For repetitions
      for (h in 1:n_rep){
      t_choice[i,j,h] ~ dbern(theta[i,j])

      }
      }
      }
      }','TradeModel.bug')


# Parameters to estimate
parameters <- c('t_gamma','t_tau','t_vartheta','t_kappa','theta','t_epsilon_trade')

# The following command calls WinBUGS with specific options.
# For a detailed description see Sturtz, Ligges, & Gelman (25).
samples <- jags.parallel(data,
                         inits = myinits,
                         parameters,
                         model = 'TradeModel.bug',
                         n.chains = 2,
                         n.iter = 15000,
                         n.burnin = 9000,
                         n.thin = 2)

unlink('TradeModel.bug')

names(samples$BUGSoutput$sims.list)
summary(samples$BUGSoutput$summary)
layout(1:2)
hist(samples$BUGSoutput$summary[,9],breaks=100)
hist(samples$BUGSoutput$summary[,8],breaks=100)
plot(samples)

theta<-samples$BUGSoutput$sims.list$theta
t_gamma<-samples$BUGSoutput$sims.list$t_gamma
t_kappa<-samples$BUGSoutput$sims.list$t_kappa
t_vartheta<-samples$BUGSoutput$sims.list$t_vartheta
t_tau<-samples$BUGSoutput$sims.list$t_tau
dim(theta)

layout(matrix(1:4,ncol=2))
plot(density(t_gamma),main=expression(gamma),axes=F,xlab='',ylab='');axis(1)
points(ga,min(density(t_gamma)$y),col='blue',pch=20,cex=2)

plot(density(t_kappa),main=expression(kappa),axes=F,xlab='',ylab='');axis(1)
points(ka,1,col='blue',pch=20,cex=2)

plot(density(t_vartheta),main=expression(vartheta),axes=F,xlab='',ylab='');axis(1)
points(va,1,col='blue',pch=20,cex=2)

plot(density(t_tau),main=expression(tau),axes=F,xlab='',ylab='');axis(1)
points(ta,1,col='blue',pch=20,cex=2)



##### ITCH ####

rm(list=ls())


### Question especifications
# Outcome of the smaller reward
x_ss <- c(5150,5300,5450,5150,5300,5150,
          6050,6200,6350,6050,6200,6050,
          5150,5600,6050,5150,5600,5150,
          1150,1250,1350,1150,1250,1150)
# Outcome of the larger reward
x_ll <- c(5300,5450,5600,5450,5600,5600,
          6200,6350,6500,6350,6500,6500,
          5600,6050,6500,6050,6500,6500,
          1250,1350,1450,1350,1450,1450)
# Delay of the sooner reward
t_ss <- c(1,2,3,1,2,1,
          7,8,9,7,8,7,
          1,4,7,1,4,1,
          1,4,7,1,4,1)
# Delay of the larger reward
t_ll <- c(2,3,4,3,4,4,
          8,9,10,9,10,10,
          4,7,10,7,10,10,
          4,7,10,7,10,10)

### Especifications
n_sub <- 1
n_que <- length(t_ll)
n_rep <- 100

### Choice
t_choice <- array(NA,dim=c(n_sub,n_que,n_rep))

### Model
itchmode <- function(beta_1,beta_x_A,beta_x_R,beta_t_A,beta_t_R){
 dif_x_A <- array(NA,dim=c(n_sub,n_que))
 dif_x_R <- array(NA,dim=c(n_sub,n_que))
 dif_t_A <- array(NA,dim=c(n_sub,n_que))
 dif_t_R <- array(NA,dim=c(n_sub,n_que))
 y <- array(NA,dim=c(n_sub,n_que))
 theta <- array(NA,dim=c(n_sub,n_que))
 
 for(i in 1:length(kappa)){
  for (j in 1:n_que){
   dif_x_A[i,j] <- beta_x_A[i]*(x_ll[j]-x_ss[j])
   dif_x_R[i,j] <- beta_x_R[i]*((x_ll[j]-x_ss[j])/(0.5*(x_ll[j]+x_ss[j])))
   dif_t_A[i,j] <- beta_t_A[i]*(t_ll[j]-t_ss[j])
   dif_t_R[i,j] <- beta_t_R[i]*((t_ll[j]-t_ss[j])/(0.5*(t_ll[j]+t_ss[j])))
   
   y[i,j] <- beta_1[i]+dif_x_A[i,j]+dif_x_R[i,j]+dif_t_A[i,j]+dif_t_R[i,j]
   
   theta[i,j] <- pnorm(y[i,j])
   
   for (h in 1:n_rep){
    t_choice[i,j,h] <<- rbinom(1,1,theta[i,j])
   }
  }
 } 
}

xA <- 0.2
xR <- 0.9
tA <- 0.6
tR <- -0.5

### Choice generator 
itchmode(beta_1 = 0.5,
         beta_x_A = xA,
         beta_x_R = xR,
         beta_t_A = tA,
         beta_t_R = tR)

### Bayesian evaluation 
# Data that JAGS will use
data <- list("n_que","n_sub","n_rep","t_choice","x_ss","x_ll","t_ss","t_ll")

# Initial values
myinits = list(
 list(beta_1=rep(0.2,n_sub),
      beta_x_A=rep(0.01,n_sub),
      beta_x_R=rep(0.02,n_sub),
      beta_t_A=rep(0.03,n_sub),
      beta_t_R=rep(0.03,n_sub)))

# Model
write('model{

      # ITCH MODEL
      prec <- 1/(10^2)

      for (i in 1:n_sub){

      # PARAMETERS TO ESTIMATE
      beta_1[i] ~ dnorm(0,prec)
      beta_x_A[i] ~ dnorm(0,prec)
      beta_x_R[i] ~ dnorm(0,prec)
      beta_t_A[i] ~ dnorm(0,prec)
      beta_t_R[i] ~ dnorm(0,prec)

      for (j in 1:n_que){

      # FUNCTIONS
      dif_x_A[i,j] <- beta_x_A[i]*(x_ll[j]-x_ss[j])
      dif_x_R[i,j] <- beta_x_R[i]*((x_ll[j]-x_ss[j])/(0.5*(x_ll[j]+x_ss[j])))
      dif_t_A[i,j] <- beta_t_A[i]*(t_ll[j]-t_ss[j])
      dif_t_R[i,j] <- beta_t_R[i]*((t_ll[j]-t_ss[j])/(0.5*(t_ll[j]+t_ss[j])))

      # Regression
      y[i,j] <- beta_1[i]+dif_x_A[i,j]+dif_x_R[i,j]+dif_t_A[i,j]+dif_t_R[i,j]

      # Probability of choosing the larger later
      theta[i,j] <- phi(y[i,j])

      # For repetitions
      for (h in 1:n_rep){
      t_choice[i,j,h] ~ dbern(theta[i,j])

      }
      }
      }
      }','ITCHModel.bug')


# Parameters to estimate
parameters <- c('beta_1','beta_x_A','beta_x_R','beta_t_A','beta_t_R','theta')

# The following command calls WinBUGS with specific options.
# For a detailed description see Sturtz, Ligges, & Gelman (25).
samples <- jags.parallel(data,
                         inits = myinits,
                         parameters,
                         model = 'ITCHModel.bug',
                         n.chains = 2,
                         n.iter = 15000,
                         n.burnin = 9000,
                         n.thin = 2)

unlink('ITCHModel.bug')

names(samples$BUGSoutput$sims.list)
summary(samples$BUGSoutput$summary)
layout(1:2)
hist(samples$BUGSoutput$summary[,9],breaks=100)
hist(samples$BUGSoutput$summary[,8],breaks=100)
plot(samples)

theta<-samples$BUGSoutput$sims.list$theta
beta_1<-samples$BUGSoutput$sims.list$beta_1
beta_t_A<-samples$BUGSoutput$sims.list$beta_t_A
beta_t_R<-samples$BUGSoutput$sims.list$beta_t_R
beta_x_R<-samples$BUGSoutput$sims.list$beta_x_R
beta_x_A<-samples$BUGSoutput$sims.list$beta_x_A


layout(matrix(1:4,ncol=2))
plot(density(beta_t_A),main=expression(beta[tA]),axes=F,xlab='',ylab='');axis(1)
points(tA,min(density(beta_t_A)$y),col='blue',pch=20,cex=2)

plot(density(beta_t_R),main=expression(beta[tR]),axes=F,xlab='',ylab='');axis(1)
points(tR,min(density(beta_t_R)$y),col='blue',pch=20,cex=2)

plot(density(beta_x_A),main=expression(beta[xA]),axes=F,xlab='',ylab='');axis(1)
points(xA,min(density(beta_x_A)$y),col='blue',pch=20,cex=2)

plot(density(beta_x_R),main=expression(beta[xR]),axes=F,xlab='',ylab='');axis(1)
points(xR,min(density(beta_x_R)$y),col='blue',pch=20,cex=2)




#### DD ####
rm(list=ls())

### Question especifications
# Outcome of the smaller reward
x_ss <- c(5150,5300,5450,5150,5300,5150,
          6050,6200,6350,6050,6200,6050,
          5150,5600,6050,5150,5600,5150,
          1150,1250,1350,1150,1250,1150)
# Outcome of the larger reward
x_ll <- c(5300,5450,5600,5450,5600,5600,
          6200,6350,6500,6350,6500,6500,
          5600,6050,6500,6050,6500,6500,
          1250,1350,1450,1350,1450,1450)
# Delay of the sooner reward
t_ss <- c(1,2,3,1,2,1,
          7,8,9,7,8,7,
          1,4,7,1,4,1,
          1,4,7,1,4,1)
# Delay of the larger reward
t_ll <- c(2,3,4,3,4,4,
          8,9,10,9,10,10,
          4,7,10,7,10,10,
          4,7,10,7,10,10)

### Especifications
n_sub <- 1
n_que <- length(t_ll)
n_rep <- 100

### Choice
t_choice <- array(NA,dim=c(n_sub,n_que,n_rep))

### Model
ddmode <- function(weight,delta,sigma){
 
 dif_x <- array(NA,dim=c(n_sub,n_que))
 dif_t <- array(NA,dim=c(n_sub,n_que))
 dif <- array(NA,dim=c(n_sub,n_que))
 
 y <- array(NA,dim=c(n_sub,n_que))
 theta <- array(NA,dim=c(n_sub,n_que))
 
 for(i in 1:length(kappa)){
  for (j in 1:n_que){
   
   dif_x[i,j] <-   (weight[i])*(x_ll[j]-x_ss[j])
   dif_t[i,j] <- (1-weight[i])*(t_ll[j]-t_ss[j])
   
   dif[i,j] <- dif_x[i,j]-dif_t[i,j]
   
   y[i,j] <- (dif[i,j]-delta[i])/sigma[i]
   
   theta[i,j] <- pnorm(y[i,j])
   
   for (h in 1:n_rep){
    t_choice[i,j,h] <<- rbinom(1,1,theta[i,j])
   }
  }
 } 
}

we <- 0.2
de <- 0.9
si <- 0.6


### Choice generator 
ddmode(weight = we,
       delta = de,
       sigma = si)

### Bayesian evaluation 
# Data that JAGS will use
data <- list("n_que","n_sub","n_rep","t_choice","x_ss","x_ll","t_ss","t_ll")

# Initial values
myinits = list(
 list(sigma=rep(4,n_sub),
      delta=rep(2,n_sub),
      weight=rep(1,n_sub)))

# Model
write('model{

      # DIRECT DIFFERENCES MODEL
      prec <- 1/(5^2)

      for (i in 1:n_sub){

      # PARAMETERS TO ESTIMATE
      sigma[i] ~ dnorm(0,prec)T(0,)
      weight[i] ~ dnorm(0,prec)
      delta[i] ~ dnorm(0,prec)

      for (j in 1:n_que){

      # FUNCTIONS
      dif_x_R[i,j] <-   (weight[i])*(x_ll[j]-x_ss[j])
      dif_t_R[i,j] <- (1-weight[i])*(t_ll[j]-t_ss[j])

      # Difference
      dif[i,j] <- dif_x_R[i,j]-dif_t_R[i,j]

      # Probit
      y[i,j] <- (dif[i,j]-delta[i])/sigma[i]

      # Probability of choosing the larger later
      theta[i,j] <- phi(y[i,j])

      # For repetitions
      for (h in 1:n_rep){
      t_choice[i,j,h] ~ dbern(theta[i,j])

      }
      }
      }
      }','DDModel.bug')


# Parameters to estimate
parameters <- c('sigma','theta','weight','delta')

# The following command calls WinBUGS with specific options.
# For a detailed description see Sturtz, Ligges, & Gelman (25).
samples <- jags.parallel(data,
                         inits = myinits,
                         parameters,
                         model = 'DDModel.bug',
                         n.chains = 2,
                         n.iter = 15000,
                         n.burnin = 9000,
                         n.thin = 2)

unlink('DDModel.bug')

names(samples$BUGSoutput$sims.list)
summary(samples$BUGSoutput$summary)
layout(1:2)
hist(samples$BUGSoutput$summary[,9],breaks=100)
hist(samples$BUGSoutput$summary[,8],breaks=100)
plot(samples)

theta<-samples$BUGSoutput$sims.list$theta
weight<-samples$BUGSoutput$sims.list$weight
delta<-samples$BUGSoutput$sims.list$delta
sigma<-samples$BUGSoutput$sims.list$sigma

layout(matrix(1:4,ncol=2))
plot(density(weight),main=expression(w),axes=F,xlab='',ylab='');axis(1)
points(we,min(density(weight)$y),col='blue',pch=20,cex=2)

plot(density(delta),main=expression(delta),axes=F,xlab='',ylab='');axis(1)
points(de,min(density(delta)$y),col='blue',pch=20,cex=2)

plot(density(sigma),main=expression(sigma),axes=F,xlab='',ylab='');axis(1)
points(si,min(density(sigma)$y),col='blue',pch=20,cex=2)



