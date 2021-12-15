#### 
#### Running all the models from the article 
#### January, 2020 
#### By Elena Villalobos 
####


###### Hyperboloid ####
### Clear workspace
rm(list=ls())

# Load Package
library(R2jags)
set.seed(77)

### General especifications
n_sub <- 25            #i
n_que <- 24            #j
n_rep <- 10            #k

### Upload Data
# Raw
dire_t <- NA
for (i in 1:n_sub){
 dire_t[i] <- paste('~/Google Drive/Nube/Intertemporal_Risky_Choice_Project/Tesis/Fase_buena/data/sujeto_',i,'_tiempo.csv',sep='')
}
csvs_time <- lapply(dire_t,read.csv)

# Order raw data
t_choice <- array(dim=c(n_sub,n_que,n_rep))
for(i in 1:n_sub){
 for(j in 1:n_que){
  t_choice[i,j,] <- subset(csvs_time[[i]][,7:8],pair==j)$biggerchosen
 }
}


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

### JAGS especifications
# Data that JAGS will use
data <- list("n_que","n_sub","n_rep","t_choice","x_ss","x_ll","t_ss","t_ll")

# Initial values
myinits = list(
 list(epsilon=4, 
      tau=rep(2,n_sub),
      kappa=rep(1,n_sub)))


# Model
write('model{
      
      # HYPERBOLOID MODEL

      prec <- 1/(5^2)
      epsilon  ~ dnorm(0,prec)T(0,)

      for (i in 1:n_sub){
      
      # PARAMETERS TO ESTIMATE
      kappa[i] ~ dnorm(0,prec)T(0,)
      tau[i] ~ dnorm(0,prec)
      
      for (j in 1:n_que){
      
      # FUNCTIONS
      # Subjective value
      value_ll[i,j] <-   x_ll[j]/(1+kappa[i]*t_ll[j])^tau[i]
      value_ss[i,j] <-   x_ss[j]/(1+kappa[i]*t_ss[j])^tau[i]
      
      # Probability of choosing the larger later
      theta[i,j] <- (value_ll[i,j]^(1/epsilon))/((value_ll[i,j]^(1/epsilon))+(value_ss[i,j]^(1/epsilon)))
      
      # For repetitions
      for (h in 1:n_rep){
      t_choice[i,j,h] ~ dbern(theta[i,j])
      
      }
      }
      }
      }','HyperModel.bug')

# Parametros y nodos a rastrear
parameters <- c("kappa","theta", "epsilon",'tau')


# The following command calls WinBUGS with specific options.
# For a detailed description see Sturtz, Ligges, & Gelman (25).
samples <- jags.parallel(data, 
                         inits=myinits, 
                         parameters,
                         model ="HyperModel.bug",
                         n.chains=2, 
                         n.iter = 1500000,
                         n.burnin = 900000,
                         n.thin = 200)


unlink('HyperModel.bug')

save.image("~/Documents/Heavy_R_Stuff/PublicatedArticleModels/HyperModelArticle.RData")


###### Trade-off #####
# ### Clear workspace
# rm(list=ls())
# 
# # Load Package
# library(R2jags)
# set.seed(77)
# 
# ### General especifications
# n_sub <- 25            #i
# n_que <- 24            #j
# n_rep <- 10            #k
# 
# ### Upload Data
# # Raw
# dire_t <- NA
# for (i in 1:n_sub){
#  dire_t[i] <- paste('~/Google Drive/Nube/Intertemporal_Risky_Choice_Project/Tesis/Fase_buena/data/sujeto_',i,'_tiempo.csv',sep='')
# }
# csvs_time <- lapply(dire_t,read.csv)
# 
# # Order raw data
# t_choice <- array(dim=c(n_sub,n_que,n_rep))
# for(i in 1:n_sub){
#  for(j in 1:n_que){
#   t_choice[i,j,] <- subset(csvs_time[[i]][,7:8],pair==j)$biggerchosen
#  }
# }
# 
# ### Question Especifications
# # Outcome of the smaller reward
# x_ss <- c(5150,5300,5450,5150,5300,5150,
#           6050,6200,6350,6050,6200,6050,
#           5150,5600,6050,5150,5600,5150,
#           1150,1250,1350,1150,1250,1150)
# # Outcome of the larger reward
# x_ll <- c(5300,5450,5600,5450,5600,5600,
#           6200,6350,6500,6350,6500,6500,
#           5600,6050,6500,6050,6500,6500,
#           1250,1350,1450,1350,1450,1450)
# # Delay of the sooner reward
# t_ss <- c(1,2,3,1,2,1,
#           7,8,9,7,8,7,
#           1,4,7,1,4,1,
#           1,4,7,1,4,1)
# # Delay of the larger reward
# t_ll <- c(2,3,4,3,4,4,
#           8,9,10,9,10,10,
#           4,7,10,7,10,10,
#           4,7,10,7,10,10)
# 
# 
# #### JAGS Especifications
# # Data that JAGS will use
# data <- list("n_que","n_sub","n_rep","t_choice","x_ss","x_ll","t_ss","t_ll")
# 
# # Initial values
# myinits = list(
#  list(t_kappa=rep(0.5,n_sub), 
#       t_vartheta=rep(1.5,n_sub),
#       t_gamma=rep(0.9,n_sub), 
#       t_tau=rep(0.3,n_sub),
#       t_epsilon_trade=0.1))
# 
# # Model
# write('model{
#       
#       # TRADE-OFF MODEL
#       prec <- 1/(5^2)
#       t_epsilon_trade ~ dlnorm(0,prec)
# 
#       for (i in 1:n_sub){
#       
#       # PARAMETERS TO ESTIMATE
#       t_gamma[i] ~ dlnorm(0,prec)
#       t_tau[i] ~ dlnorm(0,prec)
#       t_kappa[i] ~ dlnorm(0,prec)
#       t_vartheta[i] ~ dlnorm(1,prec)
#       
#       for (j in 1:n_que){
#       
#       # FUNCTIONS
#       v_ll[i,j] <- (1/t_gamma[i])*log(1+t_gamma[i]*x_ll[j])        
#       v_ss[i,j] <- (1/t_gamma[i])*log(1+t_gamma[i]*x_ss[j])
#       
#       w_ll[i,j] <- (1/t_tau[i])*log(1+t_tau[i]*t_ll[j])
#       w_ss[i,j] <- (1/t_tau[i])*log(1+t_tau[i]*t_ss[j])
#       
#       q_value[i,j] <- v_ll[i,j]-v_ss[i,j]
#       q_time[i,j] <- (t_kappa[i])*log(1+((w_ll[i,j]-w_ss[i,j])/t_vartheta[i])^t_vartheta[i])
#       
#       # Probability of choosing the larger later
#       theta[i,j] <- (q_value[i,j]^(1/t_epsilon_trade))/((q_value[i,j]^(1/t_epsilon_trade))+(q_time[i,j]^(1/t_epsilon_trade)))
#       
#       # For repetitions 
#       for (h in 1:n_rep){
#       t_choice[i,j,h] ~ dbern(theta[i,j])
#       
#       }
#       }    
#       }
#       }','TradeModel.bug')
# 
# 
# # Parameters to estimate
# parameters <- c('t_gamma','t_tau','t_vartheta','t_kappa','theta','t_epsilon_trade')
# 
# # The following command calls WinBUGS with specific options.
# # For a detailed description see Sturtz, Ligges, & Gelman (25).
# samples <- jags.parallel(data,
#                          inits = myinits,
#                          parameters,
#                          model = 'TradeModel.bug',
#                          n.chains = 2,
#                          n.iter = 1500000,
#                          n.burnin = 900000,
#                          n.thin = 200)
# 
# 
# 
# unlink('TradeModel.bug')
# 
# save.image("~/Documents/Heavy_R_Stuff/PublicatedArticleModels/TradeModelArticle.RData")
# 
# ###### ITCH ####
# ### Clear workspace
# rm(list=ls())
# 
# # Load Package
# library(R2jags)
# set.seed(77)
# 
# ### General especifications
# n_sub <- 25            #i
# n_que <- 24            #j
# n_rep <- 10            #k
# 
# #### Upload Data
# # Raw
# dire_t <- NA
# for (i in 1:n_sub){
#  dire_t[i] <- paste('~/Google Drive/Nube/Intertemporal_Risky_Choice_Project/Tesis/Fase_buena/data/sujeto_',i,'_tiempo.csv',sep='')
# }
# csvs_time <- lapply(dire_t,read.csv)
# 
# # Order raw data
# t_choice <- array(dim=c(n_sub,n_que,n_rep))
# for(i in 1:n_sub){
#  for(j in 1:n_que){
#   t_choice[i,j,] <- subset(csvs_time[[i]][,7:8],pair==j)$biggerchosen
#  }
# }
# 
# ### Question Especifications
# # Outcome of the smaller reward
# x_ss <- c(5150,5300,5450,5150,5300,5150,
#           6050,6200,6350,6050,6200,6050,
#           5150,5600,6050,5150,5600,5150,
#           1150,1250,1350,1150,1250,1150)
# # Outcome of the larger reward
# x_ll <- c(5300,5450,5600,5450,5600,5600,
#           6200,6350,6500,6350,6500,6500,
#           5600,6050,6500,6050,6500,6500,
#           1250,1350,1450,1350,1450,1450)
# # Delay of the sooner reward
# t_ss <- c(1,2,3,1,2,1,
#           7,8,9,7,8,7,
#           1,4,7,1,4,1,
#           1,4,7,1,4,1)
# # Delay of the larger reward
# t_ll <- c(2,3,4,3,4,4,
#           8,9,10,9,10,10,
#           4,7,10,7,10,10,
#           4,7,10,7,10,10)
# 
# 
# #### Especifications for JAGS 
# # Data that JAGS will use
# data <- list("n_que","n_sub","n_rep","t_choice","x_ss","x_ll","t_ss","t_ll")
# 
# # Initial values
# myinits = list(
#  list(beta_1=rep(0.2,n_sub), 
#       beta_x_A=rep(0.01,n_sub),
#       beta_x_R=rep(0.02,n_sub),
#       beta_t_A=rep(0.03,n_sub),
#       beta_t_R=rep(0.03,n_sub)))
# 
# # Model
# write('model{
#       
#       # ITCH MODEL
#       prec <- 1/(10^2)
#       
#       for (i in 1:n_sub){
#       
#       # PARAMETERS TO ESTIMATE
#       beta_1[i] ~ dnorm(0,prec)
#       beta_x_A[i] ~ dnorm(0,prec)
#       beta_x_R[i] ~ dnorm(0,prec)
#       beta_t_A[i] ~ dnorm(0,prec)
#       beta_t_R[i] ~ dnorm(0,prec)
#       
#       for (j in 1:n_que){
#       
#       # FUNCTIONS
#       dif_x_A[i,j] <- beta_x_A[i]*(x_ll[j]-x_ss[j])
#       dif_x_R[i,j] <- beta_x_R[i]*((x_ll[j]-x_ss[j])/(0.5*(x_ll[j]+x_ss[j])))
#       dif_t_A[i,j] <- beta_t_A[i]*(t_ll[j]-t_ss[j])
#       dif_t_R[i,j] <- beta_t_R[i]*((t_ll[j]-t_ss[j])/(0.5*(t_ll[j]+t_ss[j])))
#       
#       # Regression
#       y[i,j] <- beta_1[i]+dif_x_A[i,j]+dif_x_R[i,j]+dif_t_A[i,j]+dif_t_R[i,j]
#       
#       # Probability of choosing the larger later
#       theta[i,j] <- phi(y[i,j])
#       
#       # For repetitions
#       for (h in 1:n_rep){
#       t_choice[i,j,h] ~ dbern(theta[i,j])
#       
#       }
#       }
#       }
#       }','ITCHModel.bug')
# 
# 
# # Parameters to estimate
# parameters <- c('beta_1','beta_x_A','beta_x_R','beta_t_A','beta_t_R','theta')
# 
# # The following command calls WinBUGS with specific options.
# # For a detailed description see Sturtz, Ligges, & Gelman (25).
# samples <- jags.parallel(data,
#                          inits = myinits,
#                          parameters,
#                          model = 'ITCHModel.bug',
#                          n.chains = 2,
#                          n.iter = 1500000,
#                          n.burnin = 900000,
#                          n.thin = 200)
# 
# 
# 
# unlink('ITCHModel.bug')
# 
# save.image("~/Documents/Heavy_R_Stuff/PublicatedArticleModels/ITCHModelArticle.RData")

###### Proportional Differences ####
### Clear workspace
rm(list=ls())

# Load Package
library(R2jags)
set.seed(77)

#### General especifications
n_sub <- 25            #i
n_que <- 24            #j
n_rep <- 10            #k

#### Upload Data
# Raw
dire_t <- NA
for (i in 1:n_sub){
 dire_t[i] <- paste('~/Google Drive/Nube/Intertemporal_Risky_Choice_Project/Tesis/Fase_buena/data/sujeto_',i,'_tiempo.csv',sep='')
}
csvs_time <- lapply(dire_t,read.csv)

# Order raw data
t_choice <- array(dim=c(n_sub,n_que,n_rep))
for(i in 1:n_sub){
 for(j in 1:n_que){
  t_choice[i,j,] <- subset(csvs_time[[i]][,7:8],pair==j)$biggerchosen
 }
}

### Questions Especification 
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


#### JAGS Especifications 
# Data that JAGS will use
data <- list("n_que","n_sub","n_rep","t_choice","x_ss","x_ll","t_ss","t_ll")

# Initial values
myinits = list(
 list(sigma=rep(0.2,n_sub), 
      delta=rep(0.01,n_sub)))

# Model
write('model{
      
      # PROPORTIONAL DIFFERENCES MODEL
      prec <- 1/(5^2)
      
      for (i in 1:n_sub){
      
      # PARAMETERS TO ESTIMATE
      sigma[i] ~ dnorm(0,prec)T(0,)
      delta[i] ~ dnorm(0,prec)
      
      for (j in 1:n_que){
      
      # FUNCTIONTS
      dif_x_R[i,j] <- (max(x_ll[j],x_ss[j]) - min(x_ll[j],x_ss[j]))/max(x_ll[j],x_ss[j])
      dif_t_R[i,j] <- (max(t_ll[j],t_ss[j]) - min(t_ll[j],t_ss[j]))/max(t_ll[j],t_ss[j])
      
      # Regression
      dif[i,j] <- dif_x_R[i,j]-dif_t_R[i,j]
      
      y[i,j] <- (dif[i,j]-delta[i])/sigma[i]
      
      # Probability of choosing the larger later
      theta[i,j] <- phi(y[i,j])
      
      # For repetitions
      for (h in 1:n_rep){
      t_choice[i,j,h] ~ dbern(theta[i,j])
      
      }
      }
      }
      }','PDModel.bug')


# Parameters to estimate
parameters <- c('sigma','delta','theta')

# The following command calls WinBUGS with specific options.
# For a detailed description see Sturtz, Ligges, & Gelman (25).
samples <- jags.parallel(data,
                         inits = myinits,
                         parameters,
                         model = 'PDModel.bug',
                         n.chains = 2,
                         n.iter = 1500000,
                         n.burnin = 900000,
                         n.thin = 200)



unlink('PDModel.bug')

save.image("~/Documents/Heavy_R_Stuff/PublicatedArticleModels/PDModelArticle.RData")

###### Direc Differences #####
### Clearworkspace
rm(list=ls())

# Load Package
library(R2jags)
set.seed(77)

#### General especifications
n_sub <- 25            #i
n_que <- 24            #j
n_rep <- 10            #k

#### Upload Data
# Raw
dire_t <- NA
for (i in 1:n_sub){
 dire_t[i] <- paste('~/Google Drive/Nube/Intertemporal_Risky_Choice_Project/Tesis/Fase_buena/data/sujeto_',i,'_tiempo.csv',sep='')
}
csvs_time <- lapply(dire_t,read.csv)

# Order raw data
t_choice <- array(dim=c(n_sub,n_que,n_rep))
for(i in 1:n_sub){
 for(j in 1:n_que){
  t_choice[i,j,] <- subset(csvs_time[[i]][,7:8],pair==j)$biggerchosen
 }
}

#### Especification Questions
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


#### Especifications for JAGS
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
                         n.iter = 1500000,
                         n.burnin = 900000,
                         n.thin = 200)



unlink('DDModel.bug')

save.image("~/Documents/Heavy_R_Stuff/PublicatedArticleModels/DDModelArticle.RData")
