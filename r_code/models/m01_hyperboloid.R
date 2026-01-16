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
 dire_t[i] <- paste('../../data/time_data/sujeto_',i,'_tiempo.csv',sep='')
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

### JAGS Especifications 
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

# Parameters to estimate
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
