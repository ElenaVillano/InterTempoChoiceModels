#### Trade-off #####
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

### Question Especifications
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
                         n.iter = 1500000,
                         n.burnin = 900000,
                         n.thin = 200)


unlink('TradeModel.bug')

save.image("~/Documents/Heavy_R_Stuff/PublicatedArticleModels/TradeModelArticle.RData")
