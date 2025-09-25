#### Familia de modelos basados en atributos
#### Codigo para correr modelo de diferencias proporcionales
#### By Elena Villalobos

#### Especificaciones generales ####

# Clean workspace
rm(list=ls())

# Load Package
library(R2jags)
set.seed(77)

# Para loops
n_sub <- 25            #i, sujetos
n_que <- 24            #j, preguntas
n_rep <- 10            #k, repeticiones

# Cargar datos
load("~/Desktop/article/Datos.rdata")

# datos 
csvs_time

# Ordenar datos 
t_choice <- array(dim=c(n_sub,n_que,n_rep))
for(i in 1:n_sub){
 for(j in 1:n_que){
  t_choice[i,j,] <- subset(csvs_time[[i]][,7:8],pair==j)$biggerchosen
 }
}

# Cantidad de la recompensa pequena
x_ss <- c(5150,5300,5450,5150,5300,5150,
          6050,6200,6350,6050,6200,6050,
          5150,5600,6050,5150,5600,5150,
          1150,1250,1350,1150,1250,1150)
# Cantidad de la recompensa grande
x_ll <- c(5300,5450,5600,5450,5600,5600,
          6200,6350,6500,6350,6500,6500,
          5600,6050,6500,6050,6500,6500,
          1250,1350,1450,1350,1450,1450)
# Demora de la recompensa inmediata
t_ss <- c(1,2,3,1,2,1,
          7,8,9,7,8,7,
          1,4,7,1,4,1,
          1,4,7,1,4,1)
# Demora de la recompensa demorada
t_ll <- c(2,3,4,3,4,4,
          8,9,10,9,10,10,
          4,7,10,7,10,10,
          4,7,10,7,10,10)
# Ver preguntas
data.frame(x_ss,t_ss,x_ll,t_ll)


#### Especificaciones para JAGS ####
# Data that JAGS will use
data <- list("n_que","n_sub","n_rep","t_choice","x_ss","x_ll","t_ss","t_ll")

# Initial values
myinits = list(
 list(sigma=rep(4,n_sub), 
      delta=rep(2,n_sub),
      weight=rep(1,n_sub)))

# Model
write('model{
      
      # Precision 
      prec <- 1/(5^2)
      
      # Loop for each subject
      for (i in 1:n_sub){
      
      # Priors
      sigma[i] ~ dnorm(0,prec)T(0,)
      weight[i] ~ dnorm(0,prec)
      delta[i] ~ dnorm(0,prec)
      
      # Loop for questions
      for (j in 1:n_que){
      
      # Value for each ATTRIBUTE
      dif_x[i,j] <-   (weight[i])*(x_ll[j]-x_ss[j])
      dif_t[i,j] <- (1-weight[i])*(t_ll[j]-t_ss[j])
      
      # Difference
      dif[i,j] <- dif_x[i,j]-dif_t[i,j]
      
      # Probit function
      y[i,j] <- (dif[i,j]-delta[i])/sigma[i]
      
      # Probability of choosing the larger later reward
      t_theta[i,j] <- phi(y[i,j])
      
      # Loop for repetitions
      for (h in 1:n_rep){
      t_choice[i,j,h] ~ dbern(t_theta[i,j])
      
      }
      }
      }
      }','Model.bug')


# Parameters to estimate
parameters <- c('sigma','t_theta','weight','delta')

# The following command calls WinBUGS with specific options.
# For a detailed description see Sturtz, Ligges, & Gelman (25).
samples <- jags.parallel(data,
                         inits = myinits,
                         parameters,
                         model = 'Model.bug',
                         n.chains = 2,
                         n.iter = 1500,
                         n.burnin = 900,
                         n.thin = 2)



unlink('Model.bug')


#### Evaluacion de sampleos ####
summary(samples$BUGSoutput$summary)
names(samples$BUGSoutput$sims.list)

sigma<-samples$BUGSoutput$sims.list$sigma
delta<-samples$BUGSoutput$sims.list$delta
t_theta<-samples$BUGSoutput$sims.list$t_theta
weight<-samples$BUGSoutput$sims.list$weight

plot(samples)
