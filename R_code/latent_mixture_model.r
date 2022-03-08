##### Latent Mixture Model for evaluation
##### Noviembre 2021
##### By Elena Villalobos Nolasco

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

#hofdsa
# Cargar datos
load("~/Documents/InterTempoChoiceModels/Data/Datos.rdata")

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
  list(
    # ITCH
    beta_1=rep(0.2,n_sub),
    beta_x_A=rep(0.01,n_sub),
    beta_x_R=rep(0.02,n_sub),
    beta_t_A=rep(0.03,n_sub),
    beta_t_R=rep(0.03,n_sub),
    # DD
    sigma=rep(4,n_sub), 
    delta=rep(2,n_sub),
    weight=rep(1,n_sub),
    # TRADE
    t_kappa=rep(0.5,n_sub),
    t_vartheta=rep(1.5,n_sub),
    t_gamma=rep(0.9,n_sub),
    t_tau=rep(0.3,n_sub),
    t_epsilon_trade=0.1))



write('model{
      
      prec <- 1/(5^2)
      
      phi_z ~ ddirich(rep(1,3))

      t_epsilon_trade ~ dlnorm(0,prec)
      
      for(i in 1:n_sub){
      
        z[i] ~ dcat(phi_z)
        
        # ITCH
        beta_1[i] ~ dnorm(0,prec)
        beta_x_A[i] ~ dnorm(0,prec)
        beta_x_R[i] ~ dnorm(0,prec)
        beta_t_A[i] ~ dnorm(0,prec)
        beta_t_R[i] ~ dnorm(0,prec)
        
        # DD
        sigma[i] ~ dnorm(0,prec)T(0,)
        weight[i] ~ dnorm(0,prec)
        delta[i] ~ dnorm(0,prec)
        
        # TRADE
        t_gamma[i] ~ dlnorm(0,prec)
        t_tau[i] ~ dlnorm(0,prec)
        t_kappa[i] ~ dlnorm(0,prec)
        t_vartheta[i] ~ dlnorm(1,prec)
          
      
        for(j in 1:n_que){
        
          # ITCH MODEL
          # functions
          dif_x_A[i,j] <- beta_x_A[i]*(x_ll[j]-x_ss[j])
          dif_x_R[i,j] <- beta_x_R[i]*((x_ll[j]-x_ss[j])/(0.5*(x_ll[j]+x_ss[j])))
          dif_t_A[i,j] <- beta_t_A[i]*(t_ll[j]-t_ss[j])
          dif_t_R[i,j] <- beta_t_R[i]*((t_ll[j]-t_ss[j])/(0.5*(t_ll[j]+t_ss[j])))
          y_itch[i,j] <- beta_1[i]+dif_x_A[i,j]+dif_x_R[i,j]+dif_t_A[i,j]+dif_t_R[i,j]
      
          theta[i,j,1] <- phi(y_itch[i,j])
          
          # DD MODEL
          # functions
          dd_x_R[i,j] <-   (weight[i])*(x_ll[j]-x_ss[j])
          dd_t_R[i,j] <- (1-weight[i])*(t_ll[j]-t_ss[j])
          dif[i,j] <- dd_x_R[i,j]-dd_t_R[i,j]
          y_dd[i,j] <- (dif[i,j]-delta[i])/sigma[i]
          
          theta[i,j,2] <- phi(y_dd[i,j])
          
          # TRADE MODEL
          # functions
          v_ll[i,j] <- (1/t_gamma[i])*log(1+t_gamma[i]*x_ll[j])        
          v_ss[i,j] <- (1/t_gamma[i])*log(1+t_gamma[i]*x_ss[j])
          w_ll[i,j] <- (1/t_tau[i])*log(1+t_tau[i]*t_ll[j])
          w_ss[i,j] <- (1/t_tau[i])*log(1+t_tau[i]*t_ss[j])
          q_value[i,j] <- v_ll[i,j]-v_ss[i,j]
          q_time[i,j] <- (t_kappa[i])*log(1+((w_ll[i,j]-w_ss[i,j])/t_vartheta[i])^t_vartheta[i])
          
          theta[i,j,3] <- (q_value[i,j]^(1/t_epsilon_trade))/((q_value[i,j]^(1/t_epsilon_trade))+(q_time[i,j]^(1/t_epsilon_trade)))
      
          # For repetitions
          for(r in 1:n_rep){
            t_choice[i,j,r] ~ dbern(theta[i,j,z[i]])
            pred_t_choice[i,j,r] ~ dbern(theta[i,j,z[i]])
          }
  
      }    
      
      }
}','latent_model.bug')


# Parameters to estimate                       
parameters <- c('z','theta','pred_t_choice',
                'sigma','weight','delta',
                'beta_x_A','beta_x_R','beta_t_A','beta_t_R',
                't_gamma','t_tau','t_vartheta','t_kappa')

# The following command calls WinBUGS with specific options.
# For a detailed description see Sturtz, Ligges, & Gelman (25).
samples <- jags.parallel(data,
                inits = myinits,
                parameters,          
                model = 'latent_model.bug',
                n.chains = 2,
                n.iter = 4500000,
                n.burnin = 4300000,
                n.thin = 20)

unlink('latent_model.bug')

# cuantos sampleos
((4500000-4300000)/20)
# cuantas horas
((24*4500000)/100000)/60

save.image("~/Documents/Heavy_Stuff/article/latent_mixture_model_v1.RData")


load("~/Documents/Heavy_Stuff/article/latent_mixture_model_v1.RData")


summary(samples$BUGSoutput$summary)

plot(samples)

theta<-samples$BUGSoutput$sims.list$theta
z<-samples$BUGSoutput$sims.list$z
dim(z)
summary(z)

layout(matrix(c(1:25),nrow=5,byrow=T))


for(i in 1:n_sub){
  hist(z[,i],xlim=c(0.5,3.5), breaks=seq(0.5,3.5,0.2),axes=F)
  axis(1, at=c(1,2,3), lwd.ticks = 0 )
  
}

layout(1)

sigma<-samples$BUGSoutput$sims.list$sigma
delta<-samples$BUGSoutput$sims.list$delta
weight<-samples$BUGSoutput$sims.list$weight


beta_t_A<-samples$BUGSoutput$sims.list$beta_t_A
beta_t_R<-samples$BUGSoutput$sims.list$beta_t_R
beta_x_R<-samples$BUGSoutput$sims.list$beta_x_R
beta_x_A<-samples$BUGSoutput$sims.list$beta_x_A

t_gamma<-samples$BUGSoutput$sims.list$t_gamma
t_kappa<-samples$BUGSoutput$sims.list$t_kappa
t_vartheta<-samples$BUGSoutput$sims.list$t_vartheta
t_tau<-samples$BUGSoutput$sims.list$t_tau

autocorr.plot(as.mcmc(t_tau))
autocorr.plot(as.mcmc(t_kappa))

# los valores para n.eff es el tamaño de muestra efectiva, 
# entre menores sean peor, deben de ser equivalentes a la muestra de sampleos
# en las cadenas. 
layout(1)
hist(samples$BUGSoutput$summary[,9],breaks = 100)
hist(samples$BUGSoutput$summary[,8],breaks = 100)
which(samples$BUGSoutput$summary[,8]>1.001)

tail(which(samples$BUGSoutput$summary[,8]>1.001),100)

