# Latent mixture model evaluation


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
  axis(1, at=c(1,2,3), lwd.ticks = 0, )
  
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

