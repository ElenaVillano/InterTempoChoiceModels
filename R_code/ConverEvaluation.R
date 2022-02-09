#### 
#### Convergency Evaluation of each model
#### January, 2020 
#### By Elena Villalobos 
####

library(R2jags)

#autocorr.plot(as.mcmc(t_tau))

# Latent mixture model 
rm(list=ls())
load("~/Documents/Heavy_Stuff/article/latent_mixture_model_v1.RData")
samples$model
names(samples$BUGSoutput$sims.list)
summary(samples$BUGSoutput$summary)
layout(1:2)
hist(samples$BUGSoutput$summary[,9],breaks=100)
hist(samples$BUGSoutput$summary[,8],breaks=100)
plot(samples)


which(samples$BUGSoutput$summary[,8]>1.001)
sum(which(samples$BUGSoutput$summary[,8]>1.001))
tail(which(samples$BUGSoutput$summary[,8]>1.001),100)

row.names(samples$BUGSoutput$summary) 
row.names(samples$BUGSoutput$summary)
nomo <- grep("pred_t_choice", row.names(samples$BUGSoutput$summary))
nomi <- grep("theta", row.names(samples$BUGSoutput$summary))
totod <- c(nomo,nomi)

sum(which(samples$BUGSoutput$summary[-totod,8]>1.001))
length(which(samples$BUGSoutput$summary[-totod,8]>1.001))
length(samples$BUGSoutput$summary[-totod,8])

which(samples$BUGSoutput$summary[-totod,8]>1.001)


# Hyperboloid Model
rm(list=ls())
#load("~/Documents/Heavy_R_Stuff/PublicatedArticleModels/HyperModelArticle.RData")
samples$model
names(samples$BUGSoutput$sims.list)
summary(samples$BUGSoutput$summary)
plot(samples)

rm(list=ls())
#load("/Users/Ele/Documents/Heavy_R_Stuff/ModelamientoTesis/HiperTime.RData")

samples$model
names(samples$BUGSoutput$sims.list)
summary(samples$BUGSoutput$summary)
layout(1:2)
hist(samples$BUGSoutput$summary[,9],breaks=100)
hist(samples$BUGSoutput$summary[,8],breaks=100)
plot(samples)

# Trade-off Model 
rm(list=ls())
#load("~/Documents/Heavy_R_Stuff/PublicatedArticleModels/TradeModelArticle.RData")
names(samples$BUGSoutput$sims.list)
summary(samples$BUGSoutput$summary)
layout(1:2)
hist(samples$BUGSoutput$summary[,9],breaks=100)
hist(samples$BUGSoutput$summary[,8],breaks=100)
plot(samples)



# ITCH Model
rm(list=ls())
#load("~/Documents/Heavy_R_Stuff/PublicatedArticleModels/ITCHModelArticle.RData")
names(samples$BUGSoutput$sims.list)
summary(samples$BUGSoutput$summary)
layout(1:2)
hist(samples$BUGSoutput$summary[,9],breaks=100)
hist(samples$BUGSoutput$summary[,8],breaks=100)
samples$model
plot(samples)

rm(list=ls())
#load("~/Documents/Heavy_R_Stuff/AttributeModels/ITCHVersions/Workitch.RData")
names(samples$BUGSoutput$sims.list)
summary(samples$BUGSoutput$summary)
layout(1:2)
hist(samples$BUGSoutput$summary[,9],breaks=100)
hist(samples$BUGSoutput$summary[,8],breaks=100)
samples$model
plot(samples)

# Proportional Differences Model
rm(list=ls())
#load("~/Documents/Heavy_R_Stuff/PublicatedArticleModels/PDModelArticle.RData")
names(samples$BUGSoutput$sims.list)
summary(samples$BUGSoutput$summary)

# Direct Differences Model
rm(list=ls())
#load("~/Documents/Heavy_R_Stuff/PublicatedArticleModels/DDModelArticle.RData")
names(samples$BUGSoutput$sims.list)
summary(samples$BUGSoutput$summary)
hist(samples$BUGSoutput$summary[,9],breaks=100)
hist(samples$BUGSoutput$summary[,8],breaks=100)
plot(samples)

rm(list=ls())
#load("~/Documents/Heavy_R_Stuff/AttributeModels/PDVersions/model4.RData")
names(samples$BUGSoutput$sims.list)
summary(samples$BUGSoutput$summary)
hist(samples$BUGSoutput$summary[,9],breaks=100)
hist(samples$BUGSoutput$summary[,8],breaks=100)

