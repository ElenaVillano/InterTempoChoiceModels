#### 
#### Parameter evaluation from Trade, ITCH and DD models
#### February, 2020 
#### By Elena Villalobos 
####


#############ITCH###################
rm(list=ls())
#load("/Users/Ele/Documents/Heavy_R_Stuff/PublicatedArticleModels/ITCHModelArticle.RData")
load("/Users/Ele/Documents/Heavy_R_Stuff/AttributeModels/ITCHVersions/Workitch.RData")

names(samples$BUGSoutput$sims.list)
t_theta_trade<-samples$BUGSoutput$sims.list$t_theta_trade
beta_1<-samples$BUGSoutput$sims.list$beta_1
beta_t_A<-samples$BUGSoutput$sims.list$beta_t_A
beta_t_R<-samples$BUGSoutput$sims.list$beta_t_R
beta_x_R<-samples$BUGSoutput$sims.list$beta_x_R
beta_x_A<-samples$BUGSoutput$sims.list$beta_x_A

# Needed things
source("~/Google Drive/Nube/Intertemporal_Risky_Choice_Project/R_Functions/latestfunciontsTOT.R")
source("~/Google Drive/Nube/Intertemporal_Risky_Choice_Project/R_Functions/PosterPlots.R")
setwd("~/Google Drive/Nube/Intertemporal_Risky_Choice_Project/Article_Tesina/Texto_Article/")
library('extrafont')
library('png')


# Convertion for prediction
mean_t_trade_theta <- array(NA,dim=c(n_sub,n_que))
predicha_t_trade<- array(NA,dim=c(n_sub,n_que,n_rep))
for(i in 1:n_sub){
 for(j in 1:n_que){
  mean_t_trade_theta[i,j] <- mean(t_theta_trade[,i,j])
  for(k in 1:n_rep){
   predicha_t_trade[i,j,k] <- rbinom(1,1,mean_t_trade_theta[i,j]) 
  }
 }
}

# Participants Order
t_total_unos <- NA
for(i in 1:n_sub){
 t_total_unos[i] <- sum(t_choice[i,,])
}
unos_part_t <- cbind(t_total_unos,parti=c(1:25))
t_unos_ordenados <- unos_part_t[order(t_total_unos),]
t_choice_clean <- t_choice[,c(1,2,3,7,8,9,4,5,10,11,13:18,19:24),]
t_predic_clean_tr <- predicha_t_trade[,c(1,2,3,7,8,9,4,5,10,11,13:18,19:24),]

pos_preguntas <- c(1,seq(10,220,10))
tam_linea <- 1.8

actual_choice <- t_choice_clean
predicted_choice <- t_predic_clean_tr

# Taking means from each parameter
media_var <- summary(beta_x_A)[4,]
me_var <- as.numeric(gsub("Mean   :","",media_var))
orden_var <- order(me_var)

me_b_t_A <- summary(beta_t_A)[4,]
me_b_t_A <- as.numeric(gsub("Mean   :","",me_b_t_A))

me_b_x_A <- summary(beta_x_A)[4,]
me_b_x_A <- as.numeric(gsub("Mean   :","",me_b_x_A))

me_b_t_R <- summary(beta_t_R)[4,]
me_b_t_R <- as.numeric(gsub("Mean   :","",me_b_t_R))

me_b_x_R <- summary(beta_x_R)[4,]
me_b_x_R <- as.numeric(gsub("Mean   :","",me_b_x_R))


pdf('parametros_ITCH_article.pdf',height = 6.5,width=6)

layout(matrix(c(1:4),ncol=4)) 
par(mar=c(3,2.5,0.5,0.5),oma=c(1,1.1,1,0))

values_densidades_vert(beta_x_A, c(-0.01,0.03),expression(beta[x_A]),'#20B2AA',T,c(0,0.01,0.02))
mtext('Posteriores',1,col=gray[5],line=2.5,cex=0.7)
mtext('Participantes',2,col=gray[5],line=2.5,cex=0.8)
values_densidades_vert(beta_t_A, c(-5,1),expression(beta[t_A]),'#20B2AA',T,c(-4,-2,0))
values_densidades_vert(beta_x_R, c(-5,1),expression(beta[x_R]),'#20B2AA',T,c(-4,-2,0))
values_densidades_vert(beta_t_R, c(-0.3,0.3),expression(beta[t_R]),'#20B2AA',T,c(-0.1,0,0.1))

dev.off()




# pdf('jo_betas_article.pdf',width = 6,height = 5.5)
# layout(matrix(c(1,2,3,4),ncol=2,byrow=T))
# par(mar=c(2.5,2.5,0,0),oma=c(1,2,1,0))
# 
# plot(0,type='n',xlim=c(-0.005,0.01), ylim=c(-0.15,0.15),axes=F,xlab='',ylab='')
# axis(1,col='gray48',col.axis='gray48',cex.axis=0.6)
# axis(2,las=2,col='gray48',col.axis='gray48',cex.axis=0.6,tck=-0.03,hadj=0.9)
# mtext(expression(beta[t_R]),2,line=2,col='gray48',las=2,cex=1.5)
# 
# lines(c(0,0),c(-0.15,0.15),col='gray90')
# lines(c(0.005,0.005),c(-0.15,0.15),col='gray90')
# 
# lines(c(-0.005,0.01),c(0.05,0.05),col='gray90')
# lines(c(-0.005,0.01),c(0,0),col='gray90')
# lines(c(-0.005,0.01),c(-0.05,-0.05),col='gray90')
# 
# partidos <- c(1:25)
# points(me_b_x_A,me_b_t_R,col='#20B2AA33',bg='#20B2AA33',pch=21,cex=3)
# text(me_b_x_A,me_b_t_R,labels=paste(partidos),cex=0.6,col='gray40')
# 
# 
# 
# plot(0,type='n',xlim=c(-3.5,1), ylim=c(-0.15,0.15),axes=F,xlab='',ylab='')
# axis(1,col='gray48',col.axis='gray48',cex.axis=0.6)
# axis(2,las=2,col='gray48',col.axis='gray48',cex.axis=0.6,tck=-0.03,hadj=0.9)
# #mtext(expression(beta[t_R]),2,line=2.2,col='gray48',las=2,cex=2)
# 
# lines(c(0,0),c(-0.15,0.15),col='gray90')
# lines(c(-1,-1),c(-0.15,0.15),col='gray90')
# lines(c(-2,-2),c(-0.15,0.15),col='gray90')
# 
# lines(c(-3.5,1),c(0.05,0.05),col='gray90')
# lines(c(-3.5,1),c(0,0),col='gray90')
# lines(c(-3.5,1),c(-0.05,-0.05),col='gray90')
# 
# partidos <- c(1:25)
# points(me_b_x_R,me_b_t_R,col='#20B2AA33',bg='#20B2AA33',pch=21,cex=3)
# text(me_b_x_R,me_b_t_R,labels=paste(partidos),cex=0.6,col='gray40')
# 
# 
# 
# 
# plot(0,type='n',xlim=c(-0.005,0.01), ylim=c(-4,0.5),axes=F,xlab='',ylab='')
# axis(1,col='gray48',col.axis='gray48',cex.axis=0.6)
# axis(2,las=2,col='gray48',col.axis='gray48',cex.axis=0.6,tck=-0.03,hadj=0.9)
# mtext(expression(beta[x_A]),1,line=2.2,col='gray48',cex=1.5)
# mtext(expression(beta[t_A]),2,line=2,col='gray48',las=2,cex=1.5)
# 
# lines(c(0,0),c(-4,0.5),col='gray90')
# lines(c(0.005,0.005),c(-4,0.5),col='gray90')
# 
# lines(c(-0.005,0.01),c(0,0),col='gray90')
# lines(c(-0.005,0.01),c(-1,-1),col='gray90')
# lines(c(-0.005,0.01),c(-2,-2),col='gray90')
# 
# partidos <- c(1:25)
# points(me_b_x_A,me_b_t_A,col='#20B2AA33',bg='#20B2AA33',pch=21,cex=3)
# text(me_b_x_A,me_b_t_A,labels=paste(partidos),cex=0.6,col='gray40')
# 
# 
# 
# plot(0,type='n',xlim=c(-3.5,1), ylim=c(-4,0.5),axes=F,xlab='',ylab='')
# axis(1,col='gray48',col.axis='gray48',cex.axis=0.6)
# axis(2,las=2,col='gray48',col.axis='gray48',cex.axis=0.6,tck=-0.03,hadj=0.9)
# mtext(expression(beta[x_R]),1,line=2.2,col='gray48',cex=1.5)
# 
# lines(c(0,0)  ,c(-4,0.5),col='gray90')
# lines(c(-1,-1),c(-4,0.5),col='gray90')
# lines(c(-2,-2),c(-4,0.5),col='gray90')
# 
# lines(c(-3.5,1),c(0,0),col='gray90')
# lines(c(-3.5,1),c(-1,-1),col='gray90')
# lines(c(-3.5,1),c(-2,-2),col='gray90')
# 
# partidos <- c(1:25)
# points(me_b_x_R,me_b_t_A,col='#20B2AA33',bg='#20B2AA33',pch=21,cex=3)
# text(me_b_x_R,me_b_t_A,labels=paste(partidos),cex=0.6,col='gray40')
# 
# dev.off()






#############DD###################
rm(list=ls())

# Modelo de diferencias directas
load("/Users/Ele/Documents/Heavy_R_Stuff/PublicatedArticleModels/DDModelArticle.RData")
names(samples$BUGSoutput$sims.list)
t_theta_trade<-samples$BUGSoutput$sims.list$t_theta_trade
weight<-samples$BUGSoutput$sims.list$weight
delta<-samples$BUGSoutput$sims.list$delta
sigma<-samples$BUGSoutput$sims.list$sigma


# cosas que necesitas 
source("~/Google Drive/Nube/Intertemporal_Risky_Choice_Project/R_Functions/latestfunciontsTOT.R")
source("~/Google Drive/Nube/Intertemporal_Risky_Choice_Project/R_Functions/PosterPlots.R")
setwd("~/Google Drive/Nube/Intertemporal_Risky_Choice_Project/Article/Texto/")
library('extrafont')
library('png')


# Convertion for prediction
mean_t_trade_theta <- array(NA,dim=c(n_sub,n_que))
predicha_t_trade<- array(NA,dim=c(n_sub,n_que,n_rep))
for(i in 1:n_sub){
  for(j in 1:n_que){
    mean_t_trade_theta[i,j] <- mean(t_theta_trade[,i,j])
    for(k in 1:n_rep){
      predicha_t_trade[i,j,k] <- rbinom(1,1,mean_t_trade_theta[i,j]) 
    }
  }
}

# Orden de participantes
t_total_unos <- NA
for(i in 1:n_sub){
  t_total_unos[i] <- sum(t_choice[i,,])
}
unos_part_t <- cbind(t_total_unos,parti=c(1:25))
t_unos_ordenados <- unos_part_t[order(t_total_unos),]
t_choice_clean <- t_choice[,c(1,2,3,7,8,9,4,5,10,11,13:18,19:24),]
t_predic_clean_tr <- predicha_t_trade[,c(1,2,3,7,8,9,4,5,10,11,13:18,19:24),]

pos_preguntas <- c(1,seq(10,220,10))
tam_linea <- 1.8

actual_choice <- t_choice_clean
predicted_choice <- t_predic_clean_tr

# ORDEN MEDIAS
media_var <- summary(delta)[4,]
me_var <- as.numeric(gsub("Mean   :","",media_var))
orden_var <- order(me_var)


pdf('parametros_DD_article.pdf',height = 6.5,width=6)
layout(matrix(c(1:3),ncol=3)) 
par(mar=c(3,2.5,0.5,0.5),oma=c(1,1.1,1,1))
values_densidades_vert(sigma, c(0,10),expression(sigma),'#20B2AA',T,c(1,3,5,7,9))
mtext('Posteriores',1,col=gray[5],line=2.5,cex=0.7)
mtext('Participantes',2,col=gray[5],line=2.5,cex=0.8)
values_densidades_vert(delta, c(-20,10),expression(delta),'#20B2AA',T,c(0,-2.5,-5,12,12))
values_densidades_vert(weight, c(-0.02,0.01),expression(w),'#20B2AA',T,c(0,0.005,-0.005,7,9))


dev.off()
# 


me_del <- summary(delta)[4,]
me_del <- as.numeric(gsub("Mean   :","",me_del))

me_sig <- summary(sigma)[4,]
me_sig <- as.numeric(gsub("Mean   :","",me_sig))

me_wei <- summary(weight)[4,]
me_wei <- as.numeric(gsub("Mean   :","",me_wei))


pdf('jo_sigma_delta_article.pdf',width = 6,height = 5.5)
layout(matrix(c(1,0,2,3),ncol=2,byrow=T))
par(mar=c(2.5,2.5,0,0),oma=c(1,1.5,1,0.5))

plot(0,type='n',xlim=c(0,6), ylim=c(-15,10),axes=F,xlab='Sigma',ylab='Delta')
axis(1,col='gray48',col.axis='gray48',cex.axis=0.6)
axis(2,las=2,col='gray48',col.axis='gray48',cex.axis=0.6,tck=-0.03,hadj=0.9)
mtext(expression(delta),2,line=2.2,col='gray48',las=2,cex=2)

lines(c(5,5),c(-15,10),col='gray90')
lines(c(3,3),c(-15,10),col='gray90')
lines(c(1,1),c(-15,10),col='gray90')

lines(c(0,6),c(0,0),col='gray90')
lines(c(0,6),c(-5,-5),col='gray90')
lines(c(0,6),c(-10,-10),col='gray90')

partidos <- c(1:25)
points(me_sig,me_del,col='#20B2AA33',bg='#20B2AA33',pch=21,cex=3)
text(me_sig,me_del,labels=paste(partidos),cex=0.6,col='gray40')


plot(0,type='n',xlim=c(0,6), ylim=c(-0.01,0.01),axes=F,xlab='Sigma',ylab='Weight')
axis(1,col='gray48',col.axis='gray48',cex.axis=0.6)
axis(2,las=2,col='gray48',col.axis='gray48',cex.axis=0.6,tck=-0.03,hadj=0.9)
partidos <- c(1:25)
mtext('w',2,line=2.5,col='gray48',las=2,cex=2)
mtext(expression(sigma),1,line=2.2,col='gray48',cex=2)
lines(c(5,5),c(-15,10),col='gray90')
lines(c(3,3),c(-15,10),col='gray90')
lines(c(1,1),c(-15,10),col='gray90')

lines(c(0,6),c(0,0),col='gray90')
lines(c(0,6),c(0.005,0.005),col='gray90')
lines(c(0,6),c(-0.005,-0.005),col='gray90')

points(me_sig,me_wei,col='#20B2AA33',bg='#20B2AA33',pch=21,cex=3)
text(me_sig,me_wei,labels=paste(partidos),cex=0.6,col='gray40')

plot(0,type='n',xlim=c(-15,10), ylim=c(-0.01,0.01),axes=F,xlab='Delta',ylab='Weight')
axis(1,col='gray48',col.axis='gray48',cex.axis=0.6)
axis(2,las=2,col='gray48',col.axis='gray48',cex.axis=0.6,tck=-0.03,hadj=0.9)
mtext(expression(delta),1,line=2.2,col='gray48',cex=2)

lines(c(0,0),c(-0.010,0.010),col='gray90')
lines(c(-5,-5),c(-0.010,0.010),col='gray90')
lines(c(-10,-10),c(-0.010,0.010),col='gray90')

lines(c(-15,10),c(0,0),col='gray90')
lines(c(-15,10),c(0.005,0.005),col='gray90')
lines(c(-15,10),c(-0.005,-0.005),col='gray90')

partidos <- c(1:25)
points(me_del,me_wei,col='#20B2AA33',bg='#20B2AA33',pch=21,cex=3)
text(me_del,me_wei,labels=paste(partidos),cex=0.6,col='gray40')

dev.off()

# 
# 
# 
# 




#############TradeOff###################
rm(list=ls())
load("~/Documents/Heavy_R_Stuff/PublicatedArticleModels/TradeModelArticle.RData")

names(samples$BUGSoutput$sims.list)
t_theta_trade<-samples$BUGSoutput$sims.list$t_theta_trade
t_gamma<-samples$BUGSoutput$sims.list$t_gamma
t_kappa<-samples$BUGSoutput$sims.list$t_kappa
t_vartheta<-samples$BUGSoutput$sims.list$t_vartheta
t_tau<-samples$BUGSoutput$sims.list$t_tau


# cosas que necesitas 
source("~/Google Drive/Nube/Intertemporal_Risky_Choice_Project/R_Functions/latestfunciontsTOT.R")
source("~/Google Drive/Nube/Intertemporal_Risky_Choice_Project/R_Functions/PosterPlots.R")
setwd("~/Google Drive/Nube/Intertemporal_Risky_Choice_Project/Article_Tesina/Texto_Article/")
library('extrafont')
library('png')


# Convertion for prediction
mean_t_trade_theta <- array(NA,dim=c(n_sub,n_que))
predicha_t_trade<- array(NA,dim=c(n_sub,n_que,n_rep))
for(i in 1:n_sub){
 for(j in 1:n_que){
  mean_t_trade_theta[i,j] <- mean(t_theta_trade[,i,j])
  for(k in 1:n_rep){
   predicha_t_trade[i,j,k] <- rbinom(1,1,mean_t_trade_theta[i,j]) 
  }
 }
}

# Orden de participantes
t_total_unos <- NA
for(i in 1:n_sub){
 t_total_unos[i] <- sum(t_choice[i,,])
}
unos_part_t <- cbind(t_total_unos,parti=c(1:25))
t_unos_ordenados <- unos_part_t[order(t_total_unos),]
t_choice_clean <- t_choice[,c(1,2,3,7,8,9,4,5,10,11,13:18,19:24),]
t_predic_clean_tr <- predicha_t_trade[,c(1,2,3,7,8,9,4,5,10,11,13:18,19:24),]

pos_preguntas <- c(1,seq(10,220,10))
tam_linea <- 1.8

actual_choice <- t_choice_clean
predicted_choice <- t_predic_clean_tr

# Taking means from each parameter
media_var <- summary(t_vartheta)[4,]
me_var <- as.numeric(gsub("Mean   :","",media_var))
orden_var <- order(me_var)

me_gamma <- summary(t_gamma)[4,]
me_gamma <- as.numeric(gsub("Mean   :","",me_gamma))

me_tau <- summary(t_tau)[4,]
me_tau <- as.numeric(gsub("Mean   :","",me_tau))

me_kappa <- summary(t_kappa)[4,]
me_kappa <- as.numeric(gsub("Mean   :","",me_kappa))


pdf('parametros_trade_article.pdf',height = 6.5,width=6)

layout(matrix(c(1:4),ncol=4)) 
par(mar=c(3,2.5,0.5,0.5),oma=c(1,1.1,1,0))

values_densidades_vert(t_vartheta, c(0,6),expression(vartheta),'#20B2AA',T,c(2,3,4))
mtext('Posteriors',1,col=gray[5],line=2.5,cex=0.7)
mtext('Participants',2,col=gray[5],line=2.5,cex=0.8)
values_densidades_vert(t_tau, c(0,0.2),expression(tau),'#20B2AA',T,c(0.025,0.05,0.075))
values_densidades_vert(t_gamma, c(0,80),expression(gamma),'#20B2AA',T,c(1,10,20))
values_densidades_vert(t_kappa, c(0,80),expression(kappa),'#20B2AA',T,c(1,10,20))

dev.off()


# 


me_del <- summary(delta)[4,]
me_del <- as.numeric(gsub("Mean   :","",me_del))

me_sig <- summary(sigma)[4,]
me_sig <- as.numeric(gsub("Mean   :","",me_sig))

me_wei <- summary(weight)[4,]
me_wei <- as.numeric(gsub("Mean   :","",me_wei))


pdf('jo_sigma_delta_article.pdf',width = 6,height = 5.5)
layout(matrix(c(1,0,2,3),ncol=2,byrow=T))
par(mar=c(2.5,2.5,0,0),oma=c(1,1.5,1,0.5))

plot(0,type='n',xlim=c(0,6), ylim=c(-15,10),axes=F,xlab='Sigma',ylab='Delta')
axis(1,col='gray48',col.axis='gray48',cex.axis=0.6)
axis(2,las=2,col='gray48',col.axis='gray48',cex.axis=0.6,tck=-0.03,hadj=0.9)
mtext(expression(delta),2,line=2.2,col='gray48',las=2,cex=2)

lines(c(5,5),c(-15,10),col='gray90')
lines(c(3,3),c(-15,10),col='gray90')
lines(c(1,1),c(-15,10),col='gray90')

lines(c(0,6),c(0,0),col='gray90')
lines(c(0,6),c(-5,-5),col='gray90')
lines(c(0,6),c(-10,-10),col='gray90')

partidos <- c(1:25)
points(me_sig,me_del,col='#20B2AA33',bg='#20B2AA33',pch=21,cex=3)
text(me_sig,me_del,labels=paste(partidos),cex=0.6,col='gray40')


plot(0,type='n',xlim=c(0,6), ylim=c(-0.01,0.01),axes=F,xlab='Sigma',ylab='Weight')
axis(1,col='gray48',col.axis='gray48',cex.axis=0.6)
axis(2,las=2,col='gray48',col.axis='gray48',cex.axis=0.6,tck=-0.03,hadj=0.9)
partidos <- c(1:25)
mtext('w',2,line=2.5,col='gray48',las=2,cex=2)
mtext(expression(sigma),1,line=2.2,col='gray48',cex=2)
lines(c(5,5),c(-15,10),col='gray90')
lines(c(3,3),c(-15,10),col='gray90')
lines(c(1,1),c(-15,10),col='gray90')

lines(c(0,6),c(0,0),col='gray90')
lines(c(0,6),c(0.005,0.005),col='gray90')
lines(c(0,6),c(-0.005,-0.005),col='gray90')

points(me_sig,me_wei,col='#20B2AA33',bg='#20B2AA33',pch=21,cex=3)
text(me_sig,me_wei,labels=paste(partidos),cex=0.6,col='gray40')

plot(0,type='n',xlim=c(-15,10), ylim=c(-0.01,0.01),axes=F,xlab='Delta',ylab='Weight')
axis(1,col='gray48',col.axis='gray48',cex.axis=0.6)
axis(2,las=2,col='gray48',col.axis='gray48',cex.axis=0.6,tck=-0.03,hadj=0.9)
mtext(expression(delta),1,line=2.2,col='gray48',cex=2)

lines(c(0,0),c(-0.010,0.010),col='gray90')
lines(c(-5,-5),c(-0.010,0.010),col='gray90')
lines(c(-10,-10),c(-0.010,0.010),col='gray90')

lines(c(-15,10),c(0,0),col='gray90')
lines(c(-15,10),c(0.005,0.005),col='gray90')
lines(c(-15,10),c(-0.005,-0.005),col='gray90')

partidos <- c(1:25)
points(me_del,me_wei,col='#20B2AA33',bg='#20B2AA33',pch=21,cex=3)
text(me_del,me_wei,labels=paste(partidos),cex=0.6,col='gray40')

dev.off()

# 
# 
# 
# 
