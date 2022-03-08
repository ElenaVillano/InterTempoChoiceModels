# Latent mixture model evaluation

rm(list=ls())

#load("~/Documents/Heavy_Stuff/article/latent_mixture_model_v1.RData")
load("/Volumes/MemoriaEle/HeavyData/Heavy_Stuff/article/latent_mixture_model_v1.RData")

#plot(samples)

setwd('~/Documents/InterTempoChoiceModels/')

# Needed things
source("R_code/usefull_functions_r/latestfunciontsTOT.R")
source("R_code/usefull_functions_r/PosterPlots.R")
source("R_code/usefull_functions_r/functions.R")

library('extrafont')
library('png')


# los valores para n.eff es el tamaño de muestra efectiva, 
# entre menores sean peor, deben de ser equivalentes a la muestra de sampleos
# en las cadenas. 

row.names(samples$BUGSoutput$summary)
nomo <- grep("pred_t_choice", row.names(samples$BUGSoutput$summary))
nomi <- grep("theta", row.names(samples$BUGSoutput$summary))
totod <- c(nomo,nomi)

sum(which(samples$BUGSoutput$summary[-totod,8]>1.001))
length(which(samples$BUGSoutput$summary[-totod,8]>1.001))
length(samples$BUGSoutput$summary[-totod,8])

parametros_conver <- samples$BUGSoutput$summary[-totod,c(8,9)]
parametros_conver
length(which(parametros>1.001))


hist(parametros,breaks=100)
parametros
samples$BUGSoutput$summary[-totod,]

####################

theta <- samples$BUGSoutput$sims.list$theta
deviance <-  samples$BUGSoutput$sims.list$deviance


pdf(file='images/trace.pdf',height = 8, width = 8)
plot(samples$BUGSoutput$sims.array[,1,'deviance'],type='l',col='#E3597299',
     xlab='deviance',ylab='')
lines(samples$BUGSoutput$sims.array[,2,'deviance'],type='l',col='#48C9B099')
dev.off()


pred_t_choice <- samples$BUGSoutput$sims.list$pred_t_choice
z<-samples$BUGSoutput$sims.list$z
dim(z)
summary(z)

predicha_t <- array(NA,dim=c(n_sub,n_que,n_rep))

for(i in 1:n_sub){
  for(j in 1:n_que){
    for(k in 1:n_rep){
      predicha_t[i,j,k] <- strtoi(names(sort(table(pred_t_choice[,i,j,k]), decreasing = TRUE)[1]))
    }
  }
}


t_total_unos <- NA
for(i in 1:n_sub){
  t_total_unos[i] <- sum(t_choice[i,,])
}
unos_part_t <- cbind(t_total_unos,parti=c(1:25))
orden_partici <- unos_part_t[order(t_total_unos),]
t_choice_clean <- t_choice[,c(1,2,3,7,8,9,4,5,10,11,13:18,19:24),]
t_predic_clean <- predicha_t[,c(1,2,3,7,8,9,4,5,10,11,13:18,19:24),]
actual_choice <- t_choice_clean
t_predic_clean_tr <- t_predic_clean

pos_preguntas <- c(1,seq(10,220,10))
tam_linea <- 1.8

theta_1 <- theta[,,,1]
theta_2 <- theta[,,,2]
theta_3 <- theta[,,,3]



pdf(file='images/zts.pdf',height = 8, width = 8)
layout(matrix(c(1:25),nrow=5,byrow=T))
par(mar=c(3,2,1,0.5),oma=c(0,1.1,0,0))
for(i in 1:n_sub){
  plot(0,type='n',xlim=c(0.5,3.5), ylim=c(0,25000),axes=F,xlab='',ylab='')
  axis(1, at=c(0,1,2,3,4), lwd.ticks = 0.01)
  text(1.5,10000,i, cex=3,col='#BDACAF99')
  hist(z[,i], xlim=c(0,4),main='', breaks=seq(0.75,3.7,0.5),
       axes=F, add=T, ylim=c(0,25000), col='#CE6173F2', border='white')
}
dev.off()


names(samples$BUGSoutput$sims.list)

t_gamma<-samples$BUGSoutput$sims.list$t_gamma
t_kappa<-samples$BUGSoutput$sims.list$t_kappa
t_vartheta<-samples$BUGSoutput$sims.list$t_vartheta
t_tau<-samples$BUGSoutput$sims.list$t_tau

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

# Max values
q3_var <- summary(t_vartheta)[5,]
q3_var <- as.numeric(gsub("3rd Qu.:","",q3_var))
orden_var_max <- order(max_var)

q3_gamma <- summary(t_gamma)[5,]
q3_gamma <- as.numeric(gsub("3rd Qu.:","",q3_gamma))

q3_tau <- summary(t_tau)[5,]
q3_tau <- as.numeric(gsub("3rd Qu.:","",q3_tau))

q3_kappa <- summary(t_kappa)[5,]
q3_kappa <- as.numeric(gsub("3rd Qu.:","",q3_kappa))



pdf('images/parametros_trade_article.pdf',height = 6.5,width=6)

layout(matrix(c(1:4),ncol=4)) 
par(mar=c(3,2.5,0.5,0.5),oma=c(1,1.1,1,0))

values_densidades_vert(t_vartheta, c(0,max(q3_var)),expression(vartheta),'#20B2AA',T,c(10,20,30))
mtext('Posteriors',1,col=gray[5],line=2.5,cex=0.7)
mtext('Participants',2,col=gray[5],line=2.5,cex=0.8)
values_densidades_vert(t_tau, c(0,max(q3_tau)),expression(tau),'#20B2AA',T,c(0.025,10,20))
values_densidades_vert(t_gamma, c(0,max(q3_gamma)),expression(gamma),'#20B2AA',T,c(100,200,600))
values_densidades_vert(t_kappa, c(0,max(q3_kappa)),expression(kappa),'#20B2AA',T,c(500,1000,2000))

dev.off()



pdf('images/parametros_trade_article_axesdif.pdf',height = 6.5,width=6)

layout(matrix(c(1:4),ncol=4)) 
par(mar=c(3,2.5,0.5,0.5),oma=c(1,1.1,1,0))

values_densidades_vert(t_vartheta, c(0,10),expression(vartheta),'#20B2AA',T,c(10,20,30))
mtext('Posteriors',1,col=gray[5],line=2.5,cex=0.7)
mtext('Participants',2,col=gray[5],line=2.5,cex=0.8)
values_densidades_vert(t_tau, c(0,1),expression(tau),'#20B2AA',T,c(0.025,10,20))
values_densidades_vert(t_gamma, c(0,10),expression(gamma),'#20B2AA',T,c(100,200,600))
values_densidades_vert(t_kappa, c(0,10),expression(kappa),'#20B2AA',T,c(500,1000,2000))

dev.off()



######################




pdf(file='images/ArticleGrand.pdf',width = 6.5,height =4.7 )
tam_linea <- 0.94

layout(matrix(c(1,1,2,2,
                3,3,4,4),ncol=4,byrow=T))
par(mar=c(1.2,1.5,1,0.2),oma=c(1,1,2,0))
set.seed(50)
predicciones_datos(T,actual_choice,theta_1,'','Data')
mtext('Questions',1,cex=0.5, line=1,col='gray48')
mtext('Participants',2,cex=0.5, line=1.3,col='gray48')
predicciones_datos(F,actual_choice,theta_1,'','')
mtext('ITCH (theta_1)',2,cex=1, line=1.3,col='gray48')
predicciones_datos(F,actual_choice,theta_2,'','')
mtext('DD (theta_2)',2,cex=1, line=1.2,col='gray48')
predicciones_datos(F,actual_choice,theta_3,'','')
mtext('Trade-off (theta_3)',2,cex=1, line=1.3,col='gray48')

dev.off()




i <- 1
file <- paste('images/representaDD',i,'.pdf',sep='')

start_poster(file,6.5,7,rep(0,4))
# 
new_plot(which_point = 'center_center',
         coordinates = c(0,2.5),
         width = 6.3,height = 1,
         label='plot_1')
tam_linea <- 1.1

plot(0,type='n',xlim=c(0,220),ylim=c(0.2,1),
     axes=F,xlab='',ylab='')
axis(3,line=0,at=seq(0,220,10),tck=0.06,col.axis='white',cex.axis=0.7,
     col='gray48',padj=0,adj=0.9,
     labels=paste(c(1,2,3,7,8,9,4,5,10,11,13,14,15,16,17,18,19,20,21,22,23,24,""),sep="")) 
axis(3,line=-0.5,at=seq(0,220,10),tck=0.06,col.axis='gray48',cex.axis=0.7,
     col='white',padj=0,adj=0.9,
     labels=paste(c(1,2,3,7,8,9,4,5,10,11,13,14,15,16,17,18,19,20,21,22,23,24,""),sep="")) 
axis(3,line=0,at=c(0,60,100,160,220),
     labels=c('','','','',''),tck=0.1,lwd = 2,col='gray48')
mtext(text = 'Intervalos Peque\u{00F1}os',side=3,adj=0.08,padj=-3.5, col='gray48',cex=0.7)
mtext(text = 'Intervalos Medianos'       ,side=3,adj=0.35,padj=-3.5, col='gray48',cex=0.7)
mtext(text = 'Intervalos Grandes'        ,side=3,adj=0.62,padj=-3.5, col='gray48',cex=0.7)
mtext(text = 'Cantidades Peque\u{00F1}as',side=3,adj=0.95,padj=-3.5, col='gray48',cex=0.7)
for(j in 1:22){
  for(k in 1:n_rep){
    if(actual_choice[i,j,k]==1){
      points(k+pos_preguntas[j],0.6,col='turquoise3',cex=tam_linea,pch='l')}
    else{
      points(k+pos_preguntas[j],0.6,col='tomato',cex=tam_linea,pch='l')}
  }
}
donde_y <- 0.8
for(j in 1:22){
  for(k in 1:n_rep){
    if(actual_choice[i,j,k]==0 & predicted_choice[i,j,k]==0){
      points(k+pos_preguntas[j],donde_y,col='tomato',cex=tam_linea,pch='l')}
    if(actual_choice[i,j,k]==1 & predicted_choice[i,j,k]==1){
      points(k+pos_preguntas[j],donde_y,col='turquoise3',cex=tam_linea,pch='l')}
    if(actual_choice[i,j,k]==1 & predicted_choice[i,j,k]==0){
      points(k+pos_preguntas[j],donde_y,col='yellow',cex=tam_linea,pch='l')}
    if(actual_choice[i,j,k]==0 & predicted_choice[i,j,k]==1){
      points(k+pos_preguntas[j],donde_y,col='green',cex=tam_linea,pch='l')}
  }
}


########### conjuntas 
#### Plot1 sigma delta
new_plot(which_point = 'center_center',
         coordinates = c(-0.6,-0.3),
         width = 2,height = 2,
         label='1')

plot(sigma[5000:6000,i],delta[5000:6000,i],xlim=c(0,4),ylim=c(-3,0),axes=F,xlab='',ylab='',
     col='#20B2AA44',bg='#66CDAA11',pch=21)

#lines(c(1,1),c(-3,0),col='gray75')
#lines(c(2,2),c(-3,0),col='gray75')
#lines(c(3,3),c(-3,0),col='gray75')
#lines(c(0,4),c(-1,-1),col='gray75')
#lines(c(0,4),c(-2,-2),col='gray75')
#lines(c(0,4),c(-3,-3),col='gray75')



#### Plot2 delta weight 
new_plot(which_point = 'center_center',
         coordinates = c(1.5,-0.3),
         width = 2,height = 2,
         label='2')

plot(weight[5000:6000,i],delta[5000:6000,i],xlim=c(-0.005,0.008),ylim=c(-3,0),axes=F,xlab='',ylab='',
     col='#20B2AA44',bg='#66CDAA11',pch=21)
#lines(c(-0.004,0.008),c(-3,-3),col='gray75')
#lines(c(-0.004,0.008),c(-2,-2),col='gray75')
#lines(c(-0.004,0.008),c(-1,-1),col='gray75')
#lines(c(0,0),c(-4,0),col='gray75')
#lines(c(0.004,0.004),c(-4,0),col='gray75')


#### Plot3 Sigma Weigth
new_plot(which_point = 'center_center',
         coordinates = c(-0.6,-2.5),
         width = 2,height = 2,
         label='3')

plot(sigma[5000:6000,i],weight[5000:6000,i],xlim=c(0,4),ylim=c(-0.005,0.008),axes=F,xlab='',ylab='',
     col='#20B2AA44',bg='#66CDAA11',pch=21)
#lines(c(1,1),c(-0.004,0.008),col='gray75')
#lines(c(2,2),c(-0.004,0.008),col='gray75')
#lines(c(3,3),c(-0.004,0.008),col='gray75')
#lines(c(0,4),c(0,0),col='gray75')
#lines(c(0,4),c(0.004,0.004),col='gray75')

####### individuales

### Sigma hori
new_plot(which_point = 'center_center',
         coordinates = c(-0.6,1.5),
         width = 2,height = 1,
         label='A')

dindi(sigma[,i],'#20B2AA99','#20B2AA44',3,rango = c(0,4),c(0,4))
mtext(expression(sigma),3,col=gray[3],cex=3)
axis(1,cex.axis=0.6,tck=-0.020,col=gray[5],col.axis=gray[5],line=0.3,hadj = 0.5,las=2)

### Weight Hori
new_plot(which_point = 'center_center',
         coordinates = c(1.5,1.5),
         width = 2,height = 1,
         label='B')
dindi(weight[,i],'#20B2AA99','#20B2AA44',3,rango = c(-0.005,0.008),c(-0.005,0.008))
mtext('w',3,col=gray[3],cex=3)
axis(1,cex.axis=0.6,tck=-0.020,col=gray[5],col.axis=gray[5],las=2,line=0.3,hadj = 0.5)

### Delta verti 
new_plot(which_point = 'center_center',
         coordinates = c(-2.5,-0.3),
         width = 1,height = 2,
         label='C')

dindi(delta[,i],'#20B2AA99','#20B2AA44',2,c(-3,0),c(-3,0))
mtext(expression(delta),3,col=gray[3],cex=3,line=-4)
axis(4,cex.axis=0.6,tck=-0.020,col=gray[5],col.axis=gray[5],las=2,line=0.3,hadj=0.7)

### Weight Verti
new_plot(which_point = 'center_center',
         coordinates = c(-2.5,-2.5),
         width = 1,height = 2,
         label='D')

dindi(weight[,i],'#20B2AA99','#20B2AA44',2,rango = c(-0.005,0.008),c(-0.005,0.008))
mtext('w',3,col=gray[3],cex=3,line=-4)
axis(4,cex.axis=0.6,tck=-0.020,col=gray[5],col.axis=gray[5],las=2,line=0.3,hadj=0.5)


new_plot(which_point = 'center_center',
         coordinates = c(1.6,-2.2),
         width = 1,height = 1,
         label='D')
plot(0,type='n',xlim=c(0.5,3.5), ylim=c(0,25000),axes=F,xlab='',ylab='')
axis(1, at=c(1,2,3), lwd.ticks = 0.01)
#text(1.5,10000,i, cex=3,col='#BDACAF99')
hist(z[,i], xlim=c(0,4),main='', breaks=seq(0.75,3.7,0.5),
     axes=F, add=T, ylim=c(0,25000), col='#CE6173F2', border='white')


new_plot(which_point = 'center_center',
         coordinates = c(-2.5,1.5),
         width = 1,height = 1,
         label='D')

plot(0,type='n',xlim=c(0,1),ylim=c(0,1),xlab='',ylab='',axes=F)
text(0.5,0.5,i, cex=5,col='gray80')

end_poster(global_guides = F,
           local_guides = F)
embed_fonts(file)














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




