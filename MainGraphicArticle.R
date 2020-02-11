#### 
#### Main graphic
#### January, 2020 
#### By Elena Villalobos 
####

rm(list=ls())

# Hyperboloid Model
load("~/Documents/Heavy_R_Stuff/PublicatedArticleModels/HyperModelArticle.RData")
names(samples$BUGSoutput$sims.list)
theta_hiper<-samples$BUGSoutput$sims.list$theta
rm(csvs_time,data,myinits,samples)

# Trade-off Model
load("/Users/Ele/Documents/Heavy_R_Stuff/PublicatedArticleModels/ITCHModelArticle.RData")
names(samples$BUGSoutput$sims.list)
theta_trade<-samples$BUGSoutput$sims.list$t_theta_trade
rm(csvs_time,data,myinits,samples)

# ITCH Model
load("/Users/Ele/Documents/Heavy_R_Stuff/PublicatedArticleModels/ITCHModelArticle.RData")
names(samples$BUGSoutput$sims.list)
theta_itch<-samples$BUGSoutput$sims.list$t_theta_trade
rm(csvs_time,data,myinits,samples)

# Proportional Differences Model
load("/Users/Ele/Documents/Heavy_R_Stuff/PublicatedArticleModels/PDModelArticle.RData")
names(samples$BUGSoutput$sims.list)
theta_PD<-samples$BUGSoutput$sims.list$theta
rm(csvs_time,data,myinits,samples)

# Direct Differences Model
load("/Users/Ele/Documents/Heavy_R_Stuff/PublicatedArticleModels/DDModelArticle.RData")
names(samples$BUGSoutput$sims.list)
theta_DD<-samples$BUGSoutput$sims.list$t_theta_trade


source("~/Google Drive/Nube/Intertemporal_Risky_Choice_Project/R_Functions/latestfunciontsTOT.R")
source("~/Google Drive/Nube/Intertemporal_Risky_Choice_Project/R_Functions/PosterPlots.R")
setwd("~/Google Drive/Nube/Intertemporal_Risky_Choice_Project/Article_Tesina/Texto_Article/")


library('extrafont')
library('png')




pdf(file='ArticleGrand.pdf',width = 6.5,height = 7)
tam_linea <- 0.94

layout(matrix(c(1,1,2,2,
                3,3,4,4,
                5,5,6,6),ncol=4,byrow=T))
par(mar=c(1.2,1.5,1,0.2),oma=c(1,1,2,0))

predicciones_datos(T,t_choice_clean,theta_hiper,'','Data')
mtext('Questions',1,cex=0.5, line=1,col='gray48')
mtext('Participants',2,cex=0.5, line=1.3,col='gray48')
predicciones_datos(F,t_choice_clean,theta_hiper,'','')
mtext('Hyperboloid',2,cex=1, line=1.3,col='gray48')
predicciones_datos(F,t_choice_clean,theta_trade,'','')
mtext('Trade-off',2,cex=1, line=1.2,col='gray48')
predicciones_datos(F,t_choice_clean,theta_itch,'','')
mtext('ITCH',2,cex=1, line=1.3,col='gray48')
predicciones_datos(F,t_choice_clean,theta_PD,'','')
mtext('PD',2,cex=1, line=1.2,col='gray48')
predicciones_datos(F,t_choice_clean,theta_DD,'','')
mtext('DD',2,cex=1, line=1.2,col='gray48')

#plot(0,type='n',axes=F,xlab='',ylab='')
#legend(0.5,0.5,
#       legend = c('Data | Prediction','LLR  | LLR','SSR | SSR','SSR | LLR', 'LLR  | SSR'),
#       fill = c('white','turquoise3','tomato','green','yellow'),
#       border = c('white','turquoise3','tomato','green','yellow'),box.col = 'white',
#       cex = 1,text.col = 'gray48')

# mtext('Preguntas',1,col='gray48',line=1.5)
# axis(3,line=0.5,at=c(1,60,100,160,220),
#      labels=c('','','','',''),tck=0.02,lwd = 1,col='gray48')
# 
# mtext(text = ' Intervalos \n Peque\u{00F1}os', side = 3,adj=0.12,padj=-0.5, col='gray48',cex=0.75)
# mtext(text = ' Intervalos \n Medianos',side = 3,adj=0.35 ,padj=-0.5, col='gray48',cex=0.75)
# mtext(text = ' Intervalos \nGrandes', side = 3,adj=0.6,padj=-0.5, col='gray48',cex=0.75)
# mtext(text = '     Cantidades \nPeque\u{00F1}as',  side = 3,adj=0.9,padj=-0.5, col='gray48',cex=0.75)
dev.off()


