
######Colores######
colores <- list(gray <- c("#6F6F6F37","#6F6F6F64","#6F6F6F96","#6F6F6FC8","#6F6F6FFA"),
                set1 <- c("#FFA50046","#FFA50096","#FFA500C8"),
                set2 <- c("#9ACD3246","#9ACD3296","#9ACD32C8"),
                set3 <- c("#48D1CC46","#48D1CC96","#48D1CCC8"),
                set4 <- c("#FF818E46","#FF818E96","#FF818EC8"),
                navy <- c("#1594C546","#1594C596","#1594C5C8"),
                blue <- c("#79C3F146","#79C3F196","#79C3F1C8"),
                pur1 <- c("#D07FD819","#D07FD87D","#D07FD8FA"),
                pur2 <- c("#7571FE19","#7571FE7D","#7571FEFA"))

# #########Funciones#######
# predicciones_datos <- function(solo_dato,actual_choice,predicted_choice,
#                                orden_partici,titulo1,titulo2){
#  aciertos_prediccion <- array(dim=c(25,22,10))
#  plot(0,type='n',xlim=c(1,220),ylim=c(0.6,25),axes=F,xlab='',ylab='')
#  axis(1,at=c(1,seq(10,220,10)),cex.axis=0.6,las=2,hadj=0.3,col='gray48',
#       labels = c(1,2,3,7,8,9,4,5,10,11,13:18,19:24,''),tck=-0.012,col.axis='gray48')
#  axis(2,las=2,at=c(1:25),labels=orden_partici[,2],tck=-0.012,col='gray48',
#       cex.axis=0.6,hadj=0.3,col.axis='gray48')
#  mtext(titulo1,2,line=2,col='gray48')
#  mtext(titulo2,3,col='gray48')
#  
#  if(solo_dato==F){
#   for(i in 1:n_sub){
#    for(j in 1:22){
#     for(k in 1:n_rep){
#      if(actual_choice[orden_partici[i,2],j,k]==0 & predicted_choice[orden_partici[i,2],j,k]==0){
#       points(k+pos_preguntas[j],i,col='tomato',cex=tam_linea,pch='l')
#       aciertos_prediccion[i,j,k] <- 1}
#      if(actual_choice[orden_partici[i,2],j,k]==1 & predicted_choice[orden_partici[i,2],j,k]==1){
#       points(k+pos_preguntas[j],i,col='turquoise3',cex=tam_linea,pch='l')
#       aciertos_prediccion[i,j,k] <- 1}
#      if(actual_choice[orden_partici[i,2],j,k]==1 & predicted_choice[orden_partici[i,2],j,k]==0){
#       points(k+pos_preguntas[j],i,col='yellow',cex=tam_linea,pch='l')
#       aciertos_prediccion[i,j,k] <- 0}
#      if(actual_choice[orden_partici[i,2],j,k]==0 & predicted_choice[orden_partici[i,2],j,k]==1){
#       points(k+pos_preguntas[j],i,col='green',cex=tam_linea,pch='l')
#       aciertos_prediccion[i,j,k] <- 0}
#     }
#    }
#   }
#   total <- sum(aciertos_prediccion)/5500
#   text(x = 200,y=5,paste(round(total,3)))
#   
#  }
#  else{
#   for(i in 1:n_sub){
#    for(j in 1:22){
#     for(k in 1:n_rep){
#      if(actual_choice[orden_partici[i,2],j,k]==1){
#       points(k+pos_preguntas[j],i,col='turquoise3',cex=tam_linea,pch='l')}
#      else{
#       points(k+pos_preguntas[j],i,col='tomato',cex=tam_linea,pch='l')}
#     }
#    }
#   }
#  }
#  lines(c(60,60),c(1,25),col='grey48',lwd=1.4)
#  lines(c(100,100),c(1,25),col='grey48',lwd=1.4)
#  lines(c(160,160),c(1,25),col='grey48',lwd=1.4)
# }



predicciones_datos <- function(solo_dato,actual_choice,que_theta,titulo1,titulo2){
 
 mean_t_trade_theta <- array(NA,dim=c(n_sub,n_que))
 predicha_t_trade<- array(NA,dim=c(n_sub,n_que,n_rep))
 for(i in 1:n_sub){
  for(j in 1:n_que){
   mean_t_trade_theta[i,j] <- mean(que_theta[,i,j])
   for(k in 1:n_rep){
    predicha_t_trade[i,j,k] <- rbinom(1,1,mean_t_trade_theta[i,j]) 
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
 t_predic_clean_tr <- predicha_t_trade[,c(1,2,3,7,8,9,4,5,10,11,13:18,19:24),]
 
 pos_preguntas <- c(0,seq(10,220,10))
 actual_choice <- t_choice_clean
 
 aciertos_prediccion <- array(dim=c(25,22,10))
 plot(0,type='n',xlim=c(1,220),ylim=c(0.6,25),axes=F,xlab='',ylab='')
 axis(1,at=c(1,seq(10,220,10)),cex.axis=0.6,las=2,hadj=0.3,col='gray48',
      labels = c(1,2,3,7,8,9,4,5,10,11,13:18,19:24,''),tck=-0.012,col.axis='gray48')
 axis(2,las=2,at=c(1:25),labels=orden_partici[,2],tck=-0.012,col='gray48',
      cex.axis=0.6,hadj=0.3,col.axis='gray48')
 mtext(titulo1,2,line=2,col='gray48')
 mtext(titulo2,3,col='gray48')
 rojo <- 'tomato'
 azul <- 'turquoise3'
amarillo <- 'yellow'
 verde <-  'green'

 if(solo_dato==F){
  for(i in 1:n_sub){
   for(j in 1:22){
    for(k in 1:n_rep){
     if(actual_choice[orden_partici[i,2],j,k]==0 & t_predic_clean_tr[orden_partici[i,2],j,k]==0){
      points(k+pos_preguntas[j],i,col=rojo,cex=tam_linea,pch='l')
      aciertos_prediccion[i,j,k] <- 1}
     if(actual_choice[orden_partici[i,2],j,k]==1 & t_predic_clean_tr[orden_partici[i,2],j,k]==1){
      points(k+pos_preguntas[j],i,col=azul,cex=tam_linea,pch='l')
      aciertos_prediccion[i,j,k] <- 1}
     if(actual_choice[orden_partici[i,2],j,k]==1 & t_predic_clean_tr[orden_partici[i,2],j,k]==0){
      points(k+pos_preguntas[j],i,col=amarillo,cex=tam_linea,pch='l')
      aciertos_prediccion[i,j,k] <- 0}
     if(actual_choice[orden_partici[i,2],j,k]==0 & t_predic_clean_tr[orden_partici[i,2],j,k]==1){
      points(k+pos_preguntas[j],i,col=verde,cex=tam_linea,pch='l')
      aciertos_prediccion[i,j,k] <- 0}
    }
   }
  }
  total <- sum(aciertos_prediccion)/5500
  text(x = 200,y=5,paste(round(total,3)))
  
 }
 else{
  for(i in 1:n_sub){
   for(j in 1:22){
    for(k in 1:n_rep){
     if(actual_choice[orden_partici[i,2],j,k]==1){
      points(k+pos_preguntas[j],i,col=azul,cex=tam_linea,pch='l')}
     else{
      points(k+pos_preguntas[j],i,col=rojo,cex=tam_linea,pch='l')}
    }
   }
  }
 }
 lines(c(60,60),c(1,25),col='grey48',lwd=1.4)
 lines(c(100,100),c(1,25),col='grey48',lwd=1.4)
 lines(c(160,160),c(1,25),col='grey48',lwd=1.4)
}




values_densidades <- function(densidad,
                              limitesy,
                              parametro,
                              color){
  media <- summary(densidad)[4,]
  me <- as.numeric(gsub("Mean   :","",media))
  
  q3 <- NA
  for(i in 1:n_sub){
    q3[i] <- quantile(densidad[,i],0.975)
  }
  
  q1 <- NA
  for(i in 1:n_sub){
    q1[i] <- quantile(densidad[,i],0.025)
  }
  orden <- order(me)
  values <- data.frame(me,q1,q3,me_var)
  ordenados <- values[order(values$me_var),]
  
  
  plot(0,type='n',xlim=c(0.5,25.5),ylim=limitesy,axes=F,xlab='',ylab='')
  axis(1,at=c(1:25),labels=c(orden_var),cex.axis=0.5,las=2,
       tck=-0.015,col=gray[5],col.axis=gray[5],line=0.5)
 
  axis(2,cex.axis=0.5,las=2,tck=-0.015,col=gray[5],col.axis=gray[5])
  for(i in 1:n_sub){
    points(i,ordenados[i,1],cex=0.8,pch=20,col=color)
    lines(c(i,i),c(ordenados[i,2],ordenados[i,3]),col=color)
  }
}






comparison_order <- function(actual_order,predic_order){
  pos_y <- c(1:25)
  plot(0,type='n',axes=F,xlab='',ylab='',xlim=c(1,3),ylim=c(25,1))
  for(i in 1:length(actual_order)){
    text(1.5,pos_y[i],paste(predic_order[i]),cex=0.6)
    text(2.5,pos_y[i],paste(actual_order[i]),cex=0.6)
  }
  
  diffe_pos <- which(actual_order!=predic_order)
  same_pos <- rep(NA,length(diffe_pos))
  for(t in 1:length(diffe_pos)){
    same_pos[t] <- which(actual_order[diffe_pos[t]]==predic_order)
    lines(c(2.45,1.55),c(diffe_pos[t],same_pos[t]),col='gray48')
  }
}



values_densidades_vert <- function(densidad,
                                   limitesy,
                                   parametro,
                                   color,reversa,where_lines){
  media <- summary(densidad)[4,]
  me <- as.numeric(gsub("Mean   :","",media))
  
  q3 <- NA
  for(i in 1:n_sub){
    q3[i] <- quantile(densidad[,i],0.975)
  }
  
  q1 <- NA
  for(i in 1:n_sub){
    q1[i] <- quantile(densidad[,i],0.025)
  }
  orden <- order(me)
  values <- data.frame(me,q1,q3,me_var)
  ordenados <- values[order(values$me_var),]
  
  if(reversa==T){
   plot(0,type='n',ylim=c(25.5,0.5),xlim=limitesy,axes=F,xlab='',ylab='')
   
   lines(c(where_lines[1],where_lines[1]),c(0.5,25.5),col='gray90', lwd=1)
   lines(c(where_lines[2],where_lines[2]),c(0.5,25.5),col='gray90', lwd=1)
   lines(c(where_lines[3],where_lines[3]),c(0.5,25.5),col='gray90', lwd=1)
   lines(c(where_lines[4],where_lines[4]),c(0.5,25.5),col='gray90', lwd=1)
   lines(c(where_lines[5],where_lines[5]),c(0.5,25.5),col='gray90', lwd=1)
   
  }
  else{plot(0,type='n',ylim=c(0.5,25.5),xlim=limitesy,axes=F,xlab='',ylab='')}
  axis(2,at=c(1:25),labels=c(orden_var),cex.axis=0.8,las=2,
       tck=-0.015,col=gray[5],col.axis=gray[5],line=0.5)
  axis(1,cex.axis=0.8,tck=-0.015,col=gray[5],col.axis=gray[5],las=2)
  mtext('',2,col=gray[3],line=2)
  mtext(parametro,3,col=gray[5],line=-0.5)
  for(i in 1:n_sub){
    points(ordenados[i,1],i,cex=0.8,pch=20,col=color)
    lines(c(ordenados[i,2],ordenados[i,3]),c(i,i),col=color)
  }
}

