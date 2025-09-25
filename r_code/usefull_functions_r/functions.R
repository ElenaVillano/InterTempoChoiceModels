
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

#########Funciones#######
olala <- function(propo,ancho,variable,colorset){
  x <- variable[1]
  y <- variable[2]
  
  x1 <- x
  x2 <- x+propo*ancho
  x3 <- x+propo*ancho
  x4 <- x
  
  y1 <- y
  y2 <- y
  y3 <- y+0.4
  y4 <- y+0.4
  
  polygon(x=c(x1,x2,x3,x4),y=c(y1,y2,y3,y4),col=colorset,border=NA)
  text(x4+ancho+0.15,y2+.25,paste(propo*10),cex=0.5,col=rgb(10,10,10,maxColor=255, alpha=125))
}

tierra <- function(ancho,posicion,colorset){
  x <- posicion[1]
  y <- posicion[2]
  
  x1 <- x
  x2 <- x+ancho
  x3 <- x+ancho
  x4 <- x
  
  y1 <- y
  y2 <- y
  y3 <- y+0.4
  y4 <- y+0.4
  
  polygon(x=c(x1,x2,x3,x4),y=c(y1,y2,y3,y4),col=colorset,border=NA)  
}

punto_predi_une <- function(propo,ancho,variable,color){
  x <- variable[1]
  y <- variable[2]
  
  x1 <- x+propo*ancho
  
  y1 <- y+0.25
  
  lines(c(x1,x1),c(y-0.05,y+0.55),col=color,lwd=0.9)
  
}

dindi <- function(distribucion,color,colorborde,orienta,rango,rangolab,...){

  ejey <- distribucion
  d.ejey <- density(ejey)
  q_1 <- quantile(ejey,0.025)
  q_3 <- quantile(ejey,0.975)
  
  
  if(orienta==1){
    plot(0,type = 'n',xlim = rango, 
         ylim =rev(range(d.ejey$y)), xlab = '', ylab = '', axes = F)
    poly_y=c(0,d.ejey$y,0)
    poly_x=c(d.ejey$x[1],d.ejey$x,d.ejey$x[length(d.ejey$x)])
    polygon(poly_x,poly_y,col=color,border=colorborde,lwd=0.8)    
    lines(c(q_1,q_3),c(0,0),lwd=2)
  }
  
  if(orienta==2){
    
    plot(0,type = 'n',xlim = rev(range(d.ejey$y)), 
         ylim =rango, xlab = '', ylab = '', axes = F)
    poly_y=c(0,d.ejey$y,0)
    poly_x=c(d.ejey$x[1],d.ejey$x,d.ejey$x[length(d.ejey$x)])
    polygon(poly_y,poly_x,col=color,border=colorborde,lwd=0.8)
    lines(c(0,0),c(q_1,q_3))
  }
  
  if(orienta==3){
    plot(0,type = 'n',xlim = rango, 
         ylim =range(d.ejey$y), xlab = '', ylab = '', axes = F)
    #axis(1,at=rangolab,cex.axis=0.8,tck=0.01,col='gray',line=0.2,padj=-2,col.axis='gray')
    poly_y=c(0,d.ejey$y,0)
    poly_x=c(d.ejey$x[1],d.ejey$x,d.ejey$x[length(d.ejey$x)])
    polygon(poly_x,poly_y,col=color,border=colorborde,lwd=0.8)
    lines(c(q_1,q_3),c(0,0))
  }
  
  if(orienta==4){
    plot(0,type = 'n',xlim = range(d.ejey$y), 
         ylim =rango, xlab = '', ylab = '', axes = F)
    poly_y=c(0,d.ejey$y,0)
    poly_x=c(d.ejey$x[1],d.ejey$x,d.ejey$x[length(d.ejey$x)])
    polygon(poly_y,poly_x,col=color,border=colorborde,lwd=0.8)
    lines(c(0,0),c(q_1,q_3),lwd=2)
  }
  
}




conjunta <- function(distri1,distri2,color,x,y){
  plot(distri1,distri2,pch=16,col=color,cex=1.5,axes=F,xlab='',ylab='',
       xlim=x,ylim=y)
}



ciclo <- function(pri,secu,coloscu,colclaro,colmedi){
  for(k in pri:secu){
    text(x[k],coorY[k]-0.2,labels=paste(k),col=colmedi,cex=tam_pregunta)
    tierra(tama[k],c(coorX[k],coorY[k]),colclaro)
    olala(propo_sub[k],tama[k],c(coorX[k],coorY[k]),coloscu)
    punto_predi_une(trade[k],tama[k],c(coorX[k],coorY[k]),col=colo_tra_o)
    #punto_predi_une(hiper[k],tama[k],c(coorX[k],coorY[k]),col=colo_hi_o)
  }
} 

ciclo2 <- function(pri,secu,coloscu,colclaro,colmedi){
 for(k in pri:secu){
  text(x[k],coorY[k]-0.2,labels=paste(k),col=colmedi,cex=tam_pregunta)
  tierra(size_intervals[k],c(coorX[k],coorY[k]),colclaro)
  olala(propo_sub[k],size_intervals[k],c(coorX[k],coorY[k]),coloscu)

 }
} 



ciclo_raw <- function(pri,secu,coloscu,colclaro,colmedi){
  for(k in pri:secu){
    text(x[k],coorY[k]-0.3,labels=paste(k),col=colmedi,cex=tam_pregunta)
    tierra(tama[k],c(coorX[k],coorY[k]),colclaro)
    olala(propo_sub[k],tama[k],c(coorX[k],coorY[k]),coloscu)
  }
} 




eje_canti_gran <- function(cual,
                           ta_canti, #tamano cantidad
                           le_canti, #lejania cantidad
                           ta_atri,  #tamano tiempo o proba
                           le_atri,  #lejania tiempo o proba
                           ta_letra, #tamano letra
                           le_letra, #lejania letra
                           le_eje,
                           color_ejes){    #lejania eje
  plot(0,type='n', xlim=c(0.5,10.8),ylim=c(20,0.5),axes=F,xlab='',ylab='')
  if(cual=='tiempo'){
    axis(3,at=c(1:4,5.5,7:10),
         labels=c('5150','5300','5450','5600','','6050', '6200','6350', '6500'),
         tck=0,cex.axis=ta_canti,col='white',padj=le_canti,col.axis=color_ejes)
    axis(3,at=c(1:4,5.5,7:10),
         labels=c('1','2','3','4','','7', '8','9', '10'),
         tck=0,cex.axis=ta_atri,col='white',padj=le_atri,col.axis=color_ejes)
    axis(3,at=c(1:10),labels=(c(LETTERS[1:4],'','',LETTERS[5:8])),
         tck=0.01,cex.axis=ta_letra,padj=le_letra,lwd=1,col=color_ejes,col.axis=gris_m,line=le_eje)
    
  }
  if(cual=='proba'){
    axis(3,at=c(1:10),
         labels=c('5150','5300','5450','5600','','','6050', '6200','6350', '6500'),
         tck=0,cex.axis=ta_canti,col='white',padj=le_canti,col.axis=gris_o)
    axis(3,at=c(1:10),
         labels=c('100','90','80','70','','','40', '30','20', '10'),
         tck=0,cex.axis=ta_atri,col='white',padj=le_atri,col.axis=gris_o)
    axis(3,at=c(1:10),labels=(c(LETTERS[1:4],'','',LETTERS[5:8])),
         tck=0.01,cex.axis=ta_letra,padj=le_letra,lwd=1,col=gris,col.axis=gris_c,line=le_eje)
    
  }
  
}



eje_inferior <- function(linea){
 axis(1,at=c(1,4,7,10),labels=c('I','J','K','L'),
      line=linea-1.1,col='white',col.axis='black',padj=-7,cex.axis=tam_letra)
 axis(1,at=c(1,4,7,10),labels=c('1','4','7','10'),
      line=linea-1.2,col='white',col.axis='black',padj=-3,cex.axis=tam_sem)
 axis(1,at=c(1,4,7,10),labels=c('1150','1250','1350','1450'),
      line=linea,col='black',tck=-0.01,col.axis='black',padj=-9,cex.axis=tam_canti)
}










eje_canti_peque <- function(cual,
                            ta_canti, #tamano cantidad
                            le_canti, #lejania cantidad
                            ta_atri,  #tamano tiempo o proba
                            le_atri,  #lejania tiempo o proba
                            ta_letra, #tamano letra
                            le_letra, #lejania letra
                            le_eje){    #lejania eje)
  if(cual=='tiempo'){
    plot(0,type='n', xlim=c(1,10.8),ylim=c(6,0.5),axes=F,xlab='',ylab='')
    axis(3,at=c(1,4,7,10),labels=c('I','J','K','L'),
         tck=0,padj=le_letra,cex.axis=ta_letra,lwd=0.8,col='white',col.axis='gray80')
    axis(3,at=c(1:10),
         labels=c('1150','','','1250','','','1350','','','1450'),
         tck=0,cex.axis=ta_canti,col='white',padj=le_canti,col.axis='gray80')
    axis(3,at=c(1:10),
         labels=c('1','','','4','','','7','','','10'),
         tck=-0.008,cex.axis=ta_atri,col=gris,padj=le_atri,col.axis='gray80',lwd=0.8)
  }
  
  if(cual=='proba'){
    plot(0,type='n', xlim=c(1,10.8),ylim=c(6,0.5),axes=F,xlab='',ylab='')
    axis(3,at=c(1,4,7,10),labels=c('I','J','K','L'),
         tck=0,padj=le_letra,cex.axis=ta_letra,lwd=0.8,col='white',col.axis=gris_c)
    axis(3,at=c(1:10),
         labels=c('1150','','','1250','','','1350','','','1450'),
         tck=0,cex.axis=ta_canti,col='white',padj=le_canti,col.axis=gris_o)
    axis(3,at=c(1:10),
         labels=c('1','','','4','','','7','','','10'),
         tck=-0.008,cex.axis=ta_atri,col=gris,padj=le_atri,col.axis=gris_o,lwd=0.8)
  }
}






eje_x <- function(rango_x,label_x){
  plot(0,xlim=rango_x,xlab=label_x,ylab='', axes=F,type='n')
  axis(1)
}




all_odds <- function(data,pregu_set,color){
 conjunto <- data[pregu_set] #De donde a donde esta la pregunta de ese set
 conju <- data.frame(conjunto[1]*conjunto[2],
                     conjunto[4],
                     conjunto[2]*conjunto[3],
                     conjunto[5],
                     conjunto[1]*conjunto[2]*conjunto[3],
                     conjunto[1]*conjunto[5],
                     conjunto[4]*conjunto[3],
                     conjunto[6])
 points(c(1:8),conju,col=color,pch=20,type='b')
}

individual_odds <- function(data,pregu_set,sub,color){
 conjunto <- data[sub,pregu_set] #De donde a donde esta la pregunta de ese set
 conju <- data.frame(conjunto[1]*conjunto[2],
                     conjunto[4],
                     conjunto[2]*conjunto[3],
                     conjunto[5],
                     conjunto[1]*conjunto[2]*conjunto[3],
                     conjunto[1]*conjunto[5],
                     conjunto[4]*conjunto[3],
                     conjunto[6])
 conju <- replace(conju,conju==0,0.005)
 points(c(1:8),conju,col=color,pch=20,type='b')
}
