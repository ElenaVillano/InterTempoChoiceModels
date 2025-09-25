############Funciones############
ejes_lindos_fun<- function(titulo,xlab,ylab,xlimi,ylimi,tam_axes,tam_title,atx,aty,...){
  plot(0,type='n',xlab='',ylab='',axes=F,xlim=xlimi,ylim=ylimi)
  axis(1,at=atx,tck=-0.01,col=gray[5],col.axis=gray[5],
       cex.axis=tam_axes,padj=-2,lwd = 0.5,...)
  axis(2,at=aty,tck=-0.01,las=2,col=gray[5],col.axis=gray[5],
       cex.axis=tam_axes,hadj=0,lwd=0.5)
  mtext(xlab,1,col=gray[3],line=1,cex=tam_axes)
  mtext(ylab,2,col=gray[3],line=1.1,cex=tam_axes)
  mtext(titulo,3,col=gray[3],line=-0.5,cex=0.7)
  
}


cuadros <- function(dato,prediccion,colconju){
  x_ayuda <- round(seq(0,1,0.1),1)
  y_ayuda <- round(seq(0,1,0.1),1)
  conteo <- array(NA,dim=c(11,11))
  for(i in 1:11){
    for(j in 1:11){
      conteo[i,j] <- length(which(dato==x_ayuda[i]&prediccion==y_ayuda[j]))
    } 
  }
  
  tampunto <- (conteo*1.8)/max(conteo)
  
  for(i in 1:11){
    for(j in 1:11){
      points(x_ayuda[i],y_ayuda[j],
             cex=if(tampunto[i,j]==0){cex==0}
             else{cex=tampunto[i,j]+.6},
             col=colconju,pch=15)
    }
  }
}


cuadros_propos <- function(set_data,pregunta,pos_pregunta,cual_propo){
 longitud <- length(which(set_data[,pregunta]==cual_propo))
 maximotam <- 1.7
 tamacuadro <- (maximotam*longitud)/25
 points(pos_pregunta,cual_propo,
        cex=if(tamacuadro==0){cex=0}
         else{cex=tamacuadro+0.4},
         pch=15,col='#f5254999')
}





ejes_lindos <- function(titulo,xlab,ylab,xlimi,ylimi,linea,...){
  plot(0,type='n',xlab='',ylab='',axes=F,xlim=xlimi,ylim=ylimi)
  axis(2,las=2,col=gray[3],col.axis=gray[3],tck=-0.015,cex.axis=0.5,hadj = -0.1)
  mtext(xlab,1,col=gray[2],line=3,cex=0.5,padj=-3)
  mtext(ylab,2,col=gray[2],line=1,cex=0.5,padj=-0.5)
  mtext(titulo,3,col=gray[3],line=2,cex=1.3)
  if(linea=='scater'){
    lines(c(0,1),c(0,1),col='red',lwd=0.5)
  }
  if(linea=='diferencia'){
    lines(c(0,25),c(0,0),col='red')
  }
  if(linea=='otro'){
    lines(c(0,0),c(0,0),col='white')
  }
}

pun_ty_b <- function(x,y,color){
  points(x,y,col=color,type='b',pch=19,cex=0.5)
}

ID <- function(x,y,texto,tama){
  text(x,y,texto,cex=tama,col=gray[1])
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
 mtext('Posteriors',2,col=gray[3],line=2.5,cex=0.6)
 mtext('Participants',1,col=gray[3],line=2.5,cex=0.6)
 axis(2,cex.axis=0.5,las=2,tck=-0.015,col=gray[5],col.axis=gray[5])
 mtext('',2,col=gray[3],line=2)
 mtext(parametro,3,col=gray[5],line=0.5)
 for(i in 1:n_sub){
  points(i,ordenados[i,1],cex=0.8,pch=20,col=color)
  lines(c(i,i),c(ordenados[i,2],ordenados[i,3]),col=color)
 }
}

fondo <- function(primerax,segundax,primeray,segunday,colorset){
 #funcion para hacer fondo en el plot. 
 x1 <- primerax
 x2 <- primerax
 x3 <- segundax
 x4 <- segundax
 
 y1 <- primeray
 y2 <- segunday
 y3 <- segunday
 y4 <- primeray
 
 polygon(x=c(x1,x2,x3,x4),y=c(y1,y2,y3,y4),col=colorset,border=NA)  
}
