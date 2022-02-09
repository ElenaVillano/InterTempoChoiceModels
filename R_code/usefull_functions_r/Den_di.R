#Densidades dibujadas
dendi <- function(distribucion,etix,etiy,color,colborde){#distribucion,main,colordensidad,colorbordes,xlab
  plot(0,type = 'n',xlim = c(round(quantile(distribucion,0),0),round(quantile(distribucion,1),0)), 
        ylim = c(0,max(density(distribucion)$y)), xlab = etix, ylab = etiy, axes = F)
  axis(1,tck=-0.01,cex.axis=0.5,col='gray',padj=-2.5)
  lines(density(distribucion),type='n')
  poly_y=c(0,density(distribucion)$y,0)
  poly_x=c(density(distribucion)$x[1],density(distribucion)$x,density(distribucion)$x[length(density(distribucion)$x)])
  polygon(poly_x,poly_y,col=color,border=colborde,lwd=0.8)
  q_1 <- quantile(distribucion,0.025)
  q_3 <- quantile(distribucion,0.975)
  lines(c(q_1,q_3),c(0,0),col='red',lwd=1)
  text((max(density(distribucion)$x))/2,(max(density(distribucion)$y))/2,paste('Mean = ',summary(distribucion)[4],sep=''),cex=0.5)
  #text((max(density(distribucion)$x))/2,(max(density(distribucion)$y))/2.5,paste('Prevalue = ',preval,sep=''),cex=0.5)
  mtext(etix,2,cex=0.5)
  }

#####para hacer el traceplot de cada una de las distribuciones
trace <- function(distribucion){
  plot(samples$BUGSoutput$sims.array[,1,distribucion],type='l',col='orange',
       xlab=distribucion,ylab='')

  lines(samples$BUGSoutput$sims.array[,2,distribucion],type='l',col='turquoise')
}
  


###plot con dos ejes
plotbonito <- function(ejex,ejey,etix,etiy,ondex,ondey){
  plot(0,type = 'n',xlim = ejex, ylim = ejey, xlab = etix, ylab = etiy, axes = F)
  axis(1,tck=0.01,at=ondex,cex.axis=0.5,lwd=0.5,padj=-3)
  axis(2,las=2, tck=-0.01, at=ondey,cex.axis=0.5)
}


###plot con el eje x
plotbonitoD <- function(distribucion,etix,etiy){
  plot(0,type = 'n',xlim = c(round(quantile(distribucion,0),0),round(quantile(distribucion,1),0)), 
       ylim = c(0,max(density(distribucion)$y)), xlab = etix, ylab = etiy, axes = F)
  axis(1,tck=-0.01,cex.axis=1)
}
