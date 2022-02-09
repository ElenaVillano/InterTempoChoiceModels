
plt_x_coord <- function(real_x_location){
  plt_x <- real_x_location/g_pw+1/2
  return(plt_x=plt_x)
}

plt_y_coord <- function(real_y_location){
  plt_y <- real_y_location/g_ph+1/2
  return(plt_y=plt_y)
}

to_plt <- function(x,y){
  plt_coords <- c(plt_x_coord(x),plt_y_coord(y))
  return(plt_coords)
}

display_margin <- function(color='#ff0000'){
  # marref <- to_plt(x=global_margins[c(2,4)],y=global_margins[c(1,3)])
  color_lines <- paste(color,'44',sep='')
  par(plt=to_plt(c(-g_pw/2,g_pw/2),c(-g_ph/2,g_ph/2)),xaxs='i',yaxs='i',new=T)
  plot(0,type='n',xlim=c(-g_pw/2,g_pw/2),ylim=c(-g_ph/2,g_ph/2))
  segments(x0=c(rep(global_margins[2],2),global_margins[c(2,4)]),
           x1=c(rep(global_margins[4],2),global_margins[c(2,4)]),
           y0=c(global_margins[c(1,3)],rep(global_margins[1],2)),
           y1=c(global_margins[c(1,3)],rep(global_margins[3],2)),col=color_lines,lwd=15)
  lines(rep(0,2),global_margins[c(1,3)],col=color_lines,lwd=15)
  lines(global_margins[c(2,4)],rep(0,2),col=color_lines,lwd=15)
  text(global_margins[2],0,paste(global_margins[2]),col=color,cex=6,adj=c(0.5,1.5),srt=90)
  text(global_margins[4],0,paste(global_margins[4]),col=color,cex=6,adj=c(0.5,1.5),srt=-90)
  text(0,global_margins[1],paste(global_margins[1]),col=color,cex=6,adj=c(0.5,-.5),srt=0)
  text(0,global_margins[3],paste(global_margins[3]),col=color,cex=6,adj=c(0.5,1.5),srt=0)
  
  grid_x <- -ceiling(g_pw):ceiling(g_pw)
  grid_y <- -ceiling(g_ph):ceiling(g_ph)
  segments(x0=c(grid_x,rep(global_margins[2],length(grid_y))),
           x1=c(grid_x,rep(global_margins[4],length(grid_y))),
           y0=c(rep(global_margins[1],length(grid_x)),grid_y),
           y1=c(rep(global_margins[3],length(grid_x)),grid_y),
           col=color_lines)
}

display_plot <- function(x_coords,y_coords,reference_point,plot_label,plot_color){
  par(plt=to_plt(x_coords,y_coords),xaxs='i',yaxs='i',new=T)
  plot(0,type='n',xlim=x_coords,ylim=y_coords,axes=F,ann=F)
  segments(x0=c(rep(x_coords[1],2),x_coords),
           x1=c(rep(x_coords[2],2),x_coords),
           y0=c(y_coords,rep(y_coords[1],2)),
           y1=c(y_coords,rep(y_coords[2],2)),
           col=plot_color,lwd=10)
  # Add plot label
  text(x_coords[1]+(x_coords[2]-x_coords[1])/2,
       y_coords[1]+(y_coords[2]-y_coords[1])/2,
       paste(plot_label),col=plot_color,cex=2,font=2)
  # Add plot_references
  text(x_coords[1]+(x_coords[2]-x_coords[1])/2,
       y_coords[1],paste(y_coords[1]),col=plot_color,cex=2.5,adj=c(0.5,-.5),srt=0)
  text(x_coords[1]+(x_coords[2]-x_coords[1])/2,
       y_coords[2],paste(y_coords[2]),col=plot_color,cex=2.5,adj=c(0.5,1.5),srt=0)
  text(x_coords[1],
       y_coords[1]+(y_coords[2]-y_coords[1])/2,
       paste(x_coords[1]),col=plot_color,cex=2.5,adj=c(0.5,1.5),srt=90)
  text(x_coords[2],
       y_coords[1]+(y_coords[2]-y_coords[1])/2,
       paste(x_coords[2]),col=plot_color,cex=2.5,adj=c(0.5,-.5),srt=90)
  # Mark reference point
  points(reference_point[1],reference_point[2],pch=16,cex=5,col=plot_color)
}

start_poster <- function(archive,
                         poster_width,
                         poster_height,
                         margins){
  g_ph <<- poster_height
  g_pw <<- poster_width
  global_margins <<- c(-g_ph/2+margins[1],-g_pw/2+margins[2],g_ph/2-margins[3],g_pw/2-margins[4])
  global_plot_list <<- list()
  pdf(file=archive,width=poster_width,height=poster_height)
  par(plt=c(0,1,0,1),xaxs='i',yaxs='i')
  plot(0,type='n',xlim=c(0,1),ylim=c(0,1),axes=F,ann=F)
}

end_poster <- function(global_guides=T,
                       local_guides=T){
  if(global_guides){
    display_margin() 
  }
  if(local_guides){
    if(length(global_plot_list)>0){
      for(plts in 1:length(global_plot_list)){
        plt_label <- paste(plts)
        if(class(global_plot_list[[plts]]$plot_label)!='NULL'){
          plt_label <- global_plot_list[[plts]]$plot_label
        }
        display_plot(x_coords = global_plot_list[[plts]]$coords$x,
                     y_coords = global_plot_list[[plts]]$coords$y,
                     reference_point = global_plot_list[[plts]]$coords$reference,
                     plot_label = plt_label,
                     plot_color = global_plot_list[[1]]$plot_color)
      }
    }
  }
  dev.off()
}

new_plot <- function(which_point=NULL,
                     coordinates=NULL,
                     width=NULL,
                     height=NULL,
                     label=NULL,
                     inner_margins='none',
                     color='#0044ff'){
  
  # Prepare these guys to get filled
  x_coords <- NULL
  y_coords <- NULL
  
  # Location methods here
  wp <- strsplit(which_point,split='_')[[1]]  
  # find x_coords
  if(wp[1]=='left'){
    x_coords[1] <- coordinates[1]
    x_coords[2] <- coordinates[1]+width
  }
  else if(wp[1]=='center'){
    x_coords[1] <- coordinates[1]-width/2
    x_coords[2] <- coordinates[1]+width/2
  }
  else if(wp[1]=='right'){
    x_coords[1] <- coordinates[1]-width
    x_coords[2] <- coordinates[1]
  }
  
  # find y_coords
  if(wp[2]=='top'){
    y_coords[1] <- coordinates[2]-height
    y_coords[2] <- coordinates[2]
  }
  else if(wp[2]=='center'){
    y_coords[1] <- coordinates[2]-height/2
    y_coords[2] <- coordinates[2]+height/2
  }
  else if(wp[2]=='bottom'){
    y_coords[1] <- coordinates[2]
    y_coords[2] <- coordinates[2]+height
  }
  
  
  # Gotta have x_coords and y_coords at this point!
  global_plot_list <<- append(global_plot_list,
                              list(list(coords=list(x=x_coords,
                                                    y=y_coords,
                                                    reference=coordinates),
                                        plot_label=label,
                                        plot_color=color)))
  if(length(inner_margins)==1){
    if(inner_margins=='default'){
      effective_x_left <- x_coords[1]+(x_coords[2]-x_coords[1])*.05
      effective_x_right <- x_coords[2]-(x_coords[2]-x_coords[1])*.05
      effective_y_bottom <- y_coords[1]+(y_coords[2]-y_coords[1])*.05
      effective_y_top <- y_coords[2]-(y_coords[2]-y_coords[1])*.05
    }
    if(inner_margins=='none'){
      effective_x_left <- x_coords[1]
      effective_x_right <- x_coords[2]
      effective_y_bottom <- y_coords[1]
      effective_y_top <- y_coords[2]
    }
  }
  else{
    effective_x_left <- x_coords[1]+inner_margins[2]
    effective_x_right <- x_coords[2]-inner_margins[4]
    effective_y_bottom <- y_coords[1]+inner_margins[1]
    effective_y_top <- y_coords[2]-inner_margins[3]
  }
  par(plt=to_plt(x=c(effective_x_left,effective_x_right),
                 y=c(effective_y_bottom,effective_y_top)),new=T)
}


