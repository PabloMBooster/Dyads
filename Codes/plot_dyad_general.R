plot.dyad = function(data.video,engin.args=NULL,file.name,point.size=0.6,anonymity=FALSE,land,land.color='darkgreen',
                     harbour.info=FALSE,harbour.data=NULL,dossier.stats.outputs=getwd(),line.width=3){ 
  
  x <- data.video$LONGITUDE
  y <- data.video$LATITUDE
  xlim = range(x) 
  ylim = range(y) 
  
  tiempos <- unique(data.video$date)
  
  ind.bateaux <- which(duplicated(data.video$id) == 'FALSE')
  
  pdf(paste0(dossier.stats.outputs,file.name,'.pdf'))
  if (is.null(engin.args) == FALSE){
  plot(x[1],y[1],xlim = xlim,ylim = ylim,cex=point.size,pch=data.video$pch.engin[1],
       col=data.video$col.engin[1],main=range(tiempos),xlab='',ylab='')
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "white")#Coloring the sea lightsteelblue1
  ind.peche <- which(!is.na(data.video$ENGIN.2))
  points(x[ind.peche],y[ind.peche],cex=point.size,
         pch=data.video$pch.engin[ind.peche],
         bg=data.video$col.engin[ind.peche],col='black')
  }else{
    plot(x[1],y[1],xlim = xlim,ylim = ylim,cex=point.size,pch=20,
         col=20,main=range(tiempos),xlab='',ylab='')
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "white")#Coloring the sea lightsteelblue1
  }
  bato <- unique(data.video$id)
  nothing <- lapply(bato,function(b){
    ind.bato <- which(data.video$id == b)
    if (length(ind.bato) > 1){
      t <- length(ind.bato)-1
      segments(x[ind.bato[1:t]],y[ind.bato[1:t]],
               x[ind.bato[(1:t)+1]],y[ind.bato[(1:t)+1]],                                                                         
               col=data.video$colores[ind.bato[1]],lty=data.video$traits[ind.bato[1]],lwd=line.width)
    }
  })
  # vessels in the last time step
  barcos <- unique(data.video$id)
  flechas <- sapply(barcos,function(z) {
    ind.last.barco <- which(data.video$id == z)
    if (length(ind.last.barco) > 1){
      s <- ind.last.barco[(length(ind.last.barco)-1):length(ind.last.barco)]
      if (x[s[1]] != x[s[2]] || y[s[1]] != y[s[2]]){
        arrows(x[s[1]],y[s[1]],x[s[2]],y[s[2]],length=0.1,lwd=3,col=data.video$colores[s[1]])
      }
    }
  })
  
  if (anonymity == FALSE){
    plot(land,add=T,col=land.color)
    if (harbour.info==TRUE){      
      text(x=harbour.data$harbours.lon,harbour.data$harbours.lat,labels=harbour.data$port,cex=0.6,col = 'black')
    }
  }
  
  legend("bottomright",legend=as.character(data.video$id[ind.bateaux]),col=data.video$colores[ind.bateaux],
         lty=data.video$traits[ind.bateaux],
         bty = "n",text.col = 'black',cex = 1,lwd=3)
  dev.off()
  
}
