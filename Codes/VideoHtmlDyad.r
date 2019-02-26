video.dyad.general = function(data.video,engin.args=NULL,file.name,image.name,image.dir,sleep=0.25,
                              point.size=0.6,traza=11,harbour.info=FALSE,harbour.data=NULL,espece=NULL,
                              legend.vessels=TRUE,legend.gears=TRUE,
                              range.x = FALSE,range.y = FALSE,col.text.bat='black',anonymity=FALSE,land,land.color='darkgreen'){ 
  x <- data.video$LONGITUDE
  y <- data.video$LATITUDE
  if (range.x == FALSE){
    xlim = range(x) 
  }else{
    xlim = range.x
  }
  if (range.y == FALSE){
    ylim = range(y) 
  }else{
    ylim = range.y
  }
  
  tiempos <- unique(data.video$date)
  
  # generating graphic
  ind.bateaux <- which(duplicated(data.video$id) == 'FALSE')
  
  saveHTML({
    
    ani.record(reset = TRUE)
    
    ani.options(interval = sleep)
    
    for(i in seq_along(tiempos)){
      
      if (is.null(engin.args) == FALSE){
        plot(x[1],y[1],xlim = xlim,ylim = ylim,cex=point.size,pch=data.video$pch.engin[1],
             col=data.video$col.engin[1],main=tiempos[i],xlab='longitude',ylab='latitude')
        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "white")#Coloring the sea lightsteelblue1
        
        if (traza == 0 || i <= traza){
          orig <- 1
        }else{
          ind.time.orig <- which(data.video$date == tiempos[i-traza])
          orig <- ind.time.orig[1]
        }
        ind.time <- which(data.video$date == tiempos[i])
        
        vector <- orig:ind.time[length(ind.time)]
        ind.peche <- which(!is.na(data.video$ENGIN.2[vector]))
        
        points(x[vector[ind.peche]],y[vector[ind.peche]],cex=point.size,
               pch=data.video$pch.engin[vector[ind.peche]],
               bg=data.video$col.engin[vector[ind.peche]],col='black')
        if (is.null(espece) == FALSE){
          ind.esp <- which(data.video$espece.kg[vector] == espece)
          points(x[vector[ind.esp]],y[vector[ind.esp]],pch=data.video$pch.engin[vector[ind.esp]],bg="black",cex=point.size)
        }
      }else{
        plot(x[1],y[1],xlim = xlim,ylim = ylim,cex=point.size,pch=20,
             col=20,main=tiempos[i],xlab='longitude',ylab='latitude')
        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "white")#Coloring the sea lightsteelblue1
        ind.time <- which(data.video$date == tiempos[1])
        
        if (traza == 0 || i <= traza){
          orig <- 1
        }else{
          ind.time.orig <- which(data.video$date == tiempos[i-traza])
          orig <- ind.time.orig[1]
        }
        ind.time <- which(data.video$date == tiempos[i])
        
        vector <- orig:ind.time[length(ind.time)]
      }
      
      if(i>1){ 
        
        bato <- unique(data.video$id[vector])
        
        # for each vessel, we have to search its past
        nothing <- lapply(bato,function(b){
          vector.2 <- 1:ind.time[length(ind.time)]
          ind.bato <- which(data.video$id[vector.2] == b)
          if (length(ind.bato) > 1){
            
            t <- length(ind.bato)-1
            if (traza == 0 || i <= traza || t <= traza){
              orig <- 1
            }else{
              tiempo.seg <- tiempos[i-traza]
              ind.time.orig <- which(data.video$date[vector.2[ind.bato]] == tiempos[i-traza])
              if (length(ind.time.orig)>0){
                orig <- ind.time.orig[1]
              }else{
                ind.time.orig <- which(data.video$date[vector.2[ind.bato]] >= tiempos[i-traza])
                orig <- ind.time.orig[1]
              }
              
              # orig <- t - traza
            }
            
            segments(x[vector.2[ind.bato[orig:t]]],y[ind.bato[vector.2[orig:t]]],
                     x[vector.2[ind.bato[(orig:t)+1]]],y[vector.2[ind.bato[(orig:t)+1]]],                                                                         
                     col=data.video$colores[vector.2[ind.bato[1]]],lty=data.video$traits[vector.2[ind.bato[1]]],
                     lwd=data.video$epaisseur[vector.2[ind.bato[1]]])
            
          }
        })
        # vessels from the present time
        barcos <- unique(data.video$id[ind.time])
        flechas <- sapply(barcos,function(z) {
          vector.2 <- 1:ind.time[length(ind.time)]
          ind.last.barco <- which(data.video$id[vector.2] == z)
          if (length(ind.last.barco) > 1){
            s <- ind.last.barco[(length(ind.last.barco)-1):length(ind.last.barco)]
            if (x[vector.2[s[1]]] != x[vector.2[s[2]]] || y[vector.2[s[1]]] != y[vector.2[s[2]]]){
              print(i)
              arrows(x[vector.2[s[1]]],y[vector.2[s[1]]],x[vector.2[s[2]]],y[vector.2[s[2]]],length=0.1,
                     lwd=data.video$epaisseur[vector.2[s[1]]],col=data.video$colores[vector.2[s[1]]])
            }
          }
        })
      }
      
      if (anonymity == FALSE){
        plot(land,add=T,col=land.color)
        if (harbour.info==TRUE){      
          text(x=harbour.data$harbours.lon,harbour.data$harbours.lat,labels=harbour.data$port,cex=0.6,col = 'black')
        }
      }
      
      if (is.null(engin.args) == FALSE){
        if (legend.gears == TRUE){
          legend("bottomleft",legend=as.character(engin.args$engins[-dim(engin.args)[1]]),pch=engin.args$pch.engin[-dim(engin.args)[1]],border='black',pt.bg=engin.args$col.engin[-dim(engin.args)[1]],
                 bty = "n",text.col = 'black',cex = 0.8)
        }
      }
      if (legend.vessels == TRUE){
        ind.dyad <- which(data.video$traits[ind.bateaux] == 1)
        legend("bottomright",legend=as.character(data.video$id[ind.bateaux[ind.dyad]]),
               col=data.video$colores[ind.bateaux[ind.dyad]],
               lty=data.video$traits[ind.bateaux[ind.dyad]],
               bty = "n",text.col = col.text.bat,cex = 1,lwd=data.video$epaisseur[ind.bateaux[ind.dyad]])
      }
      
      ani.pause()
      
    }}, img.name = image.name,htmlfile = file.name,
    ani.height=800,ani.width=800, autoplay = FALSE,imgdir=image.dir,autobrowse=FALSE,
    navigator = FALSE,ani.nmax=length(tiempos))

}
