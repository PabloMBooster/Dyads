subplot.uni <- function(fecha,variable,nombre,color="black",rango=range(variable,na.rm=TRUE),cal=FALSE){
  
  tlim = range(fecha) 
  ylim = rango

  if (cal == TRUE){
    x = mean(variable,na.rm=TRUE)
  }
  
  plot(fecha,variable,type='l',xaxt='n',xlim=tlim, lwd = 3,
       ylim=ylim,xlab='',main=nombre,col=color,ylab="")
  r <- as.POSIXct(round(range(fecha), "hours"))
  secuencia = seq(r[1], r[2], by = "hour")
  secuencia2 = seq(from=secuencia[1],to=secuencia[length(secuencia)],
                   length.out=min(30,length(variable)))
  axis.POSIXct(1, at = secuencia2, format = "%m/%d %H")
  if (cal == TRUE){
  legend("bottomleft",c(paste0(nombre," = ",round(x,2))),bty="n",cex=1.2)
  }
}


# subplot.uni.DO.gif <- function(file.name, fechas,variable, nombre, color="black",
#                             rango=range(variable,na.rm=TRUE), cal=FALSE,
#                             param_Prox_delta = param$Prox$delta, n.prox = n.prox, colores = colores){
  
subplot.uni.DO.gif <- function(image.name, file.name, fechas, variable, n.prox, param_Prox_delta, colores, color = "black"){
  
  tlim = range(fechas) 
  ylim = range(variable, na.rm = TRUE)
  
  texte <- NULL
  for (prox in n.prox:1){
    
    texte <- c(texte,paste0("<",param_Prox_delta[prox], " (",round(sum(variable <= param_Prox_delta[prox])/length(fechas),2),")"))
  }
  
  rep10 <- rep(NA, length(fechas))
  rep30 <- rep(NA, length(fechas))
  rep60 <- rep(NA, length(fechas))
  
  rep10[which(variable <= param_Prox_delta[1])] <- variable[which(variable <= param_Prox_delta[1])]
  rep30[which(variable <= param_Prox_delta[2])] <- variable[which(variable <= param_Prox_delta[2])]
  rep60[which(variable <= param_Prox_delta[3])] <- variable[which(variable <= param_Prox_delta[3])]
  
  saveGIF({
    for(i in seq_along(fechas)){
      par(las=2)
      plot(NA,type='l',xaxt='n',xlim=tlim, lwd = 3,
           ylim=ylim,xlab='',main=nombre,ylab="")
      i_seq <- seq(1,i, by = 1)
      lines(fechas[i_seq],variable[i_seq],col=color,lwd = 3, type = "o", pch = 16, cex = 0.5)
      
      points(fechas[i_seq], rep60[i_seq], col=colores[3],cex=1,pch=21,bg=colores[3])
      points(fechas[i_seq], rep30[i_seq], col=colores[2],cex=1,pch=21,bg=colores[2])
      points(fechas[i_seq], rep10[i_seq], col=colores[1],cex=1,pch=21,bg=colores[1])
      
      legend("topleft",texte[n.prox:1],col=colores,pch=20,bty="n")
      
      r <- as.POSIXct(round(range(fechas), "hours"))
      secuencia = seq(r[1], r[2], by = "hour")
      secuencia2 = seq(from=secuencia[1],to=secuencia[length(secuencia)],
                       length.out=min(30,length(variable)))
      
      axis.POSIXct(1, at = secuencia2, format = "%m/%d %H", las = 2)
      # if (cal == TRUE){
      # legend("bottomleft",c(paste0(nombre," = ",round(x,2))),bty="n",cex=1.2)
      # }
    }
  }, img.name = image.name, movie.name = file.name,interval = 0.5)
}


# que hacemos con este
subplot.uni.DI.theta <- function(image.name, file.name, fecha,variable,nombre,color="black"){
    
  tlim = range(fecha) 
  ylim = range(variable,na.rm=TRUE)
  
  #if (cal == TRUE){
    x = mean(variable,na.rm=TRUE)
  #}
  
  saveGIF({
    for(i in seq_along(fecha)){
      par(las=2)    
      plot(NA, type='l', xaxt='n', xlim=tlim, lwd = 3,
           ylim=ylim,xlab='',main=nombre,col=color,ylab="")
      
      i_seq <- seq(1,i, by = 1)
      lines(fecha[i_seq],variable[i_seq],col=color,lwd = 3, type = "o", pch = 16, cex = 0.5)
      
      r <- as.POSIXct(round(range(fecha), "hours"))
      secuencia = seq(r[1], r[2], by = "hour")
      secuencia2 = seq(from=secuencia[1],to=secuencia[length(secuencia)],
                       length.out=min(30,length(variable)))
      axis.POSIXct(1, at = secuencia2, format = "%m/%d %H", las = 2)
      #if (cal == TRUE){
        legend("bottomleft",c(paste0(nombre," = ",round(x,2))),bty="n",cex=1.2)
      #}
    }
    }, img.name = image.name, movie.name = file.name, interval = 0.5)
}
