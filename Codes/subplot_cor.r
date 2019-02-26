subplot.cor <- function(tr1,tr2,variable,r.cal=TRUE){
  
  tlim = range(tr1$date) 
  ylim = range(c(tr1[,variable],tr2[,variable]),na.rm=TRUE)
  
  if (r.cal == TRUE){
    cor.p = cor(tr1[,variable],tr2[,variable],use="pairwise.complete.obs",method="pearson")
  }
  
  plot(tr1$date[1],tr1[1,variable],type='p',xaxt='n',xlim=tlim, lwd = 3,
       ylim=ylim,col='white',xlab='',main=variable,ylab="")
  lines(tr1$date,tr1[,variable],col="red",lwd = 3)
  lines(tr2$date,tr2[,variable],col="blue",lwd = 3)
  
  r <- as.POSIXct(round(range(tr1$date), "hours"))
  secuencia = seq(r[1], r[2], by = "hour")
  secuencia2 = seq(from=secuencia[1],to=secuencia[length(secuencia)],
                   length.out=min(30,length(tr1$date)))
  axis.POSIXct(1, at = secuencia2, format = "%m/%d %H", las = 2)
  if (r.cal == TRUE){
  legend("topleft",c(paste0("r = ",round(cor.p,2))),bty="n",cex=1.2)
  }
}


subplot.cor.gif <- function(image.name, file.name,tr1,tr2,variable,r.cal=FALSE){
  
  tlim = range(tr1$date) 
  ylim = range(c(tr1[,variable],tr2[,variable]),na.rm=TRUE)
  
  if (r.cal == TRUE){
    cor.p = cor(tr1[,variable],tr2[,variable],use="pairwise.complete.obs",method="pearson")
  }
  
  saveGIF({
    for(i in seq_along(tr1$date)){
      plot(NA,type='p',xaxt='n',xlim=tlim, lwd = 3,
           ylim=ylim,col='white',xlab='',main=variable,ylab="")
      i_seq <- seq(1,i, by = 1)
      lines(tr1$date[i_seq],tr1[,variable][i_seq],col="red",lwd = 3, type = "o", pch = 16, cex = 0.5)
      lines(tr2$date[i_seq],tr2[,variable][i_seq],col="blue",lwd = 3, type = "o", pch = 16, cex = 0.5)
      r <- as.POSIXct(round(range(fechas), "hours"))
      secuencia = seq(r[1], r[2], by = "hour")
      secuencia2 = seq(from=secuencia[1],to=secuencia[length(secuencia)],
                       length.out=min(30,length(tr1$date)))
      
      axis.POSIXct(1, at = secuencia2, format = "%m/%d %H", las = 2)
      
    }

    if (r.cal == TRUE){
      legend("topleft",c(paste0("r = ",round(cor.p,2))),bty="n",cex=1.2)
    }
  }, img.name = image.name, movie.name = file.name,interval = 0.5)
}

