pre.video.dyads.all <- function(subset.plot,col_vector,engin.args=NULL,dyad,epais,anonymity=FALSE){
  
  # one color per vessel
  barcos <- unique(subset.plot$id)
  n <- length(unique(barcos))
  
  if(n > 9){
    colores <-  col_vector(n)
  }else if(n < 3){
    colores <- c("red","blue")
  }else{ 
    colores=brewer.pal(n, "Set1")
  }
  
  traits <- sample(c(2,3),size=n,replace=TRUE)
  epaisseur <- rep(1,n)

  ## for dyad
  ind.dyad.1 <- which(barcos == dyad[1])
  ind.dyad.2 <- which(barcos == dyad[2])
  traits[c(ind.dyad.1,ind.dyad.2)] <- rep(1,2)
  epaisseur[c(ind.dyad.1,ind.dyad.2)] <- rep(epais,2)

  color.trait <- data.frame(barcos,colores,traits,epaisseur)
  matriz.2 <- merge(subset.plot,color.trait,by.x='id',by.y='barcos')
  
  if (anonymity == TRUE){
    matriz.2$lon.real <- matriz.2$LONGITUDE
    matriz.2$lat.real <- matriz.2$LATITUDE
    matriz.2$LONGITUDE <- matriz.2$lon.real - min(matriz.2$lon.real)
    matriz.2$LATITUDE <- matriz.2$lat.real - min(matriz.2$lat.real)
  }

  if(is.null(engin.args) == FALSE){
    # one symbol per gear
    # one column to specify to which gear category it belongs
    matriz.2$ENGIN.2 <- matriz.2$LE_GEAR
    otros <- which(!is.na(matriz.2$LE_GEAR) & matriz.2$LE_GEAR!='OTB' & matriz.2$LE_GEAR!='DRB' & 
                     matriz.2$LE_GEAR!='OTT' & matriz.2$LE_GEAR!='MIS' & matriz.2$LE_GEAR!='OTM' &
                     matriz.2$LE_GEAR!='PTM' & matriz.2$LE_GEAR!='PTB')
    if (length(otros)>0){
      matriz.2$ENGIN.2[otros] <- 'other'
    }
    
    matriz.3 <- merge(matriz.2,engin.args,by.x='ENGIN.2',by.y='engins',all.x=TRUE)
    
    ## reordering data chronologically
    matriz.3 = matriz.3[order(matriz.3$date),]
    variables.plot <- c('ENGIN.2','id','date','burst','LONGITUDE','LATITUDE','colores',
                        'traits','epaisseur','pch.engin','col.engin','espece.kg','capture.kg')
    data.plot <- matriz.3[,variables.plot]
    
  }else{
    
    matriz.3 = matriz.2[order(matriz.2$date),]
    variables.plot <- c('id','date','burst','LONGITUDE','LATITUDE','colores',
                        'traits','epaisseur')
    data.plot <- matriz.3[,variables.plot]
  }
  
  data.plot$colores <- as.character(data.plot$colores)
  
  return(data.plot)
}
