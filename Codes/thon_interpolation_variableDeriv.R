thon_interpolation_variableDeriv <- function(ech.vms.data){
  
  
  ## linear interpolation
  
  interpL <- lapply(split(ech.vms.data,ech.vms.data$cod.trip,drop=TRUE),function(x){
    # x <- ech.vms.data[which(ech.vms.data$cod.trip == sort(unique(ech.vms.data$cod.trip))[1]),]
    # print(x$cod.trip[1])
    
    t2 <- seq(from=floor_date(x$DATE[1],unit="hour"),ceiling_date(x$DATE[length(x$DATE)],unit="hour"),by=60*60)
    if (length(t2) >= 3){ # if there are at least 3 records in the trip
      # tempo <- t2[2:(length(t2)-1)][1]
      interp <- lapply(t2[2:(length(t2)-1)],function(tempo){
        dif.tiempo <- difftime(x$DATE,tempo)
        ind1 <- which(dif.tiempo<0)[sum(dif.tiempo<0)]
        ind3 <- ind1+1
        lon2 <- (x$LONGITUDE_M[ind3] - x$LONGITUDE_M[ind1])*as.numeric(difftime(tempo,x$DATE[ind1],units = "hour"))/as.numeric(difftime(x$DATE[ind3],x$DATE[ind1],units="hours")) + x$LONGITUDE_M[ind1]
        lat2 <- (x$LATITUDE_M[ind3] - x$LATITUDE_M[ind1])*as.numeric(difftime(tempo,x$DATE[ind1],units = "hour"))/as.numeric(difftime(x$DATE[ind3],x$DATE[ind1],units="hours")) + x$LATITUDE_M[ind1]
        ind.comp <- which.min(abs(difftime(x$DATE,tempo,units = "mins"))) 
        
        cbind.data.frame(lon2,lat2,LON.PROCHE=x$v_lon[ind.comp],LAT.PROCHE=x$v_lat[ind.comp],DATE.PROCHE=x$DATE[ind.comp],dist.nearest.port=x$dist.port[ind.comp],
                         nearest.port=x$port[ind.comp],DATE.INTERP=tempo)
      })
      
      matriz.int <- do.call(rbind.data.frame,interp)
      
      # we have a matrix of interpolated locations for hourly dates, but now we need to decide if we begin
      # by one of these or one hour before, and if we end by one of these or one hour after
      
      ind.comp <- 1
      tempo <- t2[1]
      comp <- as.numeric(difftime(x$DATE[1],tempo,units = "mins"))
      if (comp <= 30){
        ind1 <- 1 #order(dif.tiempo)[1]
        ind3 <- 2 #order(dif.tiempo)[2]
        lon2 <- (x$LONGITUDE_M[ind3] - x$LONGITUDE_M[ind1])*as.numeric(difftime(tempo,x$DATE[ind1],units = "hour"))/as.numeric(difftime(x$DATE[ind3],x$DATE[ind1],units="hours")) + x$LONGITUDE_M[ind1]
        lat2 <- (x$LATITUDE_M[ind3] - x$LATITUDE_M[ind1])*as.numeric(difftime(tempo,x$DATE[ind1],units = "hour"))/as.numeric(difftime(x$DATE[ind3],x$DATE[ind1],units="hours")) + x$LATITUDE_M[ind1]
        
        matriz.int <- rbind.data.frame(cbind.data.frame(lon2,lat2,LON.PROCHE=x$v_lon[ind.comp],LAT.PROCHE=x$v_lat[ind.comp],DATE.PROCHE=x$DATE[ind.comp],dist.nearest.port=x$dist.port[ind.comp],
                                                        nearest.port=x$port[ind.comp],DATE.INTERP=tempo),matriz.int)
        
      }
      ind.comp <- length(x$DATE)
      tempo <- t2[length(t2)]
      comp <- as.numeric(difftime(x$DATE[ind.comp],tempo,units = "mins"))
      if (comp >= -30){
        ind1 <- ind.comp #order(dif.tiempo)[1]
        ind3 <- ind.comp - 1 #order(dif.tiempo)[2]
        lon2 <- (x$LONGITUDE_M[ind3] - x$LONGITUDE_M[ind1])*as.numeric(difftime(tempo,x$DATE[ind1],units = "hour"))/as.numeric(difftime(x$DATE[ind3],x$DATE[ind1],units="hours")) + x$LONGITUDE_M[ind1]
        lat2 <- (x$LATITUDE_M[ind3] - x$LATITUDE_M[ind1])*as.numeric(difftime(tempo,x$DATE[ind1],units = "hour"))/as.numeric(difftime(x$DATE[ind3],x$DATE[ind1],units="hours")) + x$LATITUDE_M[ind1]
        ind.comp <- which.min(abs(difftime(x$DATE,tempo,units = "mins"))) 
        
        matriz.int <- rbind.data.frame(matriz.int,cbind.data.frame(lon2,lat2,LON.PROCHE=x$v_lon[ind.comp],LAT.PROCHE=x$v_lat[ind.comp],DATE.PROCHE=x$DATE[ind.comp],dist.nearest.port=x$dist.port[ind.comp],
                                                                   nearest.port=x$port[ind.comp],DATE.INTERP=tempo))
        
      }
      matriz.int <- cbind.data.frame(matriz.int,VESSEL_FK=rep(x$l_bat[1],dim(matriz.int)[1]),
                                     FISHING_TRIP_FK=rep(x$cod.trip[1],dim(matriz.int)[1]),
                                     DEPARTURE_DATE_TIME=rep(x$DATE[1],dim(matriz.int)[1]),
                                     RETURN_DATE_TIME=rep(x$DATE[length(x$DATE)],dim(matriz.int)[1]),
                                     DEPARTURE_LOCATION_NAME=rep(x$port[1],dim(matriz.int)[1]),
                                     RETURN_LOCATION_NAME=rep(x$port[length(x$port)],dim(matriz.int)[1])
      )
      
      
    }
  })
  
  interpoL <- do.call(rbind.data.frame,interpL)
  
  vms.interpol <- unprojet(interpoL)
  vms.interpol$date.dif <- abs(as.numeric(difftime(vms.interpol$DATE.PROCHE,vms.interpol$DATE.INTERP,units = 'hours')))
  
  # just in case there are duplicates after interpolation and adding first or last record
  vms.interpol$VESSEL_FK <- as.character(vms.interpol$VESSEL_FK)
  check <- 1
  while(length(check) > 0){
    vms.interpol$lignes <- 1:dim(vms.interpol)[1]
    check <- unlist(lapply(split(vms.interpol,vms.interpol$VESSEL_FK,drop=TRUE),function(x){
      if (sum(diff(x$DATE.INTERP)==0)>0){
        ind <- which(diff(x$DATE.INTERP) == 0)
        ind <- c(ind, ind+1) # lineas repetidas
        x$lignes[ind[which.max(abs(difftime(x$DATE.INTERP[ind],x$DATE.PROCHE[ind],units="hours")))]]
      }
    }))
    if (length(check) > 0){
      vms.interpol <- vms.interpol[-check,]
    }
  }
  
  # derivation of other variables
  
  data.ltraj <- as.ltraj(xy = vms.interpol[,c("lon2","lat2")],date=vms.interpol$DATE.INTERP,id=vms.interpol[,"VESSEL_FK"],
                         burst=vms.interpol[,"FISHING_TRIP_FK"],infolocs=vms.interpol[,c(3:7,11:18)])
  vms.ld <- ld(data.ltraj)
  
  # average speed
  vms.ld$avg.speed <- vms.ld$dist/vms.ld$dt # m/s
  # persistence speed (tendency to persist in the previous direction)
  vms.ld$p.speed <- vms.ld$avg.speed*cos(vms.ld$rel.angle)
  # rotational speed (tendency to turn)
  vms.ld$r.speed <- vms.ld$avg.speed*sin(vms.ld$rel.angle)
  vms.ld$lignes <- 1:dim(vms.ld)[1]
  
  return(vms.ld)
}