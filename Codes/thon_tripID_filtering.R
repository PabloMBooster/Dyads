thon_tripID_filtering <- function(db,port,thres.dist.port.1,thres.port,vit,seuil){
  
  db$timestamp_vms <- as.character(db$timestamp_vms)
  db$date_vms <- as.character(db$date_vms)
  db$time_vms <- as.character(db$time_vms)
  db$v_lat <- as.numeric(as.character(db$v_lat))
  db$v_lon <- as.numeric(as.character(db$v_lon))
  # db$DATE <- as.POSIXct(db$timestamp_vms,tz='GMT',format='%Y-%m-%d %H:%M:%S')
  
  # trip identification
  
  # computing minimum distance to a port (and which port that is)
  dist.port.min <- sapply(1:dim(db)[1],function(y){
    dist.port <- sapply(1:dim(port)[1],function(x){
      distance(lon = db$v_lon[y],lat=db$v_lat[y],lonRef = port$lon[x],latRef = port$lat[x]) # it's in km
    })
    
    # if(min(dist.port) < thres.port){
    cbind(min(dist.port),which.min(dist.port))
    # }
  })
  db$dist.port <- dist.port.min[1,]
  db$port <- port$port[dist.port.min[2,]]
  
  # sorting data by vessel and date
  db <- db[with(db, order(l_bat, DATE)), ]
  
  # check for not duplicated records
  db$ligne <- 1:dim(db)[1]
  sub.vms.data <- unlist(sapply(split(db,db$l_bat,drop=TRUE),function(x){
    x$ligne[which(duplicated(x$DATE) == 'FALSE')]
  })) # no hay
  db <- db[sub.vms.data,]
  db$ligne <- 1:dim(db)[1]
  
  # computing time and distance (with vmstools so in km) between consecutive records, and then deriving speed
  tiempo <- as.numeric(difftime(db$DATE[2:length(db$DATE)],db$DATE[1:(length(db$DATE)-1)],units = "hours"))
  db$delta.t <- c(NA,tiempo)
  db$delta.t[which(duplicated(db$l_bat) == 'FALSE')] <- NA
  distancia <- distance(db$v_lon[2:length(db$DATE)],db$v_lat[2:length(db$DATE)],db$v_lon[1:(length(db$DATE)-1)],db$v_lat[1:(length(db$DATE)-1)])
  db$delta.d <- c(NA,distancia)
  db$delta.d[which(duplicated(db$l_bat) == 'FALSE')] <- NA
  db$sp.calc <- db$delta.d/db$delta.t
  
  # port or no port?
  ind.port <- which(db$dist.port < thres.dist.port.1  | (db$dist.port < thres.port & db$sp.calc < vit))
  db$ind.port <- rep(0,dim(db)[1])
  db$ind.port[ind.port] <- 1
  
  # generating an ID for the trips
  db$cod.trip <- rep(NA,dim(db)[1])
  cod <- 1
  # x <- split(db,db$c_bat,drop=TRUE)[[10]]
  # x$c_bat[1]
  
  maree <- sapply(split(db,db$c_bat,drop=TRUE),function(x){
    ind.diff <- diff(x$ind.port) 
    ind.diff <- c(NA,ind.diff)
    ind.ini <- which(ind.diff == -1) - 1
    ind.ini <- ind.ini[ind.ini > 0]
    ind.fin <- which(ind.diff == 1)
    if (length(ind.ini) > 0){
      # k = 1
      if (ind.fin[1] < ind.ini[1]){ # if the track starts at sea, we give it a new trip ID
        if (ind.fin[1] > 10){
          db$cod.trip[x$ligne[1:ind.fin[1]]] <<- cod #
          cod <<- cod + 1
        }
        ind.fin <- ind.fin[2:length(ind.fin)]
      }
      while (length(ind.fin) > 0 & length(ind.ini) > 0){
        if (db$delta.t[x$ligne[ind.ini[1]+1]] > 24 ){ # if there is more than 1 day between consecutive records
          if (ind.fin[1] - ind.ini[1] > 10){ # but there are more than 10 records, we could split in two tracks
            db$cod.trip[x$ligne[(ind.ini[1]+1):ind.fin[1]]] <<- cod #
            cod <<- cod + 1
            ind.fin <- ind.fin[-1]
            ind.ini <- ind.ini[-1]
          }else{
            ind.fin <- ind.fin[-1]
            ind.ini <- ind.ini[-1]
          }
          # print('help!')
        }else if(ind.fin[1] - ind.ini[1] > 10){ # vu que les senneurs font des marees tres longues
          db$cod.trip[x$ligne[ind.ini[1]:ind.fin[1]]] <<- cod #
          cod <<- cod + 1
          ind.fin <- ind.fin[-1]
          ind.ini <- ind.ini[-1]
        }else{
          ind.fin <- ind.fin[-1]
          ind.ini <- ind.ini[-1]
        }
        
      }
      if (length(ind.ini) == 1 & length(ind.fin) == 0){
        if (length(x$ligne)-ind.ini>10){
          db$cod.trip[x$ligne[ind.ini[1]:length(x$ligne)]] <<- cod #
          cod <<- cod + 1
        }
        ind.ini <- ind.ini[-1]
      }
    }
  }) 
  
  
  # the code was already put in db column cod.trip
  
  # keep only the lines corresponding to trips 
  db.maree <- db[which(is.na(db$cod.trip) == FALSE),]
  db.maree$ligne <- 1:dim(db.maree)[1]
  
  # keep only trips with less or equal than 1 hour between consecutive records, or, if they have longer time intervals,
  # that those differences (after the allowed hour, in cumulated time for the whole trip, should not represent more than 10% of the trip's duration)
  # seuil <- 1 
  time.maree <- unlist(sapply(split(db.maree,db.maree$cod.trip,drop=TRUE),function(x){
    a <- as.numeric(difftime(x$DATE[2:length(x$DATE)],x$DATE[1:(length(x$DATE)-1)],units='hours'),units='hours')
    if (any(a>seuil)){
      if (sum(a[which(a > seuil)] - seuil)/as.numeric(difftime(time1=x$DATE[length(x$DATE)],time2 = x$DATE[1],units = 'hours'))*100 >= 10){
        x$ligne
        # b <<- b + 1
      }
      # print(x$FISHING_TRIP_FK[1])
      
    }
    # return(b)
  }))
  
  db.maree <- db.maree[-time.maree,] 
  db.maree$ligne <- 1:dim(db.maree)[1]
  
  # now, projecting 
  ech.vms.data <- projet(db.maree,"v_lon","v_lat")
  
  return(ech.vms.data)
  
}