thon_dyadsegmentation <- function(vms.data){
  
  sub.data <- vms.data[,c("id","date","burst","lignes")]
  rownames(sub.data) <- NULL
  rm(vms.data)
  sub.data$id <- as.character(sub.data$id)
  sub.data$burst <- as.numeric(as.character(sub.data$burst))
  
  
  # dyads of vessels at sea
  
  # x <- sub.data[which(sub.data$id == sort(unique(sub.data$id))[1]),]
  # y <- x[which(x$burst == sort(unique(x$burst))[1]),]
  # z <- otros.fechas[which(otros.fechas$id == sort(unique(otros.fechas$id))[1]),]
  # w <- z[which(z$burst == sort(unique(z$burst))[1]),]
  prueba <- sapply(split(sub.data,sub.data$id,drop=T),function(x){ # for each vessel, we make a loop for each trip
    otros.barcos <- sub.data[which(sub.data$id > x$id[1]),]
    if (dim(otros.barcos)[1] > 0){ # only worth doing this if there are other vessels in the dataset
      prueba.3 <- lapply(split(x,x$burst,drop=T),function(y){ # one loop per trip of vessel from x; check now dates from this trip in which other vessels are at sea 
        otros.fechas <- otros.barcos[which(otros.barcos$date >= min(y$date) & otros.barcos$date <= max(y$date)),]
        if (dim(otros.fechas)[1] > 0){
          prueba.2 <- sapply(split(otros.fechas,otros.fechas$id,drop=T),function(z){ # one loop per vessel with trip dates in that range
            # matriz <- matrix(NA,ncol=9,nrow=length(unique(z$FISHING_TRIP_FK)))
            prueba.4 <- sapply(split(z,z$burst,drop=T),function(w){ # one loop per trip of the companion vessel
              derniere <- dim(w)[1]
              dif.time <- as.numeric(difftime(w$date[derniere],w$date[1],unit="hours"))
              lineas.1 <- which(y$date>= w$date[1] & y$date <= w$date[derniere])
              if (length(lineas.1) > 0){
                matriz <- cbind(vessel1=y$id[lineas.1[1]],vessel2=w$id[1],
                                trip1=y$burst[lineas.1[1]],trip2=w$burst[1],
                                duree=dif.time,
                                # dateini=as.character(w$DATE.ROUND[1]),datefin=as.character(w$DATE.ROUND[derniere]),
                                lineini1=y$lignes[lineas.1[1]],
                                linefin1=y$lignes[lineas.1[length(lineas.1)]],
                                lineini2=w$lignes[1],
                                linefin2=w$lignes[derniere],
                                year = lubridate:::year(w$date[1]))
                return(matriz)
              }
              
            })
            # prueba.4 <- as.data.frame(prueba.4)
            prueba.4 <- prueba.4[!sapply(prueba.4,is.null)]
            return(prueba.4)
          })
          prueba.2 <- matrix(unlist(prueba.2),ncol=10,nrow = length(unlist(prueba.2))/10,byrow=TRUE)
          # as.data.frame(prueba.2)
          print(y$burst[1])
          return(prueba.2)
        }
      })
      # prueba.t <- t(as.data.frame(prueba.3))
      # prueba.t[1,6][[1]] # asi recupero fechas
      prueba.t <- do.call(rbind.data.frame,prueba.3)
      prueba.t$V1 <- as.character(prueba.t$V1)
      prueba.t$V2 <- as.character(prueba.t$V2)
      prueba.t$V3 <- as.numeric(as.character(prueba.t$V3))
      prueba.t$V4 <- as.numeric(as.character(prueba.t$V4))
      prueba.t$V5 <- as.numeric(as.character(prueba.t$V5))
      prueba.t$V6 <- as.numeric(as.character(prueba.t$V6))
      prueba.t$V7 <- as.numeric(as.character(prueba.t$V7))
      prueba.t$V8 <- as.numeric(as.character(prueba.t$V8))
      prueba.t$V9 <- as.numeric(as.character(prueba.t$V9))
      prueba.t$V10 <- as.numeric(as.character(prueba.t$V10))
      print(x$id[1])
      return(prueba.t)
      
    }
  })
  
  prueba <- prueba[!sapply(prueba,is.null)]
  segments <- do.call(rbind.data.frame,prueba)
  
  colnames(segments) <- c("id.1","id.2","burst.1","burst.2","dur","first.line.1","last.line.1","first.line.2","last.line.2","year")
  rownames(segments) <- NULL
  
  segments <- segments[segments$dur > 3,] # they need to have at least 3 time steps for next step (metrics computation)
  
  return(segments)
  
}
