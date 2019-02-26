plots.video.dyads2 <- function(vms.data,dyads,year,gear,new.variables,dossier.stats.outputs,dossier.data.outputs,n=5,text.dyads=TRUE,
                              video.par,land.info,param=NULL,colores=NULL){
  # harbours.info=FALSE,ports=NULL,context.video.info=FALSE,rayon=1,anonymity=FALSE,traza=5,
  # col_vector=NULL,engin.args=NULL,epais=4){
  # n dyads from each cluster for plots and videos
  # those n dyads will be the closest to the center of gravity of their cluster; a.k.a the most representative ones
  # that is equal to the difference between mjm and the mean mjm of the cluster
  
  mjm.cluster <- aggregate(mjm ~ numclust,data=dyads,FUN=mean)
  dyads$mjm.dif <- abs(dyads$mjm - mjm.cluster[dyads$numclust,2])
  k <- length(unique(dyads$numclust))
  
  # echantillon <- NULL
  echantillon <- as.data.frame(matrix(NA,ncol=dim(dyads)[2],nrow = n*))
  colnames(echantillon) <- names(dyads)
  # n <- 5
  ## vessels and dyads in first (strongest) group
  rien <- sapply(1:k,function(group){
    ind.1 <- which(dyads$numclust == group)
    muestra <- order(dyads$mjm.dif[ind.1],decreasing = FALSE)[1:n]
    echantillon[(1:n)+((group-1)*n),] <<- dyads[ind.1[muestra],]
  })
  
  if (!any(names(echantillon) == "year")){ # that's anchovy!
    echantillon$year <- rep(year,nrow(echantillon))
    names(echantillon) <- c(names(echantillon)[1:5],"first.line.1","last.line.1","first.line.2","last.line.2",
                            names(echantillon)[10:ncol(echantillon)])
  }else if(any(names(echantillon) == "line.first.1")){ # that's tuna!
    names(echantillon) <- c(names(echantillon)[1:5],"first.line.1","last.line.1","first.line.2","last.line.2",
                            names(echantillon)[10:ncol(echantillon)])
  }
  
  save(echantillon,file = paste0(dossier.data.outputs,'muestra_clusters_CG_',gear,'_',year,'_',paste(str_replace_all(new.variables, "[[:punct:]]", ""),collapse='-'),'.RData'))
  
  # echantillon = read.csv(file=paste0('muestra_clusters_CG_',gear,'_',year,'_',paste(str_replace_all(variables, "[[:punct:]]", ""),collapse='-'),'.csv'),header=TRUE)
  
  if (text.dyads == TRUE){
    dyad.file = paste0(dossier.stats.outputs,'dyads_',gear,'_clusters_',k,'_samplesCG_',year,'_','-',paste(str_replace_all(new.variables, "[[:punct:]]", ""),collapse='-'),'.txt')
    sink(dyad.file)
    sink()
    
  }
  
  action <- sapply(1:(dim(echantillon)[1]),function(x){
    print(x)
    # print(echant)
    echant <- echantillon[x,]
    bato1 <- echant$id.1
    bato2 <- echant$id.2
    
    vms.year <- vms.data[which(lubridate:::year(vms.data$date) == echant$year),]
    first_lines <- sapply(echant$first.line.1:echant$last.line.1,function(x) match(x,vms.year$lignes)) # if error, try line.first.1 and line.last.1
    tr1 <- vms.year[first_lines,] 
    first_lines <- sapply(echant$first.line.2:echant$last.line.2,function(x) match(x,vms.year$lignes)) # if error, try line.first.1 and line.last.1
    tr2 <- vms.year[first_lines,] # idem
    
    if (text.dyads == TRUE){
      dyad_info_gral(tr1,tr2,echant,file.name=dyad.file)
    }
    
    matriz <- rbind(tr1,tr2)
    fecha.min <- min(matriz$date)
    fecha.max <- max(matriz$date)
    data.plot.2 <- pre.video.modif(subset.plot = matriz,col_vector = video.par$col_vector,engin.args=video.par$engin.args,epais=video.par$epais,anonymity=video.par$anonymity)
    
    if (video.par$anonymity == TRUE){
      Name.Mod <- "Modif"
    }else{
      Name.Mod <- "Origi"
    }
    
    if (video.par$harbours.info == TRUE){
      harb <- unique(c(as.character(unique(matriz$DEPARTURE_LOCATION_NAME)),as.character(unique(matriz$RETURN_LOCATION_NAME))))
      harbours.plot <- tolower(sapply(harb,function(y) str_replace_all(y, "[^[:alnum:]]", " ")))
      harbours.comprime <- sapply(harbours.plot,function(x) paste(unlist(strsplit(as.character(x)," ")),collapse=""))
      ind.port <- sapply(harbours.comprime,function(y) which(video.par$ports$port.comprime == y))
      puertos.info <- video.par$ports[unlist(ind.port),]
      if ((dim(puertos.info)[1] > 0) == FALSE){
        video.par$harbours.info = FALSE
        puertos.info <- NULL
      }
    }
    
    
    if (video.par$type == 'video' || video.par$type == 'all'){
      
      if (video.par$context.video.info == TRUE){
        rango.lat <- range(matriz$LATITUDE) + c(-video.par$rayon/110.57,video.par$rayon/110.57) # +- 1km it's better to do it with lon lat than x y for visual purposes
        rango.lon <- range(matriz$LONGITUDE) + c(-video.par$rayon/110.57,video.par$rayon/110.57) # +- 1km
        ind.all <- which(vms.year$date <= fecha.max & vms.year$date >= fecha.min &
                           vms.year$LONGITUDE <= rango.lon[2] & vms.year$LONGITUDE >= rango.lon[1] &
                           vms.year$LATITUDE <= rango.lat[2] & vms.year$LATITUDE >= rango.lat[1])
        matriz.all <- vms.year[ind.all,]
        data.plot <- pre.video.dyads.all(subset.plot=matriz.all,video.par$col_vector,video.par$engin.args,dyad=c(bato1,bato2),epais=video.par$epais,anonymity=video.par$anonymity)  
        
        name <- paste0("film-",Name.Mod,"-All-",gear,'-id-',gsub(" ", "-", bato1),'-',gsub(" ", "-", bato2),"-",
                       paste(unlist(strsplit(as.character(date(fecha.min)),"-")),collapse = ""),"-",
                       paste(unlist(strsplit(as.character(date(fecha.max)),"-")),collapse = ""))
        image.name <- paste0("film-",Name.Mod,"-All-id-",gsub(" ", "-", bato1),'-',gsub(" ", "-", bato2),"-",
                             paste(unlist(strsplit(as.character(date(fecha.min)),"-")),collapse = ""),"-",
                             paste(unlist(strsplit(as.character(date(fecha.max)),"-")),collapse = ""),"-")
        
      }else{
        data.plot <- data.plot.2
        name <- paste0("film-",Name.Mod,"-Two-",gear,'-id-',gsub(" ", "-", bato1),'-',gsub(" ", "-", bato2),"-",
                       paste(unlist(strsplit(as.character(date(fecha.min)),"-")),collapse = ""),"-",
                       paste(unlist(strsplit(as.character(date(fecha.max)),"-")),collapse = ""))
        
        image.name <- paste0("film-",Name.Mod,"-Two-id-",gsub(" ", "-", bato1),'-',gsub(" ", "-", bato2),"-",
                             paste(unlist(strsplit(as.character(date(fecha.min)),"-")),collapse = ""),"-",
                             paste(unlist(strsplit(as.character(date(fecha.max)),"-")),collapse = ""),"-")
      }
      
      #file.name <- paste0(name,".gif") # ><>><>><>><>
      image.dir.movie <- paste0(dossier.stats.outputs,gear,"/Movies/",name)
      
      if (dir.exists(image.dir.movie)){
        print(paste0(name,' already exists, so not doing anything!'))
        # 
        # old.path <- getwd()
        # setwd(paste0(dossier.stats.outputs,"Movies/",gear,"/")) # t
        # print(getwd())
        # setwd(old.path)
        # # unlink(image.dir,recursive=TRUE)
      }else{
        if (x == 1){
          dir.create(image.dir.movie,recursive=TRUE) # creating directory where png files will be stocked
        }else{dir.create(image.dir.movie,recursive=FALSE)} # creating directory where png files will be stocked
        
        # old.path <- getwd()
        # setwd(paste0(dossier.stats.outputs,"Movies/")) # this if for the html file
        png.dir <- paste0(dossier.stats.outputs,gear,"/Movies/",name)
        
        # creating html
        no.display <- video.dyad.giff(data.video=data.plot,engin.args=video.par$engin.args,file.name=file.name,image.name=image.name,image.dir=png.dir,
                                      sleep=video.par$sleep,point.size=video.par$point.size,traza=video.par$traza,harbour.info=video.par$harbours.info,
                                      harbour.data=puertos.info,espece=video.par$espece,legend.vessels=video.par$legend.vessels,
                                      legend.gears=video.par$legend.gears,range.x = video.par$range.x,range.y = video.par$range.y,
                                      col.text.bat='black',anonymity=video.par$anonymity,land=land.info,land.color='darkgreen')
        
        if (video.par$mp4 == TRUE){
          # creating mp4 (needs png created during html process)
          # setwd(image.dir.movie)
          fichiers= paste0(image.dir.movie,"/",image.name,"%d.png")
          fichier_out <- paste0(dossier.stats.outputs,gear,"/Movies/",name,".mp4")
          system(paste("ffmpeg -framerate 10 -i ",fichiers," -c:v libx264 ", fichier_out,sep=""))
        }
        # setwd(old.path)
      }
    }
    
    if (video.par$type == 'plot' || video.par$type == 'all'){
      
      image.name <- paste0("plot-id-",gsub(" ", "-", bato1),'-',gsub(" ", "-", bato2),"-",
                           paste(unlist(strsplit(as.character(date(fecha.min)),"-")),collapse = ""),"-",
                           paste(unlist(strsplit(as.character(date(fecha.max)),"-")),collapse = ""))
      
      plot.dyad(data.video=data.plot.2,engin.args=video.par$engin.args,file.name=image.name,point.size=video.par$point.size,anonymity=video.par$anonymity,
                land=land.info,land.color='darkgreen',harbour.info=video.par$harbours.info,harbour.data=puertos.info,dossier.stats.outputs=paste0(dossier.stats.outputs,gear,"/"),
                line.width=3)
    }
    
    if (video.par$type == 'series' || video.par$type == 'all'){
      # vector fechas
      fechas <- tr1$date

      # GIF LONGITUDE -----------------------------------------------------------

      variable <- "LONGITUDE"
      image.name =  paste0('VariableSeries-id-',gsub(" ", "-", bato1),'-',gsub(" ", "-", bato2),"-",
                              paste(unlist(strsplit(as.character(date(fecha.min)),"-")),collapse = ""),
                              '-',
                              paste(unlist(strsplit(as.character(date(fecha.max)),"-")),collapse = ""),'-',variable)
      file.name = paste0(image.name, ".gif")
      subplot.cor.gif(image.name, file.name, tr1, tr2, variable, r.cal = FALSE)

      # GIF LATITUDE ------------------------------------------------------------

      variable <- "LATITUDE"
      image.name =  paste0('VariableSeries-id-',gsub(" ", "-", bato1),'-',gsub(" ", "-", bato2),"-",
                              paste(unlist(strsplit(as.character(date(fecha.min)),"-")),collapse = ""),
                              '-',
                              paste(unlist(strsplit(as.character(date(fecha.max)),"-")),collapse = ""),
                              '-',variable)
      file.name = paste0(image.name, ".gif")
      subplot.cor.gif(image.name, file.name, tr1, tr2, variable, r.cal = FALSE)


      # GIF DISTANCE ------------------------------------------------------------
      DO <- sqrt(((tr1$x - tr2$x)^2) + ((tr1$y - tr2$y)^2))/1000
      fechas <- tr1$date

      n.prox <- length(param$Prox$delta)
      
      if (is.null(colores)){
        colores=brewer.pal(n.prox, "Set1") # max 9
      }
      
      variable <- "DO"
      image.name =  paste0('VariableSeries-id-',gsub(" ", "-", bato1),'-',gsub(" ", "-", bato2),"-",
                              paste(unlist(strsplit(as.character(date(fecha.min)),"-")),collapse = ""),
                              '-',
                              paste(unlist(strsplit(as.character(date(fecha.max)),"-")),collapse = ""),
                              '-',variable)
      file.name = paste0(image.name, ".gif")
      subplot.uni.DO.gif(image.name, file.name, fechas, variable = DO, n.prox, param_Prox_delta = param$Prox$delta, colores)


      # GIF SPEED ---------------------------------------------------------------

      tr1$avg.speed <-  tr1$avg.speed*3600/(1000*1.852) # m/s to knots
      tr2$avg.speed <-  tr2$avg.speed*3600/(1000*1.852) # m/s to knots
      #variable <- "avg.speed"

      variable <- "avg.speed"
      image.name =  paste0('VariableSeries-id-',gsub(" ", "-", bato1),'-',gsub(" ", "-", bato2),"-",
                          paste(unlist(strsplit(as.character(date(fecha.min)),"-")),collapse = ""),
                          '-',
                          paste(unlist(strsplit(as.character(date(fecha.max)),"-")),collapse = ""),
                          '-',str_replace_all(variable, "[[:punct:]]", ""))
      file.name = paste0(image.name, ".gif")
      subplot.cor.gif(image.name, file.name, tr1, tr2, variable, r.cal = TRUE)


      # GIF DItheta -------------------------------------------------------------

      distance1 <- sqrt((diff(tr1$x))^2 + (diff(tr1$y))^2)
      distance2 <- sqrt((diff(tr2$x))^2 + (diff(tr2$y))^2)
      DId0 <- 1 - (abs(distance1-distance2)/(distance1+distance2))^param$DI$beta
      DId0[which(distance1+distance2 == 0)] <- 1
      DId0.d <- c(NA,DId0)
      DItheta <- cos(tr1$abs.angle - tr2$abs.angle)
      DItheta[which(is.na(tr1$abs.angle)== TRUE & is.na(tr2$abs.angle) == TRUE)] <- 1
      DItheta[which(is.na(DItheta))] <- 0
      DI0 <- DItheta*DId0.d

      variable <- "DItheta"
    
      image.name = paste0('VariableSeries-id-',gsub(" ", "-", bato1),'-',gsub(" ", "-", bato2),"-",
                         paste(unlist(strsplit(as.character(date(fecha.min)),"-")),collapse = ""),
                         '-',
                         paste(unlist(strsplit(as.character(date(fecha.max)),"-")),collapse = ""),
                         '-',str_replace_all(variable, "[[:punct:]]", ""))
                         
      file.name <- paste0(image.name, ".gif")
      fechas <- tr1$date
      # 
      subplot.uni.DI.theta(image.name, file.name, fecha=fechas,variable=DItheta,nombre="DItheta")
    #   img.name = image.name, movie.name = file.name
     }
  })
}
