Indices.Derivation <- function(data,segments,year,dossier.data.outputs,param,all=TRUE,gear){
  
  # if all is TRUE, then list of indices are c("Prox","Cs1","Cs2","jPPA","r","DId","DItheta","DI","CSEM","WCavg","WCmax","WCavglss","WCmaxlss")
  # r is correlation in lon, lat, lonlat (avg of lon and lat), speed
  # if all is FALSE, then all of them except jPPA are derived
  
  if (all == TRUE){
    # indices.others <- indices.list[-ind.jPPA]
    indices.jPPA <- 1
  }else{
    # ind.jPPA <- grep('jPPA',indices.list)
    # if (length(ind.jPPA) == 1){
    # indices.others <- indices.list[-ind.jPPA]
    indices.jPPA <- 0
    # }else{
    # indices.others <- indices.list
    # indices.jPPA <- 1
    # }
  } 
  
  pre.trait <- function(dyad.db,vms.db){
    # vms.year <- vms.db[which(year.lubridate(vms.db$date) == dyad.db$year),]
    tr1 <- data[dyad.db$first.line.1:dyad.db$last.line.1,]
    tr2 <- data[dyad.db$first.line.2:dyad.db$last.line.2,]
    A <- cbind(tr1[,c("x","y")])
    B <- cbind(tr2[,c("x","y")])
    distances <- diag(as.matrix(pdist.pdist(X = A,Y = B)))/1000
    longueur <- length(A[,1])
    distance1 <- sqrt((diff(A[,1]))^2 + (diff(A[,2]))^2)
    distance2 <- sqrt((diff(B[,1]))^2 + (diff(B[,2]))^2)
    return(list(A=A,B=B,distances=distances,longueur=longueur,
                distance1=distance1,distance2=distance2,tr1=tr1,tr2=tr2))
  }
  
  data.indic <- do.call(rbind.data.frame,lapply(1:(dim(segments)[1]),function(x){
    print(x)
    echant <- segments[x,]
    pre.donnees <- pre.trait(dyad.db = echant,vms.db = data)
    
    # number.indices <- (length(indices.list)-2) + length(param$Prox$delta) + length(param$r$var)
    # all indices get one value except Prox and r
    
    
    Prox <- sapply(1:length(param$Prox$delta),function(delta){
      delta.value <- param$Prox$delta[delta]
      sum(as.numeric(pre.donnees$distances < delta.value))/pre.donnees$longueur
    })
    
    ind.dProche <- which(pre.donnees$distances <= param$Dyad$Proche)
    Proche <- 0
    if (length(ind.dProche) > 0){
      if(any(ind.dProche!= 1 & ind.dProche!= pre.donnees$longueur) == TRUE){
        Proche <- 1
      }
    }
      
    # Cs.res <- Cs.function(A=pre.donnees$A,B=pre.donnees$B,distances=pre.donnees$distances)
    
    rlon = cor(pre.donnees$A[,1],pre.donnees$B[,1])
    rlat = cor(pre.donnees$A[,2],pre.donnees$B[,2])
    rtot = (rlon+rlat)/2
    rspeed = cor(pre.donnees$tr1$avg.speed,pre.donnees$tr2$avg.speed,use='pairwise.complete.obs')
    
    # CSEM <- CSEM.function(distances=pre.donnees$distances,longueur = pre.donnees$longueur,
                          # delta=param$CSEM$delta)
    
    DI <- DI.function(A=pre.donnees$A,B=pre.donnees$B,delta.DI = param$DI$beta,
                      longueur = pre.donnees$longueur,distance1 = pre.donnees$distance1,
                      distance2 = pre.donnees$distance2)
    
    # WC <- WC.function(distance1 = pre.donnees$distance1,distance2 = pre.donnees$distance2,
                      # sl=param$WC$sl,su=param$WC$su,epsilon=param$WC$xi)
    
    # return(cbind.data.frame(t(Prox),Cs.res$Cs,Cs.res$Cs.2,rlon,rlat,rtot,rspeed,CSEM,DI$DI,DI$DI.d,DI$DI.theta,WC$WC.avg,WC$WC.max,WC$WC.avg.lss,WC$WC.max.lss,Proche))
    return(cbind.data.frame(t(Prox),rlon,rlat,rtot,rspeed,DI$DI,DI$DI.d,DI$DI.theta,Proche))
  }))
  
  colnames(data.indic) <- c(paste0("Prox.",param$Prox$delta),"r.lon","r.lat","r.lonlat","r.speed","DI.d","DI.theta","DI","Proche")
                     
             
    if (all == TRUE){
      
      jPPA.stat <- unlist(lapply(1:(dim(segments)[1]),function(x){
        print(x)
        echant <- segments[x,]
        pre.donnees <- pre.trait(dyad.db = echant,vms.db = vms.data)
        jPPA.index <- jPPA(A = pre.donnees$A,B=pre.donnees$B,Dmax=param$jPPA$vmax,tam.cel = param$jPPA$cell.size) # in this case vmax is the same as Dmax # cellgrids of 3km
        return(jPPA.index)
      }))
      
      # first.names <- colnames(data.indic)
      data.indic <- cbind.data.frame(data.indic,jPPA.stat)
      
    }
  
    data.dyads <- cbind.data.frame(segments,data.indic)
    
    save(data.dyads,file=paste0(dossier.data.outputs,"segments-indic-",gear,'-',year,'.RData'))
  
    return(data.dyads)
  
}