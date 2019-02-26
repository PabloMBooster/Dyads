Fleet.Analysis <- function(vms.data,dyads,Weights,dossier.data.outputs,dossier.stats.outputs,gear,year,fleet.plot.par,remplir.mjm,fleet.cluster=NULL){
  
  vessels.1 <- unique(c(dyads$id.1,dyads$id.2))
  k <- length(unique(dyads$numclust))
  
  stats.bat <- as.data.frame(matrix(0,nrow = length(vessels.1),ncol=4+k*2))
  colnames(stats.bat) <- c("Vessel","Total",paste0("Clust.",1:k),paste0("Prop.",1:k),"MJM.avg","MJM.median")
  stats.bat$Vessel <- vessels.1
  
  stats.bat.trip <- as.data.frame(matrix(0,nrow = length(vessels.1),ncol=4))
  colnames(stats.bat.trip) <- c("Vessel","Total","Clust.1",
                                "Prop.1")
  stats.bat.trip$Vessel <- vessels.1
  
  # stats of vessels in total and for each cluster
  rien <- sapply(1:length(vessels.1),function(b) {
    ind.bat <- which(dyads$id.1 == vessels.1[b] | dyads$id.2 == vessels.1[b])
    stats.bat$Total[b] <<- length(ind.bat)
    stats.bat$MJM.avg[b] <<- mean(dyads$mjm[ind.bat])
    stats.bat$MJM.median[b] <<- median(dyads$mjm[ind.bat])
    rien.2 <- sapply(1:k,function(cl){
      ind <- which(dyads$numclust[ind.bat] == cl)
      stats.bat[b,cl+2] <<- length(ind)
      if (length(ind) > 0){
        stats.bat[b,cl+k+2] <<- stats.bat[b,cl+2]/stats.bat$Total[b]
      }
    })
    ind.bat.trip.1 <- which(dyads$id.1 == vessels.1[b])
    cluster_num_1 <- which(dyads$numclust[ind.bat.trip.1] == 1)
    ind.bat.trip.2 <- which(dyads$id.2 == vessels.1[b])
    cluster_num_2 <- which(dyads$numclust[ind.bat.trip.2] == 1)
    if (length(cluster_num_2) >0 || length(cluster_num_1) > 0){
    trips <- unique(c(dyads$burst.1[ind.bat.trip.1[cluster_num_1]],
               dyads$burst.2[ind.bat.trip.2[cluster_num_2]]))
    stats.bat.trip$Clust.1[b] <<- length(trips)
    }
    # if (length(ind.bat.trip.1) > 0 || length(ind.bat.trip.2) > 0){
    #   stats.bat.trip$Total[b] <<- length(unique(c(dyads$burst.1[ind.bat.trip.1],
    #                                 dyads$burst.2[ind.bat.trip.2])))
    # }
    ind.bateau <- which(vms.data$id == vessels.1[b])
    stats.bat.trip$Total[b] <<- length(unique(vms.data$burst[ind.bateau]))
    
  })
  
  stats.bat.trip$Prop.1 <- stats.bat.trip$Clust.1/stats.bat.trip$Total
  
  print(median(stats.bat.trip$Prop.1))
  
  pdf(paste0(dossier.stats.outputs,'vessel_trip_prop_',gear,'_cluster_1_',year,'.pdf'),
      width=24,height=20,pointsize = 20)
  par(las=2)
  variable <- stats.bat.trip[,4]
  barplot(variable[order(variable,decreasing=TRUE)],
          names.arg = stats.bat.trip$Vessel[order(variable,decreasing=TRUE)],
          main = paste0('Proportion of trips with at least one dyad in cluster 1'),
          cex.names=0.6,width=stats.bat.trip$Total[order(variable,decreasing=TRUE)])
  dev.off()
  
  # rien.2 <- sapply(1:k,function(cl){
  # pdf(paste0(dossier.stats.outputs,'vessel_prop_',gear,'_cluster_',cl,'_',year,'.pdf'),
  #     width=24,height=20,pointsize = 20)
  # par(las=2)
  # variable <- stats.bat[,cl+k+2]
  # barplot(variable[order(variable,decreasing=TRUE)],
  #         names.arg = stats.bat$Vessel[order(variable,decreasing=TRUE)],
  #         main = paste0('Proportion of dyads from the vessel in cluster ',cl),
  #         cex.names=0.6,width=stats.bat$Total[order(variable,decreasing=TRUE)])
  # dev.off()
  # })
  
  # pdf(paste0(dossier.stats.outputs,'vessel_mjm_mean_',gear,'_',year,'.pdf'),
  #     width=24,height=20,pointsize = 20)
  # par(las=2)
  # barplot(stats.bat$MJM.avg[order(stats.bat$MJM.avg,decreasing=TRUE)],
  #         names.arg = stats.bat$Vessel[order(stats.bat$MJM.avg,decreasing=TRUE)],
  #         main = paste0('Average MJM from the vessel'),
  #         cex.names=0.6,width=stats.bat$Total[order(stats.bat$MJM.avg,decreasing=TRUE)])
  # dev.off()
  
  # # proportion of dyads from each couple in cluster 1 (the other ones are not easy to interpret so I'm not wasting time in plotting them)
  # 
  dyads$couple <- paste0(str_replace_all(dyads$id.1, " ", ""),'-',str_replace_all(dyads$id.2, " ", ""))
  couples <- unique(dyads$couple)
  num.couples <- length(couples)
  stats.coup <- data.frame(couple=couples,cl1=rep(NA,num.couples),tot=rep(NA,num.couples),prop=rep(NA,num.couples))
  group <- 1
  rien <- sapply(1:num.couples,function(b) {
    stats.coup$tot[b] <<- sum(dyads$couple == couples[b])
    stats.coup$cl1[b] <<- sum(dyads$couple == couples[b] & dyads$numclust == group)
    stats.coup$prop[b] <<- stats.coup$cl1[b]/stats.coup$tot[b]
  })
  ind.pres <- which(stats.coup$prop > fleet.plot.par$th)
  stats.coup2 <- stats.coup[ind.pres,]
  # pdf(paste0(dossier.stats.outputs,'couples_prop_',gear,'_cluster_',group,'_',year,'_thres_',fleet.plot.par$th*100,'.pdf'),
  #     width=24,height=fleet.plot.par$height.couple,pointsize = 20)
  # par(las=2)
  # barplot(stats.coup2$prop[order(stats.coup2$prop,decreasing=TRUE)],
  #         names.arg = stats.coup2$couple[order(stats.coup2$prop,decreasing=TRUE)],
  #         main = paste0('Proportion of dyads from the couple in cluster ',group),
  #         cex.names=fleet.plot.par$cx,width=stats.coup2$cl1[order(stats.coup2$prop,decreasing=TRUE)])
  # dev.off()
  # 
  
  # exclusiveness in cluster 1
  # 
  stats.coup.c1 <- data.frame(couple=couples,cl1=rep(NA,num.couples),tot=rep(NA,num.couples),prop=rep(NA,num.couples))
  group <- 1
  ind.c1 <- which(dyads$numclust == group)
  rien <- sapply(1:num.couples,function(b) {
    v.coup <- as.numeric(unlist(strsplit(couples[b],'-')))
    stats.coup.c1$tot[b] <<- sum(dyads$id.1[ind.c1] == v.coup[1]  |  dyads$id.2[ind.c1] == v.coup[1]) +
      sum(dyads$id.1[ind.c1] == v.coup[2] |  dyads$id.2[ind.c1] == v.coup[2]) -
      sum(dyads$couple[ind.c1] == couples[b])
    stats.coup.c1$cl1[b] <<- sum(dyads$couple[ind.c1] == couples[b])
    stats.coup.c1$prop[b] <<- stats.coup.c1$cl1[b]/stats.coup.c1$tot[b]
  })
  ind.coup.c1 <- which(stats.coup.c1$tot > fleet.plot.par$th) # anchovy fleet.plot.par$th <- 0.025
  ind.pres <- which(stats.coup.c1$prop > fleet.plot.par$th)
  stats.coup2.c1 <- stats.coup.c1[ind.pres,]
  # pdf(paste0(dossier.stats.outputs,'couples_exclus_prop_',gear,'_cluster_',group,'_',year,'.pdf'),
  #     width=24,height=fleet.plot.par$height.couple.excl,pointsize = 20)
  # par(las=2)
  # barplot(stats.coup2.c1$prop[order(stats.coup2.c1$prop,decreasing=TRUE)],
  #         names.arg = stats.coup2.c1$couple[order(stats.coup2.c1$prop,decreasing=TRUE)],
  #         main = paste0('Proportion of dyads from the two vessels in the couple in cluster ',group, ' that are composed by them'),
  #         cex.names=fleet.plot.par$cx,width=stats.coup2.c1$cl1[order(stats.coup2.c1$prop,decreasing=TRUE)])
  # dev.off()
  
  # computing disimilarity/similarity matrices (I could have used this for above)
  dyads$mjm.inv <- 1/dyads$mjm
  remplir.mjm.inv <- 1/remplir.mjm
  
  df.stats.sim <- aggregate(dyads$mjm~dyads$id.1+dyads$id.2,FUN=mean)
  colnames(df.stats.sim) <- c("id1","id2","mjm")
  df.stats.dis <- aggregate(dyads$mjm.inv~dyads$id.1+dyads$id.2,FUN=mean)
  colnames(df.stats.dis) <- c("id1","id2","mjm.inv")
  df.stats <- cbind.data.frame(df.stats.sim,mjm.inv=df.stats.dis$mjm.inv)
  rm(df.stats.sim,df.stats.dis)
  df.stats.sim.cl <- aggregate(dyads$mjm~dyads$id.1+dyads$id.2+dyads$numclust,FUN=mean)
  colnames(df.stats.sim.cl) <- c("id1","id2","cluster","mjm")
  df.stats.dis.cl <- aggregate(dyads$mjm.inv~dyads$id.1+dyads$id.2+dyads$numclust,FUN=mean)
  colnames(df.stats.dis.cl) <- c("id1","id2","cluster","mjm.inv")
  df.stats.counts.cl <- aggregate(dyads$mjm~dyads$id.1+dyads$id.2+dyads$numclust,FUN=length)
  colnames(df.stats.counts.cl) <- c("id1","id2","cluster","counts")
  df.stats.cl <- cbind.data.frame(df.stats.sim.cl,mjm.inv=df.stats.dis.cl$mjm.inv,counts=df.stats.counts.cl$counts)
  rm(df.stats.sim.cl,df.stats.dis.cl,df.stats.counts.cl)
  
  num.vessels <- length(vessels.1)
  mat.dis <- matrix(NA, ncol=num.vessels, nrow=num.vessels)
  mat.sim <- matrix(NA, ncol=num.vessels, nrow=num.vessels)
  array.counts.cl <- array(0, dim = c(num.vessels,num.vessels,k))
  array.sim.cl <- array(0, dim = c(num.vessels,num.vessels,k))
  array.dis.cl <- array(0, dim = c(num.vessels,num.vessels,k))
  
  for (i in 1:(num.vessels-1)){
    for (j in (i+1):num.vessels){
      ind <- which((df.stats$id1 == vessels.1[i] & df.stats$id2 == vessels.1[j]) | (df.stats$id2 == vessels.1[i] & df.stats$id1 == vessels.1[j]))
      if (length(ind) == 1){
        mat.dis[j,i] <- df.stats$mjm.inv[ind]
        mat.sim[j,i] <- df.stats$mjm[ind]
        for (cl in 1:k){
          ind.cl <- which(((df.stats.cl$id1 == vessels.1[i] & df.stats.cl$id2 == vessels.1[j]) | (df.stats.cl$id2 == vessels.1[i] & df.stats.cl$id1 == vessels.1[j]))
                          & df.stats.cl$cluster == cl)
          if (length(ind.cl) == 1){
            array.counts.cl[j,i,cl] <- df.stats.cl$counts[ind.cl]
            array.sim.cl[j,i,cl] <- df.stats.cl$mjm[ind.cl]
            array.dis.cl[j,i,cl] <- df.stats.cl$mjm.inv[ind.cl]
          }
        }
      }else{
        mat.dis[j,i] <- remplir.mjm.inv # just necessary for HCA 
        mat.sim[j,i] <- remplir.mjm # just necessary for HCA 
      }
      
      
    }
  }
  
  rownames(mat.sim) <- vessels.1
  rownames(mat.dis) <- vessels.1
  dist.1 <- as.dist(mat.dis)
  save(dist.1,file=paste0(dossier.data.outputs,'DistanceMatrix-',gear,'-',year,'.RData'))
  save(mat.sim,file=paste0(dossier.data.outputs,'SimMatrix-',gear,'-',year,'.RData'))
  save(array.counts.cl,file=paste0(dossier.data.outputs,'ComptageArray-',gear,'-',year,'.RData'))
  save(array.sim.cl,file=paste0(dossier.data.outputs,'SimArray-',gear,'-',year,'.RData'))
  save(array.dis.cl,file=paste0(dossier.data.outputs,'DisArray-',gear,'-',year,'.RData'))
  
  res.agnes <- agnes(dist.1,diss=TRUE)
  pdf(paste0(dossier.stats.outputs,'Flotille-agnes-',gear,'-',year,'.pdf'),
      width=24,height=18,pointsize = 20)
  plot(res.agnes,which.plots = 2)
  dev.off()
  
  if (is.null(fleet.cluster) == FALSE){
    numclust2<-cutree(res.agnes,k=fleet.cluster) #
  }
  
  aggl <- res.agnes$ac
  
  #### networks with counts for first cluster
  
  set.seed(3952)
  
  # # load(paste0(dossier.data.outputs,'ReproducingResults/PTM-comptage1.RData'))
  # load(paste0(dossier.data.outputs,'ReproducingResults/OTB-large-comptage1.RData'))
  # load(paste0(dossier.data.outputs,'ReproducingResults/OTB-small-comptage1.RData'))
  # load(paste0(dossier.data.outputs,'ReproducingResults/OTM-comptage1.RData'))
  # load(paste0(dossier.data.outputs,'ReproducingResults/Thon-jour--comptage1.RData'))
  
  # first, with the whole matrix
  
  mat.counts.cl <- array.counts.cl[,,1]
  # mat.counts.cl <- mat.counts.c1 # for Reproducing results
  rownames(mat.counts.cl) <- vessels.1
  colnames(mat.counts.cl) <- vessels.1
  g <- graph.adjacency(mat.counts.cl, weighted=T, mode = "lower",diag = FALSE)
  g <- simplify(g)
  V(g)$label <- V(g)$name
  V(g)$degree <- degree(g)
  layout1 <- layout.fruchterman.reingold(g)
  V(g)$label.color <- "black" # rgb(0, 0, .2, .8)
  V(g)$frame.color <- "grey"
  V(g)$label.cex <- 0.7 # in cases of too many vessels
  V(g)$color <- alpha("darkgreen",V(g)$degree/ max(V(g)$degree))
  V(g)$size <- 15 * V(g)$degree / max(V(g)$degree)+ .2
  egam <- (log(E(g)$weight)+1.4) / max(log(E(g)$weight)+1.4)
  E(g)$color <- alpha("aquamarine3", egam)
  E(g)$width <- egam*10
  # plot the graph in layout1
  pdf(paste0(dossier.stats.outputs,'Network-comptage-c1-',gear,'-',year,'.pdf'),
      width=24,height=20,pointsize = 20)
  plot(g, layout=layout1)
  dev.off()
  Din <- table(degree(g, mode=c("all"))) # think about in, out and total # when you care about direction
  Loyalty <- Din[names(Din) == "1"]/sum(Din[as.numeric(names(Din))>0]) # loyalty
  Trans <-  transitivity(g) # clustering coef (seems to make sense but cannot interpret it perfectly)
  Ed <- 1 - edge_density(g,loops=FALSE)
  
  pdf(paste0(dossier.stats.outputs,'Prop-partners-barplot-cl-1-',gear,'-',year,'.pdf'),
      width=24,height=20,pointsize = 20)
  barplot(prop.table(table(degree(g))),main='')
  dev.off()
  
  deg.data <- degree(g) # to how many vessels are they connected to each
  Degree.Vessel <- cbind.data.frame(Vessel=as.numeric(names(deg.data)),degree=deg.data)
  rownames(Degree.Vessel) <- NULL
  stats.bat <- merge(x = stats.bat,y=Degree.Vessel)
  
  if (length(Loyalty)==0){
    Loyalty <- 0
  }
  stats.fleet <- data.frame(Loyalty=as.numeric(Loyalty),Trans=Trans,Ed=Ed,Agnes=aggl)
  
  # now, taking zeros out
  
  suma <- apply(X = mat.counts.cl,MARGIN = 1,FUN = sum) + apply(X = mat.counts.cl,MARGIN = 2,FUN = sum)
  ind.non.zero <- which(suma > 0)
  mat.counts.cl.zero <- mat.counts.cl[ind.non.zero,ind.non.zero]
  g <- graph.adjacency(mat.counts.cl.zero, weighted=T, mode = "lower",diag = FALSE)
  g <- simplify(g)
  Din <- table(degree(g, mode=c("all"))) # think about in, out and total # when you care about direction
  Ed.NoZero <- 1 - edge_density(g,loops=FALSE)
  
  stats.fleet <- cbind.data.frame(stats.fleet,Ed.NoZero)
  
  save(stats.bat,file=paste0(dossier.stats.outputs,'StatsPerVesselFromDyads-',gear,'-',year,'.RData'))
  save(stats.bat.trip,file=paste0(dossier.stats.outputs,'StatsPerVesselFromTrips-',gear,'-',year,'.RData'))
  save(stats.coup,file=paste0(dossier.stats.outputs,'StatsPerCoupleFromDyads-',gear,'-',year,'.RData'))
  save(stats.coup.c1,file=paste0(dossier.stats.outputs,'StatsPerExclusiveCouple-',gear,'-',year,'.RData'))
  save(stats.fleet,file=paste0(dossier.stats.outputs,'StatsFleet-',gear,'-',year,'.RData'))
  
  sum(stats.bat.trip$Prop.1 > 0.5)/length(stats.bat.trip$Prop.1)
  
}

