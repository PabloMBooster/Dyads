clustering.dyads <- function(dyads,variables,gear,year,dossier.data.outputs,dossier.stats.outputs,new.variables,k=NULL){
  
  
  matriz <- scale(dyads[,variables],center=TRUE,scale=TRUE)
  
  ###### HCA #########
  clu_scor_2 <- Rclusterpp.hclust(matriz, method="ward", distance="euclidean") # 
  save(clu_scor_2,file=paste0(dossier.data.outputs,'HCA_',gear,'_',paste(str_replace_all(variables, "[[:punct:]]", ""),collapse='-'),'-',year,'.RData'))
 
  png(paste0(dossier.stats.outputs,'Dendrogram_',gear,'_',paste(str_replace_all(variables, "[[:punct:]]", ""),collapse='-'),'-',year,'.png')) ## Se ejecuta linea por linea 
  plot(clu_scor_2, labels = FALSE, hang = -1,axes = TRUE,col='black', frame.plot = FALSE, ann = TRUE,sub = NULL, ylab = 
         "Height",xlab="",main="")
  dev.off()
  
  if (is.null(k)){
  if (dim(dyads)[1] < 10000){
    res <- NbClust(data=matriz,distance = "euclidean",method = "ward.D2")
    BestNC <- res$Best.nc
    save(BestNC,file=paste0(dossier.stats.outputs,'/BestNC_',gear,'_',paste(str_replace_all(variables, "[[:punct:]]", ""),collapse='-'),'-',year,'.RData'))
    k <- as.numeric(names(which.max(table(BestNC[1,])))) # you can change it manually if you disagree
  }else{
    # trying from 1 to 7 clusters and use DB criterion to decide
    numclust2<-cutree(clu_scor_2,k=1:7) #
    dh7 <- cbind.data.frame(dyads,matriz,numclust2)
    test.H <- sapply(c(1:6),function(x){
      index.DB(matriz, dh7[,as.character(x+1)], d=NULL, centrotypes="centroids", p=2, q=2)
    })
    k <- which.min(unlist(test.H[1,])) + 1 # you can change it manually if you disagree
  }
  }
  
  numclust<-cutree(clu_scor_2,k=k)
  dh4 <- cbind.data.frame(dyads,matriz,numclust)
  save(dh4,file=paste0(dossier.data.outputs,'hca_',k,'-',gear,'-',paste(str_replace_all(variables, "[[:punct:]]", ""),collapse='-'),'-',year,'.RData'))
  
  medianes <- aggregate(matriz~numclust,FUN="median")
  
  ###################
  
  ###### kmeans #########
  
  res.kmeans <- kmeans(x = matriz,centers = medianes[,2:dim(medianes)[2]])
  numclust <- res.kmeans$cluster
  colnames(matriz) <- new.variables
  
  ##### IntraCluster Similarity Measure ########"
  VarIntra <- rep(0,length(new.variables))
  VarTotal <- rep(0,length(new.variables))
  sapply(c(1:length(new.variables)),function(x){
    sapply(1:k,function(y){
      VarIntra[x] <<- VarIntra[x] + var(matriz[which(numclust == y),x])
    })
    VarTotal[x] <<- var(matriz[,x])
  })
  (Intra.Cluster.Sim <- VarIntra/VarTotal)
  name.IntraSim <- paste0(dossier.data.outputs,'IntraSim_',k,'-',gear,'-',paste(str_replace_all(new.variables, "[[:punct:]]", ""),collapse='-'),'-',year,'.RData')
  save(Intra.Cluster.Sim,file=name.IntraSim)
  
  ###### Multidimensional joint movement index MJM  #######
  Weight.Not.Ordered <- (1/Intra.Cluster.Sim)/sum(1/Intra.Cluster.Sim)
  order.var <- order(Weight.Not.Ordered,decreasing = TRUE)
  matriz.pesos <- cbind.data.frame(dh4$numclust,wgt.mean=apply(matriz,1,FUN=weighted.mean,w=Weight.Not.Ordered))
  colnames(matriz.pesos) <- c("numclust","z")
  matriz.pesos$numclust <- as.factor(matriz.pesos$numclust)
  matriz.pesos$z.sim <- 1/(1+exp(-matriz.pesos$z))
  med.mjm <- aggregate(z.sim~numclust,data=matriz.pesos,FUN='median')
  
  ##### Ordering clusters by their median value of MJM
  order.clusters <- order(med.mjm$z.sim,decreasing = TRUE)
  numclust.order <- numclust
  for (cl in 1:k){
    numclust.order[which(dh4$numclust == order.clusters[cl])] <- cl 
  }
  numclust <- numclust.order
  dh4 <- cbind.data.frame(dyads,matriz,numclust,matriz.pesos$z.sim)
  colnames(dh4)[dim(dh4)[2]] <- "mjm"
  save(dh4,file=paste0(dossier.data.outputs,'kmeans_',k,'-',gear,'-',paste(new.variables,collapse='-'),'-',year,'.RData'))
  
  ### Boxplot and median graphics ########
  
  pdf(paste0(dossier.stats.outputs,'Boxplots_kmeans-',k,'-',gear,'-',paste(str_replace_all(new.variables, "[[:punct:]]", ""),collapse='-'),'-',year,'.pdf'))
  par(mfrow=c(2,3))
  for (x in 1:length(variables)){    
    boxplot(dh4[,variables[x]]~dh4[,"numclust"],outline=FALSE,main=new.variables[x])
  }
  dev.off()
  
  quantil.prob <- c(0.025,0.25,0.5,0.75,0.975)
  Ind.Cat.Matrix <- array(NA,dim=c(length(quantil.prob),k,length(variables)))
  rien <- sapply(1:length(variables),function(x){
    rien <- sapply(1:k, function(cl){
      Ind <- dh4[which(dh4$numclust == cl),variables[order.var[x]]]
      # Ind <- as.vector(get(variable[x])[,cat.seuil[cl]:cat.seuil[cl+1]])
      Ind.Cat.Matrix[,cl,x] <<- quantile(Ind,probs = quantil.prob)
    })
  })
  transp <- 0.2
  lim.y=c(-1,1)
  taille.point=2
  col.cat <- c("darkgreen","firebrick","black","blue")
  # p.ribbon <- list(NULL)
  # p.line <- list(NULL)
  # p.point <- list(NULL)
  # for (cl in k:1){
  #   test <- data.frame(t(Ind.Cat.Matrix[,cl,]))
  #   p.ribbon[[cl]] <- geom_ribbon(aes(1:length(variables),ymin = X2, ymax = X4), test, col=col.cat[cl],linetype=2, fill=alpha(col.cat[cl],transp)) 
  #   p.line[[cl]] <-  geom_line(aes(1:length(variables), X3), test, col=col.cat[cl],size=taille.point/2) 
  #   p.point[[cl]] <- geom_point(aes(1:length(variables), X3), test,col=col.cat[cl],size=taille.point) 
  # }
  # p <- ggplot() + theme(axis.text.x=element_text(color = "black", size=14, angle=0, vjust=.8, hjust=0.5),axis.text.y=element_text(color = "black", size=14)) + 
  #   xlab("") + ylab("") 
  # for(i in k:1){ 
  #   p<-p+p.ribbon[[i]]+p.line[[i]]+p.point[[i]]
  # }
  # p <- p + scale_y_continuous(breaks = round(seq(lim.y[1], lim.y[2], by = 0.1),1)) +
  #   scale_x_continuous(breaks=1:length(variables),labels=new.variables[order.var]) 
  # 
  # pdf(file=paste0(dossier.stats.outputs,"Kmeans-q13-",gear,"-",year,"-k-",k,"-",paste(str_replace_all(new.variables, "[[:punct:]]", ""),collapse='-'),".pdf"))
  # print(p)
  # dev.off()
  # 
  # old version of plot
  matriz.2 <- dh4[,variables]
  medianas <- sapply(1:k,function(x){
    apply(matriz.2[which(dh4$numclust == x),order.var],2,FUN=median)})
  q1 <- sapply(1:k,function(x){
    apply(matriz.2[which(dh4$numclust == x),order.var],2,FUN=quantile,probs=0.25)})
  q3 <- sapply(1:k,function(x){
    apply(matriz.2[which(dh4$numclust == x),order.var],2,FUN=quantile,probs=0.75)})
  # orden <- order(cluster.res$poids,decreasing = TRUE)
  rango <- c(min(q1)-0.05,max(q3)+0.05)
  pdf(file=paste0(dossier.stats.outputs,"Kmeans-q13-",gear,"-",year,"-k-",k,"-",paste(str_replace_all(new.variables, "[[:punct:]]", ""),collapse='-'),".pdf"))
  plot(medianas[,1],cex=1.5,xaxt='n',ylim=rango, xlab='',ylab='Median', main='',col='darkgreen',pch=21,bg='darkgreen',type='b')
  points(medianas[,2],cex=1.5,col='firebrick',pch=23,bg='firebrick',type='b')
  points(medianas[,3],cex=1.5,col='black',pch=22,bg='black',type='b')
  # points(medianas[,4],cex=1.5,col='blue',pch=22,bg='blue',type='b')
  arrows(1:length(variables),q1[,1],1:length(variables),q3[,1],code=3,length=0.2,angle=90,col='darkgreen',lty=2)
  arrows(1:length(variables),q1[,2],1:length(variables),q3[,2],code=3,length=0.2,angle=90,col='firebrick',lty=2)
  arrows(1:length(variables),q1[,3],1:length(variables),q3[,3],code=3,length=0.2,angle=90,col='black',lty=2)
  # arrows(1:length(variables),q1[,4],1:length(variables),q3[,4],code=3,length=0.2,angle=90,col='blue',lty=2)
  axis(1, at=1:length(new.variables), labels=new.variables[order.var])
  dev.off()
  
  ####### stats from clustering for tables #######
  
  stats.clusters <- data.frame(Vessels=rep(NA,k+1),Couples=rep(NA,k+1),Dyads=rep(NA,k+1),Dyads.Perc=rep(NA,k+1),
                               Duration=rep(NA,k+1),MJM=rep(NA,k+1))
  rownames(stats.clusters) <- c("Total",paste0("Cluster-",1:k))
  stats.clusters$Vessels[1] <- length(unique(c(dh4$id.1,dh4$id.2)))
  stats.clusters$Couples[1] <- length(unique(paste0(dh4$id.1,'-',dh4$id.2)))
  stats.clusters$Dyads[1] <- dim(dh4)[1]
  stats.clusters$Dyads.Perc[1] <- 100
  stats.clusters$Duration[1] <- round(median(dh4$dur),2)
  stats.clusters$MJM[1] <- round(median(dh4$mjm),2)
  
  rien <- sapply(1:k,function(x){
    ind <- which(dh4$numclust == x)
    stats.clusters$Vessels[x+1] <<- length(unique(c(dh4$id.1[ind],dh4$id.2[ind])))
    stats.clusters$Couples[x+1] <<- length(unique(paste0(dh4$id.1[ind],'-',dh4$id.2[ind])))
    stats.clusters$Dyads[x+1] <<- length(ind)
    stats.clusters$Dyads.Perc[x+1] <<- round(stats.clusters$Dyads[x+1]/stats.clusters$Dyads[1]*100,2)
    stats.clusters$Duration[x+1] <<- round(median(dh4$dur[ind]),2)
    stats.clusters$MJM[x+1] <<- round(median(dh4$mjm[ind]),2)
  })
  
  save(stats.clusters,file=paste0(dossier.data.outputs,"ClusterTable-",k,'-',gear,'-',paste(new.variables,collapse='-'),'-',year,'.RData'))
  
  print(xtable(t(stats.clusters)), file = paste0(dossier.stats.outputs,"ClusterLatexTable-",k,'-',gear,'-',paste(new.variables,collapse='-'),'-',year,'.txt'))
  
  return(list(dyads=dh4,poids=Weight.Not.Ordered))
}

