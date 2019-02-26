 ### list of indices functions

# library('pdist')
pdist.pdist <- pdist::pdist
# library('polyclip','colorspace')
# library(biwavelet)
# library(geoR)

getEllipse <- function (X_, Y_, D_, npts=300) {  ## computing ellipses for jPPA
  x0 = mean(X_)
  y0= mean(Y_)
  a <- D_/2
  c2 <- (diff(X_)^2+diff(Y_)^2)/2
  if (a^2-c2 < 0){
    print('Error: max dist is too small regarding the distance between points. Change it. ')
  }else{
    b <- sqrt(a^2-c2)
    phi <- atan( diff(Y_)/diff(X_))
    
    theta <- seq(0, 2 * pi, length = npts)
    xh <-  a * cos(theta)
    yh <-  b * sin(theta)
    co <- cos(phi)
    si <- sin(phi)
    x  <- x0 + co*xh - si*yh
    y  <- y0 + si*xh + co*yh
    # x  <- x0 + xh
    # y  <- y0 + yh
    
    return(list(x = x, y = y))
  }
}

jPPA <- function(A,B,Dmax,factor.color=1,tam.cel=3){ 
  # in contrast to the jppa function in the wildlifeTG package, there is no need to have ltraj objects, nor spatial polygons
  # by gridding the space to compute the areas, the function efficiently computes the intersection and union areas without encountering problems related to disjoint sets
  XA <- A[,1]
  YA <- A[,2]
  XB <- B[,1]
  YB <- B[,2]
  testA <- lapply(1:(length(XA)-1), function(i){ getEllipse(X_=XA[c(i,i+1)],Y_=YA[c(i,i+1)],D_=Dmax)})
  testB <- lapply(1:(length(XB)-1), function(i){ getEllipse(X_=XB[c(i,i+1)],Y_=YB[c(i,i+1)],D_=Dmax)})
  range.x <- range(do.call( rbind, testA)[,1],do.call( rbind, testB)[,1],na.rm = TRUE)
  range.y <- range(do.call( rbind, testA)[,2],do.call( rbind, testB)[,2],na.rm = TRUE)
  xgrid=seq(from=range.x[1],to=range.x[2],by=tam.cel)
  ygrid=seq(from=range.y[1],to=range.y[2],by=tam.cel)
  grilla.inter <- grilla.union <- matrix(0,ncol=length(xgrid),
                                         nrow=length(ygrid),byrow = FALSE)
  test.2 <- sapply(1:length(testA),function(i){
    UnionAB <- polyclip(testA[[i]], testB[[i]], op="union", fillA = 'positive', fillB = 'positive', x0 = mean(c(XA[i],XB[i])), y0 = mean(c(YA[i],YB[i])), eps=1e-5 )
    InterAB <- polyclip(testA[[i]], testB[[i]], op="intersection", fillA = 'positive', fillB = 'positive', x0 = mean(c(XA[i],XB[i])), y0 = mean(c(YA[i],YB[i])), eps=1e-5 )
    if (length(InterAB) > 0){
      poly.inter.sub <- polygrid(xgrid=seq(from=range.x[1],to=range.x[2],by=tam.cel),
                                 ygrid=seq(from=range.y[1],to=range.y[2],by=tam.cel),
                                 borders = InterAB[[1]], vec=TRUE)
      toto <- matrix(as.numeric(poly.inter.sub$vec.inout),ncol=length(xgrid),
                     nrow=length(ygrid),byrow = FALSE)
      grilla.inter <<- toto + grilla.inter
    }
    test <- sapply(1:length(UnionAB),function(j){
      poly.union.sub <- polygrid(xgrid=seq(from=range.x[1],to=range.x[2],by=tam.cel),
                                 ygrid=seq(from=range.y[1],to=range.y[2],by=tam.cel),
                                 borders = UnionAB[[j]], vec=TRUE)
      toto <- matrix(as.numeric(poly.union.sub$vec.inout),ncol=length(xgrid),
                     nrow=length(ygrid),byrow = FALSE)
      grilla.union <<- toto + grilla.union
    })
    
  })
  grilla.den <- sum(grilla.union>0)
  grilla.num <- sum(grilla.inter>0)
  
  jPPA <- grilla.num/grilla.den
  
  return(jPPA)
}

gamma.s <- function(ind.s,epsilon,cono.x,WC.edge){ # necessary for WCavglss and WCmaxlss indices
  L <- length(cono.x)
  beta <- alpha <- rep(0,L+1)
  for (j in 2:(L+1)){
    if (sum(WC.edge[ind.s,] > epsilon,na.rm = TRUE) > 0){
      if (is.na(WC.edge[ind.s,j-1]) == FALSE && (WC.edge[ind.s,j-1]> epsilon) == TRUE){
        alpha[j] <- max(alpha[j-1],1+beta[j-1])
        beta[j] <- 1 + beta[j-1]
      }else{
        alpha[j] <- alpha[j-1]
        beta[j] <- 0
      }
    }else{
      beta[j] <- alpha[j] <- 0
    }
  }
  return(alpha[L+1])
}

Cs.function <- function(A,B,distances){
  do <- mean(distances)
  distances.de <- as.matrix(pdist.pdist(X = A,Y = B))
  de <- mean(distances.de)
  Cs <- (de-do)/(de+do)
  Cs.2 <- (1-binom.test(x=sum(distances<=de),n=length(distances),p=0.5,alternative="greater")$p.value) 
  return(list(Cs=Cs,Cs.2=Cs.2))
}

CSEM.function <- function(distances,longueur,delta){
  m <- 1
  PM <- 1
  P <- NULL
  while (PM[m] != 0 & m < longueur){
    seq1 <- 1:(longueur-m)
    seq2 <- seq1+m
    pairs <- as.matrix(cbind(seq1,seq2))
    P[[m]] <- as.numeric(sapply(1:length(seq1),function(x){max(distances[pairs[x,1]:pairs[x,2]])}) < delta)
    PM <- c(PM,sum(P[[m]]))
    (m <- m + 1)
  }
  PM <- PM[-1]
  
  if (PM[length(P)] == 0){
    CSEM <- (length(P)-1)/(longueur-1)
  }else{
    CSEM <- (length(P))/(longueur-1)
  }
  return(CSEM)
}

DI.function <- function(A,B,delta.DI,longueur,distance1,distance2){ # adapted from the wildlifeDI package without requiring ltraj objects
  DId0 <- 1 - (abs(distance1-distance2)/(distance1+distance2))^delta.DI
  DId0[which(distance1+distance2 == 0)] <- 1
  DI0.d <- mean(DId0,na.rm=TRUE)
  x1 <- A[-1, ]
  x2 <- A[-longueur, ]
  dx <- c(x1[,1] - x2[,1])
  dy <- c(x1[,2] - x2[,2])
  abs.angle.A <- atan2(dy, dx)
  x1 <- B[-1, ]
  x2 <- B[-longueur, ]
  dx <- c(x1[,1] - x2[,1])
  dy <- c(x1[,2] - x2[,2])
  abs.angle.B <- atan2(dy, dx)
  DItheta <- cos(abs.angle.A - abs.angle.B)
  DItheta[which(is.na(abs.angle.A)== TRUE & is.na(abs.angle.B) == TRUE)] <- 1
  DItheta[which(is.na(DItheta))] <- 0
  DI.theta <- mean(DItheta,na.rm=TRUE)
  
  # DI
  DI0 <- mean(DItheta*DId0,na.rm = TRUE)
  return(list(DI=DI0,DI.d=DI0.d,DI.theta=DI.theta))
}

# We basically modify the WTC function in the biwavelet package to avoid fitting an arima (which is not always possible)
wtc.modif <- function (d1, d2, pad = TRUE, dj = 1/12, s0 = 2 * dt, J1 = NULL,
                       max.scale = NULL, mother = "morlet", param = -1, lag1 = NULL,
                       sig.level = 0.95, sig.test = 0, nrands = 300, quiet = FALSE){
  mother <- match.arg(tolower(mother), MOTHERS)
  checked <- check.data(y = d1, x1 = d2)
  xaxis <- d1[, 1]
  dt <- checked$y$dt
  t <- checked$y$t
  n <- checked$y$n.obs
  if (is.null(J1)) {
    if (is.null(max.scale)) {
      max.scale <- (n * 0.17) * 2 * dt
    }
    J1 <- round(log2(max.scale/s0)/dj)
  }
  wt1 <- wt(d = d1, pad = pad, dj = dj, s0 = s0, J1 = J1, max.scale = max.scale,
            mother = mother, param = param, sig.level = sig.level,
            sig.test = sig.test, lag1 = lag1[1],do.sig = FALSE)
  wt2 <- wt(d = d2, pad = pad, dj = dj, s0 = s0, J1 = J1, max.scale = max.scale,
            mother = mother, param = param, sig.level = sig.level,
            sig.test = sig.test, lag1 = lag1[2],do.sig = FALSE)
  d1.sigma <- sd(d1[, 2], na.rm = T)
  d2.sigma <- sd(d2[, 2], na.rm = T)
  s.inv <- 1/t(wt1$scale)
  s.inv <- matrix(rep(s.inv, n), nrow = NROW(wt1$wave))
  smooth.wt1 <- smooth.wavelet(s.inv * (abs(wt1$wave)^2), dt,
                               dj, wt1$scale)
  smooth.wt2 <- smooth.wavelet(s.inv * (abs(wt2$wave)^2), dt,
                               dj, wt2$scale)
  coi <- pmin(wt1$coi, wt2$coi, na.rm = T)
  CW <- wt1$wave * Conj(wt2$wave)
  CW.corr <- (wt1$wave * Conj(wt2$wave) * max(wt1$period))/matrix(rep(wt1$period,
                                                                      length(t)), nrow = NROW(wt1$period))
  power <- abs(CW)^2
  power.corr <- (abs(CW)^2 * max.scale)/matrix(rep(wt1$period,
                                                   length(t)), nrow = NROW(wt1$period))
  smooth.CW <- smooth.wavelet(s.inv * (CW), dt, dj, wt1$scale)
  rsq <- abs(smooth.CW)^2/(smooth.wt1 * smooth.wt2)
  phase <- atan2(Im(CW), Re(CW))
  if (nrands > 0) {
    signif <- wtc.sig(nrands = nrands, lag1 = lag1, dt = dt,
                      n, pad = pad, dj = dj, J1 = J1, s0 = s0, max.scale = max.scale,
                      mother = mother, sig.level = sig.level, quiet = quiet)
  }else {
    signif <- NA
  }
  results <- list(coi = coi, wave = CW, wave.corr = CW.corr,
                  power = power, power.corr = power.corr, rsq = rsq, phase = phase,
                  period = wt1$period, scale = wt1$scale, dt = dt, t = t,
                  xaxis = xaxis, s0 = s0, dj = dj, d1.sigma = d1.sigma,
                  d2.sigma = d2.sigma, mother = mother, type = "wtc", signif = signif)
  class(results) <- "biwavelet"
  return(results)
}

WC.function <- function(distance1,distance2,sl,su,epsilon){
  wtc.t1t2 <- wtc.modif(cbind(1:length(distance1),distance1),cbind(1:length(distance2),distance2), nrands = 0,sig.level = 0.95,quiet = TRUE)
  cono.x <- 1:length(wtc.t1t2$coi)
  matriz.ind.edge <- matrix(NA,nrow=length(wtc.t1t2$scale),ncol=length(cono.x))
  test <- sapply(1:length(wtc.t1t2$coi),function(x){
    matriz.ind.edge[which(wtc.t1t2$scale < wtc.t1t2$coi[x]),x] <<- 1
  })
  WC.edge <- wtc.t1t2$rsq*matriz.ind.edge
  WC.edge.scale <- apply(WC.edge,MARGIN = 1,FUN=mean,na.rm=TRUE)
  if (is.null(sl)){
    sl = wtc.t1t2$scale[1]
  }
  if (is.null(su)){
    su = max(wtc.t1t2$scale)
  }
  ind.scales <- which(wtc.t1t2$scale >= sl & wtc.t1t2$scale <= su)
  WC.avg <- mean(WC.edge.scale[ind.scales],na.rm=TRUE)
  WC.max.scale <- apply(WC.edge,MARGIN = 1,FUN=max,na.rm=TRUE)
  WC.max <- max(WC.max.scale[ind.scales])
  gamma.ns <- sapply(ind.scales,function(x){
    gamma.s(x,epsilon,cono.x,WC.edge)/sum(is.na(WC.edge[x,])==FALSE)
  })
  WC.avg.lss <- mean(gamma.ns,na.rm=TRUE)
  WC.max.lss <- max(gamma.ns,na.rm=TRUE)
  return(list(WC.avg=WC.avg,WC.max=WC.max,WC.avg.lss=WC.avg.lss,WC.max.lss=WC.max.lss))
}

# Lixn

LixnT.Convex <- function(A,B,resol= 1e-5,xi=1e-4,side.pol=2){
  hpts.A <- chull(A)
  testA <- NULL
  testA$x <- A[hpts.A,1]
  testA$y <- A[hpts.A,2]
  hpts.B <- chull(B)
  testB <- NULL
  testB$x <- B[hpts.B,1]
  testB$y <- B[hpts.B,2]
  InterAB <- polyclip(testA, testB, op="intersection", eps=resol,
                      fillA = 'evenodd',fillB = 'evenodd',x0=0,y0=0)
  longueur <- Nstep
  
  if (length(InterAB) > 0){
    InterAB.2 <- polyoffset(InterAB, xi, jointype="square") # and that's why it is not a good idea to derive p from areas
    
    poligono <- as.data.frame(InterAB.2)
    poligono <- rbind.data.frame(poligono, poligono[1,])
    A.sAB <- pnt.in.poly(A, poligono)
    B.sAB <- pnt.in.poly(B, poligono)
    nA0 = sum(A.sAB$pip == 1 & B.sAB$pip == 0)
    n0B = sum(A.sAB$pip == 0 & B.sAB$pip == 1)
    nAB = sum(A.sAB$pip == 1 & B.sAB$pip == 1)
    n00 = sum(A.sAB$pip == 0 & B.sAB$pip == 0)
    
    pAB = ((sum(A.sAB$pip == 1))/longueur)*((sum(B.sAB$pip == 1))/longueur)
    pA0 = ((sum(A.sAB$pip == 1))/longueur)*(1-(sum(B.sAB$pip == 1))/longueur)
    p0B = (1-(sum(A.sAB$pip == 1))/longueur)*((sum(B.sAB$pip == 1))/longueur)
    p00 = (1-(sum(A.sAB$pip == 1))/longueur)*(1-(sum(B.sAB$pip == 1))/longueur)
    
    if (pA0 == 0 && p0B == 0 && p00 == 0){
      if (pAB == 0){
        lixnT <- 0
      }else{ 
        lixnT <- 1  
      }
    }else if(pA0 == 0 && p0B == 0){
      print(paste0('iw = ', iw,' ;'))
      print(paste0('i6 = ', i6))
    }else{
      lixn <- log(((nAB/pAB)+(n00/p00))/((nA0/pA0)+(n0B/p0B)))
      lixnT <- 1/(1+exp(-lixn))
    }
    
    
  }else{
    InterAB.2 <- InterAB
    lixnT <- NA
    nA0 <- 0
    n0B <- 0
  }
  
  if (side.pol != 0){
    poligono.fijo <- NULL
    poligono.fijo$x <- c(rep(-side.pol,2),rep(side.pol,2))
    poligono.fijo$y <- c(-side.pol,side.pol,side.pol,-side.pol)
    poligono.fijo <- as.data.frame(poligono.fijo)
    poligono.fijo <- rbind.data.frame(poligono.fijo, poligono.fijo[1,])
    A.sAB.fix <- pnt.in.poly(A, poligono.fijo)
    B.sAB.fix <- pnt.in.poly(B, poligono.fijo)
    nA0.fix = sum(A.sAB.fix$pip == 1 & B.sAB.fix$pip == 0)
    n0B.fix = sum(A.sAB.fix$pip == 0 & B.sAB.fix$pip == 1)
    nAB.fix = sum(A.sAB.fix$pip == 1 & B.sAB.fix$pip == 1)
    n00.fix = sum(A.sAB.fix$pip == 0 & B.sAB.fix$pip == 0)
    pAB.fix = ((sum(A.sAB.fix$pip == 1))/longueur)*((sum(B.sAB.fix$pip == 1))/longueur)
    pA0.fix = ((sum(A.sAB.fix$pip == 1))/longueur)*(1-(sum(B.sAB.fix$pip == 1))/longueur)
    p0B.fix = (1-(sum(A.sAB.fix$pip == 1))/longueur)*((sum(B.sAB.fix$pip == 1))/longueur)
    p00.fix = (1-(sum(A.sAB.fix$pip == 1))/longueur)*(1-(sum(B.sAB.fix$pip == 1))/longueur)
    if (pA0.fix == 0 && p0B.fix == 0 && p00.fix == 0){
      if (pAB.fix == 0){
        lixnT.fix <- 0
      }else{ 
        lixnT.fix <- 1  
      }
    }else if(pA0.fix == 0 && p0B.fix == 0){
      lixnT.fix <- NA
    }else{
      lixn.fix <- log(((nAB.fix/pAB.fix)+(n00.fix/p00.fix))/((nA0.fix/pA0.fix)+(n0B.fix/p0B.fix)))
      lixnT.fix <- 1/(1+exp(-lixn.fix))
    }
  }
  
  return(list(hullA=hpts.A,hullB=hpts.B,InterPol=InterAB.2,lixnT=lixnT,nA0=nA0,n0B=n0B,lixnT.fix=lixnT.fix,nA0.fix=nA0.fix,n0B.fix=n0B.fix))
  
}


LixnT.Pol <- function(A,B,puntos.area=NULL){
  A.sAB <- pnt.in.poly(A, as.data.frame(puntos.area))
  B.sAB <- pnt.in.poly(B, as.data.frame(puntos.area))
  
  longueur <- dim(A)[1]
  
    nA0 = sum(A.sAB$pip == 1 & B.sAB$pip == 0)
    n0B = sum(A.sAB$pip == 0 & B.sAB$pip == 1)
    nAB = sum(A.sAB$pip == 1 & B.sAB$pip == 1)
    n00 = sum(A.sAB$pip == 0 & B.sAB$pip == 0)
    pAB = ((sum(A.sAB$pip == 1))/longueur)*((sum(B.sAB$pip == 1))/longueur)
    pA0 = ((sum(A.sAB$pip == 1))/longueur)*(1-(sum(B.sAB$pip == 1))/longueur)
    p0B = (1-(sum(A.sAB$pip == 1))/longueur)*((sum(B.sAB$pip == 1))/longueur)
    p00 = (1-(sum(A.sAB$pip == 1))/longueur)*(1-(sum(B.sAB$pip == 1))/longueur)
    if (pA0 == 0 && p0B == 0 && p00 == 0){
      if (pAB == 0){
        lixnT <- 0
      }else{ 
        lixnT <- 1  
      }
    }else if(pA0 == 0 && p0B == 0){
      lixnT <- NA
    }else{
      lixn <- log(((nAB/pAB)+(n00/p00))/((nA0/pA0)+(n0B/p0B)))
      lixnT <- 1/(1+exp(-lixn))
    }
  
  return(list(lixnT=lixnT,nA0=nA0,n0B=n0B))
  
}
######## for multiplots

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols),byrow=TRUE)
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#### for CPU time

Cs.function.CPU <- function(A,B,distances){
  start.time <- Sys.time()
  do <- mean(distances)
  distances.de <- as.matrix(pdist.pdist(X = A,Y = B))
  de <- mean(distances.de)
  end.time <- Sys.time()
  time.taken <- difftime(end.time, start.time,units = "secs")
  prior.time <- as.numeric(time.taken)
  start.time <- Sys.time()
  Cs <- (de-do)/(de+do)
  end.time <- Sys.time()
  time.taken <- difftime(end.time, start.time,units = "secs")
  Cs.time <- as.numeric(time.taken) + prior.time
  start.time <- Sys.time()
  Cs.2 <- 1 - binom.test(x=sum(distances<=de),n=length(distances),p=0.5,alternative="greater")$p.value
  # min.p <- binom.test(x=length(distances),n=length(distances),p=0.5,alternative="greater")$p.value
  # Cs.2 <- (1-p)/(1-(0.5)^nrow(A))
  end.time <- Sys.time()
  time.taken <- difftime(end.time, start.time,units = "secs")
  Cs2.time <- as.numeric(time.taken) + prior.time
  return(list(Cs=Cs,Cs.2=Cs.2,Cs.time=Cs.time,Cs2.time=Cs2.time))
}

DI.function.CPU <- function(A,B,delta.DI,longueur,distance1,distance2){
  
  start.time <- Sys.time()
  DId0 <- 1 - (abs(distance1-distance2)/(distance1+distance2))^delta.DI
  DId0[which(distance1+distance2 == 0)] <- 1
  end.time <- Sys.time()
  time.taken <- difftime(end.time, start.time,units = "secs")
  DId.time1 <- as.numeric(time.taken) 
  
  start.time <- Sys.time()
  DI0.d <- mean(DId0,na.rm=TRUE)
  end.time <- Sys.time()
  time.taken <- difftime(end.time, start.time,units = "secs")
  DId.time <- as.numeric(time.taken) + DId.time1
  
  
  # DItheta
  start.time <- Sys.time()
  x1 <- A[-1, ]
  x2 <- A[-longueur, ]
  dx <- c(x1[,1] - x2[,1])
  dy <- c(x1[,2] - x2[,2])
  abs.angle.A <- atan2(dy, dx)
  x1 <- B[-1, ]
  x2 <- B[-longueur, ]
  dx <- c(x1[,1] - x2[,1])
  dy <- c(x1[,2] - x2[,2])
  abs.angle.B <- atan2(dy, dx)
  DItheta <- cos(abs.angle.A - abs.angle.B)
  DItheta[which(is.na(abs.angle.A)== TRUE & is.na(abs.angle.B) == TRUE)] <- 1
  DItheta[which(is.na(DItheta))] <- 0
  end.time <- Sys.time()
  time.taken <- difftime(end.time, start.time,units = "secs")
  DItheta.time1 <- as.numeric(time.taken) 
  
  start.time <- Sys.time()
  DI.theta <- mean(DItheta,na.rm=TRUE)
  end.time <- Sys.time()
  time.taken <- difftime(end.time, start.time,units = "secs")
  DItheta.time <- as.numeric(time.taken) + DItheta.time1
  
  
  # DI
  start.time <- Sys.time()
  DI0 <- mean(DItheta*DId0,na.rm = TRUE)
  end.time <- Sys.time()
  time.taken <- difftime(end.time, start.time,units = "secs")
  DI.time <- as.numeric(time.taken) + DItheta.time1 + DId.time1
  
  return(list(DI=DI0,DI.d=DI0.d,DI.theta=DI.theta,DId.time=DId.time,DItheta.time=DItheta.time,DI.time=DI.time))
}

WC.function.CPU <- function(distance1,distance2,sl,su,epsilon){
  
  start.time <- Sys.time()
  wtc.t1t2 <- wtc.modif(cbind(1:length(distance1),distance1),cbind(1:length(distance2),distance2), nrands = 0,sig.level = 0.95,quiet = TRUE)
  # primero, ver como identificar que puntos estan dentro del cono de influencia:
  cono.x <- 1:length(wtc.t1t2$coi)
  matriz.ind.edge <- matrix(NA,nrow=length(wtc.t1t2$scale),ncol=length(cono.x))
  test <- sapply(1:length(wtc.t1t2$coi),function(x){
    matriz.ind.edge[which(wtc.t1t2$scale < wtc.t1t2$coi[x]),x] <<- 1
  })
  WC.edge <- wtc.t1t2$rsq*matriz.ind.edge
  WC.edge.scale <- apply(WC.edge,MARGIN = 1,FUN=mean,na.rm=TRUE)
  if (is.null(sl)){
    sl = wtc.t1t2$scale[1]
  }
  if (is.null(su)){
    su = max(wtc.t1t2$scale)
  }
  ind.scales <- which(wtc.t1t2$scale >= sl & wtc.t1t2$scale <= su)
  end.time <- Sys.time()
  time.taken <- difftime(end.time, start.time,units = "secs")
  prior.time <- as.numeric(time.taken) 
  
  start.time <- Sys.time()
  WC.avg <- mean(WC.edge.scale[ind.scales],na.rm=TRUE)
  end.time <- Sys.time()
  time.taken <- difftime(end.time, start.time,units = "secs")
  WCavg.time <- as.numeric(time.taken) + prior.time
  
  start.time <- Sys.time()
  WC.max.scale <- apply(WC.edge,MARGIN = 1,FUN=max,na.rm=TRUE)
  WC.max <- max(WC.max.scale[ind.scales])
  end.time <- Sys.time()
  time.taken <- difftime(end.time, start.time,units = "secs")
  WCmax.time <- as.numeric(time.taken) + prior.time
  
  start.time <- Sys.time()
  gamma.ns <- sapply(ind.scales,function(x){
    gamma.s(x,epsilon,cono.x,WC.edge)/sum(is.na(WC.edge[x,])==FALSE)
  })
  end.time <- Sys.time()
  time.taken <- difftime(end.time, start.time,units = "secs")
  prior2.time <- as.numeric(time.taken) + prior.time
  
  start.time <- Sys.time()
  WC.avg.lss <- mean(gamma.ns,na.rm=TRUE)
  end.time <- Sys.time()
  time.taken <- difftime(end.time, start.time,units = "secs")
  WCavglss.time <- as.numeric(time.taken) + prior2.time
  
  start.time <- Sys.time()
  WC.max.lss <- max(gamma.ns,na.rm=TRUE)
  end.time <- Sys.time()
  time.taken <- difftime(end.time, start.time,units = "secs")
  WCmaxlss.time <- as.numeric(time.taken) + prior2.time
  
  return(list(WC.avg=WC.avg,WC.max=WC.max,WC.avg.lss=WC.avg.lss,WC.max.lss=WC.max.lss,WCavg.time=WCavg.time,WCmax.time=WCmax.time,
              WCavglss.time=WCavglss.time,WCmaxlss.time=WCmaxlss.time))
}

