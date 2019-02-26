# libraries
library(stringr)
library(vmstools)
library(sp)
library(lubridate)
library(adehabitatLT)
library(Rclusterpp)
library(NbClust)
library(xtable)
library(maps)
library(maptools)
library(mapdata)
library(animation)
library(RColorBrewer)
library(igraph)
library(cluster)
library(scales)

# functions
source("Codes/thon_tripID_filtering.R") # identifying trips and filtering records at port
source("Codes/thon_interpolation_variableDeriv.R") # linear interpolation and second and third order variable derivation
source("Codes/thon_dyadsegmentation.R") # dyad identification
source('Codes/fonctions-indices-20180606.R') # functions to compute dyadic metrics
source('Codes/Etape5-Indices-Thon.R') # Code to compute dyadic metrics using the functions above
source('Codes/Etape6-ClusteringDyads.r') # cluster analysis of dyads
source('Codes/Etape7-PlotVideoDyads.R') # plots of dyad examples for each cluster
source('Codes/dyad_info_general.r') # function to get general information for each dyadic example
source('Codes/pre_video_dyads_all.r') # get the right format for dyad and all vessels around to be used for plotting and videos
source('Codes/pre_video_modif.r') # get the right format for dyad to be used for plotting and videos
source('Codes/VideoHtmlDyad.r') # produce the animation
source('Codes/VideoGIFDyad.R') # produce giff
source('Codes/plot_dyad_general.R') # plot dyad
source('Codes/subplot_cor.r') # plot correlation series
source('Codes/subplot_uni.r') # plot univariate series
source('Codes/Etape8-FleetAnalysis.r') # do fleet and vessel analysis based on clusters (e.g. network analysis)

# for projecting
projet <- function(data,var.lon,var.lat){
  xy <- data[c(var.lon,var.lat)]
  data[c("LONGITUDE_M","LATITUDE_M")] <- coordinates(spTransform(SpatialPointsDataFrame(xy, data, 
                                                                                        proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"),
                                                                                        match.ID = TRUE),CRS("+init=epsg:32740 +units=m")))
  return(data)
}

unprojet <- function(data){
  xy <- data[c("lon2","lat2")]
  data[c("LONGITUDE","LATITUDE")] <- coordinates(spTransform(SpatialPointsDataFrame(xy,data,proj4string = CRS("+init=epsg:32740")),
                                                             CRS("+proj=longlat  +ellps=WGS84 +datum=WGS84")))
  return(data)
}

# paths
dossier.inputs <- 'C:/pablo/D/github/ThonDyads/DataInputs/'
dossier.outputs <- 'C:/pablo/D/github/ThonDyads/DataOutputs/'
dossier.stats <- "C:/pablo/D/github/ThonDyads/StatsPlots/"

# read port information

port=read.csv(paste0(dossier.inputs,"ports-dВbarquement2.csv"),
              col.names=list("port","latitude","latgr","latmn","latsec","nos","longitude","longr","lonmn","lonsec","ew"))
head(port)
port$ew <- str_replace_all(port$ew, " ", "")
sign.ew <- rep(1,length(port$ew))
sign.ew[which(port$ew == 'O')] <- -1
port$nos <- str_replace_all(port$nos, " ", "")
sign.nos <- rep(1,length(port$nos))
sign.nos[which(port$nos == 'S')] <- -1
port$lon <- (port$longr+port$lonmn/60+port$lonsec/3600)*sign.ew
port$lat <- (port$latgr+port$latmn/60+port$latsec/3600)*sign.nos

# parameters for trip identification
thres.dist.port.1 <- 1 # at < 1km from port the vessel is considered at the port
thres.port <- 10 # at less than 10 km from port and less than 2 km/h the vessel is considered at port (not moving)
vit <- 2
seuil <- 1 # for time interval between consecutive records (to keep the trip, see details in the code)

# parameters for metric computation
param <- NULL
param$Prox$delta <- c(10,30,60)
param$jPPA$vmax <- 60*1000*1.852 # 20 knots was too small  -- for data in meters (vmax*delta.t) # leave it like this since we are not interpreting this in the end
param$DI$beta <- 1 # leave it like this since we are not interpreting this in the end
# we are coding CSEM derivations for locations and WC for displacements; if other variable is used the inner code should be modified
param$CSEM$delta <- 10 # leave it like this since we are not interpreting this in the end
param$WC$sl <- 2 # leave it like this since we are not interpreting this in the end
param$WC$su <- 6 # leave it like this since we are not interpreting this in the end
param$WC$xi <- 0.5 # leave it like this since we are not interpreting this in the end
param$jPPA$cell.size <- 10*3000 # 3 km # leave it like this since we are not interpreting this in the end
param$Dyad$Proche <- 60 #5 # how close do vessels have to be to be considered a dyad
param$Dyad$dur <- 3 #10 # hours
gear <- "Thon"  # or "Thon-jour"

# parameters for clustering
variables <- c("Prox.10","r.lonlat","r.speed","DI.theta")
new.variables <- c("Prox10","Lonlat","Speed","DItheta")

# Donnees Emily

# vms=read.csv2(paste0(dossier.inputs,"Export_VMS_2011-2013.csv"))
# vms$DATE <- as.POSIXct(as.character(vms$timestamp_vms),tz='GMT',format='%Y-%m-%d %H:%M:%S')
# save(vms,file=paste0(dossier.inputs,"Export_VMS_2011-2013.RData"))
load(file=paste0(dossier.inputs,"Export_VMS_2011-2013.RData")) # had to make an .RData version to get a smaller-size file for GitHub

# for some reason I have to do this by year: there is something weird in the datasets that messes up the analyses if I don't

year_min = 2011
year_max = 2013

for (year_loop in year_min:year_max){
  
  db <- vms[lubridate:::year(vms$DATE)==year_loop,]
  if (year_loop == 2011){
    db <- db[-1,]
  }
  
  ################################################# TRIP IDENTIFICATION

  ech.vms.data <- thon_tripID_filtering(db,port,thres.dist.port.1,thres.port,vit,seuil)
  
  ########################################################## interpolation and derivation of auxiliary variables
  vms.data <- thon_interpolation_variableDeriv(ech.vms.data)
  save(vms.data,file=paste0(dossier.outputs,'thonvms7ld3_',year_loop,'.RData'))
  
  ### dyad segmentation
  
  segments <- thon_dyadsegmentation(vms.data)
  
  segments$gear.1 <- "thon" 
  segments$gear.2 <- "thon"
  
  ###################################### computing dyad metrics (not really only for the day; it's also at night)
  
  
  dyads <- Indices.Derivation(data=vms.data,segments,year_loop,dossier.outputs,param,all=FALSE,gear) ## jPPA fait chier!!!
  
  
  
}

year_loop <- 2011
name <- paste0("segments-indic-Thon-",year_loop,".RData")
load(paste0(dossier.outputs,name))
dyads <- data.dyads
load(paste0(dossier.outputs,'thonvms7ld3_',year_loop,'.RData'))
vms.data.years <- vms.data
year_loop <- 2012
name <- paste0("segments-indic-Thon-",year_loop,".RData")
load(paste0(dossier.outputs,name))
dyads <- rbind.data.frame(dyads,data.dyads)
load(paste0(dossier.outputs,'thonvms7ld3_',year_loop,'.RData'))
vms.data.years <- rbind.data.frame(vms.data.years,vms.data)
year_loop <- 2013
name <- paste0("segments-indic-Thon-",year_loop,".RData")
load(paste0(dossier.outputs,name))
dyads <- rbind.data.frame(dyads,data.dyads)
load(paste0(dossier.outputs,'thonvms7ld3_',year_loop,'.RData'))
vms.data.years <- rbind.data.frame(vms.data.years,vms.data)



head(dyads)
dyads <- dyads[dyads$Proche == 1,]
summary(dyads$dur)
quantile(dyads$dur,probs=0.10) # 106.3

dyads <- dyads[dyads$dur >= 106,]
save(dyads,file=paste0(dossier.outputs,"dyads_filter_Proche60_dur_106_",year_min,"-",year_max,".RData"))
load(paste0(dossier.outputs,"dyads_filter_Proche60_dur_106_",year_min,"-",year_max,".RData"))

# now, clustering
# year = "2011-2013"

cluster.res <- clustering.dyads(dyads,variables,gear,year="2011-2013",dossier.data.outputs=dossier.outputs,
                 dossier.stats.outputs=dossier.stats,new.variables,k=NULL)
save(cluster.res,file=paste0(dossier.outputs,"cluster_object_",year_min,"-",year_max,".RData"))
load(paste0(dossier.outputs,"cluster_object_",year_min,"-",year_max,".RData"))
# ><>><>><>><>><>><>><>><>><>

load("DataOutputs/thonvms7ld3_2013.RData")
x <- vms.data
load("DataOutputs/thonvms7ld3_2012.RData")
y <- vms.data
load("DataOutputs/thonvms7ld3_2011.RData")
z <- vms.data

vms_data_years <- rbind(x,y,z)

# ><>><>><>><>><>><>><>><>><> 
# load (paste0(dossier.outputs,"cluster_object_",year_min,"-",year_max,".RData"))

######################################## PLOTS AND VIDEOS OF DYADS ##################################################################

video.par <- NULL
video.par$type <- 'all' # choose between video, plot, series and all
video.par$mp4 <- TRUE # IF SO, ffmpeg function must be able to run in the konsole

# if I want harbour info and it comes in vmstools
video.par$harbours.info <- TRUE
data(harbours) # otherwise upload it from somewhere else
port <- tolower(sapply(harbours$harbour,function(x) str_replace_all(x, "[^[:alnum:]]", " ")))
port.comprime <- sapply(port,function(x) paste(unlist(strsplit(as.character(x)," ")),collapse=""))
video.par$ports <- data.frame(port,harbours$lon,harbours$lat,port.comprime)

# ><> solo trayectorias de los pares (FALSE)
video.par$context.video.info <- FALSE # vessels around will also appear in the video; plots are composed only of dyads for clarity
video.par$rayon <- 1 # kilometers around
video.par$anonymity <- FALSE # locations are transformed and coast is not shown (consequently harbour information is not used)

video.par$col_vector <- colorRampPalette(c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00'))

# either do all this or just engin.args = NULL
engin.args = NULL
# engins <- c('OTB','DRB','OTT','MIS','OTM','PTM','PTB','other',NA)
# pch.engin <- c(21,22,24,25,24,21,23,23,1)
# col.engin <- c('red','blue','yellow','green','mediumpurple1','white','gray59','cyan','black')
# video.par$engin.args <- data.frame(engins,pch.engin,col.engin)
# video.par$engin.args$engins <- as.character(video.par$engin.args$engins)
# video.par$engin.args$col.engin <- as.character(video.par$engin.args$col.engin)

video.par$traza = 12 # number of hours for leaving a trace at each frame in the video
video.par$epais = 4 # width of trajectories
video.par$sleep=0.25
video.par$point.size = 1.2
video.par$range.x = FALSE
video.par$range.y = FALSE
video.par$espece = NULL # wanting to track catch from a given species
video.par$legend.gears <- FALSE
video.par$legend.vessels <- TRUE


# # for anchovy in Peru
# xrange = c(-90, -70)
# yrange = c(-21,-3)
# # for celtic sea and English channel
# xrange = c(-10,10)
# yrange = c(40,60)
# # for thon in Indian Ocean
xrange = c(35, 75)
yrange = c(-25, 10)

# land info for mapping
map2 <- map(database="worldHires",
            regions=".",                    # select any polygon within the long/lat range.
            fill = TRUE, col = gray(c(0.6)),
            xlim= xrange, ylim=yrange, #xlim=c(-10, 10), ylim=c(40, 60), 
            interior=TRUE, plot=FALSE)
land.info <- map2SpatialPolygons(map2,
                                 proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                                 IDs=sapply(strsplit(map2$names, ":"),
                                            function(x) x[1]))
land.info <- SpatialPolygonsDataFrame(Sr=land.info,
                                      data=data.frame(ID=sapply(land.info@polygons, slot, name="ID"),
                                                      row.names=sapply(land.info@polygons, slot, name="ID")))

colores <- c("green","red","blue") # for prox points; otherwise they are chosen automatically
vms.data.years2 <- vms.data.years[vms.data.years$year == 2011,]
vms.data.years2 = vms.data.years
# ANIMACION
plots.video.dyads2(vms.data=vms.data.years2,dyads=cluster.res$dyads,year="2011-2013",gear,new.variables,
                  dossier.stats,dossier.outputs,n=5,#number of sample dyads
                  text.dyads=TRUE,video.par,land.info,param=param,colores=colores)

# CAMBIAR DIRECTORIO DE HTML

######################################## END OF PLOTS AND VIDEOS OF DYADS ##################################################################


######################################## FLEET ANALYSIS BASED ON DYADS ##################################################################


fleet.plot.par <- NULL
fleet.plot.par$th <- 0 #.99
fleet.plot.par$cx  <- .6
fleet.plot.par$height.couple <- 20
fleet.plot.par$height.couple.excl <- 16
new.variables.indep <- rep(0,length(new.variables)) # values of no interaction for each of the indices of interest
matriz.orig <- cluster.res$dyads[,new.variables]
moy <- apply(matriz.orig,2,mean)
ecart <- apply(matriz.orig,2,sd)
remplir.mjm <- 1/(1+exp(-sum(((new.variables.indep - moy)/ecart)*cluster.res$poids)))
print(paste0('reference value for no interaction in fleet: ', remplir.mjm))

# redes sociales
Fleet.Analysis(vms.data=vms.data.years,dyads=cluster.res$dyads,Weights=cluster.res$poids,
               dossier.outputs,dossier.stats,gear,year="2011-2013",
               fleet.plot.par,remplir.mjm,fleet.cluster=NULL)
# shineapp # 7 enero
