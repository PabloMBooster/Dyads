dyad_info_gral <- function(tr1,tr2,echant,file.name){
  
  matriz <- rbind(tr1,tr2)
  fecha.min <- min(matriz$date)
  fecha.max <- max(matriz$date)
  
  sink(file.name,append = TRUE)
  cat("Cluster ",echant$numclust,"\n")
  cat("\n")
  cat("\n")
  cat("Segment from ",as.character(fecha.min)," to ",as.character(fecha.max),"(",as.character(echant$dur)," hours)","\n")
  cat("\n")
  cat("Vessel 1: ",echant$id.1,"\n")
  cat("Trip code: ",echant$burst.1,"\n")
  cat("Departure time: ",as.character(tr1$DEPARTURE_DATE_TIME[1]),"\n")
  cat("Departure port: ",as.character(tr1$DEPARTURE_LOCATION_NAME[1]),"\n")
  cat("Arrival time: ",as.character(tr1$RETURN_DATE_TIME[1]),"\n")
  cat("Arrival port: ",as.character(tr1$RETURN_LOCATION_NAME[1]),"\n")
  sink()
  
  if (length(grep("IFREMER",names(tr1))) > 0){
    gear.count <- count(tr1$LE_GEAR)
    gear.count <- gear.count[!is.na(gear.count[,1]),]
    if (dim(gear.count)[1] > 0){
      gear <- as.character(gear.count[which.max(gear.count[,2]),1])
    }else {
      gear <- NA
    }
    gear.1 <- gear
    especies.1 <- unique(tr1$espece.kg)
    especies.1 <- especies.1[!is.na(especies.1)]
    
    sink(file.name,append = TRUE)
    cat("Fleet class: ",as.character(tr1$S_S_FLOTTILLE_IFREMER_LIB[1]),"\n")
    cat("Length: ",as.character(tr1$NAVLC9_COD[1]),"\n")
    cat("Power: ",tr1$NAVP_PUISSANCE_AD[1],"\n")
    cat("Gear 1: ",as.character(tr1$ENG1[1]),"\n")
    cat("Gear 2: ",as.character(tr1$ENG2[1]),"\n")
    cat("Gear S: ",gear.1,"\n")
    cat("Especies: ",as.character(especies.1),"\n")
    sink()
    
  }
  
  sink(file.name,append = TRUE)
  cat("\n")
  cat("Vessel 2: ",echant$id.2,"\n")
  cat("Trip code: ",echant$burst.2,"\n")
  cat("Departure time: ",as.character(tr2$DEPARTURE_DATE_TIME[1]),"\n")
  cat("Departure port: ",as.character(tr2$DEPARTURE_LOCATION_NAME[1]),"\n")
  cat("Arrival time: ",as.character(tr2$RETURN_DATE_TIME[1]),"\n")
  cat("Arrival port: ",as.character(tr2$RETURN_LOCATION_NAME[1]),"\n")
  sink()
  
  if (length(grep("IFREMER",names(tr2))) > 0){
    gear.count <- count(tr2$LE_GEAR)
    gear.count <- gear.count[!is.na(gear.count[,1]),]
    if (dim(gear.count)[1] > 0){
      gear <- as.character(gear.count[which.max(gear.count[,2]),1])
    }else {
      gear <- NA
    }
    gear.2 <- gear
    especies.2 <- unique(tr2$espece.kg)
    especies.2 <- especies.2[!is.na(especies.2)]
    
    sink(file.name,append = TRUE)
    cat("Fleet class: ",as.character(tr2$S_S_FLOTTILLE_IFREMER_LIB[1]),"\n")
    cat("Length: ",as.character(tr2$NAVLC9_COD[1]),"\n")
    cat("Power: ",tr2$NAVP_PUISSANCE_AD[1],"\n")
    cat("Gear 1: ",as.character(tr2$ENG1[1]),"\n")
    cat("Gear 2: ",as.character(tr2$ENG2[1]),"\n")
    cat("Gear S: ",gear.2,"\n")
    cat("Especies: ",as.character(especies.2),"\n")
    cat("\n")
    cat("\n")
    cat("\n")
    sink()
    
  }
  
}