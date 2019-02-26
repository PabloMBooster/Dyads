library(moveVis)
library(move)
# LOAD DATA
data("move_data")
move_data

move_data$dt <- as.POSIXct(strptime(move_data$dt, "%Y-%m-%d %H:%M:%S", tz = "UTC"))

#Create new move class object list by individual
data_ani <- split(move(move_data$lon, move_data$lat, proj=CRS("+proj=longlat +ellps=WGS84"),
                       time = move_data$dt, animal=move_data$individual, data=move_data))

#Get libraries 
conv_dir <- get_libraries()

#Find out, which output file formats can be used
get_formats()


#Specify output directory
out_dir <- paste0(getwd(),"/test")


#Specify some optional appearance variables
img_title <- "Movement of the white stork population at Lake Constance, Germany"
img_sub <- paste0("including individuals ","indi_names")
img_caption <- "Projection: Geographical, WGS84; Sources: Movebank 2013; Google Maps"


#Call animate_move()
animate_move(data_ani, out_dir, conv_dir = conv_dir, tail_elements = 10,
             paths_mode = "true_data", frames_nmax = 50,
             img_caption = img_caption, img_title = img_title,
             img_sub = img_sub, log_level = 1, out_format = "mov")














