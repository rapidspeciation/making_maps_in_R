# Wall lizard maps of sampling sites 


# Set up the working environment ####
library("ggmap")
library(osmdata)
library(rnaturalearth)
library(sf)
library(raster)

#set wd to where the script is saved
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
cbPalette <- c("#F0E442","#CC79A7")

#get the sampling data 
data <- read.csv("../data/Sample_gps.csv")


# SWITZERLAND WITH CANTONS ####
#define the map region (all of Switzerland)
switzerland <- c(left = 5.7, bottom = 45.6, right = 10.5, top = 47.9)
#shape of switzerland from 
swiss_shape <- ne_states(country = "Switzerland", returnclass = "sf")
#get the map
swiss_map <- get_map(switzerland, source="stamen", maptype = "terrain-background", color = "bw", force=TRUE)
#plot it 
ggmap(swiss_map)+
  geom_jitter(data=data, aes(x=longitude, y=latitude, fill=year), size=3, shape=21, width = 0.01, height = 0.01)+
  theme(legend.position = "none")+
  scale_fill_manual(values = cbPalette)+
  geom_sf(data=swiss_shape, aes(x = longitude, y = latitude, group = adm0_a3), colour = "grey30", 
          fill=NA, linewidth =0.9, linetype="longdash")


# SWITZERLAND WITH COUNTRY OUTLINE ####
# shape of switzerland from 
swiss_shape <- ne_countries( returnclass = "sf", country = "Switzerland", scale = 'large')
#sample data points
swiss_map <- get_map(switzerland, source="stamen", maptype = "terrain-background", color = "color", force=TRUE)
ggmap(swiss_map)+
  geom_jitter(data=data, aes(x=longitude, y=latitude, fill=year), size=3, shape=21, width = 0.01, height = 0.01)+
  theme(legend.position = "none")+
  scale_fill_manual(values = cbPalette)+
  geom_sf(data=swiss_shape, aes(x = label_x, y = label_y, group = sovereignt), colour = "grey20", 
          fill=NA, linewidth =0.9, linetype="longdash")+
  ylab("Latitude")+
  xlab("Longitude")+
  theme_bw() +
  theme(axis.line = element_line(colour = "grey"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(), legend.position = "top") 




# CROP MAP WITH OUTLINE #####

# courtesy R Lovelace from https://github.com/Robinlovelace/Creating-maps-in-R/blob/master/vignettes/download-raster.R
ggmap_rast <- function(map){
  map_bbox <- attr(map, 'bb') 
  .extent <- extent(as.numeric(map_bbox[c(2,4,1,3)]))
  my_map <- raster(.extent, nrow= nrow(map), ncol = ncol(map))
  rgb_cols <- setNames(as.data.frame(t(col2rgb(map))), c('red','green','blue'))
  red <- my_map
  values(red) <- rgb_cols[['red']]
  green <- my_map
  values(green) <- rgb_cols[['green']]
  blue <- my_map
  values(blue) <- rgb_cols[['blue']]
  stack(red,green,blue)
}


che.rast <- ggmap_rast(map = swiss_map)

che.rast2 <- crop(che.rast, swiss_shape) 
che.rast3 <- mask(che.rast2, swiss_shape)

# prep raster as a data frame for printing with ggplot
che.df <- data.frame(rasterToPoints(che.rast3))
che_map <- ggplot(che.df) + 
  geom_point(aes(x=x, y=y, col=rgb(layer.1/255, layer.2/255, layer.3/255))) + 
  scale_color_identity()

data$year = as.factor(data$year)


# Plot it with correct mercator projection by using coord_map()
che_map + 
  coord_map() +
  geom_jitter(data=data, aes(x=longitude, y=latitude, fill=year), size=3, shape=21, width = 0.01, height = 0.01)+
  theme(legend.position = "none")+
  scale_fill_manual(values = cbPalette)+
  ylab("Latitude")+
  xlab("Longitude")+
  theme_void() +
  theme(legend.position = c(0.9, 0.85), 
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        plot.title = element_text(size=20, face="bold", hjust = 0.5)) +
  guides(fill=guide_legend(title="Sampling year"))+
  ggtitle("Wall lizard sampling in Switzerland")
