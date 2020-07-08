library(raster)
library(sf)
library(ggplot2)
library(geosphere)
library(sp)
wfolder<-"/archivio/shared/geodati/vettoriali/vaia/Cerrai_WRF_grid/"
# 

nodes <- readRDS("nodes.RDS")

nodes.projected <-  nodes  %>% st_set_crs(4326)   %>% st_transform(3034)

nodes.coords <-  as.data.frame(st_coordinates( nodes.projected))
names(nodes.coords)<-c("x", "y")
coordinates(nodes.coords) <-  ~x+y

nodes.hex<-sp::HexPoints2SpatialPolygons(nodes.coords, 500)



 