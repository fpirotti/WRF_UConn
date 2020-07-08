library(raster)
library(sf) 
library(sp)
wfolder<-"/archivio/shared/geodati/vettoriali/vaia/Cerrai_WRF_grid/"
# 

nodes <- readRDS("nodes.RDS")

nodes.projected <-  nodes  %>% st_set_crs(4326)   %>% st_transform(3034)

nodes.coords <-  as.data.frame(st_coordinates( nodes.projected))
names(nodes.coords)<-c("x", "y")
coordinates(nodes.coords) <-  ~x+y

nodes.hex<-sp::HexPoints2SpatialPolygons(nodes.coords, 1000)
saveRDS(nodes.hex, "nodes.hex.projected")
nodes.hex.projected<-readRDS("nodes.hex.projected")
nodes.hex.latlng <-  st_as_sf(nodes.hex.projected)  %>% st_set_crs(3034)   %>% st_transform(4326)
saveRDS(nodes.hex.latlng, "nodes.hex.latlng")
nodes.hex.latlng<-readRDS("nodes.hex.latlng")

st_write(nodes.hex.latlng, "nodes.hex.latlng.shp", append=F)

 