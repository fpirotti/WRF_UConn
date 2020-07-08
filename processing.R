library(raster)
library(sf) 
library(sp)

source("makeShape.R")
rad2deg<-function(rad){ rad*180/pi}
deg2rad<-function(deg){ deg/180*pi}
cell_width<-500
wfolder<-"/archivio/shared/geodati/vettoriali/vaia/Cerrai_WRF_grid/"
# 

# nodes <- readRDS("nodes.RDS")
# nodes <-  nodes  %>% st_set_crs(4326)   %>% st_transform(3034)
# 
# nodes.sp <-  as.data.frame(st_coordinates( nodes))
# names(nodes.sp)<-c("x", "y")
# coordinates(nodes.sp) <-  ~x+y
# crs(nodes.sp)<-CRS("+init=epsg:4326")
# saveRDS(nodes.sp, "nodes.sp")
 
nodes.sp<-readRDS("nodes.sp")
centroids<-as.data.frame(nodes.sp@coords[1:20,])
radius<-500
points<-as.data.frame(nodes.sp@coords)

degr<-1.2065 
squares<-processPoints(points , 500, rot = deg2rad(degr)  )
crs(squares)<-CRS("+init=epsg:3034")
shapefile(squares, sprintf("nodes.square.projected.rotated_%sdeg.shp", degr) , overwrite=T)
 

squares.latlng<- spTransform(squares, CRS("+init=epsg:4326"))
shapefile(squares, sprintf("nodes.square.latlng.rotated_%sdeg.shp", degr) , overwrite=T)
 

# nodes.coords <-  as.data.frame(st_coordinates( nodes.projected))
# names(nodes.coords)<-c("x", "y")
# coordinates(nodes.coords) <-  ~x+y
# 
# nodes.hex<-sp::HexPoints2SpatialPolygons(nodes.coords, 1000)
# saveRDS(nodes.hex, "nodes.hex.projected")
# nodes.hex.projected<-readRDS("nodes.hex.projected")
# nodes.hex.projected <-  st_as_sf(nodes.hex.projected)  %>% st_set_crs(3034)  
# st_write(nodes.hex.projected, "nodes.hex.projected.shp", append=F)
# 
# nodes.hex.latlng <-  st_as_sf(nodes.hex.projected)  %>% st_set_crs(3034)   %>% st_transform(4326)
# saveRDS(nodes.hex.latlng, "nodes.hex.latlng")
# nodes.hex.latlng<-readRDS("nodes.hex.latlng")
# 
# st_write(nodes.hex.latlng, "nodes.hex.latlng.shp", append=F)

 