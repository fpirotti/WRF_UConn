library(raster)
library(sf) 
library(sp)

source("makeShape.R")
rad2deg<-function(rad){ rad*180/pi}
deg2rad<-function(deg){ deg/180*pi}
cell_width<-500
wfolder<-"/archivio/shared/geodati/vettoriali/vaia/Cerrai_WRF_grid/"

## read CATEGORIE -----
#categ.for<-shapefile("/archivio/shared/geodati/vettoriali/carte_forestali_merge_totale.shp")
#saveRDS(categ.for, "categ.for.rds")
categ.for<-readRDS( "categ.for.rds")


## FROM NODES TO RECT TILES -----
myproj<-"+proj=lcc +lat_1=45.827  +lat_2=45.827  +lat_0=45.827 +lon_0=11.625 +x_0=4000000 +y_0=2800000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

nodes <- readRDS("nodes.RDS")
nodes.myproj <-  nodes  %>% st_set_crs(4326)   %>% st_transform(myproj)
# 
# nodes.sp <-  as.data.frame(st_coordinates( nodes))
# names(nodes.sp)<-c("x", "y")
# coordinates(nodes.sp) <-  ~x+y
# crs(nodes.sp)<-CRS("+init=epsg:4326")
# saveRDS(nodes.sp, "nodes.sp")
#nodes.sp<-readRDS("nodes.sp") 

square.size<-500
points<-as.data.frame(nodes.sp@coords)

#degr<-1.2065 
squares<-processPoints(points , 500, rot = deg2rad(degr)  )
crs(squares)<-CRS("+init=epsg:3034")
shapefile(squares, sprintf("nodes.square.projected.rotated_%sdeg.shp", degr) , overwrite=T)

squares.6707<- spTransform(squares, CRS("+init=epsg:6707"))
shapefile(squares, sprintf("nodes.square.epsg6707.rotated_%sdeg.shp", degr) , overwrite=T)

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

 