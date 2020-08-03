library(raster)
library(sf) 
library(sp)
library(doParallel)
library(foreach)
library(bigstatsr)
library(bench)
library(lwgeom)
library(mapview)

source("makeShape.R")
rad2deg<-function(rad){ rad*180/pi}
deg2rad<-function(deg){ deg/180*pi}
cell_width<-500
wfolder<-"/archivio/shared/geodati/vettoriali/vaia/Cerrai_WRF_grid/"
myproj<-"+proj=lcc +lat_1=45.827  +lat_2=45.827  +lat_0=45.827 +lon_0=11.625 +x_0=4000000 +y_0=2800000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

## read CATEGORIE -----
 categ.for.myproj<-st_read("/archivio/shared/geodati/vettoriali/WRF_UConn/catValidated_LCCcustom.shp")
 for ( i in  names(categ.for.myproj ) ){
   if( !(i %in% c("fid", "categoria", "geometry"))  ) categ.for.myproj[[i]]<-NULL
 }

#categ.for.myproj <- categ.for %>% st_transform(myproj)
#st_write(categ.for.myproj, "/archivio/shared/geodati/vettoriali/WRF_UConn/catValidated_LCCcustom.shp")

## FROM NODES TO RECT TILES -----

nodes <- readRDS("nodes.RDS")
nodes.myproj <-  nodes  %>% st_set_crs(4326)   %>% st_transform(myproj)
# 
# nodes.sp <-  as.data.frame(st_coordinates( nodes))
# names(nodes.sp)<-c("x", "y")
# coordinates(nodes.sp) <-  ~x+y
# crs(nodes.sp)<-CRS("+init=epsg:4326")
# saveRDS(nodes.sp, "nodes.sp")
#nodes.sp<-readRDS("nodes.sp") 

#square.size<-500
#points<-as.data.frame( st_coordinates(nodes.myproj))

#degr<-1.2065 
#squares<-processPoints(points , 500  )
#crs(squares)<-CRS(myproj)
jj<-data.frame(geometry=squares[1:14])
squares.g<-st_as_sf(data.frame(geometry=squares[1:14]))
shapefile(st_as_sf(data.frame(geom=squares)), sprintf("out/nodes.square.projected.shp") , overwrite=T)
 
st_as
 #squares<-st_read( sprintf("out/nodes.square.projected.shp") )
#   
# ### find squares overlapping 
nodes.myproj2<-nodes.myproj[1:13,]

st_bbox_by_feature = function(x) {
  x = st_geometry(x)
  f <- function(y) st_as_sfc(st_bbox(y), crs=myproj)
  do.call("c", lapply(x, f))
}

  
squares<-st_bbox_by_feature(nodes.myproj2)
squares <- squares  %>% st_set_crs( myproj)
mapview(nodes.myproj2) + squares
  
 squares.intersecting.polys<-st_intersects(squares, categ.for.myproj ) 
# 
 squares.intersecting.polys.ids<-which (lengths(squares.intersecting.polys) > 0 )


cl <- parallel::makeForkCluster(14)
doParallel::registerDoParallel(cl)

cut.Polygons.In.Squares <-
  function(squares.intersecting.polys,
           categ.for.myproj, squares,
           squares.intersecting.polys.ids) {
    foreach(i = squares.intersecting.polys.ids,
            .packages = c("sf", "lwgeom"))  %dopar% {
              
              geomes <- st_make_valid(categ.for.myproj[squares.intersecting.polys[[i]],])
              inters<-st_intersection(squares[i,], geomes)
              area<-ifelse(length(inters)>0, sum(st_area(inters)), 0)
              areas<-0
              if(length(inters)>0) areas<- as.numeric(st_area(inters))
              
              inters[["tot_area"]]<-area
              inters[["cover"]]<- area/as.numeric(st_area(squares[i,]))
              
              aa<-aggregate(areas, by=list(category=inters$categoria), FUN=sum)
              
              
              inters[["mainCategory"]]<-as.character(aa$category[[which.max(aa$x)]])
              inters[["mainCategory_cover"]]<-max(aa$x)/as.numeric(st_area(squares[i, ]))
            }
  }

cut.Polygons.In.Squares.output <- cut.Polygons.In.Squares(squares.intersecting.polys,
                         categ.for.myproj, squares,
                        squares.intersecting.polys.ids)

parallel::stopCluster(cl)


saveRDS(cut.Polygons.In.Squares.output, "cut.Polygons.In.Squares.output.rds")

# squares.6707<- spTransform(squares, CRS("+init=epsg:6707"))
# shapefile(squares, sprintf("out/nodes.square.epsg6707.shp") , overwrite=T)
# 
# squares.latlng<- spTransform(squares, CRS("+init=epsg:4326"))
# shapefile(squares, sprintf("out/nodes.square.latlng.shp", degr) , overwrite=T)
#   

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

 