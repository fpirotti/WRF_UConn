library(raster)
library(sf) 
library(sp)
library(doParallel)
library(foreach)
library(bigstatsr)
library(bench)
library(lwgeom)
library(mapview)
library(bigmemory) 
 
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

damages.for.myproj<-st_read("/archivio/shared/geodati/vettoriali/vaia/paperForzieriVAIA_crsLCCcustom.shp")

damage.bbox<-st_as_sfc(st_bbox(damages.for.myproj))
nodes <- st_read("out/fromGEEreducedVars/nodesWithGEEvars_crsLCCcustom.shp", 
                 query="SELECT FID FROM nodesWithGEEvars_crsLCCcustom")

 

#nodes.myproj <-  nodes[,"geometry"]  %>% st_set_crs(4326)   %>% st_transform(myproj)

#saveRDS(nodes,"nodes.RDS")
#shapefile(st_as_sf(data.frame(geom=squares)), sprintf("out/nodes.square.projected.shp") , overwrite=T)
 
nodes.myproj2<-st_buffer(nodes, 500, nQuadSegs = 1)

st_bbox_by_feature = function(x) {
  x = st_geometry(x)
  f <- function(y) st_as_sfc(st_bbox(y,), crs=myproj)
  do.call("c", lapply(x, f))
}

  
squares <-st_bbox_by_feature(nodes.myproj2)
squares <- squares  %>% st_set_crs( myproj)
#squares.sf<- st_as_sf(squares, data.frame(FID=nodes.myproj2$FID) ) 
 
 squares.intersecting.polys<-st_intersects(squares, categ.for.myproj )  
 squares.intersecting.polys.ids<-which (lengths(squares.intersecting.polys) > 0 )
 
 squares.intersecting.polys.dmg<-st_intersects(squares, damages.for.myproj )  
 squares.intersecting.polys.dmg.ids<-which (lengths(squares.intersecting.polys.dmg) > 0 )

 squares.intersecting.eitherOr.IDS<-union(squares.intersecting.polys.dmg.ids, squares.intersecting.polys.ids)
 ##squares.intersecting.polys.ids<-squares.intersecting.polys.ids[1:100] 
 
 ##shared matrix for output
 
 
 
cl <- parallel::makeForkCluster(14)
 doParallel::registerDoParallel(cl)
 # 
 # cut.Polygons.In.Squares <-
 #   function(squares.intersecting.polys,
 #            squares.intersecting.polys.dmg,
 #            categ.for.myproj, damages.for.myproj, squares,
 #            squares.intersecting.eitherOr.IDS) {
 #     
     mat3 <- FBM(length(squares), 6, type="integer", init=NA)
     
     options(warn = -1)
     remove(i)
     
     system.time(
     tmp <- foreach(i = squares.intersecting.eitherOr.IDS,
             .packages = c("sf", "lwgeom"))  %dopar%  {
               
               mat3[i,1]<- i
               if(length(squares.intersecting.polys[[i]]) > 0){
                 
                 geomes <-     sf::st_make_valid(categ.for.myproj[squares.intersecting.polys[[i]],]) 
                 inters<- sf::st_intersection(geomes, squares[i,])
                 
                 area<-0  
                 areas<-0
                 if(length(inters)>0){
                   areas<- as.numeric(sf::st_area(inters))
                   area <- sum(areas) 
                 }  
                 mat3[i,2]<- as.integer(area)
                 
                 aa<-aggregate(areas, by=list(category=inters$categoria), FUN=sum) 
                 
                 mat3[i,3]<-as.integer(aa$category[[which.max(aa$x)]])
                 mat3[i,4]<-as.integer(max(aa$x))
                 
               }  
               
               
               if( length(squares.intersecting.polys.dmg[[i]]) > 0 ){ 
                 geomes.dmg <- sf::st_make_valid(damages.for.myproj[squares.intersecting.polys.dmg[[i]],]) 
                 inters<-sf::st_intersection(geomes.dmg, squares[i,])
                 
                 area<-0   
                 if(length(inters)>0){ 
                   damage_deg <- ifelse(inters$Damage_deg < 0, 1, inters$Damage_deg ) 
                   area <- sum(as.numeric(sf::st_area(inters))) 
                   warea <- sum(as.numeric(sf::st_area(inters)*damage_deg)) 
                 }  
                 
                 mat3[i,5] <- area 
                 mat3[i,6] <- warea  
                 
               }
               
               NULL
             }
     )
 
 
 options(warn = 0)
 
parallel::stopCluster(cl)



final.nodes <- st_read("out/fromGEEreducedVars/nodesWithGEEvars_crsLCCcustom.shp")

mat4<-mat3[]
colnames(mat4)<-c("rown", "cat_Cov", "mainCat", "mainCat_Cov", "dmg_Cov", "dmg_wCov")
dt4<-as.data.frame(mat4)
final.nodes.binded<- st_bind_cols(final.nodes, dt4)
 
st_write(final.nodes.binded, "out/fromGEEreducedVars/nodesWithGEEvars_andRvar_crsLCCcustom.shp", append = F)

st_write(final.nodes.binded %>% st_set_crs( 4326),
         "out/fromGEEreducedVars/nodesWithGEEvars_andRvar_crs4326.shp", app)

tt<-sapply( names(final.nodes.binded), function(x){
  if(x!="geometry")
    sprintf("<tr><td>%s</td><td>%s</td></tr>", x, class(final.nodes.binded[[x]]) )
  else 
    ""
})
 

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

 