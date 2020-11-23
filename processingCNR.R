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
library(openxlsx)

rad2deg<-function(rad){ rad*180/pi}
deg2rad<-function(deg){ deg/180*pi}
cell_width<-333
wfolder<-"/archivio/shared/geodati/vettoriali/vaia/Cerrai_WRF_grid/"
myproj<-"+proj=lcc +lat_1=45.827  +lat_2=45.827  +lat_0=45.827 +lon_0=11.625 +x_0=4000000 +y_0=2800000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

## read CATEGORIE -----
categ.for.myproj<-st_read("/archivio/shared/geodati/vettoriali/categorie_forestali_triveneto_crsCustomLCC.shp")
for ( i in  names(categ.for.myproj ) ){
  if( !(i %in% c("fid", "categoria", "geometry"))  ) categ.for.myproj[[i]]<-NULL
} 

categ.for.myproj <- categ.for.myproj %>% st_set_crs( myproj)

damages.for.myproj<-st_read("/archivio/shared/geodati/vettoriali/vaia/paperForzieriVAIA_crsLCCcustom_corrected.shp")

damages.for.myproj <- damages.for.myproj %>% st_set_crs( myproj)
damage.bbox<-st_as_sfc(st_bbox(damages.for.myproj))
### 1 km
# nodes <- st_read("out/fromGEEreducedVars/nodesWithGEEvars_crsLCCcustom.shp", 
#                  query="SELECT FID FROM nodesWithGEEvars_crsLCCcustom")

# nodes.df <-read.table("d02_xy_latlon.txt" ) 
# nodes.df$V1<-NULL
# names(nodes.df)<-c("FID", "lat", "lng")
# nodes<-st_as_sf(nodes.df, coords = c("lng","lat"))
#nodes.myproj <-  nodes[,"geometry"]  %>% st_set_crs(4326)   %>% st_transform(myproj)
 
nodes.4326 <- st_read("in/CNR_nodesWithGEEvars_crs4326.shp", 
                      query="SELECT FID FROM CNR_nodesWithGEEvars_crs4326")

nodes <- nodes.4326 %>% st_transform(myproj)
#saveRDS(nodes,"nodes.RDS")
#shapefile(st_as_sf(data.frame(geom=squares)), sprintf("out/nodes.square.projected.shp") , overwrite=T)
 
nodes.buffered<-st_buffer(nodes, cell_width, nQuadSegs = 1)

st_bbox_by_feature = function(x) {
  x = st_geometry(x)
  f <- function(y) st_as_sfc(st_bbox(y,), crs=myproj)
  do.call("c", lapply(x, f))
}

squares.geom <-st_bbox_by_feature(nodes.buffered)
squares <- data.frame(  FID=nodes.buffered$FID, geometry=squares.geom)
st_geometry(squares)<-"geometry"
squares <- squares  %>% st_set_crs( myproj)
#squares.sf<- st_as_sf(squares, data.frame(FID=nodes.myproj2$FID) ) 
 
 squares.intersecting.polys<-st_intersects(squares, categ.for.myproj )  
 squares.intersecting.polys.ids<-which (lengths(squares.intersecting.polys) > 0 )
 
 squares.intersecting.polys.dmg<-st_intersects(squares, damages.for.myproj )  
 squares.intersecting.polys.dmg.ids<-which (lengths(squares.intersecting.polys.dmg) > 0 )

 squares.intersecting.eitherOr.IDS<-union(squares.intersecting.polys.dmg.ids, squares.intersecting.polys.ids)
 ##squares.intersecting.polys.ids<-squares.intersecting.polys.ids[1:100] 
 
 ##shared matrix for output
 
 categ.for.myproj$categoria<-as.factor(categ.for.myproj$categoria)
 categ.for.myproj <- categ.for.myproj  %>% st_set_crs( myproj)
 
 #squares.4326 <-  squares  %>% st_transform(4326)
 
 #st_write(squares.4326, "out/CNR_node_squares_crs4326.shp", append=F)
 ######## DO GEE STUFF HERE!
 
 
cl <- parallel::makeForkCluster(14)
 doParallel::registerDoParallel(cl)
 # 
 # cut.Polygons.In.Squares <-
 #   function(squares.intersecting.polys,
 #            squares.intersecting.polys.dmg,
 #            categ.for.myproj, damages.for.myproj, squares,
 #            squares.intersecting.eitherOr.IDS) {
 #     
     mat3 <- FBM(nrow(squares), 6, type="integer", init=NA)
     
     options(warn = -1)
     remove(i)
     
     system.time(
     tmp <- foreach(i = squares.intersecting.eitherOr.IDS,
             .packages = c("sf", "lwgeom"))  %dopar%  {
               
               mat3[i,1]<-  squares[i,]$FID
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

#final.nodes <- st_read("out/fromGEEreducedVars/CNR_nodesWithGEEvars_crsLCCcustom.shp")

final.nodes <- st_read("in/CNR_nodesWithGEEvars_crs4326.shp")

mat4<-mat3[]
colnames(mat4)<-c("FID", "cat_Cov", "mainCat", "mainCat_Cov", "dmg_Cov", "dmg_wCov")
dt4<-as.data.frame(mat4)
final.nodes.binded<- st_bind_cols(final.nodes, dt4)
final.nodes.binded$FID.1<-NULL
final.nodes.binded$cat_Cov<-final.nodes.binded$cat_Cov / (cell_width*2)^2 *100
final.nodes.binded$mainCat_Cov <-final.nodes.binded$mainCat_Cov  / (cell_width*2)^2 *100
final.nodes.binded$dmg_Cov  <-final.nodes.binded$dmg_Cov / (cell_width*2)^2 *100
final.nodes.binded$dmg_wCov <-final.nodes.binded$dmg_wCov  / (cell_width*2)^2 *100




summary(final.nodes.binded)



st_write(final.nodes.binded, "out/fromGEEreducedVars/CNR_nodesWithGEEvars_andRvar_crsLCCcustom.shp", append = F)
unzip("out/fromGEEreducedVars/CNR_nodesWithGEEvars_andRvar_crsLCCcustom.zip")
#final.nodes.binded<-st_read( "CNR_nodesWithGEEvars_andRvar_crsLCCcustom.shp")
summary(final.nodes.binded)
#file.remove( list.files( pattern = "CNR_nodesWithGEEvars_andRvar_crsLCCcustom\\.*") )



st_write(final.nodes.binded %>% st_transform(4326),
         "out/fromGEEreducedVars/nodesWithGEEvars_andRvar_crs4326.shp", append=F)

final.nodes.binded.df <- as.data.frame(final.nodes.binded)
final.nodes.binded.df <- final.nodes.binded.df[ , -which(names(final.nodes.binded.df) %in% c("FID", "geometry", "mainCat") ) ]
final.nodes.binded.df <- final.nodes.binded.df[ , sort(names(final.nodes.binded.df)) ]

cor.matrix<-cor(final.nodes.binded.df , use="pairwise.complete.obs")
write.xlsx(cor.matrix, "cor.matrix.xlsx", withFilter =T, rowNames=T )
