
cut.Polygons.In.Squares <-
  function(squares.intersecting.polys,
           squares.intersecting.polys.dmg,
           categ.for.myproj, damages.for.myproj, squares,
           squares.intersecting.eitherOr.IDS) {
    
    foreach(i = squares.intersecting.eitherOr.IDS,
            .packages = c("sf", "lwgeom"))  %do% {
              
              if(length(squares.intersecting.polys[[i]]) > 0){
                
                geomes <-     st_make_valid(categ.for.myproj[squares.intersecting.polys[[i]],]) 
                inters<-st_intersection(geomes, squares[i,])
                
                area<-0  
                areas<-0
                if(length(inters)>0){
                  areas<- as.numeric(st_area(inters))
                  area <- sum(areas) 
                }  
                mat3[i,2]<- area/1e6
                
                aa<-aggregate(areas, by=list(category=inters$categoria), FUN=sum) 
                
                mat3[i,3]<-as.integer(aa$category[[which.max(aa$x)]])
                mat3[i,4]<-max(aa$x)/1e6 
                
              }  
              
              
              if( length(squares.intersecting.polys.dmg[[i]]) > 0 ){ 
                geomes.dmg <- st_make_valid(damages.for.myproj[squares.intersecting.polys.dmg[[i]],]) 
                inters<-st_intersection(geomes.dmg, squares[i,])
                
                area<-0   
                if(length(inters)>0){ 
                  area <- sum(as.numeric(st_area(inters))) 
                  warea <- sum(as.numeric(st_area(inters)*inters$Damage_deg)) 
                }  
                if(nrow(inters)>1){ 
                  browser()
                }
                mat3[i,5]<- area/1e6
                mat3[i,6]<- warea/1e6 
                
              }
              
              NULL
            }
  }

options(warn = -1)

cut.Polygons.In.Squares.output <- cut.Polygons.In.Squares(squares.intersecting.polys,
                                                          squares.intersecting.polys.dmg,
                                                          categ.for.myproj, damages.for.myproj, squares,
                                                          squares.intersecting.eitherOr.IDS[1:20])


options(warn = 0)