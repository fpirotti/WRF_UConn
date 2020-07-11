
cut.Polygons.In.Squares <-
  function(squares.intersecting.polys,
           categ.for.myproj, squares,
           squares.intersecting.polys.ids) {
    
    foreach(i = squares.intersecting.polys.ids,
            .packages = c("sf", "lwgeom"))  %do% {
              
              geomes <- st_make_valid(categ.for.myproj[squares.intersecting.polys[[i]],])
              inters<-st_intersection(squares[i,], geomes)
              
              area<-0  
              areas<-0
              if(length(inters)>0){
                area <- as.numeric(sum(st_area(inters)))
                areas<- as.numeric(st_area(inters))
              } 
              
              inters[["cat_tot_area"]]<-area
              inters[["cat_perc_cov"]]<- area/1e6
              
              aa<-aggregate(areas, by=list(category=inters$categoria), FUN=sum)
              
              
              inters[["mainCat"]]<-as.character(aa$category[[which.max(aa$x)]])
              inters[["mainCat_perc_cov"]]<-max(aa$x)/1e6
            }
  }

cut.Polygons.In.Squares.output <- cut.Polygons.In.Squares(squares.intersecting.polys,
                                                          categ.for.myproj, squares,
                                                          squares.intersecting.polys.ids)
