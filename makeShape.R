library(sp) 
library(doParallel)
library(foreach)

## rot in radians!!!
createSquares<-function(centroids, radius, rot=0){
  
  rotate<-function(x,y,rot){ c( x*cos(rot) - y*sin(rot),x*sin(rot)+y*cos(rot)) }
  

  if(rot!=0){
    
    NE.x <-  radius 
    NE.y <-  radius 
    NW.x <-  -radius
    NW.y <-  radius 
    SE.x <-  radius
    SE.y <-  -radius 
    SW.x <-  -radius
    SW.y <-  -radius 
    
    
    NW<-rotate( NW.x, NW.y, rot)
    NE<-rotate( NE.x, NE.y, rot)
    SE<-rotate( SE.x, SE.y, rot)
    SW<-rotate( SW.x, SW.y, rot)
     
    
    sq<-cbind( centroids$x+ NW[[1]], centroids$y+NW[[2]] ,  # NW corner
               centroids$x+ NE[[1]], centroids$y+NE[[2]] ,  # NE corner
               centroids$x+ SE[[1]], centroids$y+SE[[2]] ,
               centroids$x+ SW[[1]], centroids$y+SW[[2]] ,
               centroids$x+ NW[[1]], centroids$y+NW[[2]] 
              )  # NW corner again - close ploygon
    
  } else {
    
    yPlus <- centroids$y+radius
    xPlus <- centroids$x+radius
    yMinus <- centroids$y-radius
    xMinus <- centroids$x-radius
    sq<-cbind(xMinus,yPlus,  # NW corner
              xPlus, yPlus,  # NE corner
              xPlus,yMinus,  # SE corner
              xMinus,yMinus, # SW corner
              xMinus,yPlus)  # NW corner again - close ploygon
    
  }

  

 
   matrix(sq, ncol=2, byrow=TRUE)
   
   split(sq, seq(nrow(sq)))
}


processSquares<-function(squares){ 
  cl <- makeCluster(12)
  registerDoParallel(cl)
  squares.Polygons<-foreach(square = squares, ID=names(squares), .packages = "sp" )  %dopar%   
    Polygons(list(Polygon(matrix(square, ncol=2, byrow=TRUE))), ID) 
  
  SpatialPolygons(squares.Polygons )
  
}


processPoints<-function(points, radius=NULL, rot=0, crs=NULL){
  if(is.null(radius)){
    stop("Insert radius")
  }
  if( is.matrix(points) || is.data.frame(points)  ){
    if(ncol(points) !=2) stop("2 column data frame or matrix is expected")
    if(sum(names(points)%in%c("y", "x"))!=2 ) stop("Column names have to be x and y!")
  }
 
  processSquares(createSquares(points, radius, rot=rot))
}
 
