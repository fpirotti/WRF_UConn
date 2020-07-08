library(sp)
library(parallel)
library(foreach)

## rot in radians!!!
createSquares<-function(centroids, radius, rot=0){
  
  yPlus <- centroids$y+radius
  xPlus <- centroids$x+radius
  yMinus <- centroids$y-radius
  xMinus <- centroids$x-radius
  
  if(rot!=0){
    yPlus<-xPlus*sin(rot)  -  yPlus*cos(rot)
    xPlus<-xPlus*cos(rot) - yPlus*sin(rot)
    print("rotation")
    yMinus<-xMinus*sin(rot)  -  yMinus*cos(rot)
    xMinus<-xMinus*cos(rot)  -  yMinus*sin(rot)  
  }

  
   sq<-cbind(xMinus,yPlus,  # NW corner
               xPlus, yPlus,  # NE corner
               xPlus,yMinus,  # SE corner
               xMinus,yMinus, # SW corner
               xMinus,yPlus)  # NW corner again - close ploygon
 
   split(sq, seq(nrow(sq)))
}


processSquares<-function(squares){ 
    
  squares.Polygons<-foreach(square = squares, ID=names(squares)) %do%  
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
 
  processSquares(createSquares(points, radius, rot=0))
}
 
