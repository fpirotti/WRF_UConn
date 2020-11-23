st_bbox_by_feature = function(x) {
 # xg = st_geometry(x)
  f <- function(y){ 
   SS <- st_as_sfc(st_bbox(y$geometry,), crs=myproj)
   SS$FID <- y$FID
   SS
  }
  do.call("c", apply(x, 1, f))
}


squares <-st_bbox_by_feature(nodes.buffered)