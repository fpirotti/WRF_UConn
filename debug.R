f <- function(x) {
  st_polygon( st_as_sfc(cst_bbox(
    st_buffer(x$geometry, 500, nQuadSegs = 1 )
  ) ) )
}

normal_apply <-bench::mark( squares<-apply(nodes.myproj, 1,  f) )