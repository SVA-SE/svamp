##' Calculate the distance between farms
##'
##' @title DistMatrix
##' @param farms_in_buffers
##' @param longlat
##' @param outbreak
##' @import sp
##' @return Matrix
##' @author Giampaolo Cocca
DistMatrix = function(farms_in_buffers, longlat = FALSE, outbreak) {
  distance <- sapply(1:length(farms_in_buffers[[1]]),
    function(x) {spDistsN1(coordinates(outbreak),
                           coordinates(farms_in_buffers[x,]),
                           longlat)})
  return(distance)
}
