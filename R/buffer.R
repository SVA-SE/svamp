##' Generate buffers
##'
##' @title buffer__ppn
##' @param ppn A spatial point dataframe
##' @param buffer_size A vector of integers
##' @import rgeos
##' @import sp
##' @return Spatial polygons
##' @author Giampaolo Cocca
buffer_ppn <- function(ppn, buffer_size) {
    do.call(rbind,
            lapply(buffer_size, function(x) {
                buffer <- gBuffer(ppn,
                                  width = x * 1000,
                                  id = as.character(x),
                                  byid = FALSE,
                                  quadsegs = 500)
            })
            )
}
##' Generate a polygon set with holes
##'
##' @title hole
##' @param buffer_polygons
##' @param buffer_size
##' @import sp
##' @import rgeos
##' @return spatial polygon dataframe
##' @author Giampaolo Cocca
hole <- function(buffer_polygons, buffer_size) {

  if (length(buffer_polygons) == 0 | !exists("buffer_polygons")) {
  stop("The buffers object is empty or doesn't exist")
  }

  if (length(buffer_polygons) == 1) {
    buffer_sp <- buffer_polygons[1]
    buffer_sp@polygons[[1]]@ID <- paste0(buffer_size[1])
    df <- data.frame(buffer = buffer_sp@polygons[[1]]@ID,
                                       row.names=buffer_sp@polygons[[1]]@ID)
    buffer_sp <- SpatialPolygonsDataFrame(buffer_sp, df)
    return(buffer_sp)
    }

  if (length(buffer_polygons) > 1) {
    buf_sp1 <- buffer_polygons[1]
    buf_sp1@polygons[[1]]@ID <- paste0(buffer_size[1])
    df <- data.frame(buffer = buf_sp1@polygons[[1]]@ID,
                                       row.names=buf_sp1@polygons[[1]]@ID)
    buf_sp1 <- SpatialPolygonsDataFrame(buf_sp1, df)

    buffers <- do.call("rbind",
                    lapply(rev(2:length(buffer_polygons)), function (x) {
                      a <- gDifference(buffer_polygons[x], buffer_polygons[x-1])
                      a@polygons[[1]]@ID <- buffer_polygons@polygons[[x]]@ID
                      df1 <- data.frame(buffer = a@polygons[[1]]@ID,
                                        row.names = a@polygons[[1]]@ID)
                      spdf <-  SpatialPolygonsDataFrame(a, df1)

                      return(spdf)

                      }))

    buffer_sp <- rbind(buffers, buf_sp1)

    }
}
