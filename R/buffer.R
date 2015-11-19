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
