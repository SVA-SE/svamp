##' Draw a map of PPNs over Sweden
##'
##' @description Draw a map of outbreak points over the Län of Sweden.
##' @param outbreak Scalar or numeric vector with the PPNs number to draw
##' @param ppn_obj The list of dataset output from package SVDC
##' @return A .png map
##' @author Giampaolo Cocca
draw_map <- function(ppn,
                     ppn_obj = system.file("extdata/result.rda",
                                           package = "svamp")) {
  
  data(NUTS_03M, package = "svamp", envir = .svamp_env)
  NUTS_03M <- report_data_object()$NUTS_03M
  
  # load results from svdc package
  load(ppn_obj)
  farms_RT90 <- result$farms_RT90
  nuts_label <- result$nuts_label
  
  outbreak <- subset(farms_RT90, farms_RT90$Ppn == ppn)
  NUTS_03M <- NUTS_03M[NUTS_03M$STAT_LEVL_ == 3, 1:2]
  
  # drop levels without data in NUTS_ID
  NUTS_03M@data$NUTS_ID <- droplevels(NUTS_03M@data$NUTS_ID)
  
  NUTS_03M@data$Lan <- nuts_label$Swedish[match(as.character(NUTS_03M@data$NUTS_ID),
                                                nuts_label$NUTS)]
  
  # Check for points falling outside Swedish border
  if(all(!is.na(over(outbreak, NUTS_03M)))) {
    
    lan <- over(outbreak, NUTS_03M)
    lan <- NUTS_03M[NUTS_03M@data$NUTS_ID %in% lan$NUTS_ID, ]
    plot(NUTS_03M, axes = T)
    plot(lan, add = T, col = "yellow")
    
    
    points(outbreak, pch = 19, col = "red", cex = 1)
    
    # Add labels to the polygons
    
    invisible(text(coordinates(NUTS_03M),
                   labels = as.character(NUTS_03M@data$Lan),
                   cex = 0.8))
    
    title(expression(paste("Geographic location of the PPN under investigation")))
    
    legend("bottomright",
           cex = 1.3,
           "PPN location",
           pch = 19,
           pt.cex = 1.2,
           col = "red",
           bty = "n")
    
  } else {
    
    plot(1,1,col = "white")
    text(1,1,"Some coordinates fall outside the Swedish borders")
  }
}
