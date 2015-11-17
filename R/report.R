#' Draw a map
#'
#' Draw a map of outbreak points over the Län of Sweden.
#' @param outbreak Scalar or numeric vector with the PPNs number to draw
#' @return A .jpeg? map
#' @keywords internal
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
  
#' Produce an .html report
#'
#' Produce an .html report with information for outbreak/suspicion management
#' @param ppn Ppn numbers (vector of int numbers, comma separeted)
#' @param ppn_obj Path to the list of objects output from svdc package
#' @param firstname Firstname of the person running the report
#' @param lastname Lastname of the person running the report
#' @param X X-coordinate of the outbreak provided by the user when ppn coordinates are missing
#' @param Y Y-coordinate of the outbreak provided by the user when ppn coordinates are missing
#' @param buffer_size Size in kilometers of the buffers drawn around the ppn (vector of numbers, comma separeted)
#' @return An html report
#' @import rmarkdown
#' @import leaflet
#' @import RODBC
#' @import knitr
#' @import EpiContactTrace
#' @import wordcloud
#' @import DT
#' @import sp
#' @import rgeos
#' @import maptools
#' @export

report <- function(ppn = 94403,
                   ppn_obj = system.file("extdata/result.rda", package = "svamp"), #save inUBUNTU the result from SVDC
                   firstname = "",
                   lastname = "",
                   buffer_size = 1,
#                    X = 1491350,
#                    Y = 7160041,
                   template = "report",
                   format = c("knitr")) {

    
   ## Check to make sure the environment is empty

  if (length(ls(envir=.svamp_env))) {
    stop('Unable to create report. The report object already exists')
  }

  ## Clean up the environment upon exiting the function

  on.exit(rm(list=ls(envir=.svamp_env), envir=.svamp_env))

#   if(missing(firstname))
#     stop("Missing 'firstname'")
# 
#   if(missing(lastname))
#     stop("Missing 'lastname'")
# 
#   if(missing(template))
#     stop("Missing 'template'")

  # connection via ODBC and query urax data, then close the connection

#   connect <- odbcConnect("SJUKDOMSSTATUSV",
#                          uid = "Svaladw",
#                          pwd = "svaladwpw",
#                          believeNRows=FALSE)
# 
#   urax <- sqlQuery(connect, query = " SELECT *
#                                     FROM
#                                     URAX.SJUKDOMSSTATUSV")
# 
#   odbcClose(connect)


  ## Load the output of svsc package (load a list called "result" output of svdc package)
  load(ppn_obj)
  
  ## Check arguments
  if(missing(ppn)) {
    stop("'ppn' is missing")
  }

  ## Check that the inputed ppn is numeric
  if(!is.numeric(ppn)) {
    stop("Only numeric value are admitted")
  }
  
  ## Check that the inputed ppns are present
  if (!all(ppn %in% result$PPN$Ppn)) {
    stop('One or more PPNs are not present in the database.
         Please, double check the imputed PPNs')
  }
  
  ## Load spatialpolygondataframe sticked in the ../svamp/data folder

  data(NUTS_03M, package = "svamp", envir = .svamp_env)
  data(postnummer, package = "svamp", envir = .svamp_env)

  ## Add the ppn argument to the .svamp_env so it can be accessed inside the .Rmd

  assign("ppn", ppn, envir = .svamp_env)
  assign("result", result, envir = .svamp_env)
  assign("firstname", firstname, envir = .svamp_env)
  assign("lastname", lastname, envir = .svamp_env)
#   assign("X", X, envir = .svamp_env)
#   assign("Y", Y, envir = .svamp_env)
  assign("buffer_size", buffer_size, envir = .svamp_env)


  template <- system.file(file.path(format, paste0(template, ".Rmd")), package = "svamp")

  td <- tempdir()

  outputfile_html <- rmarkdown::render(template, output_dir = td)

#  return(readLines(outputfile_html))


  a <- normalizePath(file.path(outputfile_html), winslash = "/")
  return(a)

}

.svamp_env <- new.env()

##' ReportObject
##'
##' @return The current object when generating a report
##' @export

report_data_object <- function() {
  .svamp_env
}


