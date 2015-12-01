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
#' @param ppn_sympt PPN with symptoms
#' @param days Set the number of days to use in EpiContactTrace (max 180)
#' @param view Make TRUE to pop a browser
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

report <- function(ppn,
                   ppn_sympt = "",
                   ppn_obj = system.file("extdata/result.rda", package = "svamp"), #save inUBUNTU the result from SVDC
                   firstname = "",
                   lastname = "",
                   buffer_size = c(3, 10),
                   days = 90,
#                    X = 1491350,
#                    Y = 7160041,
                   template = "report",
                   format = c("knitr"),
                   view = FALSE) {


   ## Check to make sure the environment is empty

  if (length(ls(envir=.svamp_env))) {
    stop('Unable to create report. The report object already exists')
  }

  ## Clean up the environment upon exiting the function

  on.exit(rm(list=ls(envir=.svamp_env), envir=.svamp_env))

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

  ## Check that the inputed ppn is numeric
  if(!is.numeric(buffer_size)) {
    stop("Only numeric value are admitted")
  }
  ## Check that the inputed ppns are present
  if (any(buffer_size > 50)) {
    stop('The maximum radius for the buffer is 50km')
  }

  ## Check that the inputed ppn is numeric
  if(!is.numeric(days)) {
    stop("Only numeric value are admitted")
  }
  ## Check that the inputed ppns are present
  if (days > 180) {
    stop('The maximum number of days is 180. To go more back in time
         use the Movements App')
  }

  ## Load spatialpolygondataframe sticked in the ../svamp/data folder

  data(NUTS_03M, package = "svamp", envir = .svamp_env)
  data(postnummer, package = "svamp", envir = .svamp_env)

  ## Add the ppn argument to the .svamp_env so it can be accessed inside the .Rmd

  assign("ppn", ppn, envir = .svamp_env)
  assign("result", result, envir = .svamp_env)
  assign("firstname", firstname, envir = .svamp_env)
  assign("lastname", lastname, envir = .svamp_env)
  assign("days", days, envir = .svamp_env)
  assign("ppn_sympt", ppn_sympt, envir = .svamp_env)
#   assign("X", X, envir = .svamp_env)
#   assign("Y", Y, envir = .svamp_env)
  assign("buffer_size", buffer_size, envir = .svamp_env)


  template <- system.file(file.path(format, paste0(template, ".Rmd")), package = "svamp")

  td <- tempdir()

  outputfile_html <- rmarkdown::render(template, output_dir = td)

  if(view) {
      a <- normalizePath(file.path(outputfile_html), winslash = "/")
      utils::browseURL(a)
  }
  if(!(view)) {
      invisible(readLines(outputfile_html))
  }
}

.svamp_env <- new.env()

##' ReportObject
##'
##' @return The current object when generating a report
##' @export

report_data_object <- function() {
  .svamp_env
}


