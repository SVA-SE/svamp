#' Produce an .html report 
#'
#' Produce an .html report with information for outbreak/suspicion management
#' @param ppn ppn number (single scalar value)
#' @param ppn_obj Path to the list of objects output from svdc package
#' @param firstname Firstname of the person running the report
#' @param lastname Lastname of the person running the report
#' @param X X-coordinate of the outbreak provided by the user when ppn coordinates are missing
#' @param Y Y-coordinate of the outbreak provided by the user when ppn coordinates are missing
#' @param buf1 Size in meters of the first buffer drawn around the ppn
#' @param buf2 Size in meters of the second buffer drawn around the ppn
#' @param buf3 Size in meters of the third buffer drawn around the ppn
#' @return An html report
#' @import markdown
#' @import knitr
#' @import RODBC
#' @export

report <- function(ppn = 222240,
#                    ppn_obj =  "C:/svamp/svamp/data/result.rda",
                   firstname = "Giampaolo",
                   lastname = "Cocca",
#                    X = 1491350,
#                    Y = 7160041,
#                    buf1 = 1000,
#                    buf2 = 3000,
#                    buf3 = 10000,
                   template,
                   format = c("knitr")) {

  ## Check to make sure the environment is empty
  if (length(ls(envir=.svamp_env))) {
    stop('Unable to create report. The report object already exists')
  }
  ## Clean up the environment upon exiting the function
  on.exit(rm(list=ls(envir=.svamp_env), envir=.svamp_env))
  
  ## Check arguments
  if(missing(ppn))
    stop("Missing 'ppn'")
  
  if(missing(firstname))
    stop("Missing 'firstname'")
  
  if(missing(lastname))
    stop("Missing 'lastname'")
  
  if(missing(template))
    stop("Missing 'template'")
  
  ## connection to urax data via ODBC
  
#   connect <- odbcConnect("SJUKDOMSSTATUSV", 
#   uid = "Svaladw", 
#   pwd = "svaladwpw", 
#   believeNRows=FALSE)
#   
#   urax <- sqlQuery(connect, query = " SELECT *
#                                     FROM
#                                     URAX.SJUKDOMSSTATUSV"
# ) 

  ## Load the output of svsc package
  
#   load(ppn_obj)
    
  ## Add the ppn argument to the .svamp_env so it can be accessed inside the .Rmd
  assign("ppn", ppn, envir = .svamp_env)
#   assign("ppn_obj", ppn_obj, envir = .svamp_env)
  assign("firstname", firstname, envir = .svamp_env)
  assign("lastname", lastname, envir = .svamp_env)
#   assign("X", X, envir = .svamp_env)
#   assign("Y", Y, envir = .svamp_env)
#   assign("buf1", buf1, envir = .svamp_env)
#   assign("buf2", buf2, envir = .svamp_env)
#   assign("buf3", buf3, envir = .svamp_env)
  
  template <- system.file(file.path(format, paste0(template, ".Rmd")), package = "svamp")
  
  outputfile_md <- tempfile(fileext = ".md")
  outputfile_html <- tempfile(fileext = ".html")
  knit(template, outputfile_md)
  
  markdownToHTML(file = outputfile_md,
                 output = outputfile_html)
  
  
  return(readLines(outputfile_html))

}

.svamp_env <- new.env()

##' ReportObject
##'
##' @return The current object when generating a report
##' @export
report_data_object <- function() {
  .svamp_env
}