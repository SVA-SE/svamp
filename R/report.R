##' Create a SVAMP report
##'
##' @title report
##' @param a ppn
##' @return path to temporary file
##' @import markdown
##' @import knitr
##' @export

report <- function(ppn,
                   template,
                   format = c("knitr")){

    ## Check to make sure the environment is empty
    if (length(ls(envir=.svamp_env))) {
        stop('Unable to create report. The report object already exists')
    }
    ## Clean up the environment upon exiting the function
    on.exit(rm(list=ls(envir=.svamp_env), envir=.svamp_env))

    ## Check arguments
    if(missing(ppn))
        stop("Missing 'ppn'")
    if(missing(template))
        stop("Missing 'template'")

    ## Add the ppn argument to the .svamp_env so it can be accessed inside the .Rmd
    assign("ppn", ppn, envir = .svamp_env)

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
