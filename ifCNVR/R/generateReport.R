#' generateReport
#'
#' @param outputFile a path to the html output file
#'
#' @import rmarkdown
#'
#' @return a html report
#' @export
#'
#' @examples
#'generateReport()
generateReport <- function(outputFile='n', resTable){
  if (outputFile!='n'){
    render("inst/template.Rmd",output_file = outputFile)
  } else {
    cat("An output file path is needed.")
  }
}
