#' generateReport
#'
#' @param outputFile a path to the html output file
#' @param resTable the table result of the CalculateScore() function
#'
#' @import rmarkdown
#'
#' @return a html report
#' @export
#'
#' @examples
#'generateReport()
generateReport <- function(outputFile='n', readsMtrix, resTable, CNVpos){
  if (outputFile!='n'){
    template <- system.file("template.Rmd", package = "ifCNVR")
    render(template, output_file = outputFile)
  } else {
    cat("An output file path is needed.")
  }
}
