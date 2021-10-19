#' generateReport
#'
#' @param outputFile
#'
#' @importFrom rmarkdown render
#' @return
#' @export
#'
#' @examples
#'generateReport()
generateReport <- function(outputFile='n'){
  if (outputFile!='n'){
    render("inst/template.Rmd",output_file = outputFile)
  }
}
