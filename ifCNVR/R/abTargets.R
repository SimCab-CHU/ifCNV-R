#' abTargets
#'
#' @param readsMatrix the reads matrix
#' @param abSamples the abSamples list result of the abSamples function
#'
#' @return a list of dataframes of the targets tagged as outliers
#' @export
#'
#' @examples
#' abTargets(0,0)
abTargets <- function(readsMatrix, abSamples){
  return(list(readsMatrix,abSamples))
}
