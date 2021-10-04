#' calculate Ratio
#'
#' @param abTargets a list of dataframes of the targets tagged as outliers result of the abTargets() function
#' @param readsMatrix the reads matrix
#' @param abSamples the abSamples list result of the abSamples function
#'
#' @return the ratio associated with the abTargets
#' @export
#'
#' @examples
#' calculateRatio(abTargets(readsMatrixExample,abSamples(readsMatrixExample)), readsMatrixExample,
#' abSamples(readsMatrixExample))
calculateRatio <- function(abTargets, readsMatrix, abSamples){

  normReads <- normalizeReads(readsMatrix)
  n.norm <- rowMeans(normReads[,abSamples$normSamples])
  names(n.norm) <- rownames(readsMatrix)
  for (i in names(abTargets)){
    tar <- abTargets[[i]]
    for (j in tar){
      ratio <- normReads[j,i]/n.norm[j]
      print(ratio)
    }
  }



  resturn(res)
}
