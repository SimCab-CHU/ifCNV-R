#' calculate Ratio
#'
#' @param readsMatrix the reads matrix
#' @param abSamples the abSamples list result of the abSamples function
#' @param roi the region of interest
#' @param soi the sample of interest
#'
#' @return the ratio associated with the abTargets
#' @export
#'
#' @examples
#' calculateRatio(readsMatrixExample, abSamples(readsMatrixExample), "EGFR-Ex20", "sample_2")
calculateRatio <- function(readsMatrix, abSamples, roi, soi){

  normReads <- normalizeReads(readsMatrix)
  n.norm <- rowMeans(normReads[,abSamples$normSamples])
  names(n.norm) <- rownames(normReads)

  tmp <- normReads[grepl(roi, rownames(normReads)),soi]
  f <- tmp/n.norm[grepl(roi, names(n.norm))]
  res <- mean(f)

  return(res)
}
