#' abTargets
#'
#' @param readsMatrix the reads matrix
#' @param abSamples the abSamples list result of the abSamples function
#' @param opt "regular" or "extensive" a parameter
#'
#' @return a list of dataframes of the targets tagged as outliers
#' @export
#'
#' @examples
#' abTargets(readsMatrixExample,abSamples(readsMatrixExample))
abTargets <- function(readsMatrix, abSamples, opt='regular'){
  #rownames(readsMatrix)
  normReads <- normalizeReads(readsMatrix)
  n.norm <- rowMeans(normReads[,abSamples$normSamples])

  if (opt=="regular"){
    toTest <- unlist(abSamples$abSamples)
  }

  if (opt=="extensive"){
    toTest <- c(unlist(abSamples$abSamples),unlist(abSamples$normSamples))
  }

  nam <- NULL
  res <- list()
  q <- 0
  for (i in toTest){
    res.amp <- NULL
    tmp = normReads[,i]/n.norm
    iso.f <- isolation.forest(data.frame(tmp))
    pred.amp <- predict(iso.f, data.frame(tmp))
    res.amp <- readsMatrix[,1][which(pred.amp>0.6)]
    if (sum(pred.amp>0.6)>0){
      q <- q + 1
      res[[q]] <- res.amp
      nam <- c(nam,i)
    }
  }
  names(res) <- nam

  return(res)
}
