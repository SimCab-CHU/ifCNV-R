#' abTargets
#'
#' @param readsMatrix the reads matrix
#' @param abSamples the abSamples list result of the abSamples function
#' @param opt "regular" or "extensive" a parameter
#' @param pred a threshold on the isolation forest outlier prediction (range=[0,1[)
#'
#' @return a list of dataframes of the targets tagged as outliers
#' @import isotree
#' @export
#'
#' @examples
#' abTargets(readsMatrixExample,abSamples(readsMatrixExample))
abTargets <- function(readsMatrix, abSamples, opt='regular', pred = 0.6){

  normReads <- normalizeReads(readsMatrix)
  n.norm <- rowMeans(normReads[,abSamples$normSamples])
  q <- 0
  if (opt=="regular"){
    toTest <- unlist(abSamples$abSamples)
  }  else if (opt=="extensive"){
    toTest <- c(unlist(abSamples$abSamples),unlist(abSamples$normSamples))
  } else {
    q <-  1
  }
  if (q==1){
    stop("opt must be regular or extensive")
  }


  nam <- NULL
  res <- list()
  q <- 0
  for (i in toTest){
    res.amp <- NULL
    tmp = normReads[,i]/n.norm
    iso.f <- isotree::isolation.forest(data.frame(tmp))
    pred.amp <- predict(iso.f, data.frame(tmp))
    res.amp <- readsMatrix[,1][which(pred.amp>pred)]
    if (sum(pred.amp>pred)>0){
      q <- q + 1
      res[[q]] <- res.amp
      nam <- c(nam,i)
    }
  }
  names(res) <- nam

  return(res)
}
