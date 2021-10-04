#' normalizeReads
#'
#' @param readsMatrix a reads matrix with samples in columns and targets in lines (the first column are the targets)
#'
#' @return a normalized reads matrix
#' @export
#'
#' @examples
#' normReads <- normalizeReads(readsMatrixExample)
normalizeReads <- function(readsMatrix){
  targets <- readsMatrix[,1]
  tmp <- apply(readsMatrix[,-1], 2, function(x) as.numeric(x)/median(as.numeric(x)))
  normReads <- data.frame(tmp)
  rownames(normReads) <- targets
  return(normReads)
}
