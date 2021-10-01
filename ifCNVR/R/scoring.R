#' scoring
#'
#' @param k number of modified targets on the region
#' @param n number of targets on the region
#' @param N number of targets in the panel
#'
#' @return the confidence score
#' @export
#'
#' @examples
#' scoring(10,20,150)
scoring <- function(k, n, N){
  score <- log(1/(((n/N)^k)*(1-n/N)^(n-k)))*(k/n)
  return(score)
}
