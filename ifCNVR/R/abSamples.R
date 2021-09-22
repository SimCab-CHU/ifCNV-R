#' abSamples
#'
#' @param readsMatrix a matrix of the number of reads per target
#' @param conta a parameter for the isotree function
#'
#' @return the aberant and normal samples
#' @export
#' @import data.table
#' @import isotree
#' @import stats
#'
#' @examples
#' readsMatrix = matrix(0,nrow=50,ncol=10)
#' abSamples(readsMatrix)
#'
abSamples <- function(readsMatrix, conta="auto",q=0.99){
  #data <- data.table::fread(readsMatrix,data.table = F)
  data <- readsMatrix

  qq.99 <- apply(data,2,function(x) quantile(x,q))
  qq.01 <- apply(data,2,function(x) quantile(x,1-q))
  m <- apply(data,2,mean)

  iso.f <- isolation.forest(data.frame(qq.99/m))
  pred.amp <- predict(iso.f, data.frame(qq.99/m))
  plot(pred.amp)

  iso.f <- isolation.forest(data.frame(qq.01/m))
  pred.del <- predict(iso.f, data.frame(qq.01/m))
  plot(pred.del)

  names(pred.amp) <- names(pred.del) <- colnames(data)

  if (conta=='None'){
    res.amp <- colnames(data)[which(pred.amp==max(pred.amp))]
    res.del <- colnames(data)[which(pred.del==max(pred.del))]
  }
  if (conta=='auto'){
    res.amp <- colnames(data)[which(pred.amp>(mean(pred.amp)+sd(pred.amp)))]
    res.del <- colnames(data)[which(pred.amp>(mean(pred.del)+sd(pred.del)))]
  }
  if (is.numeric(conta)){
    n = round(dim(data)[2]*conta)
    if (n==0){
      n=1
    }
    tmp = sort(pred.amp,decreasing = T)
    res.amp <- names(tmp[1:n])
    tmp = sort(pred.del,decreasing = T)
    res.del <- names(tmp[1:n])
  }

  abSamples <- unique(res.amp,res.del)
  normSamples <- colnames(data)[!colnames(data)%in%abSamples]
  res = list()
  res[[1]] <- abSamples
  res[[2]] <- normSamples

  names(res) = c("abSamples","normSamples")
  return(res)
}
