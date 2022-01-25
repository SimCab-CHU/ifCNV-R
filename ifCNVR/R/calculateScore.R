#' calculate Score
#'
#' @param readsMatrix a reads matrix with samples in columns and targets in lines (the first column are the targets)
#' @param abSamples the abSamples list result of the abSamples function
#' @param abTargets a list of dataframes of the targets tagged as outliers result of the abTargets() function
#' @param roi the region of interest (Gene or Gene-Exon)
#' @param sep a character the separator between roi in the bed file
#' @param thrScore (default 0) a threshold on the localization score
#'
#' @return the score associated with the abSamples in the desired roi
#' @export
#'
#' @examples
#' abS <- abSamples(readsMatrixExample)
#' abT <- abTargets(readsMatrixExample,abSamples(readsMatrixExample))
#' calculateScore(readsMatrixExample, abS, abT, sep="-")
calculateScore <- function(readsMatrix, abSamples, abTargets, roi="Gene", sep="-", thrScore=7){
  N <- nrow(readsMatrix)

  bed.sub <- readsMatrix[,1]

  All <- do.call(rbind, strsplit(bed.sub,split = sep))

  if (sum(grepl(sep,bed.sub))==0){
    stop("The `sep` parameter must be present in the forth column of your bed file (default: sep=\"-\")")
  }

  genes <- All[,1]
  exons <- All[,2]

  f <- NULL
  if (roi=="Gene"){
    temp <- genes
    for (i in names(abTargets)){
      tmp <- abTargets[[i]]
      for (j in unique(temp)){
        k <- sum(grepl(j, tmp))
        n <- sum(grepl(j, temp))
        score <- scoring(k, n, N)
        f <- rbind(f, c(i, j, as.numeric(score)))
      }
    }
  }

  if (roi=="Gene-Exon"){
    temp <- paste(genes,exons,sep = sep)
    for (i in names(abTargets)){
      tmp <- abTargets[[i]]
      for (j in unique(temp)){
        k <- sum(grepl(j, tmp))
        n <- sum(grepl(j, temp))
        score <- scoring(k, n, N)
        f <- rbind(f, c(i, j, as.numeric(score)))
      }
    }
  }

  if (!is.null(f)){
    ratio <- NULL
    for (i in seq(1,nrow(f))){
      ratio <- c(ratio, calculateRatio(readsMatrix, abSamples, roi = f[i,2], soi = f[i,1]))
    }

    if (nrow(f)==1){
      res <- data.frame(f[1],f[2],as.numeric(ratio),as.numeric(f[3]))
      print(res)
    } else {
      res <- data.frame(f[,1:2],as.numeric(ratio),as.numeric(f[,3]))
    }

    colnames(res) <- c("samples","RoI","Ratio","Score")
    res$Ratio <- round(res$Ratio,2)
    res$Score <- round(res$Score,2)
    res <- res[as.numeric(res$Score)>thrScore,]

    return(res)
  } else {
    return(NULL)
  }

}
