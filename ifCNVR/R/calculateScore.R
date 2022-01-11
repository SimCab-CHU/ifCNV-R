#' calculate Score
#'
#' @param abTargets a list of dataframes of the targets tagged as outliers result of the abTargets() function
#' @param bedFile a path leading to the .bed file
#' @param roi the region of interest (Gene or Gene-Exon)
#' @param sep a character the separator between roi in the bed file
#' @param column the column of the bed file with the roi
#' @param thrScore (default 0) a threshold on the localization score
#'
#' @return the score associated with the abSamples in the desired roi
#' @export
#'
#' @examples
#' bed <- "/Users/admin/Documents/GitHub/ifCNVR/ifCNVR/inst/bedFile.bed"
#' calculateScore(abTargets(readsMatrixExample,abSamples(readsMatrixExample)), bed)
calculateScore <- function(readsMatrix, abSamples, abTargets, bedFile, roi="Gene", sep="_", column=4, thrScore=0){
  bed <- fread(bedFile, data.table = F,header = T)
  N <- nrow(bed)

  bed.sub <- bed[,column]

  All <- do.call(rbind, strsplit(bed.sub,split = sep))

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

  ratio <- NULL
  for (i in seq(1,nrow(f))){
    ratio <- c(ratio, calculateRatio(readsMatrix, abSamples, roi = f[i,2], soi = f[i,1]))
  }

  res <- data.frame(f,ratio)
  colnames(res) <- c("samples","RoI","Score","Ratio")
  res <- res[as.numeric(res$Score)>thrScore,]

  return(res)
}
