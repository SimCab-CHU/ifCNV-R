#' CreateReadsMatrix
#'
#' @param bamPath a path leading to the .bam and .bai files
#' @param bedFile a path leading to the .bed file
#' @param outputFile (optional) a path leading to a text file
#' @param bedtools the path leading to bedtools
#'
#' @return a reads matrix
#' @export
#'
#' @examples
#'bamPath <- "/Users/admin/Documents/GitHub/ifCNVR/ifCNVR/inst/extdata/"
#'bed <- "/Users/admin/Documents/GitHub/ifCNVR/ifCNVR/inst/bedFile.bed"
#'bedtools <- "/Users/admin/miniconda3/bin/bedtools"
#'readsMatrix <- CreateReadsMatrix(bamPath, bed, bedtools)
#'
CreateReadsMatrix <- function(bamPath, bedFile, bedtools, outputFile='n'){
  bams <- dir(bamPath)
  bed <- fread(bedFile, data.table = F)

  if (sum(grepl(".bam$",bams))==0 & sum(grepl(".cram$",bams))==0){
    stop('bamPath must contain .bam or.cram files')
  }
  if (sum(grepl(".bam$",bams))!=sum(grepl(".bai$",bams))){
    stop('bamPath must contain .bam files and the correspondant index files (.bai)')
  }
  cat("Creating Reads Matrix\n")
  readsMatrix <- system(paste(bedtools, "multicov -bed", bedFile, paste0("-bams ", bamPath, "/", "*.bam")),intern=TRUE)

  bams <- bams[grepl(".bam$",bams)]
  samples <- unique(unlist(strsplit(bams, split = ".bam", fixed=TRUE)))

  tmp <- do.call(rbind,strsplit(readsMatrix, split="\t" ,fixed = TRUE))
  readsMatrix <- data.frame(tmp[,4], tmp[,(ncol(bed)+1):ncol(tmp)])
  colnames(readsMatrix) <- c("targets",samples)
  if (outputFile!='n'){
    fwrite(readsMatrix,outputFile,sep="\t")
  }
  cat("Done\n")
  return(readsMatrix)
}