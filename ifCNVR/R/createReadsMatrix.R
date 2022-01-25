#' CreateReadsMatrix
#'
#' @param bamPath a path leading to the .bam and .bai files
#' @param bedFile a path leading to the .bed file (Warning replace chrX by X in the position)
#' @param outputFile (optional) a path leading to a text file
#' @param bedtoolsPath the path leading to bedtools
#' @param verbose a boolean
#'
#' @return a reads matrix
#' @export
#'
#' @examples
#'bamPath <- system.file("extdata/",package = "ifCNVR")
#'bed <- system.file("bedFile.bed",package = "ifCNVR")
#'bedtools <- 'n'
#'readsMatrix <- CreateReadsMatrix(bamPath, bed, bedtools)
#'
CreateReadsMatrix <- function(bamPath, bedFile, bedtoolsPath, outputFile='n', verbose=TRUE){

  bams <- dir(bamPath)
  bed <- fread(bedFile, data.table = FALSE, header = FALSE)

  if (sum(grepl(".bam$",bams))==0 & sum(grepl(".cram$",bams))==0){
    stop('bamPath must contain .bam or.cram files')
  }

  if (sum(grepl(".bam$",bams))!=sum(grepl(".bai$",bams))){
    stop('bamPath must contain .bam files and the correspondant index files (.bai)')
  }
  if (verbose){
    cat("Creating Reads Matrix\n")
  }

  if (bedtoolsPath!='n'){
    if (grepl("bedtools", bedtoolsPath)){
      readsMatrix <- system(paste(bedtoolsPath, "multicov -bed", bedFile, paste0("-bams ", bamPath, "/", "*.bam")),intern=TRUE)
      tmp <- do.call(rbind,strsplit(readsMatrix, split="\t" ,fixed = TRUE))
      readsMatrix <- data.frame(tmp[,4], tmp[,(ncol(bed)+1):ncol(tmp)])
      bams <- bams[grepl(".bam$",bams)]
      samples <- unique(unlist(strsplit(bams, split = ".bam", fixed=TRUE)))
      colnames(readsMatrix) <- c("targets",samples)
    } else {
      stop('bedtoolsPath must be the path to bedtools (after installing bedtools, type which bedtools in your console)')
    }
  } else {
    readsMatrix <- NULL
  }

  if (outputFile!='n'){
    fwrite(readsMatrix,outputFile,sep="\t")
  }
  if (verbose){
    cat("Done\n")
  }

  return(readsMatrix)
}
