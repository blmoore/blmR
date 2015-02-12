#' Write data.frame in BED format
#' 
#' Sets options to `write.table` useful for
#' writing BED files.
#' 
#' @export
#' @param df data.frame (typically 4+ column)
#' @param fname filename to save BED file
#' 
#' @examples
#' write_bed(data.frame(chr=c("chr1", "chr2"),
#'   start=c(100, 450), end=c(200, 300), id=c("p1", "p2")),
#'   "outfile.bed")
#'   
write_bed <- function(df, fname)
  write.table(df, fname, quote=F, row.names=F,
              col.names=F, sep="\t")

#' Alias to source remote bioconductor script
#' 
#' Remembering/typing this URL is a pain.
#' 
#' @export
#' 
#' @examples
#' bioconductor()
#' biocLite()
#'   
bioconductor <- function()
  source("http://bioconductor.org/biocLite.R")