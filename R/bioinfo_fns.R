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

#' Add a fourth "ID" column to bed-style files
#' 
#' There are three required BED fields: chr, chromStart 
#' and chromEnd. The fourth (optional) column is reserved
#' for name which this function generates as "chr-chromStart".
#' 
#' @export
#' 
#' @param bed_file either a dataframe or filename to be read
#' 
#' @examples
#' bed <- data.frame(chr=c("chr1", "chr2"),
#'   start=c(100, 450), end=c(200, 300))
#'   
#' add_bed_id(bed) 
#'  
add_bed_id <- function(bed_file){
  # work with either filenames or data.frames
  bed <- if(is.character(bed_file))
    read.table(bed_file, header=F, sep="\t")[,1:3] 
  else
    bed_file[,1:3]
  
  # cat together chr-pos for identifier
  bed[,4] <- gsub(" ", "", apply(bed[,1:2], 1, paste0, collapse="-"))
  bed
}
