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

#' Circular permutation of genomic features in a BED file
#' 
#' Generate a permuted version of an input BED file (object),
#' probably for use as a null model for comparison with some
#' statistical result.
#' 
#' @export
#' 
#' @param chr chromosome reference, matching your BED file (i.e. 
#' this may be "chr1" or "1")
#' @param bed a BED format \code{data.frame}, such as that 
#' produced after using \code{read.table} on a BED file
#' @param chr.sizes a \code{data.frame} of chromsoome sizes,
#' i.e. \code{mm9.chrom.sizes.txt}
#' @param step.size if your features are discretised (e.g. can only 
#' occur at set intervals, like those introduced by binning) they
#' can be permuted by setting \code{step.size} to the size of your
#' bins in basepairs. \code{step.size} of 1 (the default) means no 
#' binning.
#' 
#' @examples
#' 
#' sizes <- read.table("http://igv.googlecode.com/svn-history/r2493/trunk/src/org/broad/igv/hic/tools/mm9.chrom.sizes")
#' 
#' bed_file <- data.frame(chr=c("chr1", "chr2"),
#'   start=c(100, 450), end=c(200, 300))
#'  
#' set.seed(42)
#'    
#' (permute_chr("chr1", bed_file, sizes))
#' #    chr     start       end
#' # 1 chr1 180395673 180395773
#' 
#' # run for all chromosomes
#' chrs <- as.character(unique(bed$chr))
#' perm <- sapply(chrs, permute_chr, bed=bed_file, 
#'                chr.sizes=sizes, simplify=F)
#' (do.call(rbind, perm))
#' #        chr     start       end
#' #  chr1 chr1 184787091 184787191
#' #  chr2 chr2  52005764  52005614
#' 
permute_chr <- function(chr, bed, chr.sizes, step.size=1){
  # get relevant chromosome
  outbed <- bed[bed[,1] == chr,]
  
  # get chromosome size (e.g. from mm9.chrom.sizes.txt)
  max <- chr.sizes[chr.sizes[,1] == chr, 2]
  avail_steps <- floor(max / step.size)
  
  # pick shift:
  shift <- sample(1:avail_steps, 1)
  
  for(i in 1:nrow(outbed)){
    # start
    outbed[i,2] <- (outbed[i,2] + shift*step.size) %% max
    # end
    outbed[i,3] <- (outbed[i,3] + shift*step.size) %% max
  }
  
  outbed 
}


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