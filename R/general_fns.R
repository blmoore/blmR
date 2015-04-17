#' Order one vector to match another
#' 
#' There are three required BED fields: chr, chromStart 
#' and chromEnd. The fourth (optional) column is reserved
#' for name which this function generates as "chr-chromStart".
#' 
#' @export
#' 
#' @param x see `?match`
#' 
#' @examples
#' a <- 1:5
#' b <- 5:1
#'   
#' (match_order(a, b))
#' # [1] 5 4 3 2 1
match_order <- function(...) 
  order(match(...))
