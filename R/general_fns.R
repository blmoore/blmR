#' Order one vector to match another
#' 
#' Simply calls \code{order(match(...)}.
#' 
#' @export
#' 
#' @param x see \code{?match}
#' 
#' @examples
#' a <- letters[5:1]
#' b <- letters[1:10]
#' 
#' (a[match_order(a, b)])
#' # [1] "a" "b" "c" "d" "e"
#' 
#' (b[match_order(b, a)])
#' #  [1] "e" "d" "c" "b" "a" "f" "g" "h" "i" "j"
match_order <- function(...) 
  order(match(...))
