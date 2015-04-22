#' Darken an R colour by a specified proportion
#' 
#' Supply a colour name or RGB and a proportion
#' between 0 and 1.
#' 
#' @export
#' 
#' @param by proportion to darken colour (default \code{.05} for 5 percent)
#' 
#' @examples
#' cols <- sapply(seq(0, .5, by=.1), function(i) darken_col("red", i))
#' image(as.matrix(1:5), col=cols)
darken_col <- function(colname, by=.05){
  rgb_vals <- col2rgb(colname) * (1-by)
  rgb(t(rgb_vals), max=255)
}

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

#' Converts a string to Title Case
#' 
#' Converts a string of any case to Title Case; that is, capitalise
#' the first letter of each word and lower case the rest. Currently
#' does not skip words like or, of etc. Edited from an R manual.
#' 
#' @export
#' 
#' @param x string
#' 
#' @examples
#' title_case("new York, nEW YorK!")
#' # "New York, New York!"
title_case <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), tolower(substring(s, 2)),
        sep = "", collapse = " ")
}


