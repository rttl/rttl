#' @title
#' turn a factor with number levels into the numbers themselves
#' @description
#' Make a factor whose levels are numbers (e.g. SOA) into a vector
#' that contains these numbers not as factor levels, but as numerics.
#' @param x the factor to be converted
#' @return numeric vector
#' @examples
#' a <- factor( c( 1, 2, 1, 1, 1, 4, 7, 8 ) )
#' as.numeric( a )
#' as.numeric.factor( a )
#' a <- data.frame( col1 = c( 1, 2, 3 ), col2 = c( 3, 4, 5 ) )
#' @author http://stackoverflow.com/questions/3418128/how-to-convert-a-factor-to-an-integer-numeric-without-a-loss-of-information
#' @export
as.numeric.factor <- function(x) {
  as.numeric(levels(x))[x]
}
