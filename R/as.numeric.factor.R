#' @title
#' select a column in a data frame based on the name (as string)
#' @description
#' select a column of a data frame by giving a name as string
#' @param df the data frame from which the column should be selected
#' @param colName the name (string) of the column
#' @return vector with selected column
#' @example
#' a <- factor( c( 1, 2, 1, 1, 1, 4, 7, 8 ) )
#' as.numeric( a )
#' as.numeric.factor( a )
#' a <- data.frame( col1 = c( 1, 2, 3 ), col2 = c( 3, 4, 5 ) )
#' @author http://stackoverflow.com/questions/3418128/how-to-convert-a-factor-to-an-integer-numeric-without-a-loss-of-information
#' @export
as.numeric.factor <- function(x) {
  as.numeric(levels(x))[x]
}
