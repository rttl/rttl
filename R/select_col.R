#' @title
#' select a column in a data frame based on the name (as string)
#' @description
#' select a column of a data frame by giving a name as string
#' @param df the data frame from which the column should be selected
#' @param colName the name (string) of the column
#' @return vector with selected column
#' @examples
#' a <- data.frame( col1 = c( 1, 2, 3 ), col2 = c( 3, 4, 5 ) )
#' select_col( a, "col1" )
#' s <- "col2"
#' select_col( a, s )
#' @author Tobias Heed
#' @export
select_col <- function( df, colName ) {
  return( df[ , names( df ) == colName ] )
}

