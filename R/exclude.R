#' @title
#' Exclude extreme values from a vector
#' @description
#' Excludes high and low outliers from a vector.
#' Criterion for exclusion is either a fixed upper/lower value,
#' or a multiple of the standard deviation.
#' @param vector The vector containing the data that is to be pruned.
#' @param upper.abs An absolute upper cutoff value
#' @param lower.abs An absolute lower cutoff value
#' @param upper.std The factor by which the standard deviation of \emph{vector}
#' will be multiplied to obtain the upper cutoff value ( mean plus upper.std * std.dev )
#' @param lower.std Equivalent to upper.std for lower cutoff value
#' @param explicit Set to TRUE to get written info output from the funtion
#' @return A vector that contains NA in all slots in which the upper or lower
#' bound were exceeded
#' @examples
#' v <- rnorm( 1000, 0, 1)
#' max( v )
#' v.clean <- exclude( v, upper.abs = 2 )
#' max( v.clean, na.rm = TRUE )
#'
#' v.clean <- exclude( v, upper.abs = 2, lower.abs = -2)
#' max( v.clean, na.rm = TRUE )
#' min( v.clean, na.rm = TRUE )
#'
#' v.clean <- exclude( v, upper.std = 2, lower.abs = -1, explicit = TRUE )
#' max( v.clean, na.rm = TRUE )
#' min( v.clean, na.rm = TRUE )
#' @author Tobias Heed
#' @importFrom magrittr %>%

exclude <- function( vector, upper.abs = NA, lower.abs = NA,
                     upper.std = NA, lower.std = NA, explicit = FALSE ) {

  # test whether a bound is defined twice
  if ( !is.na( upper.abs ) && !is.na( upper.std ) ) {
    warning( "Upper cutoff requested by both absolute and std.dev value -- using std.dev" )
    upper.abs <- NA
  }
  if ( !is.na( lower.abs ) && !is.na( lower.std ) ) {
    warning( "Lower cutoff requested by both absolute and std.dev value -- using std.dev" )
    lower.abs <- NA
  }

  # for each parameter, if it is defined, use it
  s <- sd( vector )
  if ( !is.na( upper.std ) ) {
    bound <- mean(vector, na.rm = TRUE) + upper.std * s
    vector[ vector > bound ] <- NA
    if ( explicit ) { cat( "\ncutting off at std.dev. upper bound of ", bound ) }
  }

  if ( !is.na( lower.std ) ) {
    bound <- mean(vector, na.rm = TRUE) - upper.std * s
    vector[ vector < bound ] <- NA
    if ( explicit ) { cat( "\ncutting off at std.dev. lower bound of ", bound ) }
  }

  if ( !is.na( upper.abs ) ) {
    vector[ vector > upper.abs ] <- NA
    if ( explicit ) { cat( "\ncutting off at absolute upper bound of ", upper.abs ) }
  }

  if ( !is.na( lower.abs ) ) {
    vector[ vector < lower.abs ] <- NA
    if ( explicit ) { cat( "\ncutting off at absolute lower bound of ", lower.abs ) }
  }

  return( vector )
}
