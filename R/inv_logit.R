#' @title
#' inv_logit
#' @description
#' inverse logit
#' use e.g. to plot GLMM results in the original scale (e.g. p or %)
#' @param x the logit that needs conversion back to original scale
#' @return vector
#' @examples
#' a <- log( 0.2 / (1 - 0.2) )
#' inv_logit( a )
#' @author Tobias Heed
inv_logit <- function( x ) {
  return( exp( x )/( 1 + exp( x ) ) )
}



