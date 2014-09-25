#' @title
#' mPrint
#' @description
#' Prints the summary of an lmerMod or glmerMod plus:
#' which optimizer was used, how many iterations were allowed for
#' optimization, and any warnings that occurred.
#' @param model An lmerMod or glmerMod object
#' @return nothing
#' @author Tobias Heed, 2013-09 or so

mPrint <- function( model ) {
  # check whether correct class has been passed
  if ( ( class( model ) != "glmerMod" ) & ( class( model ) != "lmerMod" ) ) {
    cat( "The object passed to mPrint is not of class (g)lmerMod" )
    mPrint <- 1
  # if yes...
  } else {
    print( summary( model ) )
    # add info about optimizer used for fitting and the number of iterations allowed
    cat( "\nmodel fitted with:", model@optinfo$optimizer, "\n" )
    if ( is.null( model@optinfo$control$maxfun ) == TRUE ) {
      cat( "no maxfun value found (probably means default == 10.000)\n" )
    } else {
      cat( "iterations (maxfun): ", model@optinfo$control$maxfun, "\n" )
    }
    # show warnings, if any are present
    if ( length( model@optinfo$warnings ) == 0 ) {
      cat( "no warnings" )
    } else {
      cat( "warnings:\n" )
      for ( i in 1 : length( model@optinfo$warnings ) ) {
        cat( i, ": ", model@optinfo$warnings[[ i ]], sep = "" )
      }
    }
    return( 0 )
  }
}
