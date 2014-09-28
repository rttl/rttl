#' @title
#' glmm_fitted
#' @description
#' Extracts the fitted values from a lmerMod or glmerMod model for
#' each line of the original data frame.
#' In one column, it adds the fixed effects alone (as fixedFit,
#' equivalent to fixef() ),
#' fixed plus random (as fitted)
#' and their difference (as RanFix)
#' For a GLMM, it uses the inverse function to create values in the original
#' scale, rather than in the link scale (e.g. inv logit for a logit link function).
#' So for example, if you have used
#' binary data that you want to interpret as percentages with a log link,
#' then the inverse log is used here and you get "real" numbers back.
#' The fix.inv.logit is equivalent to the marginal estimates of lsmeans.
#' lsmeans also gives an estimate for the confidence interval (could be implemented
#' here a well at some point...)
#' @param model An lmerMod or glmerMod object
#' @param diagnostics TRUE/FALSE for whether you want to see a plot for
#' RanFix and fitted values (these should be distributed around 0)
#' @return Returns a data frame with columns fixedFit, fitted, RanFix
#' @author Tobias Heed, 2013-09-26
#' @export

glmm_fitted <- function( model, diagnostics = FALSE )
{
  # get info about the model (family, link, inverse function)
  fam <- family(model)

  # fitted values saved in model are already inverse values
  fittedValues <- data.frame(x = fitted(model))
  names(fittedValues) <- paste0("rfx.inv.", fam$link)

  # initialize new data column
  fixedFit <- 0
  # add up all fixed effects, multiplied by design matrix
  # this is x'b (x = design matrix, b = parameters)
  for (i in 1 : length(fixef(model))) {
    fixedFit <- fixedFit + fixef(model)[i] * model@pp$X[, i]
  }


  # first the fixed effects alone
  fittedValues[[ paste0( "fx.", fam$link ) ]] <- fixedFit
  # inverse values (e.g. probability when link == logit)
  fittedValues[[ paste0( "fx.inv.", fam$link ) ]] <- fam$linkinv(fixedFit)
  # save diff between fitted (= random + fixed) and fixed alone (inverted values)
  fittedValues$rx.min.fx.inv <- fittedValues[[ paste0("rfx.inv.", fam$link) ]] - fittedValues[[ paste0( "fx.inv.", fam$link ) ]]

  # plot fitted values for a first impression
  if (diagnostics == 1)
  {
    # fitted vs. real values
    plot(fitted(model))
    # show distribution of difference between fx and rx (should be max at 0, because rx vary around 0)
    hist(fittedValues$rx.min.fx.inv)
  } # if diagnostics

  return(fittedValues)
} # function end
