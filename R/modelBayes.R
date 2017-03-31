
#'<function>
#'<name>
#' modelBayes
#'
#' @description
#' A Metropolis MCMC version for any modeling y~x, without random effects (as in lmerBayes). This version is built off lmerBayes, with the hyperdistributions
#' excluded. A single independent variable, y, can be fit against any number of predictors, x, 
#' The model error can be binomial, Poisson, negative binomial, or Gaussian, with two alternatives for the Gaussian (described below). 
#'
#' Data are submitted the way lm or lmer require, with one single table, one row per observation. The formula, however, is not submitted using the R-style 'squiggle'~. 
#' Rather, the names of x, y columns, are given. The model describing y's function of the x's is passed, and must be provided by the user 
#'(several are available within the CTFS R Package, though, in the Utilities topic). Examples below will serve to explain.
#'
#'
#' A starting set of parameters for the model must be submitted. It is a vector as long as the number of
#' parameters required by the model. 
#'
#'
#' The return value is a list with several components:
#'<ul>
#' @param resid A 2D array with the entire chain of the error parameters, from Gibbs sampler
#' @param fullparam A 2D array with the entire chain of model parameters from the Gibbs sampler
#' @param burn Atomic, the number of steps discarded as burn-in before calculated statistics from Gibbs sampler
#' @param steps Atomic, the number of steps run in Gibbs sampler
#' @param llike Full log-likelihood of the model at each step of the Gibbs'sampler
#' @param obs The original y (dependent) variable, just as submitted
#' @param data The original x (independent) variables, just as submitted
#' @param parnames The names of the model parameters
#' @param start The start parameters submitted
#' @param best Best estimate of the model parameters for the entire data 
#' @param CI Credible intervals for the model parameters
#' @param bestresid The best estimate of parameters for the error model
#' @param CIresid Credible intervals for the parameters of the error model
#' @param pred A dataframe with all observations, predictors, and the model's best prediction, mean prediction, and credible intervals at each point
#' @param many A 2D array holding N draws of the model's prediction at each sampling point; N is either the number of post-burn-in steps, or 1000, whichever is greater
#' @param keep The steps of the Gibbs sampler after burn-in, as a vector of negative numbers 
#'<\ul>
#'
#'
#'<arguments>
#'<ul>
#' @param data The table of data, in lmer-style, including one column to be modeled (dependent variable, y), one or more predictors (independent variables, x), and one random effect, using any column names.
#' @param ycol The name of the column holding the y variable, with quote marks; this variable must be numeric.
#' @param xcol The name of one or more columns holding the x variables, with quote marks; these can be numeric or character variables.
#' @param start Starting parameter values, either a vector with as many parameters as the model needs, or a matrix of such vectors, one per random effect
#' @param startSD A starting value for the error model; there must be as many startSD as parameters needed by sdfunc
#' @param model The function name holding the model describing y's relationship to all the x's, without quote marks. The first argument of the function must be named x, the second param, with additional arguments allowed. The model may accept x either a vector or a matrix, the latter for a multiple regression. There can be any number of parameters, but the number must match the number given as start parameters. The return value must be a numeric vector with the same size as x. 
#' @param error A character variable with 4 possible values, Binom, Pois, NegBinom, Flat, Gauss, or GaussMultResid, with quote marks. 
#'<ul>
#' @param 'Binom'uses binomial error, appropriate only for non-negative integer y; there are no error parameters for the binomial, so no resid parameters
#' @param 'Poisson'uses Poisson error, appropriate only for non-negative integer y; there are no error parameters for the binomial, so no resid parameters
#' @param 'NegBinom'uses negative binomial error, appropriate only for non-negative integer y; the clump parameter k of the negative binomial is also fitted, and can be modeled with sdfunc
#' @param 'Gauss'uses Gaussian error, which can be a constant or modeled on the x's
#' @param 'GaussMultResid'uses Gaussian error modeled as a fraction of the prediction at each x; again, it can be a constant fraction or a fraction modeled on x
#'        (aappropriate only if predictions are strictly positive)
#' @param 'Flat'is a trivial model where the same likelihood is returned regardless of parameters or data. It is for testing how parameter search behaves in absence of data, as for describing an implied prior. 
#'
#' @param update 'conjugate'or 'metropolis', whether to use inverse-gamma (or inverse-Wishart for full covariance) vs. metropolis steps for updating covariances.
#' @param badparam The name of a function (unquoted) that tests a set of model parameters for validity; must return TRUE if parameters are valid, otherwise FALSE.
#' @param sdfunc The name of a function (unquoted) that models the error parameter as a function of the x's; the default uses the function named constant, meaning the standard deviation is the same for all values of x. Parameters for this function are estimated, just as parameters for the model function.
#' @param badSDparam The name of a function which tests for invalid parameters for sdfunc, returning TRUE or FALSE (analogous to badparam); a simple version is provided, called badSD, which rejects a single parameter if it is < 0. 
#' @param steps The number of steps to run the Gibbs sampler.
#' @param showstep Information is printed to the screen every showstep steps.
#' @param burnin The number of steps to remove as burn-in before calculating posterior distributions; not that all parameters are saved and returned regardless.
#' @param debug TRUE or FALSE, whether to pause and debug; for advanced users and developers.
#' @param ... The typical R means for submitting additional parameters for various functions used in the model (model, sdfunc, badparam, badSDparam).
#'
#'
#' @examples
#' \dontrun{
#'
#'#'
#'
#'
#'
#'
#'<function>
#'<name>
#' residual.llike.modelBayes
#'
#' @description
#' Calculate likelihood of residual standard deviation, given observations plus the predicting model and data (to make predictions).
#' This likelihood does not depend on the hyperparameters. It does require data and prediction.
#'
#'<arguments>
#'
#'
#' @examples
#' \dontrun{
#'
#'#'
#'
#'
#'
#'<function>
#'<name>
#' summaryModelMCMC
#'
#' @description
#' Make summary calculations based on the full Gibbs sampler. The argument fit is an object holding all steps of the sampler, plus data, observations,
#' and likelihood. Estimates of confidence limits of all parameters are returned. Full likelihood at the best parameters is calculated and likelihood 
#' at each step in sampler are used to calculate DIC.
#'
#'<arguments>
#'
#'
#' @examples
#' \dontrun{
#'
#'#'
#'
#'
#'<function>'
