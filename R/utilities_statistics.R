
#'
#'


#' skewness
#'#'
#' @description
#' Sample skewness. The biased portion is the population skewness; correction is for finite sample.  
#' D. N. Joanes and C. A. Gill. “Comparing Measures of Sample Skewness and Kurtosis”. The Statistician 47(1):183–189
#'#'
#'
#'
#'
#'#'
#'#'
'skewness'


#' skewness
#'#'
#' @description
#' Standard error of skewness. Depends only on sample size. 
#'#'
#'
#'
#'
#'#'
#'#'
'skewness'


#' skewness
#'#'
#' @description
#' Sample kurtosis. The biased portion is the population kurtosis; corrected is for finite sample.  
#'#'
#'
#'
#'
#'#'
#'
'skewness'


#' skewness
#'#'
#' @description
#' Standard error of kurtosis. Depends only on sample size. 
#'#'
#'
#'
#'
#'#'
#'
'skewness'


#' regslope
#'#'
#' @description
#' Returns slope of regression as single scalar (for use with apply).
#'#'
#'
#'
#'
#'#'
#'#'
'regslope'


#' regslope.noint
#'#'
#' @description
#' Returns slope of regression with no intercept as single scalar (for use with apply).
#'#'
#'
#'
#'
#'#'
#'#'
'regslope.noint'


#' regress.plot
#'#'
#' @description
#' Performs regression in convenient way and returns coefficients and
#' probabilities in a single vector, and plots a graph.
#'#'
#'
#'
#'
#'#'
#'#'
'regress.plot'


#' regress.loglog
#'#'
#' @description
#' Performs regression and graphs in a convenient way: with or without log-transforming x and y variables (the option addone
#' can be included to handle zeros for log-transformation), with or
#' without manual point labelling, without or without the best-fit line added, and with many options for colors and points. 
#' add can be a vector of length 2, a constant to be added to every value
#' of x, y to remove zeroes.
#'#'
#'
#'
#'
#'#'
#'#'
'regress.loglog'


#' majoraxisreg
#'#'
#' @description
#' A major axis regression with parameters fitted by optim. The regression
#' is the line which minimizes perpendicular distance summed over all points
#'(and squared).
#'#'
#'
#'
#'
#'#'
#'#'
'majoraxisreg'


#' minum.perpdist
#'#'
#' @description
#' The sum of squares used by majoraxisreg.
#'#'
#'
#'
#'
#'#'
#'#'
'minum.perpdist'


#' majoraxisreg.no.int
#'#'
#' @description
#' Major axis regression with no intercept. Only a slope
#' is returned. Below is the same for standard regression.
#'#'
#'
#'
#'
#'#'
#'
'majoraxisreg.no.int'


#' standardreg.no.int
#'#'
#' @description
#' Standard regression with no intercept.
#'#'
#'
#'
#'
#'#'
#'#'
'standardreg.no.int'


#' autoregression
#'#'
#' @description
#' Autocorrelation with a given lag of a vector y.
#'#'
#'
#'
#'
#'#'
#'
'autoregression'


#' regression.Bayes
#'#'
#' @description
#' Regression using the Gibbs sampler, with just one x variable. 
#'#'
#'
#'
#'
#'#'
#'#'
'regression.Bayes'


#' Gibbs.regslope
#'#'
#' @description
#' Updates the regression slope (used in regression.Bayes).
#'#'
#'
#' @examples
#' \dontrun{
#'#'
#'#' browser()
#'#'
#'#'
#'#'
'Gibbs.regslope'


#' Gibbs.regsigma
#'#'
#' @description
#' Updates the regression standard deviation (used in regression.Bayes).
#'#'
#'
#'
#'
#'#'
#'#'
'Gibbs.regsigma'


#' Gibbs.normalmean
#'#'
#' @description
#' The standard Gibbs sampler for a normal distribution with unknown mean and variance. 
#'#' y is the vector of observations
#' sigma is the latest draw of the SD, using sqrt(Gibbs.normalvar)
#'#'
#'
#'#'
#'#'
'Gibbs.normalmean'


#' Gibbs.normalvar
#'#'
#' @description
#' Gibbs draw for the variance of a normal distribution (http://www.biostat.jhsph.edu/~fdominic/teaching/BM/3-4.pdf). If all y are
#' identical, it returns a small positive number. 
#'#'
#'
#'
#'
#'#'
#'#'
'Gibbs.normalvar'


#' model.xy
#'#'
#' @description
#' Generic Bayesian routine for fitting a model to y given 1 predictor variable x. The function
#' of y~x must be supplied, as well as the function for the SD~y. Any functions with any numbers
#' of parameters can be used: predfunc is the function of y~x, and sdfunc is the function sd~y.
#' The function badpredpar is needed so the user can make up any definition
#' for parameter values that are out of bounds. Without this, the model could not support any generic predfunc.
#' Badpredpar must accept two vectors of parameters, one for main and one for sd.
#' The ellipses allow additional parameters to be passed to the model function, but there is no such option for the
#' sd function. The additional parameters mean that some of the variables defining the model do not have to be fitted.
#' The sd function can be omitted if the likelihood does not require it. 
#' This works only if one likelihood function defines the likelihood of the model, given data and parameters only.
#' If the likelihood of some parameters is conditional on other parameters, as in hierarchical model, this can't be used.
#'#'
#'
#' @examples
#' \dontrun{
#' testx=1:10; testy=1+2*testx+rnorm(10,0,1)
#'  model.xy(x=testx,y=testy,predfunc=linear.model,llikefunc=llike.GaussModel,badpredpar=BadParam,start.predpar=c(1,1),
#'           sdfunc=constant,start.sdpar=1,llikefuncSD=llike.GaussModelSD)}
#'
#'#'
#'#'
'model.xy'


#' arrangeParam.llike
#'#'
#' @description
#' Used in likelihood function of a Gibbs sampler. Allows any of a set of parameters to be submitted to metrop1step; 
#' whichtest is the index of the parameter to test. If NULL, zero, or NA, it simply returns allparam.
#'#'
#'
#' @examples
#' \dontrun{
#'#'
#'#'
'arrangeParam.llike'


#' arrangeParam.Gibbs
#'#'
#' @description
#' Used in the loop of a Gibbs sampler, setting parameters not yet tested (j and above) to previous value (i-1), 
#' and other parameters (<j) to new value (i). 
#' This has unfortunate need for the entire matrix allparam, when only row i-1 and row i are needed. 
#'#'
#'
#'
#'
#'#'
#'#'
'arrangeParam.Gibbs'


#' llike.GaussModel
#'#'
#' @description
#' This is for model.xy. It takes the model function, its parameters, x values, observed values of the dependent variable obs, and sd values,
#' to generate a likelihood. One of the parameters is passed as testparam, for use with metrop1step.
#' This requires a badparam function for testing parameters. The standdard deviation is passed as an argument,
#' not calculated from sdmodel.
#'#'
#'
#'
#'
#'#'
#'#'
'llike.GaussModel'


#' llike.GaussModelSD
#'#'
#' @description
#' This is for model.xy. Take the function for the SD, its parameters, and both predicted and observed values of the 
#' dependent variable (pred,obs)
#' to generate a likelihood. One of the parameters is passed as testparam, for use with metrop1step. The predicted value
#' for each observation is included, and not calculated from the predicting function. 
#' MINIMUM_SD=.0001
#' MINIMUM_SD should be set in the program calling model.xy, to adjust it appropriately. If MINSD==0, then the sd can
#' collapse to a miniscule number and drive the likelihood very high, preventing parameter searches from ever escaping the sd.
#'#'
#'
#'
#'
#'#'
#'#'
'llike.GaussModelSD'


#' BadParam
#'#'
#' @description
#' This is a default for model.xy, never returning TRUE. To use model.xy, another function must be created to
#' return TRUE for any illegal parameter combination (any that would, for instance, produce a predicted y out-of-range, or a would
#' create an error in the likelihood function.)
#'#'
#'
#'
#'
#'#'
#'#'
'BadParam'


#' graph.modeldiag
#'#'
#' @description
#' Graph diagnostics of model.xy
#'#'
#'
#'
#'
#'#'
#'#'
'graph.modeldiag'


#' bootstrap.corr
#'#'
#' @description
#' Running bootstrap on a correlation. Any columsn can be chosen from the submitted dataset, by number or name.
#'#'
#'
#'
#'
#'#'
#'#'
'bootstrap.corr'


#' bootconf
#'#'
#' @description
#' A simple calculation of confidence limits based on the SD of a vector.
#'#'
#'
#'
#'
#'#'
#'#'
'bootconf'


#' metrop1step
#'#'
#' @description
#' Takes a single metropolis step on a single parameter for any given likelihood function.
#' The arguments start.param and scale.param are atomic (single values), as are adjust and target. 
#' The ellipses handle all other arguments to the function. The function func must accept the test 
#' parameter as the first argument, plus any additional arguments which come in the ellipses.
#' Note the metropolis rule: if rejected, the old value is returned to be re-used. The return value
#' includes a one if accepted, zero if rejected.
#' The step size, refered to as scale.param, is adjusted following Helene's rule. 
#' For every acceptance, scale.param is multiplied
#' by adjust, which is a small number > 1 (1.01, 1.02, 1.1 all seem to work). For every rejection, scale.param
#' is multiplied by (1/adjust); for every acceptance, by adjust^AdjExp, the latter based on the target acceptance rate.
#' When the target acceptance rate is 0.25, which is recommended for any model with > 4 parameters,
#' AdjExp=3. It's easy to see how this system arrives at an equilibrium acceptance rate=target.
#' The program calling metrop1step has to keep track of the scaling parameter: submitting it each time
#' metrop1step is called, and saving the adjusted value for the next call. Given many parameters, a
#' scale must be stored separately for every one.
#' Note the return value is a vector of 6:
#'1) the new parameter value;
#'2) the new scale (step size);
#'3) a zero or a one to keep track of the acceptance rate;
#'4) the likelihood of original parameter (if rejected) or new parameter (if accepted)
#'5) the likelihood of original parameter (if accepted) or new parameter (if rejected)
#'6) the new parameter tested (whether accepted or not)
#'#'
#'
#' @examples
#' \dontrun{
#'#'
#'##' It's possible with shifting parameters to get trapped where start.param is invalid.
#' This test allows an escape  
#'#'
#'#'
#'#'
'metrop1step'


#' metrop1step.discrete
#'#'
#' @description
#' A version for metrop1step where the alternative values are character states with no numeric meaning.
#' A random draw must be taken from all possible states, each with equal probability. There
#' is no step-size thus no adjustment.
#'#'
#'
#' @examples
#' \dontrun{
#'#'
#'#' if(is.na(likeratio)) browser()
#'#'
#'#'
#'#'
'metrop1step.discrete'


#' testmcmcfunc
#'#'
#' @description
#' For testing mcmc1step. No longer used. 
#'#'
#'
#' @examples
#' \dontrun{
#'#'
#'#' browser()
#'#'
#'#'
#'#'
'testmcmcfunc'


#' CI
#'#'
#' @description
#' Confidence limits (quantiles) from a vector at specified probabilities. Default is 95% confidence interval. 
#'#'
#'
#'
#'
#'#'
#'#'
'CI'


#' hist.compare
#'#'
#' @description
#' Compares two histograms with a Kolmogorov approach.
#'#'
#'
#'
#'
#'#'
#'#'
'hist.compare'


#' harmonic.mean
#'#'
#' @description
#' Harmonic mean of a vector x. NAs and nonzero values can be ignored, and a constant can be added to every x.
#'#'
#'
#'
#'
#'#'
#'#'
'harmonic.mean'


#' cumul.above
#'#'
#' @description
#' Given y values as a function of x, this seeks the x at which the curve passes through a given y. It sets
#' a variable whichabove to 0 for all cases where y>cutoff, otherwise 0, then fits a logistic regression.
#' The midpoint of the logistic regression is a good estimate. 
#'#'
#'
#'
#'
#'#'
#'#'
'cumul.above'


#' sumsq
#'#'
#' @description
#' A trivial function used in minimizing sums of squares.
#'#'
#'
#'
#'
#'#'
#'#'
'sumsq'


#' is.odd
#'#'
#' @description
#' A trivial function to test whether numbers (scalar or vector) are odd. 
#'#'
#'
#'
#'
#'#'
#'#'
'is.odd'


#' border.distance
#'#'
#' @description
#' Returns distance from a point to the nearest boundary of a rectangle (plot). Accepts either separate
#' x-y coordinates, or an object where x is first column, y is second. The lower left corner of the plot is
#' assumed to be 0,0. 
#'#'
#'
#'
#'
#'#'
#'#'
'border.distance'


#' regsum
#'#'
#' @description
#' This carries out either first or second order polynomial regression,
#' finds the x- and y-values at y's peak if its second order,
#' otherwise the x-intercept.
#'#'
#'
#'
#'
#'#'
#'#'
'regsum'


#' colMedians
#'#'
#' @description
#' For convenient medians, like colMeans.
#'#'
#'
#'
#'
#'#'
#'
'colMedians'


#' midPoint
#'#'
#' @description
#' Midpoint of any vector.
#'#'
#'
#'
#'
#'#'

'midPoint'
