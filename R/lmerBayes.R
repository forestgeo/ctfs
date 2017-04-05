
#'
#'

#' lmerBayes
#'
#' @description
#'
#' A Metropolis MCMC version of lmer. A single independent variable, y, can be fit against any number of predictors, x, 
#' with one random effect. Like lmer, the model error can be binomial or Gaussian, 
#' but there are two alternatives for the Gaussian (described below). Relative to lmer, the key advantage offered is that 
#' y can be any function of the x. A second advantage is that the MCMC produces 
#' posterior distributions on every parameter, so full confidence limits are available. The principal limitation relative to lmer is 
#' that only one random effect is allowed. In addition, the Bayesian MCMC approach is quite a bit slower.
#'
#'
#' Data are submitted the way lm or lmer require, with one single table, one row per observation; the random effects 
#' are in one column. The formula, however, is not submitted using the R-style 'squiggle'~. Rather, the names of x, y, and 
#' random columns, are given. The model describing y's function of the x's is passed, and must be provided by the user 
#'(several are available within the CTFS R Package, though, in the Utilities topic). Examples below will serve to explain.
#'
#'
#' As in lmer, all parameters of the model follow a Gaussian hyperdistribution across the random effects. There is an 
#' option to include a full covariance matrix as the hyperdistribution, otherwise, only the variances are fit (ie, the
#' covariance matrix has only zeroes off-diagonal). There is also an option to use the conjugate inverse-gamma or inverse-wishart for the variances and covariances; otherwise, Metropolis steps are used.
#'
#'
#' A starting set of parameters for the model must be submitted. It can be a vector as long as the number of
#' parameters required by the model, or it can be a full matrix, with one row of parameters for each of the random
#' effects. The latter requires knowing in advance the names of all the random effects. 
#'
#'
#' There is a further complication included whose purpose is reducing memory demand in big models with many MCMC steps. option paramfile allows the full parameter matrix to be written into a text file every savestep steps, then erased from memory. 
#'
#' This is to reduce memory needs. The function summaryMCMC restores the parameters from the text file into an giant R array. 
#'
#'
#' The return value is a list with several components:
#' *  mu: A 2D array with the entire chain of model parameters (ie, fixed effects) from the Gibbs sampler
#' *  sigma: A 3D array with the entire chain of covariances from the Gibbs sampler; if includeCovar==FALSE, only the diagonal is non-zero
#' *  bestmu: Best estimate of the model parameters for the entire data (ie, fixed effect) 
#' *  bestsigma: Best estimate of the covariance (ie, group-level variance or error)
#' *  resid: The entire chain parameters for the model of residuals
#' *  bestresid: The best estimate of parameters for the model of residuals
#' *  CIresid: Credible intervals for the parameters for the model of residuals
#' *  best: The best estimates of model parameters for each random effect
#' *  lower: Lower credible intervals of model parameters for each random effect
#' *  upper: Uower credible intervals of model parameters for each random effect      
#' *  burn: The burn-in
#' *  llike: Full log-likelihood of the model at each step of the Gibbs'sampler
#' *  bestlike: The log-likelihood of the optimal parameter combination (means of the posterior distribution)
#' *  DIC: Deviance information criterion of the model
#' *  obs: The original y (dependent) variable, just as submitted
#' *  data: The original x (independent) variables, just as submitted
#' *  model: The model's predictions, as a list with one element per random effect
#' *  randlike: The log-likelihood of observations for each random effect given the optimal parameters (a vector, one per random effect)
#' *  keep: The steps of the Gibbs sampler after burn-in, as a vector of negative numbers 
#' *  start: The start parameters submitted
#' *  randeffects: The names of all the random effects
#' *  parnames: The names of the model parameters
#' *  fullparam: A 3D array with all parameters of the Gibbs sampler; one dimension if for all the random effects, with each random effect having a matrix of model parameters for every step of the Gibbs's sampler
#'
#'
#' Further details are given in the description of all the arguments and the sample here, plus a tutorial on 'Mortality changes'
#'(http://ctfs.arnarb.harvard.edu/Public/CTFSRPackage/index.php/web/tutorials/MortalityChange/index.html) 
#' offers a worked example.
#'
#' @param data The table of data, in lmer-style, including one column to be modeled (dependent variable, y), one or more predictors (independent variables, x), and one random effect, using any column names.
#' @param ycol The name of the column holding the y variable, with quote marks; this variable must be numeric.
#' @param ycol The name of one or more columns holding the x variables, with quote marks; these can be numeric or character variables.
#' @param randcol The name of one column holding the random variable; must be a character variable.
#' @param start Starting parameter values, either a vector with as many parameters as the model needs, or a matrix of such vectors, one per random effect
#' @param startSD A single starting value for the residual standard deviation, only used with Gaussian and Negative Binomial error models.
#' @param startCov Starting values of the diagonal of the covariance matrix; ignored if a full matrix of start parameters is submitted. Required even if covariance matrix is not fitted, because needed as starting hyperSD.
#' @param model The function name holding the model describing y's relationship to all the x's, without quote marks. The first argument of the function must be named x, the second param, with additional arguments allowed. The model may accept as x either a vector or a matrix, the latter for a multiple regression. There can be any number of parameters, but the number must match the number given as start parameters. The return value must be a numeric vector with the same size as x. 
#' @param error A character variable with 6 possible values, Binom, NegBinom, Pois, Gauss, GaussMultResid, or Flat, with quote marks. 
#' @param 'Binom'uses binomial error for residuals
#' @param 'NegBinom'uses negative binomial error for residuals; the SD is then the dispersion parameter (k) of the negative binomial
#' @param 'Poisson'uses Poisson error for residuals
#' @param 'Gauss'uses Gaussian error for residuals with constant standard deviation across groups
#' @param 'GaussMultResid'uses Gaussian error for residuals, with standard deviation a constant fraction of the model's prediction
#'        (and thus only appropriate if predictions are strictly positive)
#' @param 'Flat'is a trivial model where the same likelihood is returned regardless of parameters or data. It is for testing how parameter search behaves in absence of data, as for describing an implied prior. 
#'
#' @param includeCovar TRUE or FALSE, whether to fit the full covariance matrix, vs. variances alone.
#' @param update 'conjugate'or 'metropolis', whether to use inverse-gamma (or inverse-Wishart for full covariance) vs. metropolis steps for updating covariances.
#' @param badparam The name of a function (unquoted) that tests a set of model parameters for validity; must return TRUE if parameters are valid, otherwise FALSE.
#' @param sdfunc The name of a function (unquoted) that models the residual standard deviation as a function of the x's, just like the model function. The default uses the function named constant, meaning the standard deviation is the same for all values of x. Parameters for this function are estimated, just as parameters for the model function are.
#' @param badSDparam The name of a function which tests for invalid parameters for sdfunc, returning TRUE or FALSE (analogous to badparam); a simple version is provided, called badSD, which rejects a single parameter if it is < 0. 
#' @param paramfile The name of a file where the entire MCMC chain of parameter values is stored at regular intervals; when parameters are written to the file, they are erased from memory, thus removing the need for the entire chain of all parameters being stored at once while the model is running.
#' @param savestep Parameters are appended to paramfile every savestep steps; must be < steps.
#' @param steps The number of steps to run the Gibbs sampler.
#' @param showstep Information is printed to the screen every showstep steps.
#' @param burnin The number of steps to remove as burn-in before calculating posterior distributions; not that all parameters are saved and returned regardless.
#' @param debug TRUE or FALSE, whether to pause and debug; for advanced users and developers.
#' @param ... The typical R means for submitting additional parameters for various functions used in the model (model, sdfunc, badparam, badSDparam).
#'
#' @examples
#' \dontrun{
#'
#' Assume two plot datasets from BCI are attached, bci.full6 and bci.full7. Subset to trees above 10 cm dbh and just 10 species for illustration (the model will run much faster). The fixed effect, species-level variation (or error), and the model parameters for each species are shown below. Check the names of the result to see what else lmerBayes returns.
#' gtable=growth.indiv(bci.full6,bci.full7,mindbh=100) 
#' a_few_species=c('termam','tachve','pri2co','gustsu','cecrin','tet2pa','guatdu', 'vochfe','virose','maquco') 
#' gtable=subset(gtable,!is.na(incgr) & sp %in% a_few_species) 
#' mod=lmerBayes(data=gtable,ycol='incgr',xcol='dbh1',randcol='sp',start=c(1,0),startSD=1,startCov=1,model=linear.model,error='Gauss', includeCovar=FALSE,badSDparam=badSD,steps=1100,showstep=50,burnin=100) 
#' mod$bestmu 
#' diag(sqrt(mod$bestsigma)) 
#' mod$best  
#' names(mod)}
#'
#'
'lmerBayes'

#' lmerBayes.hyperllike.sigma
#'
#' @description
#'
#' This is the hyper-likelihood for updating the covariances. It is always based on dmvnorm. The par is a matrix of parameters, one row per random effect,
#' one column the set of parameters. It allows the Gibbs sampler to work by passing a single scalar parameter as the first
#' argument.
#'
#'
'lmerBayes.hyperllike.sigma'

#' lmerBayes.hyperllike.mean
#'
#' @description
#'
#' This is the hyper-likelihood for updating the hypermeans, based on dmvnorm. The vector full.hypermean is the entire set; one of them, defined by the index whichtest, 
#' is to be tested; covarSD is the covariance matrix. The modelpar is a matrix of parameters, one row per random effect, one column for each parameter. 
#'
#'
'lmerBayes.hyperllike.mean'

#' full.likelihood.lmerBayes
#'
#' @description
#'
#' Calculate full likelihood for any complete set of parameters, including every set for each random effect and hypermeans and covariances ##
#'
#'
#' Further thought: the call to lmerBayes.hyperllike.sigma doesn't make sense, since llike.model.lmer already does this; the probability of 
#' each set of parameters given the hyperparameters is already calculated. 
#'
#'
#' To update fixed effects, can use full.llikelihood.lmerBayes. The fixed effects are used identically for every random effect.  
#'
#'
'full.likelihood.lmerBayes'

#' llike.model.lmer
#'
#' @description
#'
#' A llikelihood function for one set of parameters, for a single random effect. The error is specified by errormodel, typically dbinom or dnorm.  
#'
#' It includes the likelihood of observing data given a response model (model) and its parameters (allparam), plus the hyper-likelihood of observing allparam
#' given the hyperparameters, including hypermeans and covariance matrix. This is based off llike.model.occur.hierarch in fitLogisticMap.r, 
#' but differs in including the covariance in the hyper-model. If the argument mu, for hypermeans, is set NULL, the likelihood without the hyper-likelihood is returned.
#'
#' @examples
#' \dontrun{
#'
#'#' else if(errormodel=='GaussMultResid') 
#' {
#'  if(length(which(modeled<=0))>0) return(-Inf)       
#'
#' With this option, model must always be > 0
#'  llike=dnorm(x=trueN,mean=modeled,sd=withinSD*modeled,log=TRUE)
#' }
#' else if(errormodel=='Flat') llike=rep(0,length(modeled))}
#'
#'
'llike.model.lmer'

#' residual.llike.lmerBayes
#'
#' @description
#'
#' Calculate likelihood of residual standard deviation, given observations plus the predicting model and data (to make predictions).
#'
#' This likelihood does not depend on the hyperparameters. It does require data and prediction for every single random effect. 
#'
#'
#' Simply check a single SD parameter for sign.
#'
'residual.llike.lmerBayes'

#' badSD
#'
#' 
#'
#'
'badSD'

#' arrangeParam.llike.2D
#'
#' @description
#'
#' Used in likelihood function of a Gibbs sampler for lmerBayes, but for parameters submitted as a matrix, not a vector. 
#'
#' Allows any of a set of parameters to be submitted to metrop1step; whichrow, whichcol are the indices of the single parameter to test. 
#'
#' The argument forcesymmetry is needed for a covariance matrix: if one column is updated, the transposed column must also be.
#'
#' If NULL, zero, or NA, it simply returns allparam. This is based on arrangeParam.llike.
#'
#'
'arrangeParam.llike.2D'

#' arrangeParam.Gibbs.2D
#'
#' @description
#'
#' Used in the loop of a Gibbs sampler, setting parameters not yet tested (j and above) to previous value (i-1), and other parameters (<j) to new value (i). 
#'
#' But unlike arrangeParam.Gibbs, on which this is based, this is for a matrix, not a vector. Also unlike arrangeParam.Gibbs, this accepts only two versions
#' of the parameter matrix, the previous and the next. This is crucial for memory: otherwise, allparam would be a 3D array of large size.
#'
#' The scalar j refers to one row, and the scalar k a column. The function assumes that Gibbs updates occur by row, so every element on rows < j is set to the next parameter,
#' plus all those in row j up to column k.  
#'
#'
'arrangeParam.Gibbs.2D'

#' saveParamFile
#'
#' @description
#'
#' This saves a run of the full parameters into a text file, reducing the amount of memory needed.
#'
#'
'saveParamFile'

#' restoreParamFile
#'
#' @description
#'
#' Reverses the steps of saveParamFile, back to a 3D array. This requires the entire parameter set to be moved into memory, but it only happens
#' once at the very end of the run. Since paramfile is gigantic, this is slow. 
#'
#'
'restoreParamFile'

#' resetParam
#'
#' @description
#'
#' This starts a new 3D parameter array whose first element is the last element of the current one. The rest of the new one is empty, to hold
#' the next set of savestep steps from the Gibbs sampler. This only happens after the current one has been saved to a text file. 
#'
#'
'resetParam'

#' summaryMCMC
#'
#' @description
#'
#' Make summary calculations based on the full Gibbs sampler. The argument fit is an object holding all steps of the sampler, plus data, observations,
#' and likelihood. However, if parameters were saved along the way to a text file, then the argument paramfile is used to name the file and restore them
#' into a 3D array. Estimates of confidence limits of all parameters are returned. If returnfull is set TRUE, then the entire 3D array of parameters is
#' also returned. Full likelihood at the best parameters is calculated and likelihood at each step in sampler are used to calculate DIC.
#'
#'
'summaryMCMC'

#' recalculate.lmerBayesllike
#'
#' @description
#'
#' Walk through entire chain of parameters to calculate full likelihood at each step, as was done during the model run. The argument keep
#' defines the elements to be used, or if NULL, fit$keep is used. 
#'
#'
'recalculate.lmerBayesllike'

#' covTocorr
#'
#' @description
#'
#' Convert covariance matrix to correlation matrix. Each element is divided by the square root of the product of the corresponding diagonal terms.
#'
#'

'covTocorr'
