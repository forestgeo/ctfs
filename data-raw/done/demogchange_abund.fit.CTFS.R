#' Title: ' model.littleR.Gibbs
#' }
#'
#' @description
#' The main function for fitting the probability distribution of population growth rates. Accepts any two full census R Analytical Tables.
#' Five different functional forms to the distribution can be fitted, as chosen with the argument modeltype:
#' @param Gaussian [modeltype="norm", with the quotes]
#' @param Asymmetric Gaussian (a different standard deviation on left and right of the mode) [modeltype="asymnorm", with the quotes]
#' @param Laplace (exponential distribution, with mirror image for negative values) [modeltype="symexp", with the quotes]
#' @param Asymmetric Laplace (different rate constant for left and right of the center) [modeltype="asymexp", with the quotes]
#' @param Asymmetric power distribution (different rate constant for left and right of the center) [modeltype="asympower", with the quotes]
#' }
#' A Gibbs sampler is used to fit the parameter, with a hierarchical component for the distribution of species' mortality rates (mu) and
#' species' rates of population change (r). 
#' and be sure to set mindbh. Other parameters can be left at defaults. 
#' Added the bad.modelparam option to accomodate dasympower Aug 2011. Now this has to be included for asymexp; before, the
#' check for negative SD parameters was hard-coded. 
#' Optionally, a table demog can be created separately and submitted. It must have columns N1, N2, S, time.
#' }
#' @param cns1 and cns2 the two census R Analytical Tables, with earlier census first
#' @param mindbh minimum dbh to be included; all trees smaller than mindbh are excluded
#' @param demog optional, must match exactly the table created within the function
#' @param abundrange the default includes every species, but this can be set to a minimum and maximum abundance (first census); species with abundances outside the range are excluded
#' @param start.param parameter values at the outset, 1) mean of log(mortality) rate, 2) SD of log(mortality), 3) center of distribution of little r, 4) rate (or SD) of the distribution of little r; if an asymmetric model is chosen, the latter is the initial value for both left and right rate 
#' @param modeltype, as listed above
#' @param bad.modelparam name of a function which checks the model parameters for bad values; for modeltype asymexp, must be bad.asymexp.param, for modeltype asympower, must be bad.asympower.param
#' @param steps number of steps to run the Gibbs sampler
#' @param burn number of steps of sampler to exclude as burn-in
#' @param showstep print hyperparameters and likelihood to the screen every showstep steps
#' @param debug set to TRUE to call browser within the function
#' }
#' 
#' @examples
#' \dontrun{
#' lambir.modelR=model.littleR.Gibbs(cns1=lambir.full3,cns2=lambir.full4,mindbh=1,bad.modelparam=bad.asymexp.param))
#' palanan.modelR=model.littleR.Gibbs(cns1=palanan.full3,palanan.full4,mindbh=1,bad.modelparam=bad.asymexp.param)
#' For graphic output, just pass the result to graph.abundmodel. There are many options, but the defaults will show the key results. 
#' graph.abundmodel(fit=lambir.modelR)
#' Alternate distributions for little r:
#' power67=model.littleR.Gibbs(cns1=bci.full6,cns2=bci.full7,modeltype='asympower',mindbh=10,start.param=c(-3,.8,.01,-.5),  bad.modelparam=bad.asympower.param,showstep=25)
#' gauss67=model.littleR.Gibbs(cns1=bci.full6,cns2=bci.full7,modeltype='asymnorm',mindbh=10,start.param=c(-3,.8,.01,100),  bad.modelparam=bad.asymexp.param,showstep=25)
#' }
#' <source>#' }
#' 
#' }
#'
#' Title: ' full.abundmodel.llike
#' }
#'
#' @description
#' With the table of abundances, hyper-parameter estimates, and estimated mortality rate and population growth for each species, calculates full model likelihood.
#' Note use of spmean.mort.abundGibbs, not sppmean.mort.Gibbs; in the latter, the one originally used in mortality model, the likelihood of observing a 
#' mortality parameter does not depend on the population growth. Only used as a subroutine of the main modeling function, model.littleR.Gibbs.
#' }
#'
#' @examples
#' \dontrun{
#' }
#' <source>#' }
#' 
#' }
#'
#' Title: ' prob.N1
#' }
#'
#' @description
#' Calculates the probability of observing N2 given N1, assuming a community-wide
#' distribution of little.r, log(N2/N1). It uses the normal approximation to the 
#' binomial-poisson model (see dpopchange and testdpopchange in abundsim.r) as the
#' error distribution around N2. Works with one species at a time, so all submitted values except lambda.ann are
#' scalars. Only used as a subroutine of the main modeling function, model.littleR.Gibbs.
#' }
#'
#' @examples
#' \dontrun{
#' }
#' <source>#' }
#' 
#' }
#'
#' Title: ' spmean.mort.abundGibbs
#' }
#'
#' @description
#' Likelihood function for a species mean (a scalar, one species at a time), given logMu and logSD and the data, N and S 
#' (just one species here, so all parameters are scalars). In the abundance model, the mortality
#' parameter is involved in the likelihood for population change, and it must thus depend on the
#' the fitted little r. See spmean.mort.Gibbs in mortality.fit.CTFS.r for the original version, from mortality model,
#' in which there is no dependence on little r. Only used as a subroutine of the main modeling function, model.littleR.Gibbs.
#' }
#'
#' @examples
#' \dontrun{
#' }
#' <source>#' }
#' 
#' }
#'
#' Title: ' hyper.abundGibbs
#' }
#'
#' @description
#' Likelihood function for hyperparameters of abundance model, given the species values of little.r (latter a vector). Simply calculates
#' the pdf of whatever modelfunc is requested. For symmetric models, norm and symexp, the third parameter (second hyperSD) is not used.
#' Only used as a subroutine of the main modeling function, model.littleR.Gibbs.
#' }
#'
#' @examples
#' \dontrun{
#' }
#' <source>#' }
#' 
#' }
#'
#' Title: ' hyper.mortGibbs
#' }
#'
#' @description
#' Likelihood function for logMu and logSD, given the species means (latter a vector). Simply calculates
#' log-normal probability of observing the species means given logMu and logSD. This is modernized to use arrangeParam.llike,
#' and replaces the older mu.mortGibbs and sd.mortGibbs.
#' }
#'
#' @examples
#' \dontrun{
#' }
#' <source>#' }
#' 
#' }
#'
#' Title: ' bad.asympower.param
#' }
#'
#' @description
#' The 3 parameters submitted to hyper.abundGibbs have to be checked, in case dasympower is used. The second and third, the
#' two rate parameters, have to be < (-1). Since the parameters are inverted, they must be in (-1,0). 
#' Only used as a subroutine of the main modeling function, model.littleR.Gibbs.
#' }
#'
#' @examples
#' \dontrun{
#' }
#' <source>#' }
#' 
#' }
#'
#' Title: ' bad.asymexp.param
#' }
#'
#' @description
#' For either the Gaussian, or asymexp, the SD parameters must be > 0. Only used as a subroutine of the main modeling function, model.littleR.Gibbs.
#' }
#'
#' @examples
#' \dontrun{
#' }
#' <source>#' }
#' 
#' }
#'
#' Title: ' fitSeveralAbundModel
#' }
#'
#' @description
#' Run model.littleR.Gibbs for a series of census databases, for every successive pair, then the first to the last. Then repeat for 10 times the initial
#' mindbh. All arguments except allcns are the same as those in model.littleR.Gibbs; allcns is a list of two more more census dataframes. 
#' }
#'
#' @examples
#' \dontrun{
#' }
#' <source>#' }
#' 
#' }
#'
#' Title: ' graph.abundmodel
#' }
#'
#' @description
#' Output histograms of little.r across species, observed and fitted, using the result of
#' model.littleR.Gibbs. The histogram of black points is all species, blue points only those starting with N >= minabund. If the argument mortcorr=TRUE,
#' a graph of mortality rate vs. population change for every species is also produced. Otherwise, a table of the species with biggest increases and biggest
#' decreases in abundance is printed to the screen. 
#' }
#' @param fit result of model.littleR.Gibbs
#' @param datafile optional name of file where the fitted result is saved
#' @param div width of bins for histogram of observed rate of population change
#' @param tinydiv width of bins used to draw the fitted distribution
#' @param modeltype form of probability distribution, matching what was used when fit was created by model.littleR.Gibbs
#' @param xrange, yrange range of graph's x-axis and and y-axis
#' @param minabund minimum abundance of species to be used in histogram of observed rates of population change
#' @param conf number of alternate fits to graph, as indication of confidence; if conf=NULL, no confidence lines are added
#' @param returnextreme whether to print a list of the fastest increases and decreases in abundance to the screen
#' @param xname, yname axis names
#' @param graphit if set to false, 
#' @param ltype, lwidth, modelclr type, width, color of the line showing fitted distribution
#' @param bartype if TRUE, histogram is bar graph
#' @param addpts if TRUE, histogram is a point graph
#' @param makeleg whether to add legend
#' @param add just like all R graphs, whether to add points or line to an existing graph
#' @param ax if FALSE, the axes are not added
#' @param mortcorr whether to graph the correlation between mortality and population change across species
#' @param debug if TRUE, call browser to debug
#' }
#' 
#' @examples
#' \dontrun{
#' }
#' <source>#' }
#' 
#' }
#'
#' Title: ' find.xaxis.hist
#' }
#'
#' @description
#' Given an abundance fit and x axis range and divisions, return a sequence of x values for drawing the histogram. Used as a subroutine inside graph.abundmodel.
#' }
#'
#' @examples
#' \dontrun{
#' }
#' <source>#' }
#' 
#' }
#'
#' Title: ' abundmodel.fit
#' }
#'
#' @description
#' Simply return the modeled histogram for any set of parameters. Used as a subroutine inside graph.abundmodel.
#' }
#'
#' @examples
#' \dontrun{
#' }
#' <source>#' }
#' 
#' }
