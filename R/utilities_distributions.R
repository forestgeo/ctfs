
#'
#'#'
#'


#' doublenormal
#'#'
#' @description
#' Simulate draws from a double normal
#'#'
#'
#'
#'
#'#'
#'#'
'doublenormal'


#' dnormprod0
#'#'
#' @description
#' Probability density of product of two normal variates, both with mean 0, SD sx and sy
#'#'
#'
#'
#'
#'#'
#'#'
'dnormprod0'


#' dgammadexp
#'#'
#' @description
#' PDF of a function formed by adding a gamma distribution to a symmetrical exponential 
#' distribution. This means simply adding a PDF for a gamma minus an exponential to the
#' PDF for a gamma plus an exponential.
#'#'
#'
#'
#'
#'#'
#'
'dgammadexp'


#' dgammaMinusdexp
#'#'
#' @description
#' The PDF of the difference between a gamma and a negative exponential distribution. The shape and rate of the gamma 
#' are a and r; mean and sd are the mean and sd of the gamma. Lambda is the rate of the exponential. 
#' This comes from the convolution of the two distributions, which is also a gamma, and the integral 
#' of the new gamma evaluated with pgamma. Note that lambda is the rate of the exponential.
#'#'
#'
#' @examples
#' \dontrun{
#'#'
#'#' browser() 
#'#'
#'#'
#'#'
'dgammaMinusdexp'


#' dgammaPlusdexp
#'#'
#' @description
#' The PDF of the sum of gamma and negative exponential distribution. The shape and rate of the gamma 
#' are a and r; mean and sd are the mean and sd of the gamma. Lambda is the rate of the exponential. 
#' This is only an appoximation based on the observation that the resulting distribution is very
#' close to a gamma. So I simply work out a new gamma whose mean is the sum of the means of
#' the initial gamma and exponential, and likewise for the new variance. 
#' As long as gamma\'s rate > the exponential lambda, the distribution
#' can be specified (using pgamma) as in dgammaMinusdexp. But if rate < lambda, this fails.
#' The gamma approximation fails if the sd is sufficiently higher than the mean, and the mean
#' is low. Then the gamma is absurdly skewed, and the shape of the sum is dominated by the exponential
#' at low z, nothing like a true gamma. It appears to work reasonably well as long as the
#' mean >= sd, or even if mean>=0.5*sd.
#'#'
#'
#'
#'
#'#'
#'
'dgammaPlusdexp'


#' dgamma.meansd
#'#'
#' @description
#' Probability distribution of gamma, parameterized with mean and sd instead of shape and scale.
#'#'
#'
#'
#'
#'#'
#'
'dgamma.meansd'


#' rgamma.meansd
#'#'
#' @description
#' Random draws of gamma, parameterized with mean and sd instead of shape and scale.
#'#'
#'
#'
#'
#'#'
#'
'rgamma.meansd'


#' dgamma.mean
#'#'
#' @description
#' A version of dgamma where the parameters are ordered so that the mean (mu = shape x scale) is the first argument.
#' The second argument is x, the value at which dgammma is evaluated, and the third is scale. This is needed
#' for use with mcmc1step, to do a metropolis step on the mean. Only mu can be a vector.
#'#'
#'
#'
#'
#'#'
#'#'
'dgamma.mean'


#' dgamma.scale
#'#'
#' @description
#' Like above, but with scale as the first parameter.
#'#'
#'
#'
#'
#'#'
#'#'
'dgamma.scale'


#' dpower
#'#'
#' @description
#' A probability distribution defined by a power function. There is a dpareto in R, but quite different, with
#' two parameters. In this, the exponent is the only parameter, and it must be < (-1); x must be >= 0.
#'#'
#'
#'
#'
#'#'
#'#'
'dpower'


#' rpower
#'#'
#' @description
#' Random draws based on the integral.
#'#'
#'
#'
#'
#'#'
#'
'rpower'


#' dasympower
#'#'
#' @description
#' A bilateral power distribution, centered at center, decaying with exponent rate1 for positive x and rate2 for negative x. Both rate1 and rate2
#' must be < (-1). See dpower, this is analogous to dasymexp for dpower. By R. Chisholm. 
#'#'
#'
#'
#'
#'#'
#'
'dasympower'


#' rasympower
#'#'
#' @description
#' Random draws from the bilateral power distribution, dasympower. By R. Chisholm. 
#'#'
#'
#'
#'
#'
'rasympower'


#' qasympower
#'#'
#' @description
#' Quantiles from the bilateral power distribution, dasympower. By R. Chisholm. 
#'#'
#'
#'
#'
#'
'qasympower'


#' dsymexp
#'#'
#' @description
#' Probability distribution for a folded, symmetrical exponential. When x>=center, 
#' it's just a standard exponential. When x<center, it's the mirror image of same one.
#' Each must be divided by two, though, in order to integrate to one.
#'#'
#'
#'
#'
#'#'
#'#'
'dsymexp'


#' psymexp
#'#'
#' @description
#' The CDF for the symmetric exponential.
#'#'
#'
#'
#'
#'#'
#'#'
'psymexp'


#' rsymexp
#'#'
#' @description
#' Drawing a random variate on the symmetric exponential, based on the cumulative 
#' probability, as given in psymexp. A random uniform number on (0,1) is plugged in 
#' the inverse of the cumulative distribution.
#'#'
#'
#'
#'
#'#'
#'#'
'rsymexp'


#' dasymexp
#'#'
#' @description
#' Probability distributions for a folded but asymmetrical exponential. 
#' When x>=center, it's a standard exponential. When x<center, it's the mirror image 
#' of a different exponential; rate1 refers to the right half, rate2 to the
#' left. The center is not the median: the section x>center has integral rate2/(rate1+rate2),
#' and the section x<center rate1/(rate1+rate2). 
#'#'
#'
#'
#'
#'#'
'dasymexp'


#' qasymexp
#'#'
#' @description
#' Quantiles of dasymexp 
#'#' y is the vector of desired quantiles; c is the center parameter; rate1 is the rate for the right half, and rate2 the left.
#'#'
#'
#'#'
'qasymexp'


#' dasymexp
#'#'
#' @description
#' Probability distributions for an asymmetrical Gaussian, that is with different standard deviations
#' above and below the mode, or center. The mode is not the mean, though. The SD on the right is sigma1,
#' and on the left, sigma2. 
#'#'
#'
#'
#'
#'#'
'dasymexp'


#' minum.normal
#'#'
#' @description
#' The likelihood function for use by fitnorm.
#'#'
#'
#'
#'
#'#'
#'#'
'minum.normal'


#' fitnorm
#'#'
#' @description
#' Fitting a normal distribution to data
#' Parameters are a mean and sd of the normal being fitted, a scaling parameter k
#' and the last SD is for the likelihood of the deviations.
#'#' y is vector data to be fitted, at points x
#'#'@examples
#' \dontrun{
#'#'
#'#' y=y/sum(y)
#'#'
#'#'
#'#'
'fitnorm'


#' fit.pdf
#'#'
#' @description
#' Fit a random variable x to any submitted probability distribution. The number of start parameters
#' must match what the pdf needs.
#'#'
#'
#'
#'
#'#'
#'#'
'fit.pdf'


#' default.badpar
#'#'
#' @description
#' None given.
#'#'
#'
#'
#'
#'#'
#'#'
'default.badpar'


#' bad.paretopar
#'#'
#' @description
#' Test whether parameters for the Pareto distribution are acceptable. 
#'#'
#'
#'
#'
#'#'
#'#'
'bad.paretopar'


#' normalproduct
#'#'
#' @description
#' A function which returns the product of 2 normal distributions, the first
#' at x (a vector), the second at lag-x (lag is a scalar). The mean and SD 
#' of the second normal are linear functions of x, with meanint being the
#' intercept, meanslope the slope, and CV the coefficient of variation.  
#' A convolution!
#'#'
#'
#'
#'
#'#'
#'#'
'normalproduct'


#' dbeta.reparam
#'#'
#' @description
#' This reparameterizes the beta distribution as a function of its mean and
#' standard deviation. The mean must be between 0 and 1, and sd>0.
#'#'
#'
#'
#'
#'#'
#'#'
'dbeta.reparam'


#' betaproduct
#'#'
#' @description
#' This is equivalent to the normal product above.
#'#'
#'
#'
#'
#'#'
#'#'
'betaproduct'


#' beta.normalized
#'#'
#' @description
#' Normalzing beta.total. No longer used. 
#'#'
#'
#'
#'
#'#'
#'#'
'beta.normalized'


#' beta.total
#'#'
#' @description
#' A beta distribution on the interval xmin to xmax, instead of 0 to 1. No longer used. 
#'#'
#'
#'
#'
#'#'
#'#'
'beta.total'


#' fit.beta.normal
#'#'
#' @description
#' Finding a normal distribution which most closely fits a given beta distribution.
#' Parameters for a beta function are submitted, and the best fit mean and SD of a
#' normal distribution returned.
#'#'
#'
#'
#'
#'#'
#'#'
'fit.beta.normal'


#' minum.beta.normal
#'#'
#' @description
#' Function to be minimized for fitting normal to beta.
#'#'
#'
#'
#'
#'#'
#'#'
'minum.beta.normal'


#' dbinomrev
#'#'
#' @description
#' A version of dbinom in which parameters are submitted in a different order. 
#'#'
#'
#'
#'
#'#'
#'#'
'dbinomrev'


#' dnormrev
#'#'
#' @description
#' This reverses the order of parameters to dnorm, so that outer can be used
#' with a vector of x, and two vectors for mean and sd (the latter two equal in
#' length). 
#'#'
#'
#'
#'
#'#'
#'#'
'dnormrev'


#' dpois.rearrange
#'#'
#' @description
#' This rearranges dpois so that it works on a single vector, with the first
#' element being x and the remaining all being used as lambdas.
#'#'
#'
#'
#'
#'#'
#'#'
'dpois.rearrange'


#' logit
#'#'
#' @description
#' Logit transformation for a probability >0 and < 1
#'#'
#'
#'
#'
#'#'
#'#'
'logit'


#' invlogit
#'#'
#' @description
#' Inverse logit transformation, turns a logit back into a probability.
#'#'
#'
#'
#'
#'#'
#'#'
'invlogit'


#' pweibull.3param
#'#'
#' @description
#' CDF of three-parameter Weibull
#'(http://www.itl.nist.gov/div898/handbook/apr/section1/apr162.htm)
#'#'
#'
#'
#'
#'#'
#'#'
'pweibull.3param'


#' dweibull.3param
#'#'
#' @description
#' PDF of three-parameter Weibull
#'#'
#'
#'
#'
#'#'
#'#'
'dweibull.3param'


#' weibull.median.3param
#'#'
#' @description
#' Median of three-parameter Weibull
#'#'
#'
#'
#'
#'#'
#'#'
'weibull.median.3param'


#' weibull.mean.3param
#'#'
#' @description
#' Mean of three-parameter Weibull
#'#'
#'
#'
#'
#'#'
#'#'
'weibull.mean.3param'


#' weibull.sd.3param
#'#'
#' @description
#' SD of three-parameter Weibull
#'#'
#'
#'
#'
#'#'
#'#'
'weibull.sd.3param'


#' dexp.sin
#'#'
#' @description
#' Four-parameter exponential sin, as a probability distribution
#'#'
#'
#'
#'
#'#'
#'#'
'dexp.sin'


#' exponential.sin
#'#'
#' @description
#' Five-parameter exponential sin
#'#'
#'
#'
#'
#'#'
#'#'
'exponential.sin'


#' pexp.sin
#'#'
#' @description
#' CDF of four-parameter exponential sin
#'#'
#'
#'
#'
#'#'
#'#'
#'
'pexp.sin'


#' mvrnormRC
#'#'
#' @description
#' Function that takes a variance-covariance matrix and produces normal variates
#' following it, but with means 0. The R function mvrnorm does this too; this was a 
#' test of the algorithm from Tommaso Zillio. Sigma must be square. N is the number
#' to draw.
#'#'
#'
#'
#'
#'#'
#'#'
'mvrnormRC'


#' dmixnorm
#'#'
#' @description
#' Mixed normal distribution. The parameter f is the probability 
#' of following the first, with mean1 and sd1; 1-f is the probability
#' for the second normal
#'#'
#'
#'
#'
#'#'
#'#'
'dmixnorm'


#' rmixnorm
#'#'
#' @description
#' Random draw on the mixed normal distribution.
#'#'
#'
#'
#'
#'#'
#'#'
'rmixnorm'


#' minum.mixnorm
#'#'
#' @description
#' Fit a mixture of 2 normals.
#'#'
#'
#'
#'
#'#'
#'#'
'minum.mixnorm'


#' logistic.inter
#'#'
#' @description
#' Logistic function with intercept parameterization (ie, first parameter is y when all x=0). The input x are all independent variables, in a matrix
#' with each column one of the variables. The number of rows is the number of datapoints. Just one inter, which is the value
#' at all x=0, and passed as param[1]. Slope parameters follow, one per column of x. 
#' This is identical to standard 
#'#'
#'
#'
#'
#'#'
#'
'logistic.inter'


#' logistic.standard
#'#'
#' @description
#' This is standard logistic function, but with asymptote and basement allowed. The latter are only implemented
#' if extra parameters are passed. Moved from calc.surviv.r on 25 July 2010 to provide the standard logistic. 
#'#'
#'
#'
#'
#'#'
'logistic.standard'


#' logistic.power
#'#'
#' @description
#' This is the Gaussian logistic function, where logit is a second-order polynomial of x; with asymptote and basement allowed. 
#' There must be 1+2*nopredictors parameters; the asympotote and basement are only implemented
#' if extra parameters are passed.  
#'#'
#'
#'
#'
#'#'
'logistic.power'


#' logistic.power.mode
#'#'
#' @description
#' This is the Gaussian logistic function, where logit is a second-order polynomial of x, but with third parameter the position
#' of the critical point (peak or trough). Given 3 parameters for standard logistic.power, the mode is at -param[2]/2*param[3]).
#' Asymptote and basement are allowed. There must be 1+2*nopredictors parameters; the asympotote and basement are only implemented
#' if extra parameters are passed.  
#'#'
#'
#'
#'
#'#'
'logistic.power.mode'


#' logistic.power_simple
#'#'
#' @description
#' This is a mixture of logistic and logistic-standard models. The predictors n get a power model, the remaining a simple
#' model. So if nopredictors==8, and n=c(1,7), then the first and seventh predictors use a power model, while the rest a simple model.
#' There must be 1+length(n)+nopredictors parameters, plus additional 1 or 2 for asymptote and basement.  
#'#'
#'
#'
#'
#'
'logistic.power_simple'


#' logistic.ctr
#'#'
#' @description
#' This is logistic function with intercept parameterization (see logistic above), but with centering on x allowed. 
#' If center==NA, then the x values are centered on their median.
#' Or center can be a number. If NULL, no centering is done. 
#' Moved from calc.surviv.r on 25 July 2010 to provide the standard logistic. 
#'#'
#'
#'
#'
#'#'
#'#'
'logistic.ctr'


#' logistic.multiplicative
#'#'
#' @description
#' Logistic with a pair of parameters for each x; y=product of all the logistics. First set of parameters are intercepts, then
#' an equal number of slopes. If there are additional parameters, they are asymptote and basement.
#'#'
#'
#'
#'
#'#'
#'#'
'logistic.multiplicative'


#' constant
#'#'
#' @description
#' A function to return a constant at all predictors x. The predictors are a numeric vector, or a matrix of
#' many predictors (each column a single predictor). This function is useful in modeling, where the name of a function
#' is passed; this allows modeling where a response is a constant across all values of x. 
#'#'
#'
#'
#'
#'#'
#'
'constant'


#' center.predictors
#'#'
#' @description
#' Transform all data by subtracting a constant, either the mean, median value, or a submitted constant. 
#' The input may be a vector or a matrix. If a matrix, each column is centered on its mean (median), or by passing a
#' vector of constants. Note that setting by=0 amounts to no change. 
#'#'
#'
#' @examples
#' \dontrun{
#' center.predictors(x=c(1,4,7,0),by='median')
#' center.predictors(x=c(1,4,7,0),by=2)}
#'
#'#'
#'#'
'center.predictors'


#' fit.logistic
#'#'
#' @description
#' A function to fit a set of data y, observed at the vector x, to a generalized
#' logistic function, using least squares.
#'#'
#'
#'
#'
#'#'
#'#'
'fit.logistic'


#' logistic.sum.squares
#'#'
#' @description
#' Sets a prediction based on a generalized logistic, then returns the sum
#' of squared deviations
#'#'
#'
#'
#'
#'#'
#'
'logistic.sum.squares'


#' asymp.ht
#'#'
#' @description
#' The function from Sean Thomas which produces an asymptote for y as a function of x. 
#' Original version: y=ymax*(1-exp(-a*x^b))
#' This is the centered version, with x normalized by dividing by parameter k, which is the x value at which
#' y is half ymax. This eliminates correlation between the a and b parameters in the above version, but
#' not the correlation between parameters 1 and 2.
#'#'
#'
#'
#'
#'#'
#'
'asymp.ht'


#' asymp.ht.fixmax
#'#'
#' @description
#' Same formulation, but the asymptote is fixed, so only two parameters fitted.
#'#'
#'
#'
#'
#'#'
#'#'
'asymp.ht.fixmax'


#' exp.2par
#'#'
#' @description
#' An exponential distribution with an asymptote.
#'#'
#'
#'
#'
#'#'
#'#'
'exp.2par'


#' linear.model
#'#'
#' @description
#' A simple linear model, where the first parameter is intercept, remaining parameters are slopes
#'#'
#'
#'
#'
#'#'
#'
'linear.model'


#' simple.model
#'#'
#' @description
#' A trivial model to return a different value at every x. If param is atomic, then that value is returned for every x. Otherwise, param must be a vector of same size as x, and param is returned. 
#'#'
#'
#'
#'
#'
'simple.model'


#' simple
#'#'
#' @description
#' An even more trivial model to return x unchanged. The argument param is passed but not used. 
#'#'
#'
#'
#'
#'
'simple'


#' linear.model.ctr
#'#'
#' @description
#' A simple linear model, where the first parameter is intercept, second the slope, and x can be centered on their median. 
#'#'
#'
#'
#'
#'#'
#'
'linear.model.ctr'


#' expon.model
#'#'
#' @description
#' Exponential model, y = a exp(b1*x1 + b2*x2) for any number of predictors x. Compare to linear.model. 
#'#'
#'
#'
#'
#'#'
'expon.model'


#' log.model
#'#'
#' @description
#' Logarithmic model, y = a + b1 log(x1) + b2 log(x2) for any number of predictors x. Compare to linear.model. All x should be positive.
#'#'
#'
#'
#'
#'#'
'log.model'


#' constant.linear
#'#'
#' @description
#' A model which is constant for x<lim, and linear for x>lim. The first parameter is the slope, 
#' second the x value of break point, third the lower limit.
#'#'
#'
#'
#'
#'#'
#'#'
'constant.linear'


#' linearmodel.bin
#'#'
#' @description
#' Multiple bin model predicting y as a function of x in several bins. Within each bin, y is a linear function of x. 
#' A model with B bins has B-1 parameters for breaks points (initial B-1 parameters), B parameters as slopes (next B parameters), and one intercept (last parameter).
#' Intercept is assigned at x=0 by default, but argument LINEARBINMEDIAN can be used to change. 
#' This function accepts one set of parameters, separates the bin, slope, and intercept, and submits to the
#' general version of the function (linearmodel.bin.set). 
#'#'
#'
#'
#'
#'#'
#'#'
'linearmodel.bin'


#' linearmodel.bin.set
#'#'
#' @description
#' This does the work of calculating predicted values at each independent variable, given bin and line parameters separately, 
#' the latter being slope and intercept parameters in one vector. 
#' Completely revised June 2011 to use geometry.r functions for lines.
#' Create a list of lines, one for each bin, and an intersection (intercept), one for each bin:
#' This function cannot handle one bin, and linearmodel.bin escapes with linear.model if only one bin is sought
#'#'
#'
#'
#'
#'#'
#'#'
'linearmodel.bin.set'


#' addBinParam
#'#'
#' @description
#' Given parameters for a model with N linear bins, creates parameters for N+1 bins which produce the same model. 
#'#'
#'
#'
#'
#'#'
#'
'addBinParam'


#' logisticmodel.bin
#'#'
#' @description
#' Multiple bin model predicting y as a function of x, where each segment is modeled as a standard logistic.
#' A model with B bins has B-1 parameters for breaks points, B parameters as slopes, and one intercept (y at x=0).
#' Within each bin, y is a linear function of x. 
#' Predictor can centered at medv. 
#' This function accepts a set of parameters, submits to linearmodel.bin, then returns invlogit.
#'#'
#'
#'
#'
#'#'
'logisticmodel.bin'


#' constant.bin
#'#'
#' @description
#' A model like piecewise regression (linearmodel.bin), but y is a constant within each bin.
#' With B bins, 2B-1 parameters are needed. First B-1 parameters are bin breaks. The remaining B
#' parameters are the constant value of y in each bin. 
#'#'
#'
#'
#'
#'#'
#'#'
'constant.bin'


#' dpois.max
#'#'
#' @description
#' A probability distribution which is simply a curtailed poisson: all probability above a maximum integer,
#' maxx, is given to that maximum. For all x<maxx, the probability is just poission. No normalization is needed, due
#' to this definition. 
#'#'
#'
#' @examples
#' \dontrun{
#'#'
#'#'
'dpois.max'


#' dpois.trunc
#'#'
#' @description
#' A zero-truncated Poisson distribution. 
#'#'
#'
#'
#'
#'#'
'dpois.trunc'


#' dpois.maxtrunc
#'#'
#' @description
#' A zero-truncated Poisson distribution with a ceiling (combining dpois.max and dpois.trunc). 
#'#'
#'
#'
#'
#'#'
#'
'dpois.maxtrunc'


#' rpois.max
#'#'
#' @description
#' Random draws on dpois.max
#'#'
#'
#'
#'
#'#'
#'#'
'rpois.max'


#' rpois.trunc
#'#'
#' @description
#' Random draws on dpois.trunc. This is taken unchanged from an answer Peter Dalgaard posted to a list serve in 2005. I checked
#' by comparing to dpois.trunc and it was spot on. 
#'#'
#'
#'
#'
#'#'
#'#'
'rpois.trunc'


#' asymptote.exp
#'#'
#' @description
#' A 3-parameter function which asymptotes as x->infinity. The 3rd param must be >=0 and x>=0. The asymptote is a, the intercept a-b.
#'#'
#'
#'
#'
#'#'
#'#'
'asymptote.exp'


#' graph.mvnorm
#'#'
#' @description
#' Graphs contours for an mvnorm, with parameters submitted as a
#' vector, as described above, for a single 2D Gaussian.
#' The probability has to be calculated on a grid so contours can be drawn.
#' The argument exclude allows parts to be set to zero.
#'#'
#'
#'
#'
#'#'
#'
'graph.mvnorm'


#' pospower
#'#'
#' @description
#' Raise to any power, but with negative numbers converted to positive first, then reverted afterward. It thus follows what is normal behavior when
#' the exponent were a negative integer, but works also for even integers or any real exponent. For example, pospower(-4,0.5)=-2.
#'#'
#' @examples
#' \dontrun{
#' pospower(-4,0.5)}
#'
#'#'
#'
'pospower'


#' linear.mortmodel
#'#'
#' @description
#' A model for mortality as a function of one or more predictors, with the time interval for each individual incorporated (as a last predictor).
#' The log(mortality parameter) is modeled as a linear function of x[,-nopred].  The return value is a survival probability. Nothing prevents the output from
#' being outside (0,1); that must be handled in the likelihood function.
#'#'
#'
#'
#'
#'#'
#'
'linear.mortmodel'


#' discrete.mortmodel
#'#'
#' @description
#' A model for mortality as a function of a single discrete predictor, with the time interval for each individual incorporated (as a second predictor).
#' The predictor must be a factor, so the total number of levels is known. The log(mortality parameter) is modeled as a different value for each distinct predictor. The number of parameters must exceed the maximum value of the predictor. 
#' The return value is a survival probability. Nothing prevents the output from being outside (0,1); that must be handled in the likelihood function.
#'#'
#'
#'
#'
#'#'
#'
'discrete.mortmodel'


#' discrete.model
#'#'
#' @description
#' A model for a numeric response to a single discrete predictor. The predictor must be a factor, so the total number of levels is known. The response is modeled as a different value for each distinct predictor. The number of parameters must exceed the maximum value of the predictor. 
#'#'
#'
#'
#'
#'
'discrete.model'
