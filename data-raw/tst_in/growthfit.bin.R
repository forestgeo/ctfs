
#'
#'

#' extract.growthdata
#'
#' @description
#'
#' Extract data for growth rates from plot databases and 2 censuses in CTFS R format. Returns a table with 
#' growth, size (ie dbh), and species name. Default is to return log-transformed growth, with negative and zero
#' growth set to a mingrow, but with logit=TRUE, growth and dbh are not log-transformed.
#'
#'
'extract.growthdata'

#' run.growthbin.manyspp
#'
#' @description
#'
#' Run the model to fit growth rate in bins for many species, 1-4 bins. It takes a list of species, extracts
#' growth rates for each, one at a time, from the table of growth rates, then calls run.growthfit.bin to fit the model for
#' the 4 bin options. 
#'
#' Sample species vector from BCI: 
#'
#' @examples
#' \dontrun{
#' spp20=c('tri2tu','alsebl','tet2pa','tachve','beilpe','pri2co','quaras','ocotwh','hirttr','gar2in','protpa','protte',
#'        'eugeoe','virose','guargu','maquco','jac1co','cecrin','cordbi','micoar') 
#'
#' Creating the complete table of biomass growth for all individuals in a plot: 
#' agb.growth=extract.growthdata(bci.full5,bci.full6,growthfunc=growth.biomass.indiv,logit='x',
#'                              rounddown = FALSE,mindbh = 100,dbhunit = 'mm',err.limit = 4,maxgrow = 75) 
#'
#' Creating a vector of all species names in the agb.growth table. 
#' allspecies=sort(unique(agb.growth$sp)) 
#'
#' Fitting the model for all species, 1-4 bins: 
#' fit=run.growthbin.manyspp(growthdata=agb.growth,size='agb',spp=allspecies,minabund300=15,minTotal=40,startpar=c(.03,.005),startsdpar=c(.04,0))}
#'
#'
'run.growthbin.manyspp'

#' run.growthfit.bin
#'
#' @description
#'
#' Find best fits for linearmodel.bin, with one set of data and a series of bins. This starts fit for a bin
#' based on the best fit for the prior bin, thus always assuring an improved fit. This works best if the first bin=1,
#' which is just a linear model and easily fit by optim and the Gibbs sampler.
#'
#'
'run.growthfit.bin'

#' growth.flexbin
#'
#' @description
#'
#' Fitting a regression line through log growth against log dbh, binning on dbh. Growth table has 2 columns,
#' one named size (ie, dbh), other growth. The function now calls optim first to find a set of parameters, then, if Gibbs
#' is selected, runs the Gibbs sampler starting with those parameters. Since optim fails if the initial likelihood is -Inf,
#' it became necessary to check the parameters with bad.binparam before running. 
#'
#'
'growth.flexbin'

#' llike.linearbin.optim
#'
#' @description
#'
#' This is for optim, a single function taking all parameters at once, including sd, to get single likelihood. 
#'
#'
'llike.linearbin.optim'

#' defineBinBreaks
#'
#' @description
#'
#' This finds divisions of over the vector size which produce equal number of elements per division. In case
#' any of those divisions are too short, it tries equal sized divisions. A default to use for
#' start parameters when none are supplied. 
#'
#'
'defineBinBreaks'

#' defineSDpar
#'
#' @description
#'
#' For default SD parameters, if nothing else works. Choose the midpoint of x for the middle of 3 parameters.
#'
#'
'defineSDpar'

#' enoughSamplePerBin
#'
#' @description
#'
#' Test whether the number of elements in a vector x between successive breaks exceeds a minimum. If any
#' bins have too few, it returns FALSE. 
#'
#'
'enoughSamplePerBin'

#' wideEnoughBins
#'
#' @description
#'
#' Test whether all the bin widths exceed a minimum. 
#'
#'
#' MINBIN=0.1
#'
#' MINBINSAMPLE=5
#'
'wideEnoughBins'

#' bad.binparam
#'
#' @description
#'
#' This prevents the bin parameters from moving outside the x range, and keeps the minimum bin width wider than MINBIN of the
#' xrange. It also requires at least MINSAMPLE individuals per bin. The ellipsis handles the submission of MINBIN and MINBINSAMPLE, if
#' they are not submitted, default values are assigned.
#'
#'
'bad.binparam'

#' bad.binsdpar
#'
#' 
#'
#'
'bad.binsdpar'

#' calculateBinModel.BIC
#'
#' @description
#'
#' Calculate Bayes Information Criteria using Wikipedia formula
#'
#'
#' Description: Calculate Deviance Information Criteria, using Wikipedia formula
#'
'calculateBinModel.BIC'

#' calculateBinModel.AIC
#'
#' @description
#'
#' Calculate AIC of the model, using various log(likelihood) estimators: 
#' with optim, the highest likelihood found by optim
#' with mean, the mean llikelihood from the Gibbs sampler
#' with gibbs, the maximum llikelihood from the Gibbs sampler
#' with none, just return the mean Gibbs likelihood
#'
#'
'calculateBinModel.AIC'

#' calculateBinModel.bestpred
#'
#' @description
#'
#' Calculate mean predicted value at every x using every one of the Gibbs sampler parameter combinations
#'(excluding burn in)
#'
#'
'calculateBinModel.bestpred'

#' assembleBinOutput
#'
#' @description
#'
#' Use the list output from piecewise regression (growthfit.bin) and converts to a flat table. The single argument 
#' inputdata is the output compare.growthbinmodel.
#'
#' Written by Adrian Das.
#'(excluding burn in)
#'
#' @examples
#' \dontrun{
#' bciresult=compare.growthbinmodel(linearbin.fit.trim3,export=NULL,makegraph=FALSE)
#' bcitable=assembleBinOutput(bciresult,sitename='bci_12_spreadSD')
#' write.table(bcitable,file='growth/BCIpiecewise.txt',quote=FALSE,sep='\t',row.names=FALSE) }
#'

'assembleBinOutput'
