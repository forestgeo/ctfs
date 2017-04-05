
#'
#'

#' extract.growthdata
#'
#' bad.binsdpar'

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
