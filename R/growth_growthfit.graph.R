
#' 
#' 
''


#' graph.growthmodel.spp
#'#'
#' @description
#' Use output of growth.flexbin to graph observed growth and predictions. With conf>0, random draws from posterior parameters
#' are used to make predictions and overlay with gray lines, and mean predicted value at each x is calculated.
#' The option whichpred determines whether to graph the fitted values based on the median parameters of the Gibbs sampler, or
#' the mean fitted value of all the steps in the Gibbs sampler.  
#'#'
#' 
#' @examples
#' \dontrun{
#'#' 
#' #' }
#' 
#' 
'graph.growthmodel.spp'


#' graph.growthmodel
#'#'
#' @description
#' Graph growth rates and model fit. 
#'#'
#' 
#' @examples
#' \dontrun{
#'#' 
#' #' }
#' 
#' 
'graph.growthmodel'


#' overlay.growthbinmodel
#'#'
#' @description
#' Show model fits for 1, 2, 3, and 4 bins on each species
#'#'
#' 
#' @examples
#' \dontrun{
#'#' 
#' #' }
#' 
#' 
'overlay.growthbinmodel'


#' compare.growthbinmodel
#'#'
#' @description
#' Calculates various metrics of fit: DIC, BIC, AIC based on the maximum likelihood, AIC based on the mean of the Gibbs sampler,
#' from output of the model fit (all species, all bins 1:4). 
#'#'
#' 
#' @examples
#' \dontrun{
#'#' 
#' #' }
#' 
#' 
'compare.growthbinmodel'


#' graph.outliers.spp
#'#'
#' @description
#' Pass the output of extract.growth with every individual\'s growth (full), and another after outliers have been trimmed. This finds
#' the records trimmed and overlays them on the entire graph. If a model is submitted, then the curves are graphed too.
#'#'
#' 
#' @examples
#' \dontrun{
#' @param full=extract.growthdata(census1=bci.full5,census2=bci.full6,growcol='incgr',growthfunc=growth.biomass.indiv,logit='x',rounddown = FALSE,
#'                         mindbh = 100,dbhunit = 'mm',err.limit = 4000,maxgrow = 7500)
#' @param trimmed=extract.growthdata(census1=bci.full5,census2=bci.full6,growcol='incgr',growthfunc=growth.biomass.indiv,logit='x',rounddown = FALSE,
#'                            mindbh = 100,dbhunit = 'mm',err.limit = 4,maxgrow = 75)
#'#' 
#' #' }
#' 
#' 
'graph.outliers.spp'


#' graph.outliers
#'#'
#' @description
#' Graph the outliers overlaid on the model and full data for all species in a model fit result.
#' The argument export can be set to a window device (X11, win.graph, quartz) or a graphics option (png, pdf).
#'#'
#' 
#' @examples
#' \dontrun{
#'#' 
#' #' }
#' 
#' 
'graph.outliers'


#' binGraphSampleSpecies
#'#'
#' @description
#' Make a single graph, 4 panels, of AGB growth and model fit. Must submit four species names (though they could be
#' the same. Enter names of data objects (not the object, just the name in quote marks!) either one data object, or a list of four such objects; 
#' if only one, all four graphs are based on the single one. Likewise, enter either one bin number, or four.
#'#'
#' @examples
#' \dontrun{
#' attach('growth/linearbin.fittrim3.bci.rdata')
#' attach('growth/growthfitYiching/linearbin.fit.allspp.rdata')
#' attach('growth/linearbin.fit.edoro.rdata')
#' attach('growth/linearbin.fit.lenda.rdata')
#' binGraphSampleSpecies(fulldata='linearbin.fit.allspp',species=c('pri2co','tri2tu','brosal','jac1co'),whichbin=2,export=NULL)
#' binGraphSampleSpecies(fulldata=c('linearbin.fit.allspp','linearbin.fit.trim3','linearbin.fit.edoro3','linearbin.fit.lenda3'),
#'                        species=c('PYRESH','pri2co','JULBSE','GILBDE'),whichbin=3,export=NULL)
#' binGraphSampleSpecies(fulldata='linearbin.fit.allspp',species=c('pri2co','pri2co','brosal','brosal'),whichbin=c(2,3,2,3),export=NULL)
#'#' #' }
#' 
#' 
'binGraphSampleSpecies'


#' binGraphManySpecies.Panel
#'#'
#' @description
#' Make a graph, 4 panels, of AGB growth and model fit of many species overlaid, predicted functions only. Must submit names of four data objects 
#' (not the object, just the name in quote marks!).
#'#'
#' @examples
#' \dontrun{
#' binGraphManySpecies.Panel(c('linearbin.fit.allspp','linearbin.fit.trim3','linearbin.fit.edoro3','linearbin.fit.lenda3'),
#'                           export=pdf,yrange=c(0,.25),sitename=c('Fushan','BCI','Edoro','Lenda'),darken=5,xrange=c(-3,3.5))
#'#' #' }
#' 
#' 
'binGraphManySpecies.Panel'


#' binGraphManySpecies
#'#'
#' @description
#' Make a graph of AGB growth and model fit of many species overlaid, predicted functions only. Must submit name of data objects 
#' (not the object, just the name in quote marks!).
#'#'
#' @examples
#' \dontrun{
#'#' #' }
#' 

'binGraphManySpecies'
