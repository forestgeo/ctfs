
#'
#'

#' wavelet.allsp
#'
#' @description
#'
#' Function to calculate the wavelet variance curves for all species in one plot using quadrat indexation from CTFS package.
#'
#' @author Matteo Detto and Tania Brenes  
#'
#'
#' Output is a matrix with:   
#' scale: vector with scale of the analysis in meters; 
#' variance: matrix with the normalized variance at each scale (columns) for each species (rows); 
#' density: matrix with the density per area and abundance for each species in the plot; 
#' plotdim: plot dimentions parameter used; 
#' gridsize: grid size parameter used; 
#'
#' UCL: vector with the upper confidence limit for the null hypothesis; 
#'
#' LCL: vector with the lower confidence limit for the null hypothesis.
#'  censdata (): census data for the plot containing the variables gx, gy, dbh, status, and sp code; 
#'  plotdim c(1000,500):  vector with two numbers indicating the plot size; 
#'  gridsize (2.5): gives the size of the quadrats for the rasterization
#'  mindbh (NULL): if analysis is to be done at different size  classes
#'
#' @examples
#' \dontrun{
#' load("bci.full1.rdata") 
#' wavelet.variances = wavelet.allsp(censdata, plotdim=c(1000,500))}
#'
#'
'wavelet.allsp'

#' plot.wavelet
#'
#' @description
#'
#' Function to plot the wavelet variance from the output of the wavelet.allsp. 
#' @author Tania Brenes  
#'
#'  x (): output for wavelet.allsp; 
#'
#'
'plot.wavelet'

#' rasterize
#'
#' @description
#'
#' Function to calculate the count (type='point'), basal area or agb (type='marked') per quadrat 
#' by selecting quadrats of variable sizes.  
#' @author Matteo Detto and Tania Brenes  
#'
#' Output is a list containing a matrix with the raster data.
#'
#'  x , y : x and y coordinates for the point pattern;  
#'  z : marks for the marked point process, can be basal area, agb, etc. Needed only for type='marked';  
#'  gridsize  (20 m): size of the quadrats for the rasterization, ;
#'  plotdim c(1000,500):  vector with plot x-y size; 
#'  type ('point'): type of rasterization: 'point'runs a simple point pattern counting the number of individuals per quadrat; 'marked'does a marked point pattern, where a function (FUN) is applied to the variable z in each quadrat, for example a sum of basal areas;
#'  FUN (sum): function to apply to the point pattern when z is provided. By defult it sums the values of z per quadrat; 
#'  graph (FALSE): logical, plot the heat map of raster data?; 
#'
#' @examples
#' \dontrun{
#' load("bci.full1.rdata") 
#' attach("/home/brenest/Documents/Windocs/WorkFiles/R/Functions/CTFSRPackage.rdata")   
#' onesp = subset(bci.full1, sp=="rinosy")  
#'
#' plots the density of the sp in the plot  
#' rast1 = rasterize(x= onesp$gx, y=onesp$gy, gridsize=5, plotdim=c(1000,500), type='point', graph=TRUE)  
#'
#'#' }
#' marked point process
#'
#'
'rasterize'

#' wavelet.univariate
#'
#' @description
#'
#' Function to calculate the univariate wavelet variance using furier transforms. 
#'
#' It accepts a raster data or a point pattern, which is the default if raster is not provided.  
#'
#' The wavelet variance describes the spatial autocorrelation or aggregation of tree distribution.
#'
#' A wavelet variance greater than 1 indicates scales at which individuals 
#' are aggregated. A wavelet variance less than 1, indicates scales at which 
#' individuals are dis-aggregated. A wavelet variance equal to 1, indicates scales 
#' at which individuals are randomply distribuited (as Poisson process).  
#'
#' A graphical test is implemented on the null hypothesis of complete randomness. 
#'
#' If the wavelet variance is out of the conf bounds the tree distribution 
#' is significantly different from a random process.  
#'
#'
#' Dependencies: needs the package 'spatstat'and the CTFSRpackage
#'
#'
#' Authors: Matteo Detto and Tania Brenes  
#'
#'
#' Output: a list containing vectors for the wavelet variance, the scale of the wavelet variance, the normalized variance, and the confidence intervals. 
#'
#' raster (NULL): used if data is entered already as a raster matrix
#' coords  (): an alternate to a raster table, a table with two (or three) columns giving coordinates x, y (and an optional mark) in that order. This is used to calculate a raster;  
#' gridsize : is the quadrat size of the rasterization;
#' plotdim (c(1000,500)): the dimensions of the plot;
#'
#' FUN (NULL): function to apply to the marked point pattern, if function is not specified (default) it counts points. E.g. can be used to sum the basal areas or above graound biomass
#' k0 (8): numeric. smoothing parameter of the wavelet filter (k0 between 5.5-15), lower values of k0 produce a smoother wavelet variance; 
#' dj  (0.15):  numeric. discretization of the scale axis; 
#' graph (FALSE): logical. If TRUE, plots the wavelet variace as a function of scale 
#'
#' @examples
#' \dontrun{
#' load("bci.full1.rdata") 
#' rast1 = rasterize(, gridsize=5, plotdim=c(100,500), graph=TRUE)
#' wv = wavelet.var(coords=bci.full1[,c("gx","gy")], k0=8, dj=0.15, graph=TRUE)
#' plots the scale of aggregation}
#'
#'
'wavelet.univariate'

#' wavelet.bivariate
#'
#' @description
#'
#' Function to calculate the wavelet variance to evaluate the association between two point patterns using furier transforms. 
#'
#' It accepts a raster data or a point pattern, but the type of data entered has to be specified in the argument type.  
#'
#' The wavelet variance describes the spatial autocorrelation or aggregation of point distribution.
#'
#' A wavelet variance greater than 1 indicates scales at which individuals 
#' are aggregated. A wavelet variance less than 1, indicates scales at which 
#' individuals are dis-aggregated. A wavelet variance equal to 1, indicates scales 
#' at which individuals are randomply distribuited (as Poisson process).  
#'
#' A graphical test is implemented on the null hypothesis of comple randomness. 
#'
#' If the wavelet variance is out of the conf bounds the point distribution 
#' is significantly different from a random process.  
#'
#' Dependencies: needs the package 'spatstat'and the CTFSRpackage 
#'
#' Authors: Matteo Detto and Tania Brenes  
#'
#' Output: a list containing vectors for the wavelet variance, the scale of the wavelet variance, and the normalized variance. 
#'
#' coords1  (): a matrix with raster data OR a table with two or three columns that can be used to calculate a raster, the first two columns are the coordinates and the third is the mark. Type must be specified ;  
#' coords2 (): 2nd dataset for the bivariate analyisis; 
#' type ('raster'): the type of data entered, 'raster'if a raster matrix, 'point'for a point pattern, or 'marked'for a marked point pattern;
#' gridsize : is the quadrat size of the rasterization;
#' plotdim (c(1000,500)): the dimensions of the plot;
#'
#' FUN ('sum'): function to apply to the marked point pattern, by default it sums the values as would be used for sum of basal areas or sum of above graound biomass
#' k0 (8): numeric. smoothing parameter of the wavelet filter (k0 between 5.5-15), 
#' lower values of k0 produce a smoother wavelet variance; 
#' dj  (0.15):  numeric. discretization of the scale axis; 
#' graph (TRUE): logical. plot the wavelet variace ? 
#'
#' @examples
#' \dontrun{
#' load("bci.full1.rdata") 
#' sp.one = subset(bci.full7, sp=="quaras")[,c("gx","gy")] 
#' sp.two = subset(bci.full7, sp=="cordal")[,c("gx","gy")]
#' wv = wavelet.bivariate(coords=sp.one, coords2=sp.two, type='point', rk0=8, dj=0.15, graph=TRUE)
#' plots the scale of aggregation}
#'

'wavelet.bivariate'
