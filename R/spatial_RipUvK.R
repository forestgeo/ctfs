
#'
#'


#' RipUvK
#'#'
#' @description
#' Ripley's K using splancs. Computes the univariate Ripley's K for a list of species (all species at a site 
#' is the default). Also calculates the O-ring statistic, or density (per area) of
#' conspecifics in a series of annuli defined by rseq, and the omega statistic,
#' which is O-ring divided by overall density.
#' Works through all the elements of a split datafile, each of which must have gx, gy
#' coordinates, a status code, and dbh. This would usually be species, but could
#' be any other division of the data. RipUvK always performs calculations on each element of the
#' splitdata list submitted.
#' If the data have no status field, all records are included. If there is a status field, only "A"
#' values are included. If mindbh is set to 0, all records are
#' included and no dbh field is consulted. But if mindbh is set, there must be a dbh field, and records
#' are excluded if smaller.
#' The output is a list of 5 components.
#' @param K is the Ripley's K object produced by the khat function in the splancs package; please see help for that package for details.
#' @param O is the mean density of neighbors in each successive distance interval; it is a list of vectors, one per species, each vector of length matching length of rseq.
#' @param omega is the omega-statistic, or O in each successive distance interval divided the plot-wide density; density is expressed per 10,000 square distance units, which means per hectare if units are meters.
#' @param abund is a vector of species abundances, including individuals used in the calculation.
#' @param midpts is a vector of the same length as rseq, giving the midpoint of each distance interval, for graphing purposes.}
#' @param splitdata A complete CTFS plot dataset comprised of a list, each element having a dataframe for one species. There must be columns for x and y coordinates; the names of those two columns are passed as arguments (default is the CTFS standard gx, gy). 
#' @param plotdim The x and y dimensions of the plot. 
#' @param rseq The distances defining intervals in which Ripley's statistic is calculated.
#' @param mindbh the minimum dbh to include in results.
#' @param xcol and ycol the names of the columns holding x and y coordinates.
#' @param debug TRUE or FALSE, calls browser during execution to pause program; only set TRUE to debug.
#' @param show creates a graph to show one omega value for each species, as a way to track progress.
#'#'
#' @examples
#' \dontrun{
#' If a split database has been created by split.data and stored, CTFSplot will load it.
#' CTFSplot("bci",6,type="split")
#' Otherwise start with a standard R Analytical Table (could be stem also):
#' CTFSplot("bci",6,type="full")
#' bci.split6=split.data(bci.full6)
#' A quick test, run on only the first 10 species:
#' rip=RipUvK(splitdata=bci.split6[1:10],plotdim=c(1000,500),rseq=c(10,20,30,40,50,60),mindbh=10,xcol="gx",ycol="gy")
#' All the species (takes several minutes):
#' rip=RipUvK(splitdata=bci.split6,plotdim=c(1000,500),rseq=c(10,20,30,40,50,60),mindbh=10,xcol="gx",ycol="gy")
#' str(rip$K[[1]])
#' plot(rip$midpts,rip$omega[2,],ylim=c(0,10))
#'#'#' calculate K and the (number of conspecific) individuals for each tree within 
#' distances of rseq
#' }
#'#'
#'
'RipUvK'


#' Annuli
#'#'
#' @description
#' Calculates the total area in a series of annuli, summed over all individuals in the
#' submitted dataset. Calls CalcRingArea to do the calculations for the area within
#' a distance r of every individual, then subtracts successive areas to get 
#' the area within annuli. This is ordinarily only used as a subroutine of RipUvK.
#'#'@param spdata A single dataframe with x-y coordinates of individuals. 
#' @param r A vector of distances defining the successive annuli.
#' @param plotdim The x and y dimensions of the plot in which the individuals are mapped. 
#'#'
#'
#'
#'#'
'Annuli'


#' CalcRingArea
#'#'
#' @description
#' Given a dataframe that includes gx, gy coordinates, and a single value radius,
#' finds the area within radius from all points (summed). In the 
#' absence of edge corrections, this would be simply N*pi*radius^2, where N is the
#' number of points. But this uses the function partialcirclearea to do the edge
#' correction. This is ordinarily only used as a subroutine for RipUvK.
#'#'
#'}
#'
'CalcRingArea'


#' circlearea
#'#'
#' @description
#' Simply returns area of a circle of radius r.
#'#'
#'
#'
#'
'circlearea'


#' partialcirclearea
#'#'
#' @description
#' Calculates the area of a circle of radius r that is inside a rectangular plot.  
#' The distance from the circle center to plot edges are c2, cy1, cy2, where cy1 is the shortest
#' distance to a y-boundary and cy3 the longest, while c2 is the shortest x distance.
#' The longest x distance is not needed. 
#' This will not work if the longest x distance < radius of the
#' circle.  The greatest radius allowed is thus half the x dimension of the plot, generally
#'250 or 500 m.
#' Ordinarily only used as a subroutine of RipUvK.
#'#'
#'
#'
#'
'partialcirclearea'
