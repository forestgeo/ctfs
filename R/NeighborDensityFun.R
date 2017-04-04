
#'
#'
#'


#' NeighborDensities
#'#'@description
#' Calculates the total number or basal area of conspecific and heterospecific neighbors at a radius r 
#' with edge effect correction. Most arguments aim to describe the type of neighbors to count  
#' Dependencies: CalcRingArea and circlearea from the CTFS package, and splancs package. 
#' Output: a data.frame with two columns listing the density of consp and hetsp, and rows equal to the size of censdata if censdata2 is absent, or censdata2 if it is present. 
#' @author Tania Brenes 
#'#'  @param  censdata should have the coordinates (gx, gy), species id (sp), status, tag and dbh. It counts neighbors for each of the individuals in censdata.
#'  @param  censdata2 (NULL) an optional dataset for focal individuals, for example a subset of only one species or a list of seedling coordinates that are not part of census data. It must contain coordinates (gx,gy), species id (sp) and tag. It will count the neighbors in censdata but only for these focal coordinates.
#'  @param  r (20) radius to count neighbors
#'  @param  plotdim  (c(1000,500))  vector with two numbers indicating the xy-plot size;
#'  @param  mindbh (10) minimum size of neighbors to be counted
#'  @param  type (count) calculates count of stems ('count') or sum of basal areas ('basal'). Note that for the sum of basal areas is better to use the plot stem table, otherwise only one stem per tree will be included .
#'  @param  include (c("A")) a vector of status to include in the neighbors, by default it counts only alive individuals, for all status use c("A","P","D","M").
#'#'
#' @examples
#' \dontrun{
#'> load("bci.full7.rdata")
#'> attach("CTFSRPackage.rdata")
#'> library(splancs)
#'##' sum consp and hetsp neighbors for all stems in the plot  
#'> neighbor.counts <- NeighborDensities(bci.full7, r=20, type='count') 
#'##' sum consp and hetsp neighbors for only one species: 
#'> one.sp = subset(bci.full7, sp=="quaras") 
#'> neighbor.counts <- NeighborDensities(bci.full7, one.sp, type='count') }
#'
#'
'NeighborDensities'


#' NDcount
#'#'@description
#' Calculates the count of neighbors within radius r with edge effect correction. Is a quick version of NeighborDensities with limited capabilities. 
#' Dependencies: CalcRingArea from the CTFS package and splancs. 
#' Output: a single vector with neighbor counts within radius r for each individual in censdata. 
#' @author Tania Brenes
#'#' @param censdata Is the coordinates dataset, should have the coordinates (gx, gy)
#' @param r (20) radius 
#'  @param plotdim  (c(1000,500))  vector with two numbers indicating the xy-plot size;
#'#'
#' @examples
#' \dontrun{
#'##' count all the consp neighbors of one species 
#'> one.sp = subset(bci.full7, sp=="ingasa" & status=="A")
#'> neighbor.counts <- NDcount(one.sp)}
#'

'NDcount'
