
#'
#'


#' spparea.sq
#'#'@description
#' Function for calculating the number of species in replicated, randomly-placed
#' quadrats of various areas. The variable size refers to the dimension (side)
#' of the square, and can be a vector; replicates is the
#' number of random draws per dimension. Full plot data are submitted as censdata.
#' Species are counted as the number of unique values of sp in the R Analytical Table; unidentified species are not counted,
#' based on a default unidennames; see the function unidentified.species in utilitiesCTFS.r.
#' The return value is a list of two components. The first is a table giving the mean number (and SD) of individuals and species
#' in all quadrat sizes submitted. The second is a table giving the number of individuals and species in every random quadrat created.
#' In addition, a graph of species vs. individuals in every quadrat is created as the program runs. 
#' This can also be used to calculate genus- or family-area curves with use of the spcolumn argument. The censdata table must have
#' a new column added, for example the genus for every record, then spcolumn can be set to 'genus'. 
#' Note: randomly-placed quadrats produce statistically preferable species-area curves than checkerboards of non-overlapping quadrats. If
#' required, though, the function abundanceperquad() in abundance.r offers a fast way to count the number of species in checkerboard-type
#' quadrats of different sizes.
#' @param censdata one R Analytical Table, either full or stem
#' @param spcolumn name of the column in the table having the species; defaults to 'sp', but can be set to 'genus'for 'family'if desired
#' @param size a vector of quadrat sizes, referring to the x-dimension of a rectangular quadrat
#' @param rectdim the ratio of y to x dimensions of the rectangles; rectdim=1 (the default) for squares
#' @param mindbh the minimum dbh included
#' @param plotdim the x and y dimensions of the entire plot
#' @param replicates the number of random quadrats to create, of each size
#' @param unidennames a vector of species names that should not be included in species counts (see the function unidentified.species()
#'#'
#' @examples
#' \dontrun{
#' speciesPerArea=spparea.sq(bci.full6,size=c(10,20,50),mindbh=10,plotdim=c(1000,500),replicates=5,unidennames=c('unid'))
#' rowmatch=match(bci.full6$sp,bci.spptable$sp)
#' bci.full6$Genus=bci.spptable$Genus[rowmatch]
#' genusPerArea=spparea.sq(bci.full6,spcolumn='Genus',size=c(10,20,50),mindbh=10,plotdim=c(1000,500),replicates=5,unidennames=c('unid'))}
#'
#'
'spparea.sq'


#' selectrandomquad
#'#'@description
#' Draws rectangular quadrats in a plot at random. Returns a dataframe of coordinates defining the corners of all the quadrats. 
#' The variable size is a vector of dimensions; shape allows a rectangle to be drawn; rep is the
#' number of replicated random quadrats per dimension submitted. 
#' Quadrats are chosen simply: a point is chosen by randomly drawing one x and one y coordinates to serve as the lower-left corner.
#' The x is chosen from a uniform distribution between 0 and plotdim[1]-size; y is chosen similarly. 
#' All 3 algorithms for creating random quadrats under sample the plot corners. See selectrandomquad2() and selectrandomquad3(), alternative
#' intended to overcome the bias (but neither does). 
#'#' @param censdata one R Analytical Table, either full or stem
#' @param size a vector of quadrat sizes, referring to the x-dimension of a rectangular quadrat
#' @param shape the ratio of y to x dimensions of the rectangles; rectdim=1 (the default) for squares
#' @param plotdim the x and y dimensions of the entire plot
#' @param rep the number of random quadrats to create, of each size
#' @param graphit whether to graph the locations of the chosen quadrats on a plot map
#'#'
#'
#'
'selectrandomquad'


#' selectrandomquad3
#'#'@description
#' Creates randomly drawn quadrats, using same arguments and producing same return value as selectrandomquad, but using a different algorithm.
#' The lower left x, y coordinates are drawn at random from a range extending outside the plot dimensions, then quadrats are used only if
#' all 4 corners fall inside the plot.
#'#'
#'
#'
#'
#'
'selectrandomquad3'


#' selectrandomquad2
#'#'@description
#' Creates randomly drawn quadrats, using same arguments and producing same return value as selectrandomquad, but using a different algorithm 
#' aimed at capturing corners. The result, however, is not to capture corners any better than selectrandomquad() does.
#' Imagine a line running vertically at x=0 from y=0 to y=plotdim[2]-size, then continues at x=1 from 0 to plotdim[2]-size,
#' etc. It's wrapping analogous to the way quadrat indices wrap (see gxgy.to.index in quadfunc.r). 
#' This line has length (plotdim[1]-size)*(plotdim[2]-size).
#' Draw a random number on that line, and place the lower left corner of random square at that point. 
#' A position x on the line is converted to plot coordinates gx, gy using function index.to.gxgy with grid=1
#'#'
#'
#'
#'
#'
'selectrandomquad2'


#' graph.quadrats
#'#'@description
#' Make a graph of a series of quadrats whose corners are given by the rows of coord: xlo, ylo, xhi, yhi. This is used in illustrating
#' randomly selected quadrats. 
#'#'
#'
#'
#'
#'
'graph.quadrats'


#' coverage.diag
#'#'@description
#' Draws a diagonal across a plot, from lower left to upper right (if slope==1), upper left to lower right (if slope==-1),
#' or straight across the middle (if slope==0).
#' and determines for every point along the diagonal what fraction of a series of quadrats it is inside.
#' The quadrats are defined by their four corners in randomquads: xlo, ylo, xhi, yhi.
#' This is only used in testing how well random quadrat draws include corners and edges of a plot.
#'#'
#'
#'
#'

'coverage.diag'
