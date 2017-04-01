
#'
#'


#' readelevdata
#'#'@description
#' A function to read a table of elevation data into a matrix appropriate for
#' mapping. It can read a text table (if text==TRUE) or a dataframe. 
#' In either case, there must be x then y coordinates, followed by elevation, 
#' at every corner across the plot, using grid of gridsize. The output is
#' a matrix, as needed for R's contour function.
#'#'
#'
#'
#'
#'
'readelevdata'


#' elev.to.list
#'#'@description
#' A function which reads a dataframe with x,y,elevation for a given grid
#' size and converts to a list
#' note the names of the columns in the dataframe must be:
#' x y elev
#'#'
#' object 1 in list is: the input dataframe
#' object 2 in list is: matrix of elevation value sin the orientation of the plot
#'#'
#'
#'#'
#'#'
'elev.to.list'


#' allquadratslopes
#'#'@description
#' Calculating slope of all quadrats in a plot
#' calcslope takes 3 elevations and finds the slope of the plane through them
#' quadslope divides the 4 corners of a quadrat into 4 different groups of 3 stakes,
#' takes the slope of each, then averages
#' these were first written in C++
#' see slopeelev.cpp for more on the geometry
#' allquadratslopes goes through all 20x20 m quadrats in a plot and finds
#' the slope, mean elevation, and convexity of each
#' convexity is the mean elevation of one 20x20 m quadrat relative (minus) the mean of its
#' immediate neighbors
#' Helene Muller-Landau added a section to correct convexity in edge quadrats
#'#'
#'
#' @examples
#' \dontrun{
#'#'
#'#' Mean elevation of four corners
#' Convexity
#' correcting convexity in edge quadrats, based on center of the 20x20 rather
#' than surrounding 20x20s. This requires that the elev$mat has an elevation
#' at the middle of every grid cell.
#'        browser()
#'#'
#'#'
#'#'
'allquadratslopes'


#' quadslope
#'#'@description
#' Given the elevation at four corners of a square of side=gridsize, this estimates the slope in degrees of the terrain over that square. The slope is calculated for each of the 4 planes defined by omitting one of the points. 
#' The 4 slopes are averaged. Returns both the mean slope and the sqrt of the variance of the 4 different slopes. 
#'#' cornerelev: vector of 4 elevations 
#' gridsize: the side of the square
#'#'
#'
#'#'
#'#'
'quadslope'


#' calcslope
#'#'@description
#' Given the z-coordinate of 3 points in 3D space whose x-y coordinates are corners of a right, isoceles triangle whose short side = gridsize, this calculates the slope in degrees of the plane through the points. 
#' The second point is the one between the two short sides. 
#'#' z: numeric vector of length 3 
#' gridsize: numeric scalar
#'#'
#'
#'#'
#'#'
'calcslope'


#' calc.gradient
#'#'@description
#' Calculate flow using Seibert & McGlynn algorithm.
#' Takes a 3x3 matrix of elevations and works on central point; also requires grid size (usually 20 m)
#' z=matrix(c(c(268.7,275.9,283.2),c(275.9,282.8,290.0),c(283.2,290.0,297)),nrow=3,byrow=TRUE)
#' This calculates the gradient for the 8 triangular facets around the center point, following Seibert & McGlynn
#' The output is a data.frame of direction and slope for the 8 facets, starting with the lower left and moving clockwise
#'#'
#'
#' @examples
#' \dontrun{
#'#'
#'#' browser()
#'#'
#'#'
#'#'
'calc.gradient'


#' calc.directionslope
#'#'@description
#' This runs equations 1-3 of Seibert & McGlynn
#'#'
#'
#' @examples
#' \dontrun{
#'#'
#'#' browser()
#'#'
#'#'

'calc.directionslope'
