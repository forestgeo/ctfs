
#'<function>
#'<name>
#' xydist
#'
#' @description
#' Distance between two pairs of x-y coordinates. Input can be atomic or vector. 
#'
#'<arguments>
#'
#'
#' @examples
#' \dontrun{
#'
#'
#'#'
#'
#'
#'
#'
#'<function>
#'<name>
#' xydistvect
#'
#' @description
#' Distance between two x-y coordinates, but accepts each set of coordinates as a vector of length 2,
#' with the first element the x coordinates, the second y. 
#'
#'<arguments>
#'
#'
#' @examples
#' \dontrun{
#'
#'
#'#'
#'
#'
#'
#'
#'<function>
#'<name>
#' xydistmat
#'
#' @description
#' Distance between two x-y coordinates, but accepts two sets of coordinates in a single matrix (4 columns ordered x1, y1, x2, y2). 
#' For use with apply.
#'
#'<arguments>
#'
#'
#' @examples
#' \dontrun{
#'
#'
#'#'
#'
#'
#'
#'
#'<function>
#'<name>
#' perpendicular.distance
#'
#' @description
#' Distance from a point to a line (so it's the perpendicular distance);
#' m and b are slope and intercept; x and y are coordinates. If both b,m and x,y are vectors, they must all be same length.
#' Note check for infinite slope, meaning that the intercept b is the x-intercept.
#'<display>true
#'<update>true
#'<arguments>
#' b: y-intercept
#' m: line slope
#'
#' @examples
#' \dontrun{
#'
#'
#'#'
#'
#'
#' It first finds perpendicular distance, then distance to each end point, and returns the minimum. 
#'
#'
#'<function>
#'<name>
#' perpendicular.line
#'
#' @description
#' Finds the slope and intercept of the line perpendicular to a line whose slope and intercept are given, 
#' through the points x,y. Either x and y can be vectors, or b and m can be vectors, or if all are vectors, must be same length. 
#'
#'<arguments>
#'
#'
#' @examples
#' \dontrun{
#'
#'
#'#'
#'
#'
#'
#'
#'<function>
#'<name>
#' parallel.line
#'
#' @description
#' Finds the slope and intercept of the line parallel to a line whose slope and intercept are given, 
#' through the points x,y. Note that the intercept is not needed. For any m that are infinite, the intercept is x.
#'
#'<arguments>
#'
#'
#' @examples
#' \dontrun{
#'
#'
#'#'
#'
#'
#'
#'
#'<function>
#'<name>
#' intersection.of.lines
#'
#' @description
#' Finds the point where 2 lines intersect, given lines as 2 parameters each (intercept b then slope m). 
#' If the two lines are identical, it returns NAs. Note the check for both slopes being infinite (vertical lines).
#' This is vectorized: either pair can be vector if other pair is atomic, or both can be same length vectors.
#'
#'<arguments>
#'
#'
#' @examples
#' \dontrun{
#'
#'
#'#'
#'
#'<function>
#'<name>
#' intersection.line.curve
#'
#' @description
#' Finds the points where a straight line, given lines as intercept b then slope m, intersects with a curve defined by a sequence of segments. Intersections must be found for the line with every segment, then every one checked with is.between. The intersections are returned as a dataframe of x, y coordinates. The b and m must be atomic. The curve must be a dataframe with columns x, y (or capital X, Y). 
#' having any number of segments.
#'
#'<arguments>
#'
#'
#' @examples
#' \dontrun{
#'
#'
#'#'
#'
#'
#'
#'<function>
#'<name>
#' is.between
#'
#' @description
#' Check whether a point x,y falls between two other points. The typical use is where the first point falls on the segment connecting the next two points. 
#' All arguments can be vector, but must be identical in length. An x falls between when it lies in [x1,x2). 
#'
#'<arguments>
#'
#'
#' @examples
#' \dontrun{
#'
#'
#'#'
#'
#'
#'
#'<function>
#'<name>
#' line.intersection.pts
#'
#' @description
#' Finds the point where 2 lines intersect, given each line as 2 pairs of points on the line
#'
#'<arguments>
#' Both arguments must have columns x, y, with two rows, one row per point. 
#'
#' @examples
#' \dontrun{
#'
#'
#'#'
#'
#'
#'
#'
#'<function>
#'<name>
#' pts.to.interceptslope
#'
#' @description
#' Returns intercept and slope of a line given two pairs of coordinates on the line. Arguments can be vectors; if both are vectors, must be same size. 
#' If the x's are exactly equal, so slope is infinite, it returns the x as the first argument. 
#'
#'<arguments>
#'
#'
#' @examples
#' \dontrun{
#'
#'
#'#'
#'
#'
#'<function>
#'<name>
#' segmentPt
#'
#' @description
#' Draw a line segment between two points, where each point is a vector of x then y coordinates
#'
#'<arguments>
#'
#'
#' @examples
#' \dontrun{
#'
#'
#'#'
#'
#'
#'
#'
#'<function>
#'<name>
#' drawrectangle
#'
#' @description
#' Draw a rectangle given a matrix or dataframe of 4 x-y coordinates. The columns must have x first then y coordinates. 
#'
#'<arguments>
#'
#'
#' @examples
#' \dontrun{
#'
#'
#'#'
#'
#'
#'
#'
#'<function>
#'<name>
#' angleBisector
#'
#' @description
#' Given 3 sets of coordinates defining two line segments (middle point is intersection), find line bisecting the angle through middle point. This is not vectorized. It only works with 3 sets of coordinates. Note that it is necessary to correctly find the compass direction theta, which the slope alone does not establish:
#'1) If slope is negative and x2>=x1 (travel is toward Quadrant IV), then theta=atan(slope)
#'2) If slope is negative with x2<x1 (travel is toward Quadrant II), then theta=atan(slope)+pi
#'3) If slope is positive with x2>=x1 (travel is toward Quadrant I), then theta=atan(slope)
#'4) If slope is positive with x2<x1 (travel is toward Quadrant III), then theta=atan(slope)-pi
#' This returns theta in (-pi,pi). Then the turn is calculated, ie the change in direction between the two segments as the difference in theta, but it is reset to be on (-pi,pi) as well in order to correctly estimate the mean direction of travel (half the turn added to theta[1]).
#'
#'<arguments>
#'
#'
#' @examples
#' \dontrun{
#'
#'
#'#'
#'
#'<name>
#' insideRectangle
#'
#' @description
#' Checks a vector of coordinates x, y to return which are inside a rectangle. For a much more general function for checking whether
#' points are inside polygons, use the function inout() in the package splancs.
#'
#'<arguments>
#'
#'
#' @examples
#' \dontrun{
#'
#'
#'#'
#'
#'
#'<function>
#'<name>
##' are.ptsinside
#'
#' @description
#' Checks many points (dataframe pt with x and y) against a single quadrat whose corners are given by as xlo, ylo, xhi, yhi.
#' It returns a logical vector, TRUE for the points inside. This is same as insideRectange, but accepting input as a matrix pts
#' and a single vector of the four corners of the rectange. 
#'
#'<arguments>
#'
#'
#' @examples
#' \dontrun{
#'
#'
#'#'
#'<name>
#' ispt.inside
#'
#' @description
#' Check a single pt (x and y) against a large number of quadrats whose corners are given by the rows of coord, xlo, ylo, xhi, yhi.
#' It returns the fraction of quadrats which the point falls inside. This is exactly like are.ptsinside() but allows there to be many
#' rectangles, defined by a dataframe coord.
#'
#'<arguments>
#'
#'
#' @examples
#' \dontrun{
#'
#'
#'#'
#'
#'
#'<function>
#'<name>
#' inside.rect
#'
#' @description
#' Determines whether any of the 4 corners of one rectangle are within a second rectangle. Both rectangles are submitted as c(x0,x1,y0,y1). If just one
#' of the corners is inside, it returns true. See insideRectange(), which has a similar name but does something different.
#'
#'<arguments>
#'
#'
#' @examples
#' \dontrun{
#'
#'
#'#'
#'
#'
#'<function>
#'<name>
#' circle
#'
#' @description
#' Calculates points on a 
#'
#'<arguments>
#'
#'
#' @examples
#' \dontrun{
#'
#'
#'#'
#'
#'
#'
#'
#'<function>
#'<name>
#' fullcircle
#'
#' @description
#' Create a dataframe for a full circle, with x values repeated to get top then bottom. NA is inserted
#' so this can be passed directly to graphing functions.
#'<arguments>
#'
#'
#' @examples
#' \dontrun{
#'
#'
#'#'
#'
#'
#'
#'
#'<function>
#'<name>
#' ellipse
#'
#' @description
#' Equation for (half) a canonical 
#'
#'<arguments>
#'
#'
#' @examples
#' \dontrun{
#'
#'
#'#'
#'
#'
#'
#'
#'<function>
#'<name>
#' fullellipse
#'
#' @description
#' Creates a dataframe for a full ellipse. 
#'
#'<arguments>
#'<ul>
#' @param x = input x coordinates
#' @param center = x-y coordinates of ellipse center (vector of 2)
#'
#'
#' @examples
#' \dontrun{
#'
#'
#'#'
#'
#'
#'<name>
#' cartesian.to.polar
#'
#' @description
#' Convert Cartesian coordinates to polar. Returns a dataframe of two columns named r and theta. This always
#' returns a theta between -pi/2 and pi/2. Note that polar.to.cartesian may not return the starting x, y submitted
#' to cartesian.to.polar due to problems with signs. It will work if theta is kept positive (ie, quadrant 1).
#'
#'<arguments>
#'<ul>
#' @param x = input vector of x coordinates
#' @param y = input vector of y coordinates (same length as x)
#'
#'
#' @examples
#' \dontrun{
#' cartesian.to.polar(2,4)
#'
#'#'
#'
#'
#'<function>
#'<name>
#' polar.to.cartesian
#'
#' @description
#' Convert polar coordinates to Cartesian. Returns a dataframe of two columns named x and y.
#'
#'<arguments>
#'<ul>
#' @param r = input vector of radii (distance from origin)
#' @param theta = input vector of angle from horizontal (radians), same length as r
#'
#'
#' @examples
#' \dontrun{
#' polar.to.cartesian(2,pi/3)
#'
#'#'
#'
#'
#'<function>
#'<name>
#' slope.intercept.frompts
#'
#' @description
#' This is an old version of pts.to.interceptslope, returns slope than intercept of a line given two points. The alternate function, pts.to.interceptslope, 
#' returns intercept then slope, which is more standard. This is kept for compatibility with old functions. 
#'
#'<arguments>
#'
#'
#' @examples
#' \dontrun{
#'
#'
#'#' slope.intercept.frompts=function(pt1,pt2)
#'{
#' line=pts.to.interceptslope(pt1,pt2)
#' return(c(line[21]))}
#'
#'
#'
#'

#'<function>'
