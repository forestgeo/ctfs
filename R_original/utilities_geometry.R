
#'
#'


#' xydist
#'#'
#' @description
#' Distance between two pairs of x-y coordinates. Input can be atomic or vector. 
#'#'
#'
#'
#'
#'#'
#'#'
'xydist'


#' xydistvect
#'#'
#' @description
#' Distance between two x-y coordinates, but accepts each set of coordinates as a vector of length 2,
#' with the first element the x coordinates, the second y. 
#'#'
#'
#'
#'
#'#'
#'#'
'xydistvect'


#' xydistmat
#'#'
#' @description
#' Distance between two x-y coordinates, but accepts two sets of coordinates in a single matrix (4 columns ordered x1, y1, x2, y2). 
#' For use with apply.
#'#'
#'
#'
#'
#'#'
#'#'
'xydistmat'


#' perpendicular.distance
#'#'
#' @description
#' Distance from a point to a line (so it's the perpendicular distance);
#' m and b are slope and intercept; x and y are coordinates. If both b,m and x,y are vectors, they must all be same length.
#' Note check for infinite slope, meaning that the intercept b is the x-intercept.
#'#'<display>true
#'<update>true
#' b: y-intercept
#' m: line slope
#'#'
#'
#'#' Find distance from a pt (x then y coords) to a line segment given as start and end points. Either x, y or end points can be vectors, but not both.
#' It first finds perpendicular distance, then distance to each end point, and returns the minimum. 
#'#'
#'
'perpendicular.distance'


#' perpendicular.line
#'#'
#' @description
#' Finds the slope and intercept of the line perpendicular to a line whose slope and intercept are given, 
#' through the points x,y. Either x and y can be vectors, or b and m can be vectors, or if all are vectors, must be same length. 
#'#'
#'
#'
#'
#'#'
#'#'
'perpendicular.line'


#' parallel.line
#'#'
#' @description
#' Finds the slope and intercept of the line parallel to a line whose slope and intercept are given, 
#' through the points x,y. Note that the intercept is not needed. For any m that are infinite, the intercept is x.
#'#'
#'
#'
#'
#'#'
#'#'
'parallel.line'


#' intersection.of.lines
#'#'
#' @description
#' Finds the point where 2 lines intersect, given lines as 2 parameters each (intercept b then slope m). 
#' If the two lines are identical, it returns NAs. Note the check for both slopes being infinite (vertical lines).
#' This is vectorized: either pair can be vector if other pair is atomic, or both can be same length vectors.
#'#'
#'
#'
#'
#'
'intersection.of.lines'


#' intersection.line.curve
#'#'
#' @description
#' Finds the points where a straight line, given lines as intercept b then slope m, intersects with a curve defined by a sequence of segments. Intersections must be found for the line with every segment, then every one checked with is.between. The intersections are returned as a dataframe of x, y coordinates. The b and m must be atomic. The curve must be a dataframe with columns x, y (or capital X, Y). 
#' having any number of segments.
#'#'
#'
#'
#'
#'#'
#'
'intersection.line.curve'


#' is.between
#'#'
#' @description
#' Check whether a point x,y falls between two other points. The typical use is where the first point falls on the segment connecting the next two points. 
#' All arguments can be vector, but must be identical in length. An x falls between when it lies in [x1,x2). 
#'#'
#'
#'
#'
#'#'
#'
'is.between'


#' line.intersection.pts
#'#'
#' @description
#' Finds the point where 2 lines intersect, given each line as 2 pairs of points on the line
#'#' Both arguments must have columns x, y, with two rows, one row per point. 
#'#'
#'
#'#'
#'#'
'line.intersection.pts'


#' pts.to.interceptslope
#'#'
#' @description
#' Returns intercept and slope of a line given two pairs of coordinates on the line. Arguments can be vectors; if both are vectors, must be same size. 
#' If the x's are exactly equal, so slope is infinite, it returns the x as the first argument. 
#'#'
#'
#'}
#'
'pts.to.interceptslope'


#' segmentPt
#'#'
#' @description
#' Draw a line segment between two points, where each point is a vector of x then y coordinates
#'#'
#'
#'
#'
#'#'
#'#'
'segmentPt'


#' drawrectangle
#'#'
#' @description
#' Draw a rectangle given a matrix or dataframe of 4 x-y coordinates. The columns must have x first then y coordinates. 
#'#'
#'
#'
#'
#'#'
#'#'
'drawrectangle'


#' angleBisector
#'#'
#' @description
#' Given 3 sets of coordinates defining two line segments (middle point is intersection), find line bisecting the angle through middle point. This is not vectorized. It only works with 3 sets of coordinates. Note that it is necessary to correctly find the compass direction theta, which the slope alone does not establish:
#'1) If slope is negative and x2>=x1 (travel is toward Quadrant IV), then theta=atan(slope)
#'2) If slope is negative with x2<x1 (travel is toward Quadrant II), then theta=atan(slope)+pi
#'3) If slope is positive with x2>=x1 (travel is toward Quadrant I), then theta=atan(slope)
#'4) If slope is positive with x2<x1 (travel is toward Quadrant III), then theta=atan(slope)-pi
#' This returns theta in (-pi,pi). Then the turn is calculated, ie the change in direction between the two segments as the difference in theta, but it is reset to be on (-pi,pi) as well in order to correctly estimate the mean direction of travel (half the turn added to theta[1]).
#'#'
#'
#'
#'
#'
'angleBisector'


#' insideRectangle
#'#'
#' @description
#' Checks a vector of coordinates x, y to return which are inside a rectangle. For a much more general function for checking whether
#' points are inside polygons, use the function inout() in the package splancs.
#'#'
#'
#'
#'
#'#'
'insideRectangle'


#' are.ptsinside
#'#'
#' @description
#' Checks many points (dataframe pt with x and y) against a single quadrat whose corners are given by as xlo, ylo, xhi, yhi.
#' It returns a logical vector, TRUE for the points inside. This is same as insideRectange, but accepting input as a matrix pts
#' and a single vector of the four corners of the rectange. 
#'#'
#'
#'
#'
#'
'are.ptsinside'


#' ispt.inside
#'#'
#' @description
#' Check a single pt (x and y) against a large number of quadrats whose corners are given by the rows of coord, xlo, ylo, xhi, yhi.
#' It returns the fraction of quadrats which the point falls inside. This is exactly like are.ptsinside() but allows there to be many
#' rectangles, defined by a dataframe coord.
#'#'
#'
#'
#'
#'#'
'ispt.inside'


#' inside.rect
#'#'
#' @description
#' Determines whether any of the 4 corners of one rectangle are within a second rectangle. Both rectangles are submitted as c(x0,x1,y0,y1). If just one
#' of the corners is inside, it returns true. See insideRectange(), which has a similar name but does something different.
#'#'
#'
#'}
#'
'inside.rect'


#' circle
#'#'
#' @description
#' Calculates points on a 
#'#'
#'
#'
#'
#'#'
#'#'
'circle'


#' fullcircle
#'#'
#' @description
#' Create a dataframe for a full circle, with x values repeated to get top then bottom. NA is inserted
#' so this can be passed directly to graphing functions.
#'#'
#'
#'
#'
#'#'
#'#'
'fullcircle'


#' ellipse
#'#'
#' @description
#' Equation for (half) a canonical 
#'#'
#'
#'
#'
#'#'
#'#'
'ellipse'


#' fullellipse
#'#'
#' @description
#' Creates a dataframe for a full ellipse. 
#'#' @param x = input x coordinates
#' @param center = x-y coordinates of ellipse center (vector of 2)
#'#'
#'
#'
#'#'
#'
'fullellipse'


#' cartesian.to.polar
#'#'
#' @description
#' Convert Cartesian coordinates to polar. Returns a dataframe of two columns named r and theta. This always
#' returns a theta between -pi/2 and pi/2. Note that polar.to.cartesian may not return the starting x, y submitted
#' to cartesian.to.polar due to problems with signs. It will work if theta is kept positive (ie, quadrant 1).
#'#' @param x = input vector of x coordinates
#' @param y = input vector of y coordinates (same length as x)
#'#'
#' @examples
#' \dontrun{
#' cartesian.to.polar(2,4)}
#'
#'#'
'cartesian.to.polar'


#' polar.to.cartesian
#'#'
#' @description
#' Convert polar coordinates to Cartesian. Returns a dataframe of two columns named x and y.
#'#' @param r = input vector of radii (distance from origin)
#' @param theta = input vector of angle from horizontal (radians), same length as r
#'#'
#' @examples
#' \dontrun{
#' polar.to.cartesian(2,pi/3)}
#'

'polar.to.cartesian'
