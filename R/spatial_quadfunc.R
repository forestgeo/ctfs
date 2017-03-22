
#' 
#'


#' quad.to.gxgy
#'#'
#' @description
#' Convert quadrat names into x-y coordinates, assuming the first 2 digits are the column and the second two the row. Quad is a character. 
#' If the first row and column are 00, set start=0, etc. 
#'#'
#' 
#' @examples
#' \dontrun{
#'#' 
#' #' }
#' 
#'#' 
#'#' 
'quad.to.gxgy'


#' rowcol.to.index
#'#'
#' @description
#' None given.
#'#'
#' 
#' @examples
#' \dontrun{
#'#' 
#' #' }
#' 
#'#' 
#'#' 
'rowcol.to.index'


#' gxgy.to.quad
#'#'
#' @description
#' Calculate a quadrat name (column number then row number, as a 4-digit character string) from gy-gy. If start is set to zero, quadrats start with 0000, otherwise, 0101.
#'#'
#' 
#' @examples
#' \dontrun{
#'#' 
#' #' }
#' 
#'#' 
#'#' 
'gxgy.to.quad'


#' getquadratname
#'#'
#' @description
#' Convert x, y coordinates and plot dimensions into 4-character quadrat names. If x or y are missing, the quadrat=9999.
#'#'
#' 
#' @examples
#' \dontrun{
#'#' 
#' #' }
#' 
#'#' 
#'#' 
'getquadratname'


#' convert.rowcol
#'#'
#' @description
#' Convert an integer to a character, with a single leading zero if the integer is < 10. Does
#' not handle integers >99
#'#'
#' 
#' @examples
#' \dontrun{
#'#' 
#' #' }
#' 
#'#' 
#'#' 
'convert.rowcol'


#' gxgy.to.index
#'#'
#' @description
#' Assign any location(s) a single index identifying the quadrat. The index runs from 1 to the number of quadrats. 
#'#'
#' 
#' @examples
#' \dontrun{
#'#' 
#' #' }
#' 
#'#' 
#'#' 
'gxgy.to.index'


#' index.to.rowcol
#'#'
#' @description
#' Calculate the row and column given the quadrat index, as calculated in gygy.to.index. Both row and column start at 1, not 0 as in quadrat naming. 
#'#'
#' 
#' @examples
#' \dontrun{
#'#' 
#' #' }
#' 
#'#' 
#'#' 
'index.to.rowcol'


#' index.to.gxgy
#'#'
#' @description
#' Calculate the x and y coordinates given the quadrat index, as calculated in gygy.to.index.
#'#'
#' 
#' @examples
#' \dontrun{
#'#' 
#' #' }
#' 
#'#' 
#'#' 
'index.to.gxgy'


#' gxgy.to.rowcol
#'#'
#' @description
#' Returns row and column for any set of coordinates. Rows and columns both start at 1, not 0. 
#'#'
#' 
#' @examples
#' \dontrun{
#'#' 
#' #' }
#' 
#'#' 
#'#' 
'gxgy.to.rowcol'


#' gxgy.to.hectindex
#'#'
#' @description
#'#'
#' 
#' @examples
#' \dontrun{
#'#' 
#' #' }
#' 
#'#' 
#'#' 
'gxgy.to.hectindex'


#' gxgy.to.lxly
#'#'
#' @description
#' Given global coordinates and quadrat and plot dimensions, calculate local x and y, the within-quadrat coordinates
#'#'
#' 
#' @examples
#' \dontrun{
#'#' 
#' #' }
#' 
#'#' 
#' 
'gxgy.to.lxly'


#' lxly.to.p5
#'#'
#' @description
#' Given local, or  within-quadrat, coordinates for a 20-m quadrat, return the p5x5; lx and ly must be vectors of equal length. Any values outside [0,20) are returned p5=NA.
#'#'
#' 
#' @examples
#' \dontrun{
#'#' 
#' #' }
#' 
#'#' 
#' 
'lxly.to.p5'


#' findborderquads
#'#'
#' @description
#' Calculate indices of neighboring quadrats, for a given quadrat index.
#'#'
#' 
#' @examples
#' \dontrun{
#'#' 
#' #' }
#' 
#'#' 
#'#' 
'findborderquads'


#' create.neighbordata
#'#'
#' @description
#' Calculates the mean density in neighboring quadrats for every quadrat, given
#' a vector of abundances per quadrat. The vector of abundances must be ordered
#' by quadrat index.
#'#'
#' 
#' @examples
#' \dontrun{
#'#' 
#' #' }
#' 
#'#' 
#'#' 
'create.neighbordata'


#' findneighborabund
#'#'
#' @description
#' For every quadrat, finds neighboring quadrats and then returns a vector of abundances in those
#' neighbors, as well as the number of neighboring quadrats. A subroutine used by create.neighbordata.
#'#'
#' 
#' @examples
#' \dontrun{
#'#' 
#' #' }
#' 
#'#' 
#'#' 
#'#' 
'findneighborabund'


#' neighbors
#'#'
#' @description
#' Finds proportion of neighboring quadrats in which a species is present. The input vector
#' is presence-absence for every quadrat. It returns a vector of the same length.
#'#'
#' 
#' @examples
#' \dontrun{
#'#' 
#' #' }
#' 
#'#' 
#'#' 
'neighbors'


#' torus.shift
#'#'
#' @description
#' Creates a torus-shifted quadrat topographic dataset. It accepts a quadrat dataset
#' with elevation, convexity, and slope for each 20x20 m quadrat in a plot. It returns a parallel
#' dataset that is torus shifted, slip.horiz quadrats left-right and slip.vert quadrats up-down. 
#' That is, in the new dataset, the topographic information of each quadrat comes from a quadrat
#' displaced by slip.horiz and slip.vert units away in the original dataset.
#'#'
#' 
#' @examples
#' \dontrun{
#'#' 
#' #' }
#' 
#'#' 
#'#' 
#'#' 
'torus.shift'


#' getsmallerquads
#'#'
#' @description
#' Takes a vector of indices for a larger quadrat dimension, as created by gxgy.to.index, and for
#' each returns a vector of indices of smaller quadrats that would fit completely
#' within. Both larger and smaller quadrats must be square. Returns a matrix, each row being a 
#' vector of smaller quadrats inside a single larger quadrat.
#'#'
#' 
#' @examples
#' \dontrun{
#'#' 
#' #' }
#' 
#'#' 
#'#' 
'getsmallerquads'


#' full.xygrid
#'#'
#' @description
#' Create a complete of points x-y, given the sequence of unique x and the sequence of unique y. So if x=y=0:2,
#' it creates all pairs: 0,0; 0,1; 0,2; 1,0; 1,1; 1,2; etc.
#'#'
#' 
#' @examples
#' \dontrun{
#'#' 
#' #' }
#' 
#'#' 
'full.xygrid'


#' distance
#'#'
#' @description
#' Calculates the distance from one quadrat to a second quadrat, where quadrats are designated by their indices, as
#' created by gxgy.to.index. The two quadrats can be vectors, but must be of the same length (or one of the two can be atomic). 
#' Returns a vector of distances same length as input vectors. 
#'#'
#' 
#' @examples
#' \dontrun{
#'#' 
#' #' bad1=pt1$gx<0 | pt1$gy<0 
#' bad2=pt2$gx<0 | pt2$gy<0
#' xdist=pt1$gx-pt2$gx
#' ydist=pt1$gy-pt2$gy
#' dist=sqrt(xdist^2+ydist^2)
#' if(length(pt1)==1 & bad1==T) dist=rep(-1,length(pt2))
#' else if(length(pt2)==1 & bad2==T) dist=rep(-1,length(pt1)) 
#' dist[bad1]=(-1)
#' dist[bad2]=(-1)
#' return(dist)
#'#' 
#'
'distance'
