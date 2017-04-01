
#'
#'


#' coldata.to.imagemat
#'#'@description
#' These functions create maps of response variables using the R functions contour, image, and filled.contour. Those functions are
#' powerful and do the difficult work, but they are a bit tricky to learn and have many options. If you are expert with those 3 R functions,
#' the functions below will not be especially useful. They simply rearrange various types of data into the correct format. 
#'#' The first, coldata.to.imagemat, converts a vector of response variables and converts it into a matrix of the correct form for use by contour, image,
#' and filled.contour. There must be one value of the response z for every position in a grid overlain on the map. In the case of CTFS Plots,
#' this means a value for z at every quadrat, though quadrat can be any size.  The function abundanceperquad (abundance.r) produces 
#' exactly the correct kind of vector for use by coldata.to.imagemat. The function gxgy.to.index (quadfunc.r) produces quadrat numbers that  
#' are the correct vector for use by coldata.to.imagemat.
#'#' For example, consider a 50 ha plot of 1000 x 500 meters. If gridsize=20, the standard quadrat, then there are 50 columns x 25 rows in the plot.
#' In that case, the length of vector z must be exactly 1250, length(x) must be exactly 50, and length(y) exactly 25. Alternatively, x and y can be set
#' NULL and it will be calculated from plotmax=c(1000,500) and plotmin=c(0,0). The simplest application of this function for CTFS plots is to
#' set x and y NULL, and plotmax to the correct plot dimensions. Then make sure z is the right size for the number of quadrats. 
#'#' The common error with image and contour maps is getting the proper dimensions for z and the proper sizes for plotmax, x, and y. If you get an error
#' data length is not a sub-multiple or multiple of the number of rows'then one of those inputs is wrong. 
#' Return value is a list designed to match exactly the format of the RGDAL raster object. It has components
#' @param x same as the argument x; if x is submitted as NULL, this is the vector calculated by using plotmax;
#' @param y likewise for y;
#' @param z exactly as submitted;
#' @param columns number of columns in the grid, same as length(x);
#' @param rows Number of rows in the grid, same as length(y);
#' @param mat The matrix needed for graphing, exactly as needed for instance by imageGraph().
#'#' @param z A numeric vector of responses, which means an attribute which has map coordinates.
#' @param x The map coordinates of each column in the matrix, and the values with which the x-axis of the map will be numbered.
#' The number of columns of z, dim(z)[2], must be exactly length(x). If x is NULL, then it is calculated using plotmax.
#' @param y The map coordinates of each row in the matrix, and the values with which the y-axis of the map will be numbered; 
#' dim(z)[1] must be exactly length(y). If y is NULL, then it is calculated using plotmax.
#' @param gridsize Size of quadrats into which the map is divided; must be square quadrats. This cannot be NULL.  
#' @param plotmin The minimum plot x and y coordinates; in CTFS plots, these are always 0,0. Ignored if x and y are submitted.
#' @param plotmax Maximum plot x and y coordinates. The default, 1000,500, is typical CTFS 50-ha plot. Ignored if x and y are submitted.
#'#'
#' @examples
#' \dontrun{
#' CTFSplot('bci',6,'full')
#' BA=abundanceperquad(bci.full6,gridsize=20,plotdim=c(1000,500),type='ba')
#' totalBAperquad=colSums(BA$ba)
#' summary(totalBAperquad)
#' matrixdata=coldata.to.imagemat(z=totalBAperquad,x=NULL,y=NULL,gridsize=20,plotmax=c(1000,500))
#' length(totalBAperquad)
#' dim(matrixdata$mat)
#' length(matrixdata$x)}
#'
#'#'
'coldata.to.imagemat'


#' imageGraph
#'#'@description
#' Maps a response variable with R's contour or image functions, or both, using the output of coldata.to.imagemat.
#' Also can return contours as vector data, using R's contourLines. The graph can be sent to the screen or exported.
#' There are many options for tweaking the graph, explained with the arguments.
#'#' @param matrixdata a list having components x, y, and mat, exactly the output of coldata.to.imagemat
#' @param levels if set, then contours are drawn (or colors assigned) to breaks are at levels equally-spaced divisions of the response variable; 
#' can be NULL to let breaks control
#' @param breaks if set, a vector of breaks for the image colors and the contour lines; if NULL, levels controls the breaks
#' @param xname and yname names for the axes
#' @param xrange if NULL, all x are included, otherwise, the graph will only include x inside xrange
#' @param yrange likewise for y
#' @param axisdiv the division between tick marks; must be a vector of 2, first for x ticks then for y ticks
#' @param clrs colors for the image; if NULL, they are chosen by default, otherwise, must be 1 + the number of breaks
#' @param returnline TRUE or FALSE, whether to return the contour lines; see R's base function contourLines for details
#' @param img TRUE or FALSE, whether to draw the color image
#' @param cntrs TRUE or FALSE, whether to add contour lines; both img and cntrs can be TRUE
#' @param filled; TRUE or FALSE, whether to draw filled contours; if TRUE, neither image or standard contours are included
#' @param newgraph if FALSE, just graph to the screen and export is ignored; use TRUE to create a new plot window or export to a file
#' @param plotsize size of graph's x-dimension in inches; ignored if newgraph=FALSE
#' @param h, w height and width of graph window in units which depend on the export device chosen (some are pixels, some are inches); 
#' ignored if newgraph=FALSE
#' @param export if newgraph=TRUE, this defines the graphics device; options include X11, quartz, win.graph, win.metafile, png, bmp, jpeg, pdf (not quoted!)
#' @param graphfile the file name if export is pdf, png, jpeg, win.metafile, etc.
#'#'
#' @examples
#' \dontrun{
#' CTFSplot('bci',6,'full')
#' N=colSums(abundanceperquad(bci.full6,plotdim=c(1000,500),grid=20)$abund)
#' matdata=coldata.to.imagemat(z=N,gridsize=20,plotmax=c(1000,500))
#' imageGraph(matrixdata=matdata,breaks=seq(100,400,by=25),img=TRUE,cntrs=FALSE,newgraph=FALSE,plotsize=6,h=11,w=11)
#' imageGraph(matrixdata=matdata,breaks=seq(100,400,by=25),img=TRUE,cntrs=TRUE,newgraph=FALSE,plotsize=6,h=11,w=11)
#' imageGraph(matrixdata=matdata,breaks=seq(100,400,by=25),img=TRUE,cntrs=FALSE,newgraph=TRUE,export=win.graph,h=11,w=11,plotsize=8)}
#'
#'#'
'imageGraph'


#' draw.axes
#'#'@description
#' Add axes to a graph, with ticks spaced by the vector div. This is a subroutine used in imageGraph().
#'#'
#'
#'
#'
#'#'
'draw.axes'


#' image.dataframe
#'#'@description
#' Creates a color response map based on columnar data, not a matrix as used by R's image function. It is used in cases where a full matrix
#' of responses, over a complete grid, is not available. Data are submitted as a table with x and y coordinates, and a response z at each.
#' The points are mapped on an x-y plot, then colored according to the value of z. One typical use would be where soil data are available
#' at many locations, coloring points by the concentration of one nutrient. Another, illustrated below, would be to color tree locations according to dbh. 
#'#' @param data A data frame with at least 3 columns, 2 of which are coordinates and one a numerical response at those coordinates
#' @param xcol, ycol, zcol These allow the columns of data to carry non-standard names
#' @param ptsize Size of points to be drawn
#'#' Other arguments are the same as imageGraph().
#'#'@examples
#' \dontrun{
#' CTFSplot('bci',6,'full')
#' image.dataframe(data=subset(bci.full6,status=='A'),xcol='gx',ycol='gy',zcol='dbh',breaks=c(10,20,50,100,500,10000),xrange=c(0,100),yrange=c(0,100), colors=c('orange','yellow','lightgreen','green','blue'),newgraph=TRUE,h=9,w=12,plotsize=7)}
#'
#'#'

'image.dataframe'
