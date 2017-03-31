
#'
#'


#' fullplot.imageJ
#'#'
#' @description
#' Reads quadrat maps created by imageJ, converting the digitized pixel to coordinates within a quadrat (local coordinates).<br />
#' The input imageJ files must be named in a very specific way: all should have the same prefix and the quadrat names should
#' have 4 digits. If there are subquadrats, the names should end with the same consistent suffixes. Only those files with this 
#' specific naming convention are read and converted. Updated Aug 2014 to handle files with no trees (just 4 corner labels). 
#'#' @param path the complete path name where the map files to be converted are found
#' @param include.subdir whether the subfolders are to be searched for map files also
#' @param outfile the name of the text file where the results will be saved. This file will be saved in the folder specified by the path. If outfile=NULL, 
#' the results will not be written to a file.
#' @param corners specify the tags used for the map corners used for calibration. They must be named in a clockwise direction starting with the lower left corner. 
#' These tags must be the same in each and every map file.
#' @param colrange specifies the range of the columns, found as the first two digits of the quadrat name
#' @param rowrange specifies the range of the rows, found in the last two digits of the quadrat name
#' @param prefix the prefix used for all the map files before the quadrat name
#' @param suffix the extension used for the map files. The imageJ default is ".txt".
#' @param subquadratsuffix used for map files that are smaller than 20x20m (i.e. 10x10m). They should be named clockwise from the lower left subquadrat.
#' @param gridsize size of each individual map
#'#'
#' @examples
#' \dontrun{
#' mapfolder='/maps/rabi/'
#' coords=
#' fullplot.imageJ(path=mapfolder,include.subdir=T,gridsize=c(10,10),outfile='location.txt',corners=c('p2','p1','p4','p3'),
#'                prefix='Map_',colrange=c(0,49), rowrange=c(0,24),subquadsuffix=c('_1','_2','_3','_4'))
#' head(coords)
#' dim(coords)
#' range(coords$lx)
#' range(coords$ly)}
#'
#'
'fullplot.imageJ'


#' SectionCorrection
#'#'
#' @description
#' This function is called by fullplot.imageJ in the case that the maps are subquadrats to correct the coordinates and make them from 0 to 20 depending on
#' what section of the map the subquadrat belongs to.
#'#' @param pts the coordinates to be corrected
#' @param subquad the subquadrat to be corrected
#' @param gridsize size of each individual map
#' @param subquadratsuffix the suffixes used to indicate what section of the map the subquadrat refers to. They should be named clockwise from the lower left.
#'#'
#'
#'
#'
'SectionCorrection'


#' imageJ.to.lxly
#'#'
#' @description
#' Convert map pixels from digitzing program to plot coordinates. Uses 2 functions from geometry.r:
#' pts.to.interceptslope=function(pt1,pt2)
#' perpendicular.distance=function(b,m,x,y) return(sqrt((y-m*x-b)^2/(1+m^2)))
#' intersection.of.lines=function(b1,m1,b2,m2)
#'#' @param textfile the complete name of the textfile to convert, including the path
#' @param lowerleft, upperleft, upperright, lowerright the tags in each of the map files with the calibrated corners
#' @param delim the delimiter used to separate the fields in the map files
#' @param gridsize size of each individual map
#'#'
#'
#'
#'
'imageJ.to.lxly'


#' distance.to.side
#'#'
#' @description
#' Called from imageJ.to.lxly where for each point, distance to each side is calculated in 3 different ways: perpendicular, parallel to one adjacent side, 
#' and parallel to other adjacent. These 3 distances are averaged, and the discrepancy used as a measure of error, taken as the CV of the three measures.
#'#'
#'
#'

'distance.to.side'
