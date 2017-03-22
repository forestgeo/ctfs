
#' 
#'


#' map
#'#'
#' @description
#' Function to draw map of one or more species from one plot. Must give the complete
#' plot dataset, in split format (list of one dataframe per species), and one or more
#' species code. Other arguments are optional, however, many defaults can be adjusted
#' to get a good map on the screen or exported. This calls the functions maptopo() and map1species()
#' for the actual mapping. 
#' A common problem is graph size, leading to an error message "Plot region too large". This happens if the export="no" option
#' is used with the option plotside too large for the default graph size (usually 7 inches). If you use the option export="Windows", 
#' "Mac", or "Unix", then height and width can be set with arguments ht, wd, and plotside can be as large as you please. 
#'#' @param splitdatafile A full plot dataset in list format.
#' @param species One or more species codes to be mapped.
#' @param spplist A table with species codes and Latin names can be submitted as well, so that the full genus-species is added to plot. This must have species codes as row names. It should be the CTFS R format species table (eg, bci.spptable).
#' @param It can be set to NULL if not available, then only the species code (as submitted) appears on the map.
#' @param xrange and yrange Minimum and maximum x coordinates and y coordinates to map. Allows a portion of plot to be drawn. Defaults to the entire plot.
#' @param plotdim The x and y dimensions of the plot. This is used often in R package. Note it assumes the starting coordinates are zero. If they are not, then xrange and yrange must be used.
#' @param elevdata Elevation data can be submitted, then a topo map is overlaid. Elevation data must be submitted as a matrix (as described in readelevdata in utilities.r).
#' @param cutoff Diameter breaks for changing size of plotting points.
#' @param size The size of plotting points, to match the number of diameter breaks. If NULL, a default set is assigned. This can require fiddling, as big points do not work for really abundant species, and small points for rare species.
#' @param deadtree Set NULL to map all trees, alive and dead; TRUE for dead only, FALSE for live only (relies on status in the R table).
#' @param maintitle A title to appear at the top of the page, above the species name.
#' @param titlepos The position to place the title. The default is above the center of the plot, higher than the species name. It may require some fiddling on different screens to get it the right distance above.
#' @param clrs A vector of color names, one for each species. If set to NULL, default values are assigned. See bw.
#' @param bw If TRUE, only black, white, and grays are used.
#' @param bgcolor The background color. Defaults to white. For presentation exports, try bgcolor="transparent".
#' @param symbols A vector of symbols, one per species. Can be anything accepted by R for pch (plot character in the function plot()). If NULL, defaults are assigned.
#' @param addlegend, legpos, legsize For the species name, whether to include, where to place, and font size. Try the defaults first before fiddling, or just set addlegend=FALSE to remove.
#' @param ht, wd, plotside These are the height and width of the overall graph, and the vertical dimension (inches) of the map. The default work for pdf export or mapping to the screen, and ht and wd are inches. But if export is png, jpg, emf, height and width are pixels and need to be 500-1000.
#' @param labsize Size of axis labels.
#' @param bty Type of box to appear around species name. The default, 'n', means no box; set to 'o' to see the box.
#' @param axspos Distance between axis numbers and axis.
#' @param topoint Interval for topolines, if elevdata are submitted.
#' @param topoclr Color of topolines.
#' @param export See function define.graphwindow. 
#' @param filepath The folder to which map will be exported.
#' @param outfile The name of the file to export to.
#'#' 
#' @examples
#' \dontrun{
#' If you have saved split formatted data:  CTFSplot(plot='bci',census=6,type='split')
#' If you have not saved:  
#' CTFSplot(plot='bci',census=6)
#'#' bci.split6=split.data(censdata=bci.full6,splitcol='sp')
#'#' nospp=length(species)
#'#' map(splitdatafile=bci.split6,species='poular')
#'#' map(splitdatafile=bci.split6,species='poular',export='pdf',filepath='~/data/maps/',outfile='Poulsenia1995map',plotside=10,ht=8.5,wd=11)
#'#' map(splitdatafile=bci.split6,species=c('guatdu','poular'))
#'#' #' }
#' 
#'#' 
'map'


#' pdf.allplot
#'#'
#' @description
#' Export a pdf with one or more species maps. If singlefile=TRUE, 
#' all maps will be in one big pdf, otherwise, a pdf for every species is created. The file exported will be named with Map.pdf
#' in the path name given.  
#' This calls the map() function for all species
#' in the splitdata list; it is designed for all species from a plot (default is all species from the BCI plot).
#' See description of map() for details. 
#'#'
#' @examples
#' \dontrun{
#'#' 
#' #' }
#' 
#'#' 
#'#' 
'pdf.allplot'


#' png.allplot
#'#'
#' @description
#' Export species maps as png. There will be one for every species chosen. See pdf.allplot. 
#'#'
#' @examples
#' \dontrun{
#'#' 
#' #' }
#' 
#'#' 
#'#' 
'png.allplot'


#' complete.plotmap
#'#'
#' @description
#' This creates a map with every individual shown. The area to be mapped can be chosen, allowing maps of small areas.
#' On top of the map of every individual, individual species can be overlaid in different colors. If nospp is set NULL, no species
#' are added. If spnames is set NULL, then the most abundant species in the plot are chosen, up to the number nospp. 
#'#' @paramcns: a full census dataset (all species)
#' @paramspnames: names of species to map, using the mnemonic in the R tables
#' @parammindbh: smallest dbh to include
#' @paramexport: set to 'no' to graph to screen, 'pdf' to export to pdf (see define.graphwindow function)
#' @paramnospp: number of species to overlay; can be NULL or 0 for none
#' @paramplotdim: x and y plot dimensions
#' @paramclrlist: colors to use for the species to be overlaid
#' @paramptsize: size of points, the first used for the background of all species, the second for the individual species
#' @paramxrange: minimum and maximum x coordinates of area graphed
#' @paramyrange: minimum and maximum y coordinates of area graphed
#' @paramwd: graph width; see map() function
#' @paramht: graph height, same units as wd
#' @paramside: the side in inches of the graph; see map()
#' @paramfilepath: folder to save output
#' @paramoutfile: filename for output
#'#' 
#' @examples
#' \dontrun{
#'#' 
#' #' }
#' 
#'#' 
#'#' 
#' 
'complete.plotmap'


#' define.graphwindow
#'#'
#' @description
#' This defines an export device for a graph, based on the argument export. The default, export='no', does nothing, so
#' the next graph uses the default R device. With this option, the size of the graph cannot have been set larger than
#' the default size, which is usually 7 inches. If export="unix", a new graphics window in unix, size h and width w, will be opened
#' allowing you to alter the size of the output. To do the same in Windows, set export="windows", or for Mac, export="mac". 
#' To export the graph to a file, export can be "pdf", "png", "jpg", "bmp", "emf". In all cases, it is quoted. 
#'#'
#' 
#' @examples
#' \dontrun{
#'#' 
#' #' }
#' 
#'#' 
'define.graphwindow'


#' get.filename
#'#'
#' @description
#' This sets a name for outputting a map (or any graph) to a file. The argument file may be NULL, then the argument species is used
#' to name the file, or if there are more than one species, the word multispp is used. The argument exp is the export type and becomes
#' the extension (ie, .pdf). By default, type assumes a Map, but it can be set otherwise. 
#'#'
#' 
#' @examples
#' \dontrun{
#'#' 
#' #' }
#' 
#'#' 
#'#' 
'get.filename'


#' maptopo
#'#'
#' @description
#' Draws contours on a plot map. Elevmat has elevation data in matrix form; plot
#' dimensions are not needed, instead it calculates them from elevmat and the
#' gridsize. If add=TRUE, they are added to an existing map. If new=TRUE, a new screen window is created.
#' This is called by map(), and see the description of map() for the other arguments. It can be used
#' on its own though. 
#'#'
#' 
#' @examples
#' \dontrun{
#'#' 
#' #' }
#' 
#'#' 
#'#' 
'maptopo'


#' map1species
#'#'
#' @description
#' This does the mapping for a single species, called from map() but also useful on its own.
#' With add=TRUE, points are added to an existing map. All other parameters are described with map(). 
#'#'
#' 
#' @examples
#' \dontrun{
#'#' 
#' #' }
#' 
#'#' 
#'#' 
'map1species'


#' setsize
#'#'
#' @description
#' An internal function, called by map() to choose size of plotting points. It uses the number of dbh categories (n) and abundance of the species (s) 
#' to help determine the size. Note that the user can override these defaults by submitting sizes.
#'#'
#' 
#' @examples
#' \dontrun{
#'#' 
#' #' }
#' 
#'#' 
#'#' 
'setsize'


#' map2species
#'#'
#' @description
#' Map 2 species to a 2-panel pdf. Two species names are passed to spp as a vector. The data
#' must be a list of two split plot data objects, spplist is a list of two different species tables, and elev
#' a list of two different elevation matrices. To make 2 maps from the same plot, each of the lists should
#' repeated the same data twice. 
#'#'
#' 
#' @examples
#' \dontrun{
#'#' 
#' #' }
#' 
#'#' 

'map2species'
