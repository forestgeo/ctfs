#' tojulian
#'
#' @description
#' Accepts any character representation of a date and a description of the format. The submitted dates can
#' be a vector or a matrix. See strptime for details about the format. 
#' Returns a julian date, the number of days since 1 Jan 1960; a julian is an integer and can be graphed or operated as such, 
#' though it displays as a date.
#'
#'
#'
#' @examples
#' \dontrun{
#'
#' tojulian(c('23Oct2010','29Mar1956'),'%d%b%Y')}
#'
#'
#'
#'
#'
'tojulian'


#' fromjulian
#'
#' @description
#' Accepts a julian date and returns a character representation of date. See tojulian(). The input
#' can be vector or array. 
#'
#'
#'
#' @examples
#' \dontrun{
#'
#' fromjulian(1000,'%d%B%Y')}
#'
#'
#'
#'
#'
'fromjulian'


#' create.fulldate
#'
#' @description
#' Converts a vector of date character strings in any format to a dataframe with year, month, day, yday (day of the year) and julian.
#' Submitted datestr cannot be an array. 
#'
#'
#'
#' @examples
#' \dontrun{
#'
#' create.fulldate(c('23Oct2010','29Mar1956'),'%d%b%Y')}
#'
#'
#'
#'
#'
'create.fulldate'


#' create.fulldate.split
#'
#' @description
#' Converts the MySQL date format by splitting on the hyphen (or other characters by using sep). 
#' Date must be year-month-day, and month must be numeric (create.fulldate takes any format).
#' In most cases, create.fulldate
#' is preferable, but Mysql allows dates with zeroes (1995-00-00 or 2002-2-00), and create.fulldate cannot handle those;
#' this allows the 0 to be read.
#'
#'
#'
#' @examples
#' \dontrun{
#'
#' create.fulldate.split(c('23-10-2010','29-0-1956'),sep='-')}
#'
#'
#'
#'
#'
'create.fulldate.split'


#' order.by.rowcol
#'
#' @description
#' Reorder rows and columns of a matrix so they are sorted as if column and row names are numeric. This allows labels from cut to
#' be ordered numerically. Otherwise, sort order is 1, 10, 11, 12, ... 18, 19, 2, 20, 21, etc. 
#'
#'
#'
#'
#'
#'
#'
#'
#'
'order.by.rowcol'


#' order.bynumber
#'
#' @description
#' Returns ordering of a character vector with any numbers coming first, in numeric order.
#'
#'
#'
#'
#'
#'
#' 
#'
'order.bynumber'


#' fill.dimension
#'
#' @description
#' This function fills out an array of 2 dimensions, adding zeroes (or other values) for extra columns
#' and rows as named in class1 and class2. If a column (or row) is
#' missing, it will be filled with the value given by fill. It is useful for results of table
#' or tapply when some elements had no records. 
#'
#'
#'
#'
#'
#'
#'
#'
#'
'fill.dimension'


#' fill.1dimension
#'
#' @description
#' Filling a 1D array resulting from tapply. Same as fill.dimension, but for a vector. 
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
'fill.1dimension'


#' convert.factor
#'
#' @description
#' Converts any character fields in a dataframe from factors to character fields. 
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
'convert.factor'


#' split.data
#'
#' @description
#' Converts a big dataframe into a list of smaller dataframes, grouped using any
#' column in the database, or any external vector. The variable allsplit can be set to a vector
#' of data sections to be included; if allsplit includes values not in the data, empty elements
#' are included in the list, but if allsplit includes fewer values than found in the data, then
#' missing elements are omitted. 
#' The option keepsplitcol can be set to TRUE
#' in order to retain in the new dataframes the column on which the data are grouped; otherwise, that
#' column is removed. 
#'
#'
#'
#'
#'
#'
#'
#'
#'
'split.data'


#' merge.data
#'
#' @description
#' Combine many dataframes as elements of a list into a single large dataframe. Each individual dataframe must
#' have exactly the same columns. This is exactly the opposite operation as split.data. 
#'
#'
#'
#'
#'
#'
#'
#'
#'
'merge.data'


#' pst
#'
#' @description
#' A version of paste with sep=#'.
#'
#'
#'
#'
#'
#'
#'
#'
#'
'pst'


#' detachfiles
#'
#' @description
#' detachs from the searchpath files matching a submitted vector of names.
#'
#'
#'
#'
#'
#'
#'
#'
#'
'detachfiles'


#' save.searchpath
#'
#' @description
#' Saves all functions in position n to the file already 
#' attached at that position; n can be a vector. Allows changes in attached data to be saved easily, but
#' please use with care, as it will over-write the existing file. 
#'
#'
#'
#'
#'
#'
#'
#'
#'
'save.searchpath'


#' gsp
#'
#' @description
#' Returns one of the objects at a given search position. This provides a way to write programs to
#' check multiple sets of data attached at different positions. 
#'
#'
#'
#'
#'
#'
#'
#'
#'
'gsp'


#' match.dataframe
#'
#' @description
#' Matches two dataframes using two or more columns. R's function match() works only on vectors 
#'(and thus single columns only). The return is a vector of indices, exactly as match() does. 
#'
#'
#'
#'
#'
#'
#'
#'
#'
'match.dataframe'


#' trim
#'
#' @description
#' Trims leading and trailing blanks from a vector of character variables. Multibyte character strings are returned intact.
#'(extended ascii in R appears as hex values).
#'
#'
#'
#'
#'
#'
#'
#'
'trim'


#' StringToVect
#'
#' @description
#' Converts a character string into a vector of individual characters.
#'
#'
#'
#' @examples
#' \dontrun{
#' StringToVect('anystring')}
#'
#'
#'
#'
'StringToVect'


#' charlocate
#'
#' @description
#' Finds position of a substring needle inside a longer string haystack. Can return more than one position if the needle appears more
#' than once.  Returns 0 if no matches. The input are atomic: this is not vectorized.
#'
#'
#'
#' @examples
#' \dontrun{
#' charlocate("19","x190019xxx")}
#'
#'
#'
#'
'charlocate'


#' left
#'
#' @description
#' A standard left function. Returns the leftmost n characters of a string. If n<0, returns all except the rightmost n. If n==0, returns an
#' empty string. Arguments can be vectors, but both must be the same length, or n can be a scalar.
#'
#'
#'
#'
#'
#'
#'
#'
#'
'left'


#' leftbut
#'
#' @description
#' Returns the leftmost characters of a string, excluding the last n.
#'
#'
#'
#'
#'
#'
#'
#'
#'
'leftbut'


#' right
#'
#' @description
#' Returns the rightmost n characters of a string
#'
#'
#'
#'
#'
#'
#'
#'
#'
'right'


#' rightbut
#'
#' @description
#' Returns the rightmost characters of a string, excluding the initial n.
#'
#'
#'
#'
#'
#'
#'
'rightbut'


#' explode
#'
#' @description
#' Split a single (atomic) character variable into sections, separated on sep. With the default, sep=#', it divides the string into it's individual letters.  
#'
#'
#'
#'
#'
#'
#'
'explode'


#' ditch
#'
#' @description
#' Detaches all files at one or more search positions; v can be a vector. 
#'
#'
#'
#'
#'
#'
#'
#'
#'
'ditch'


#' is.leap
#'
#' @description
#' Return a logical indicating which elements of a vector are leap years.
#'
#'
#'
#'
#'
#'
#'
#'
#'
'is.leap'


#' mergeParam
#'
#' @description
#' Merges a list of parameter matrices into one
#' large matrix. Used for the parameter output from MCMCmetrop1R, stored
#' as a list. No longer used; superseded by merge.data. 
#'
#'
#'
#'
#'
#'
#'
#'
#'
'mergeParam'


#' countzero
#'
#' @description
#' Counts vector elements exactly zero.
#'
#'
#'
#'
#'
#'
#'
#'
#'
'countzero'


#' countone
#'
#' @description
#' Counts vector elements exactly one.
#'
#'
#'
#'
#'
#'
#'
#'
#'
'countone'


#' countNA
#'
#' @description
#' Counts vector elements that are NA.
#'
#'
#'
#'
#'
#'
#'
#'
#'
'countNA'


#' countEmpty
#'
#' @description
#' Counts vector elements that are NA or a string of no length.
#'
#'
#'
#'
#'
#'
#'
#'
#'
'countEmpty'


#' countpresent
#'
#' @description
#' Counts vector elements > 0. See countspp as well.
#'
#'
#'
#'
#'
#'
#'
#'
#'
'countpresent'


#' find.nonNA
#'
#' @description
#' Returns the first value of a vector x which is not NA
#'
#'
#'
#'
#'
#'
#'
#'
#'
'find.nonNA'


#' which.nonNA
#'
#' @description
#' Returns the first index at which a vector x is not NA
#'
#'
#'
#'
#'
#'
#'
#'
#'
'which.nonNA'


#' which.allnonNA
#'
#' @description
#' Finds all values of a vector which are not NA. Fills out a vector to
#' length 6 with NAs if there are fewer than 6.
#'
#'
#'
#'
#'
#'
#'
#'
#'
'which.allnonNA'


#' which.vmatch
#'
#' @description
#' Finds which subsequent element of a vector matches the first element.
#'
#'
#'
#'
#'
#'
#'
#'
#'
'which.vmatch'


#' logical.grep
#'
#' @description
#' A form of grep returning logical instead of indices (numbers).
'logical.grep'


#' nhd
#'
#' @description
#' A version of head with only 6 columns shown.
#'
#'
#'
#'
#'
#'
#'
#'
#'
'nhd'


#' TextToRdata
#'
#' @description
#' Reads a tab-delimited text file and save as rdata.
#'
#'
#'
#'
#'
#'
#'
#'
#'
'TextToRdata'


#' CountByGroup
#'
#' @description
#' Groups a dataframe by one or more columns (named by groupcol). This does exactly what
#' COUNT(*) GROUP BY does in SQL.
#'
#'
#'
#'
#'
#'
#'
#'
#'
'CountByGroup'


#' which.maxNAs
#'
#' @description
#' An unfortunate bug in which.max: if all elements are NA, it doesn't return anything. This means that for any vector, which.max returns
#' a vector of length 1 unless all are NAs. This is a silly error, since a program expecting an element of length 1 falls apart otherwise.
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
'which.maxNAs'


#' attach_if_needed
#'
#' @description
#' Attach one or more datafiles,checking first whether the file is already attached. If it is attached,
#' it is not reattached, and the search position where attached is returned. 
#'
#'
#'
#'
#'
#'
#'
#'
#'
'attach_if_needed'


#' IfElse
#'
#' @description
#' A more convenient version of the R function ifelse in cases where test, a, and b are atomic.  
#'
#'
#'
#'
#'
'IfElse'


#' AssignDiag
#'
#' @description
#' A way to assign the diagonals of a matrix that can handle input having no dimensions. Ordinarily, x is square matrix and newdiag is a vector equal in length to x's diagonal.
#' A new x is returned having the newdiag on its diagonal. In that usage, it matches the assign option for R's function diag. 
#' This improves diag by handling x with no dimensions, ie a scalar, or just one dimension. Then newdiag is simply returned. 
#'
#'
#'
#'
#'
#'
#'
#'
'AssignDiag'


#' vectToCommas
#'
#' @description
#' Given a vector of character variables, collapse into a single string with quotes, separated by commas
#'
#'
#'
#'
#'
#'
#'
#'
'vectToCommas'


#' drp
#'
#' @description
#' A version of drop which includes as.matrix. Without it, drop does not serve its purpose. This is necessary in many many situations
#' where a single row is taken out of a dataframe, but must be passed as a vector.
#'
#'
#'
#'
#'
#'
#'
#'
'drp'


#' randomRow
#'
#' @description
#' Return a random row from a dataframe
#'
#'
#'
#'
#'
#'
'randomRow'


#' randomRow
#'
#' @description
#' Return a random element from a vector
#'
#'
#'
#'
#'
#'
'randomRow'


#' countUnique
#'
#' @description
#' Count the number of unique elements in a vector
#'
#'
#'
#'
#'
#'
'countUnique'


#' graphFilledBand
#'
#' @description
#' Fill the area between two curves on a graph. Useful for confidence limits, for example. 
#' Typical use is to draw a graph first with some central y values, then add a confidence band by filling the area between upper and lower confidence limits
#'(designated by variables lower.y and upper.y in the example below). The central line should then be redrawn over the filled area.
#'
#' @param x = The x axis values, as in any R graph
#' @param y1, y2 = Two sets of y axis values, each of exactly the same length as x
#' @param fillcol = The color filling the area between the two curves
#' @param add = TRUE or FALSE, as in other R graphs; if TRUE, there must be an appropriate existing graph, otherwise, a new graph is drawn
#' @param linecol = If add==FALSE, lines drawn at y1 and y2 are this color; ignored if add==TRUE
#' @param ltype = If add==FALSE, lines drawn at y1 and y2 are this type ('solid','dashed',etc);  ignored if add==TRUE
#' @param lwidth = If add==FALSE, lines drawn at y1 and y2 are this thickness (a number);  ignored if add==TRUE
#'
#'@examples
#' \dontrun{
#' plot(x,y,type='l')
#' graphFilledBand(x,lower.y,upper.y)
#' lines(x,y)}
#'
#'
'graphFilledBand'


#' make.CredIntervalVect
#'
#' @description
#' Take a vector y having mean then lower and upper credible limits and convert to character string with parentheses. Result is a single (atomic) character vector with mean followed by parentheses enclosing lower and upper limits. This is not vectorized: it will work only with a single vector of length 3. Use apply to repeat for rows of a matrix. 
#'
#' @param y must be a vector of length 3, not a matrix. 
#' @param digits number of decimal places in result, first for the mean, then the two credible limits
#'
#'@examples
#' \dontrun{
#' make.CredIntervalVect(c(3.124,2.76,5.01),digits=c(2,1))}
#'

'make.CredIntervalVect'