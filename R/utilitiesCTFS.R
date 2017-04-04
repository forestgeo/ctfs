
#'
#'


#' CTFSplot
#'#'@description
#' A convenience for attaching the R Analytical Tables, including the species table. One or more censuses can be requested, and either the
#' full'or the 'stem'tables. The path in which R Tables are stored is submitted; if in a folder called CTFSRPackage, then the default works.
#' Within that folder, there must be subfolders named full, stem, and species for the three types of tables. The function 'attach_if_needed'
#' is used, so there is no penalty to requesting a table that is already attached.
#'#' @param plot Name of plot as it appears in the names of the R Analytical Tables, in quote marks
#' @param census census numbers as they appear in the names of the R Analytical Tables; can be a vector
#' @param type either full or stem, in quote marks
#' @param path the name of the folder in which the tables are stored, defaults to CTFSRPackage
#' @param remove if TRUE, the tables are detached, otherwise they are attached
#' @param includespp can be set to FALSE if the species table is not available
#'#'
#' @examples
#' \dontrun{
#' CTFSplot(plot='bci',census=1:2)
#' CTFSplot(plot='bci',census=2:3)
#' CTFSplot(plot='sinharaja', census=3,type='stem',path='C:/SinharajaRDataTables')}
#'
#'#'
#'
'CTFSplot'


#' load.species
#'#'@description
#' A function for extracting a single species'dataframe from the large spp dataset
#'(list of dataframes, one per species). The split data file must come
#' as a name, that is in quote marks.
#'#'
#'
#'
#'
#'#'
#'
'load.species'


#' rndown5
#'#'@description
#' Rounds a numeric value to the next lowest multiple of 5.
#'#'
#'
#'
#'
#'#'
#'
'rndown5'


#' countspp
#'#'@description
#' Returns the number of elements in a numeric vector > 0. 
#'#'
#'
#'
#'
#'#'
'countspp'


#' assemble.demography
#'#'@description
#' Takes output of a demographic analysis (produced by functions growth, mortality, or pop.change)
#' and converts into one dataframe. Only indicated dbh categories are included; be sure that whichdbhcat does 
#' not exceed the number of columns in the data submitted. Type is 'g'for growth, 'm'
#' for mortality, 'ba'for basal area, 'agb'for biomass, 'r'for recruitment, and 'a'for abundance.
#'#'
#'
#' @examples
#' \dontrun{
#'#' data=pop.change(bci.full5,bci.full6,split1=bci.full5$sp);
#' result=assemble.demography(data,type='a',whichdbhcat=1)
#' data=growth(bci.full5,bci.full6,split1=bci.full5$sp)
#' result=assemble.demography(data,type='g',whichdbhcat=1)
#' data=mortality.eachspp(bci.full5,bci.full6,classbreak=c(10,100))
#' result1=assemble.demography(data,type='m',whichdbhcat=1)
#' result2=assemble.demography(data,type='m',whichdbhcat=2)}
'assemble.demography'


#' clean.demography
#'#'@description
#' This takes a CTFS demography table, output by functions mortality, growth, or recruitment,
#' and removes rows where N==0, or key data are NA. The rownames are assumed to refer to species names, and
#' some codes can be excluded using the argument excludespp. The four columns
#' can be submitted by name or number using default column headers, or by
#' setting type to 'mort'or 'abund'.
#' It returns a logical vector indicating with TRUE which rows to keep, not the cleaned table itself.
#' This was formerly in utilities.r.
#'#'
#'
#' @examples
#' \dontrun{
#'#' rows.include=clean.demography(demogtable,type='growth',Ncol='N',ratecol='ratecol');
#' result=demogtable[rows.include,]}
#'
#'#'
#'#'
'clean.demography'


#' unidentified.species
#'#'@description
#' Takes a string of species names or codes and returns a logical vector indicating
#' with TRUE those that should be excluded. Any species name (code) matching precisely the names in exactstr
#' are excluded, as well as any which has characters matching partialstr. Either or both exactstr and partialstr
#' can be NULL. The typical use if for excluding species whose codes or names
#' indicate they are not fully identified. It returns a logical vector which is TRUE for those to be excluded.
#' This was formerly in utilities.r.
#'#'
#'
#'
#'
#'#'
#'#'
'unidentified.species'


#' exclude.unidentified
#'#'@description
#' A more specialized version of unidentified species. It excludes species codes matching any listed in speciesnames
#' but only for one specific plot. This way a code can be eliminated from one plot's results, but not any other plot.
#' It returns a logical vector, TRUE for species to be excluded. This was formerly in utilities.r.
#'#'
#'
#'
#'
#'#'
#'
'exclude.unidentified'
