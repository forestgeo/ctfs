
#'
#'#'
#'


#' growth
#'#'
#'@description
#' The principle growth function, constructed like
#' recruitment and mortality. It requires two complete datasets, one per census,
#' with dbh, pom, and date for every individual of all species in at least 2 censuses (see Data Format). 
#' It calculates the mean growth rate in one or more categories defined by the split variables, split1
#' and split2. The column date is required for annualizing rates. 
#' The columns status and stemID are both required, in order to determine which stems should have dbh change calculated. 
#' The function trim.growth handles all
#' checks for trees to include; excluded are cases where the stemID changes, extreme values
#' based on err.limit and maxgrow, and trees below a minimum dbh in the
#' first census. See the description of trim.growth for more information.
#' Growth requires fill.dimension in utilities.r. 
#' Output of the growth function is a list with components:
#'@param rate, the mean annualized growth rate per category selected, either dbh increment, or relative growth  
#'@param N, the number of individuals included in the mean (not counting any excluded)
#'@param clim, width of confidence interval; add this number to the mean rate to get upper confidence limit, substract to get lower
#'@param dbhmean, mean dbh in census 1 of individuals included
#'@param time, mean time interval in years
#'@param date1, mean date included individuals were measured in census 1, as julian object (R displays as date, but treats as integer)
#'@param date2, mean date in census 2 
#'#' Pass the list to assemble.demography (in utilities.r) with type="g" to convert the list to a data.frame.
#'#'@param Usually use rounddown=FALSE; if TRUE, all dbh<55 are rounded down to the nearest multiple of 5
#'@param With method='I', annual dbh increment is calculated, (dbh2-dbh1)/time; with method='E', relative growth rate, (log(dbh2)-log(dbh1))/time
#'@param With stdev=FALSE, confidence limits are returned, otherwise the SD in growth rate per group 
#'@param dbhunit must be 'mm'or 'cm'
#'@param mindbh is the minimum dbh to include in results
#'@param growthcol defines how growth is measured, either 'dbh'or 'agb'(agb=biomass)
#'@param for err.limit and maxgrow, see trim.growth()
#'@param split1 and split2 must both be vectors of character variables with exactly as many elements as there are rows in the tables census1 and census2
#'(or both can be NULL), for instance, species names, dbh categories, or quadrat numbers
#'#'
#'@examples
#'\dontrun{
#' CTFSplot("bci",56)
#' growth.data=growth(bci.full5,bci.full6)
#' growth.data$rate
#' growth.data=growth(bci.full5,bci.full6,split1=bci.full5$sp)
#' growth.data$rate
#' assemble.demography(grow.data,type='g')}
#'
#'#'
#'#'
'growth'


#' biomass.growth
#'#'
#'@description
#' Like growth(), but calculates change in biomass (agb) instead of dbh. The census tables must have a column
#' called agb. There is no trimming done at all -- every tree is included, and its entire biomass (the agb column in the
#' standard CTFS data object has total agb, all stems included.)
#'#'
#'
#'
#'
#'#'
#'#'
'biomass.growth'


#' growth.eachspp
#'#'
#'@description
#' This calculates growth for each species in given dbh categories. It creates the split
#' variables then uses growth(). Other arguments are as in growth().
#'#'
#'
#'@examples
#'\dontrun{
#' growth.result=growth.eachspp(bci.full5,bci.full6,classbreak=c(10,50,100,300,500))}
#'
#'#'
#'#'
'growth.eachspp'


#' growth.dbh
#'#'
#'@description
#' This calculates forest-wide growth in given dbh categories. Arguments as for growth().
#'#'
#'@examples
#'\dontrun{
#' growth.dbh=growth.eachspp(bci.full5,bci.full6,classbreak=c(10,50,100,300,500))}
#'
#'#'
#'#'
'growth.dbh'


#' growth.indiv
#'#'
#'@description
#' This returns a complete table with growth rate of every individual, both relative and dbh-increment. The table
#' also includes most other key pieces of information for every individual: species, dbh, coordinates. Growth is trimmed with trim.growth,
#' and growth is returned as NA if the individual is excluded; note, though, that every individual tree is always included in the table, even
#' if growth=NA. Arguments are the same as in growth().
#'#'
#'
#'@examples
#'\dontrun{
#' growth.table=growth.indiv(bci.full5,bci.full6)}
#'
#'#'
#'#'
'growth.indiv'


#' trim.growth
#'#'
#'@description
#' This is where growth rates are excluded. It is based on 
#' a linear model estimating the standard deviation of dbh measures (due to error, that
#' is); the parameters slope and intercept define the linear relationship between
#' this error deviation and dbh. Any case where the second dbh measure is more than
#'4 standard deviations below the first is marked false, meaning it will be excluded from
#' growth measurements. The default values of slope and intercept are based on dbh
#' remeasure tests done in both 1995 and 2000 at BCI. A line was fitted through the absolute 
#' dbh errors as a function of dbh in both years; the average slope and intercept is
#' used here. The function also excludes any growth rate > 75 mm per yr, cases
#' where the stemID changes, and if the POM changes by more than 5%. 
#' All parameters for excluding growth measures based on error can be adjusted: 
#' to include all measures, set maxgrow and err.limit to very high numbers, such as 10000;
#' to include POM changes, set pomcut to a high number, such as 10;
#' to include cases where stemID changed, set exclude.stem.change=FALSE.
#' This function is usually only used inside the other growth functions. 
#' With exclude.stem.change==FALSE, keep all cases where stem changes, regardless of growth (it does not make sense to exclude
#' a record based on growth when the growth is based on different stems).
#' Note that trees are exclude if cens1$dbh<mindbh, but not if cens2$dbh<mindbh. 
#'#'
#'
#'
#'
#'#'
#'#'
'trim.growth'


#' growth.biomass.indiv
#'#'
#'@description
#' Like growth.indiv but based on agb growth, not dbh growth. Extreme growth rates (based on dbh growth) are
#' excluded, but cases where the stemID changed are not excluded. 
#' Here pomcut is used in a very specific way probably only relevant at BCI. If the second pom is higher than the first by more than
#' the pomcut, the record is not used. The function trim.growth has already eliminated cases where the stemID is unchanged and pom
#' changes, so this will only serve for cases where two different stemIDs have measurements. At BCI, in most cases where the second pom
#' is lower than the first and the stem changed, it is a legitimate stem change. But where the second pom is higher, it is really the
#' same stem measured at a different pom, and with a different stemID because BCI lacks stem tags. 
#' For most plots, especially with stem tags, the default behavior means changes in stem allow changes in pom to be included in biomass growth.
#'#'
#'
#'
#'
#'#'
#'#'
'growth.biomass.indiv'


#' DBHtransition
#'#'
#'@description
#' Calculates a transition matrix of individuals by diameter categories from two censuses.
#' The missing code (M) is checked in codes field if misscode is set; otherwise, status=M is assumed to mean missing
#' and status=AB is assumed to mean the stem was lost, so there is no dbh.
#' Growth rates above maxgrow and below mingrow are excluded, where max and min are annual increments.
#'(Not tested recently and not part of the supported CTFS R package.)
#'#'
#'
#'
#'
#'#'

'DBHtransition'
