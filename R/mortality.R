
#'
#'


#' mortality
#'#' @description
#' Mortality is the main function, and is constructed like 
#' growth and recruitment. It requires two complete datasets, one per census,
#' with dbh, pom, and date for every individual of all species in at least 2 censuses (see Data Format). 
#' It can then calculate mortality for the entire forest, or based on one or two user-submitted factors. 
#' Mortality is based on only on the column status: any tree without an alivecode in census 2 is considered dead.  
#' Individuals whose status is NA in either census are deleted from all counts,
#' since it's impossible to count them either as survivors or dead.
#' It requires fill.dimension and climits in utilities.r.
#' Output of the mortality function is a list with components:
#' * N, the number of individuals alive in the census 1 per category selected
#' * D, the number of individuals no longer alive in census 2
#' * rate, the mean annualized mortality rate constant per category selected, calculated as (log(N)-log(S))/time 
#' * upper, upper confidence limit of mean rate
#' * lower, lower confidence limit of mean rate
#' * time, mean time interval in years
#' * date1, mean date included individuals were measured in census 1, as julian object (R displays as date, but treats as integer)
#' * date2, mean date in census 2 
#' * dbhmean, mean dbh in census 1 of individuals included
#'#' Pass the list to assemble.demography (in utilities.r) with type="m" to convert the list a data.frame.
#'#' @param Generally, alivecode="A" suffices, as this is the standard in CTFS data for a living tree; "AS" and "AB" are seldom used now
#' @param split1 and split2 must both be vectors of character variables with exactly as many elements as there are rows in the tables census1 and census2
#'(or both can be NULL), for instance, species names, dbh categories, or quadrat numbers
#'#'
#' @examples
#' \dontrun{
#' CTFSplot("bci",56)
#' mort.data=mortality(bci.full5,bci.full6)
#' mort.data$rate
#' mort.data=growth(bci.full5,bci.full6,split1=bci.full5$sp)
#' mort.data$rate
#' assemble.demography(mort.data,type='m')}
#'
#'#'
#'#'
'mortality'


#' mortality.eachspp
#'#' @description
#' Calculate mortality for each species in given dbh categories. It sets the split variables using the species name and
#' submitted dbh classbreaks and then uses mortality to do the calculation. See argument descriptions for mortality. Return object
#' is the list from mortality and can be passed to assemble.demography for a convenient format. 
#'#'
#' @examples
#' \dontrun{
#' CTFSplot("bci",5:6)
#' mort.data=mortality.eachspp(bci.full5,bci.full6)
#' mort.table1=assemble.demography(mort.data,type="m",whichdbhcat=1)
#' mort.table2=assemble.demography(mort.data,type="m",whichdbhcat=2)
#' mort.table3=assemble.demography(mort.data,type="m",whichdbhcat=3)}
#'
#'
'mortality.eachspp'


#' mortality.dbh
#'#' @description
#' Calculate forest-wide mortality in given dbh categories. See mortality and mortality.eachspp, which have same arguments and same output format.
#'#'
#'
#'
#'
'mortality.dbh'


#' mortality.calculation
#'#' @description
#' This is the calculation of mortality rate and confidence limits, given N 
#'(number alive at the outset), S (number of survivors), and time (time interval).
#' All three can be arrays, vectors, or scalars, but all three must be identical size. 
#' It relies on find.climits. Used by mortality function, but can be used alone.
#'#'
#' @examples
#' \dontrun{
#' mortality.calculation(N=c(100,1000),S=c(75,750),meantime=c(5.1,5.1))}
#'
#'#'
'mortality.calculation'


#' find.climits
#'#' @description
#' Calculates confidence limits around a number of deaths, D, out of N individuals.
#' It uses the beta distribution as the conjugate of the binomial, so the beta is the posterior of the number
#' dying. N and D can be vectors or matrices, but must have matching dimensions.
#'#' @param N, number of individuals alive at the outset
#' @param D, number of deaths by the end
#' @param alpha, the critical probability (default alpha=0.05 gives 95% confidence limits)
#' @param kind, either "upper" or "lower"
#'#'@examples
#' \dontrun{
#' find.climits(10,5,kind='lower')}
#'

'find.climits'
