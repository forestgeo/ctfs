
#'
#'

#' recruitment
#'
#' @description
#'
#' Functions for calculating recruitment rates.
#'
#' Recruitment is the main function, and is constructed like 
#' growth and mortality. It requires two complete datasets, one per census,
#' with dbh, pom, and date for every individual of all species in at least 2 censuses. 
#'
#' It can then calculate mortality based on up to user-submitted factors. The two
#' datasets have exactly the same individuals, in exactly the same order, one 
#' individual per row.
#'
#' Recruitment is based on status and dbh. Any status indicating a live tree can be submitted
#' in the variable alivecode. Survivors are all individuals alive in both censuses,
#' with status==A in the first census, and larger than the minimum dbh in the first
#' census. The total population in the second census includes all those alive,
#' above the minimum dbh, plus any other survivors.
#'
#' As in mortality, individuals whose status is NA in either census are deleted
#' from all calculations.
#'
#' Requires fill.dimension and climit function in utilities.r.  
#'
#'
'recruitment'

#' recruitment.eachspp
#'
#' @description
#'
#' A wrapper to calculate recruitment for each species.
#'
#'

'recruitment.eachspp'
