
#' #' 
'#' '


#' individual_grow.table
#'#'
#' @description
#' Create a table of individual trees and their growth over two censuses, with many species included. 
#' The option rnd is used to rounddown dbhs for certain intervals. The flag ctr is used to center the time variable,
#' which is the number of years since 1960 of the interval midpoint. There is a logarithmic transformation, for which all growth<=0
#' is converted to mingrow. There is also a power transformation, where each growth rate is raised to the power given by powertransformation.
#' In the latter, negative growths are transformed to negative, so do not need to be corrected. 
#'#'
#' 
#' @examples
#' \dontrun{
#'#' 
#' #' }
#' 
#'#' 
#' 
'individual_grow.table'


#' individual_mort.table
#'#'
#' @description
#' Create a table of individual trees and their survival status over two censuses, with many species included. 
#'#'
#' 
#' @examples
#' \dontrun{
#'#' 
#' #' }
#' 
#'#' 
#'#' 
'individual_mort.table'


#' calcMortIndivTable
#'#'
#' @description
#' Calculate mortality rate per species per census interval using the output of individual_mort.table. 
#' Formerly named calcMortlmerTable.
#'#'
#' 
#' @examples
#' \dontrun{
#'#' 
#' #' }
#' 
#'#' 
'calcMortIndivTable'


#' lmerMortLinear
#'#'
#' @description
#' A linear model of an annual mortality parameter [which is -log(annual survival)] as a function of N predictors x, which must be the first N columns of x. 
#' The parameters are standard slope and intercept for a linear model. There must be one additional column in x for the time interval t. The linear model predicts annual log(survival).
#' Return value is predicted survival rate (probability) over an interval of t years. Nothing prevents the output from being outside (0,1); that must be handled in the likelihood function.
#'#'
#' 
#' @examples
#' \dontrun{
#'#' 
#' #' }
#' 
#' 
'lmerMortLinear'


#' lmerMortFixedTime
#'#'
#' @description
#' A model for mortality as a function of a single predictor variable, with the time interval for each individual incorporated (as a secondpredictor).
#' The predictor must be an integer. The log(mortality parameter) is modeled as a different value for each distinct predictor. The number of parameters must exceed the maximum value of the predictor. 
#' The return value is a survival probability. Nothing prevents the output from being outside (0,1); that must be handled in the likelihood function.
#'#'
#' 
#' @examples
#' \dontrun{
#'#' 
#' #' }
#' 
#'
'lmerMortFixedTime'
