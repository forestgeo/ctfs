
#' Topographic calculation based on stake and the height difference between them.
#'<name>
#' solve.topo
#'
#' @description
#' This is based on the problem and solution outlined in my book on plot methods (1998):
#' Each of N stakes i has an estimated height E[i] and a true height e[i]. 
#' Pairs of stakes have a height difference d[i,j], where e[i]+d[i,j]=e[j],
#' but only estimated height differences D[i,j] are known.
#' points j for which D[i,j] was measured:
#'<ul>
#' @param E[i]=mean(E[j]-D[i,j]) = (1/n[i])*sum(E[j]) - (1/n[i])*sum(D[i,j])
#' @param n[i]*E[i]-sum(E[j]) = -sum(D[i,j])
#'
#' The latter produces N equations in N unknowns, but they are exactly singular.
#' One of the points must be assigned a value, and it's easiest to set e[1] = 0.
#' The effect is to exclude the equation for i=0, but all j for which D[0,j] was
#' measured have unchanged equations (D[0,j] are included).
#' diagonal elements are the n[i], so M[i,i] = n[i] for all i. All other entries are -1 
#' where D[i,j] was measured otherwise zero. The estimated E[i] are a vector, and the
#' vector V[i] = -sum(D[i,j]), where the summation is over j only. Then M*%*E=V, Minv
#' is the inverse of M, and E=Minv*%*V.
#' They are treated as replicates with equal weight to all other estimates.
#' There would be -2 or -3 etc off the diagonal.
#' has the label of the second stake (column header pt2), and the final column has the height
#' difference at pt2 minus pt1 (column header diff). The point labelled basept is assigned elevation baseelev. 
#' The last 5 arguments allow those column headers to be reassigned.
#'
#'<arguments>
#'
#'
#' @examples
#' \dontrun{
#' See topography tutorial
#'
#'#'
#'
#'
#'
#'
#'<function>
#'<name>
#' getTopoLinks
#'
#' @description
#' This is solely for use by solve.topo. It finds all points linked via a sighting to a given point. 
#'
#'<arguments>
#'
#'
#' @examples
#' \dontrun{
#'
#'
#'#' if(length(which(linkCount>1))>0) browser()
#'
#'
#'
#'
#'<name>
#' rearrangeSurveyData
#'
#' @description
#' Takes a table of survey sightings with columns of x and y locations of two points, and converts it to the format
#' required by solve.topo. The input table must have columns x1, y1, x2, and y2. The return value is a list consisting of two dataframes: 
#'<ul>
#' @param all points found in the input table, with an integer designation assigned to each. The designation is called pt. 
#' @param the second table matches the input table, 
#' but instead of x-y coordinates for the two points, only columns pt1 and pt2 are included to 
#' indicate the two points between which a sighting was taken. 
#'
#'
#'<arguments>
#'
#'
#' @examples
#' \dontrun{
#'
#'
#'#'
#'
#'
#'

#' Topographic calculation based on stake and the height difference between them.'
