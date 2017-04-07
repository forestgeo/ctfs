
#'
#'

#' abundance
#'
#' @description
#'
#' Calculates total abundance or basal area, dividing data with 1 or 2 categorical variables. 
#'
#'
#' BAperSpecies=abundance(bci.stem5,type='ba',mindbh=10,split1=bci.stem5$sp)
#' head(BAperSpecies$ba)
#' head(BAperSpecies$meandate)}
#'
#'
'abundance'

#' abundanceperquad
#'
#' @description
#'
#' Finds abundance, basal area, or agb of every species per square quadrat of any size; plotdim is the x dimension then y dimension of the plot and
#' must be set correctly; gridsize is the quadrat dimension. The plot is divided into a checkerboard of non-overlapping, space-filling squares.
#'
#'
#' @param dbhbreaks a vector of dbhs to define divisions of categories; the last category will be >= the final division
#'
