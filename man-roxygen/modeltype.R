#' @param modeltype Functional forms to fit to the distribution; it can be:
#' * Gaussian `modeltype = "norm"`, with the quotes
#' * Asymmetric Gaussian (a different standard deviation on left and right of
#' the mode) `modeltype = "asymnorm"`, with the quotes
#' * Laplace (exponential distribution, with mirror image for negative values)
#' `modeltype = "symexp"`, with the quotes
#' * Asymmetric Laplace (different rate constant for left and right of the
#' center) `modeltype = "asymexp"`, with the quotes
#' * Asymmetric power distribution (different rate constant for left and right
#' of the center) `modeltype = "asympower", with the quotes.
