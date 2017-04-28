#' Detect rows in a data frame full of NA (accross columns)
#'
#' @param .data A non empty matrix or data frame
#' @return A logical vector of length equal to number of rows in data.
#' @examples
#' df <- tibble::tribble(
#' ~x,  ~y,    ~z,
#'  1, "a", FALSE,
#' NA,  NA,    NA,
#' 5,  "b",  TRUE,
#' NA,  NA,    NA,
#' NA, "c",    NA
#' )
#' is_na_row(df)
is_na_row <- function(.data) {
  assertive::assert_is_non_empty(.data)
  assertive::assert_any_are_true(
    c(is.matrix(.data), is.data.frame(.data))
  )
  is_na_vector <- function(x) all(is.na(x))
  apply(.data, 1, is_na_vector)
}

#' Remove rows from data frame or matrix full of NA
#' 
#' @param .data A data frame or matrix.
#' @return Output and `.data`. have the same type.
#' @examples
#' df <- tibble::tribble(
#' ~x,  ~y,    ~z,
#'  1, "a", FALSE,
#' NA,  NA,    NA,
#' 5,  "b",  TRUE,
#' NA,  NA,    NA,
#' NA, "c",    NA
#' )
#' rm_na_row(df)
rm_na_row <- function(.data) {
  assertive::assert_is_non_empty(.data)
  assertive::assert_any_are_true(
    c(is.matrix(.data), is.data.frame(.data))
  )
  .data[!is_na_row(.data), ]
}
