# Functions for development


# Import from other packages ----------------------------------------------

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`



# Local functions ---------------------------------------------------------

#' List functions mistakenly exported as S3 methods in NAMESPACE
#' 
#' List functions mistakenly exported as S3 methods in NAMESPACE
#'
#' @return A tibble listing functions mistakenly exported as S3 methods
#'
mistaken_s3 <- function() {
  readr::read_lines("NAMESPACE") %>% 
    stringr::str_extract("S3method(.*),(.*)$") %>% 
    tibble::as_tibble() %>% 
    rm_na_row() %>% 
    dplyr::mutate(
      value = stringr::str_replace_all(
        value, "S3method\\((.*),(.*)\\)$", "\\1.\\2"
        ),
      value = stringr::str_replace_all(value, "\"", "")
    )
}
