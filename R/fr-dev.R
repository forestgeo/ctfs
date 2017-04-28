#' Show functions mistaken as S3 methods
#'
#' @export
#' @keywords internal
mistake_s3 <- function(x) {
  txt <- readr::read_lines("NAMESPACE")
  txt <- stringr::str_extract(txt, "S3method(.*),(.*)$")
  txt <- tibble::as_tibble(txt)
  txt <- rm_na_row(txt)
  txt$value <- stringr::str_replace_all(
    txt$value,
    "S3method\\((.*),(.*)\\)$", "\\1.\\2"
  )
  txt$value <- stringr::str_replace_all(txt$value, "\\", "")
  txt
}
