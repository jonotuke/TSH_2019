pacman::p_load(tidyverse, testthat)
#' Adds column that indicates if a significant results is inferred based on *
#'
#' @param x character string
#'
#' @return boolean vector
#' @export
#'
#' @examples
get_inferred  <- function(x){
  ifelse(str_detect(x, "\\*"), TRUE, FALSE)
}
expect_equal(get_inferred(c("N", "N*", "Y", "Y*")), 
             c(FALSE, TRUE, FALSE, TRUE))

