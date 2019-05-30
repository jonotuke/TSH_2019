pacman::p_load(tidyverse, refinr, testthat)
#' Looks for duplicates in levels
#'
#' @param x vector to check
#' @param check print out report of changes
#' @param blacklist list of strings to ignore
#'
#' @return new vector with duplicates gone
#' @export
#'
#' @examples
clean_levels  <- function(x, check = FALSE, blacklist = NULL){
  # Convert to character
  x  <- as.character(x)
  # Apply key collision and ngram merge
  x_refin <- x %>% 
    key_collision_merge() %>% 
    n_gram_merge()
  # Leave blacklist items as original
  if(!is.null(blacklist)){
    index  <- which(x %in% blacklist)
    x_refin[index]  <- x[index]
  }
  if(check){
    tab  <- 
      tibble(
        x, x_refin
      ) %>% 
      filter(
        x != x_refin
      ) %>% 
      knitr::kable()
    return(tab)
  }
  return(x_refin)
}

x  <- c("Bob", "BOB", "Bob ", "BOb is good", "good is bob")
clean_levels(x, check = TRUE)
clean_levels(x, check = TRUE, blacklist = c("good is bob"))
clean_levels(x)
expect_equal(clean_levels(x), c("BOB","BOB","BOB", "BOb is good", "BOb is good"))
expect_equal(clean_levels(x,blacklist = c("good is bob")), c("BOB","BOB","BOB", "BOb is good", "good is bob"))

             