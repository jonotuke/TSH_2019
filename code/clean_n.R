pacman::p_load(tidyverse, testthat)

#' Convert numbers to a single number
#'
#' @param x string vector
#' @param check return a report or not
#'
#' @return numeric vector
#' @export
#'
#' @examples
clean_n  <- function(x, check = FALSE){
  origin_x  <- x
  # Get rid of not stated
  x[str_detect(x,"not stated")]  <- NA
  # Get rid of approx
  x  <- str_replace(x, " approx", "")
  # Get rid of cases and controls
  x  <- str_replace(x, "cases", "")
  x  <- str_replace(x, "controls", "")
  # Grab enteries of type case/control and add
  case_control  <- str_match(x, "(\\d+)/(\\d+)")
  total  <- as.numeric(case_control[, 2]) + as.numeric(case_control[, 3])
  index  <- which(!is.na(total))
  x[index]  <- total[index]
  x  <- as.numeric(x)
  # Compare
  if(check){
    tab  <- 
      tibble(
        origin_x, x
      )  %>% 
      filter(origin_x != x) %>% 
      knitr::kable() 
    return(tab)
  }
  return(x)
}

x  <- c("not stated", " approx 250", "10/12", "cases 24", "controls 33")
clean_n(x, check = TRUE)
expect_equal(clean_n(x), c(NA, 250, 22, 24, 33))
