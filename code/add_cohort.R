#' add cohorts to papers
#'
#' @param n is the observed number of subjects
#'
#' @return factor for each unique size
#' @export
#'
#' @examples
add_cohort  <- function(n){
  m  <- length(unique(n))
  cohorts  <- str_c("C", 1:m)
  cohorts  <- as.character(factor(n, labels = cohorts))
  return(cohorts)
}
