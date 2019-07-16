#' Selects one from a set of models
#'
#' @param x column of sophistication
#' @param type type of model to choose
#'
#' @return one is selected. 
#' @export
#'
#' @examples
get_DA  <- function(x, type = "Low"){
  chosen  <- logical(length(x))
  i  <- which(x == type)
  if(length(i) > 0){
    i  <- sample(i, 1)
    chosen[i]  <- TRUE
  }
  return(chosen)
}

## Driver
# tmp  <- 
#   TSH %>% filter(
#   Paper == "Chaker 2015", 
#   cohort == "C2"
# ) 
# get_DA(tmp$FT4_Sophistication, type = "Low")
# tmp  <- 
#   TSH %>% filter(
#   Paper == "Chaker * 2015", 
#   cohort == "C4"
# ) 
# get_DA(tmp$FT4_Sophistication, type = "High")
# TSH %>% 
#   group_by(Paper, cohort) %>% 
#   mutate(selected_low = get_DA(FT4_Sophistication, type = "Low")) %>% 
#   count(selected_low) %>% 
#   filter(selected_low == TRUE, n > 1)
# TSH %>% 
#   group_by(Paper, cohort) %>% 
#   mutate(selected_high = get_DA(FT4_Sophistication, type = "High")) %>% 
#   count(selected_high) %>% 
#   filter(selected_high == TRUE, n > 1)
