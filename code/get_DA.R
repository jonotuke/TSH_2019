#' Selects one from a set of models
#'
#' @param x column of sophistication
#' @param type type of model to choose
#'
#' @return one is selected. 
#' @export
#'
#' @examples
get_DA  <- function(size_col, method_col, method = "min", size = "min"){
  df  <- tibble(size = size_col, method = method_col)
  chosen  <- logical(nrow(df))
  df  <- 
    df %>% 
    mutate(
      n_cov = str_split(method, ",") %>% map_dbl(length), 
      ID = row_number()
    )
  if(method == "min" & size == "min"){
    df  <- 
      df %>% 
      filter(
        size == min(size)
        ) %>% 
      filter(
        n_cov == min(n_cov)
      )
  }
  if(method == "min" & size == "max"){
    df  <- 
      df %>% 
      filter(
        size == max(size)
      ) %>% 
      filter(
        n_cov == min(n_cov)
      )
  }
  if(method == "max" & size == "min"){
    df  <- 
      df %>% 
      filter(
        size == min(size)
      ) %>% 
      filter(
        n_cov == max(n_cov)
      )
  }
  if(method == "max" & size == "max"){
    df  <- 
      df %>% 
      filter(
        size == max(size)
      ) %>% 
      filter(
        n_cov == max(n_cov)
      )
  }
  # Sample
  df  <- df %>% sample_n(1)
  chosen[df %>% pull(ID)]  <- TRUE
  return(chosen)
}

## Driver
# TSH %>% 
#   ungroup() %>% 
#   filter(Paper == TSH$Paper[1]) %>%
#   mutate(
#     test = get_DA(FT4_N, FT4_cov, size = "min", method = "min")
#   )
# 
# TSH_test  <- 
#   TSH %>% 
#   group_by(Paper) %>% 
#   mutate(
#     min_size_min_method = get_DA(FT4_N, FT4_cov, size = 'min', method = 'min'),
#     min_size_max_method = get_DA(FT4_N, FT4_cov, size = 'min', method = 'max'),
#     max_size_min_method = get_DA(FT4_N, FT4_cov, size = 'max', method = 'min'),
#     max_size_max_method = get_DA(FT4_N, FT4_cov, size = 'max', method = 'max')
#   )
# 
# TSH_test %>% filter(min_size_min_method)
# TSH_test %>% filter(min_size_max_method)
# TSH_test %>% filter(max_size_min_method)
# TSH_test %>% filter(max_size_max_method)