#' Give asymptotic CI for proportion of significant
#'
#' @param df data frame with data needs column called Thyroid method and Sig
#' @param pred predictor
#' @param ci confidence level
#' @param width errorbars width
#' @param dodge how much to separate bars
#' @param check can return table instead of plot
#'
#' @return
#' @export
#'
#' @examples
visualise_ci  <- function(df, pred, angle = -90,ci = 0.95, width = 0.1, dodge = 0.4, check = FALSE){
  pd <- position_dodge(width = dodge)
  ## Enquo the column
  pred  <- enquo(pred)
  ## Get rid of missing values in sig
  df  <- df %>% 
    filter(!is.na(Sig))
  ## Get rid of missing values in pred
  df  <- df %>% 
    filter(!is.na(!!pred))
  # Get cutoff
  ci  <- (1 - ci) / 2
  z_star  <- qnorm(ci, lower.tail = FALSE)
  # Calculate CI
  df_ci  <- 
    df %>% 
    group_by(Thyroid_test, !!pred) %>% 
    summarise(
      n_sig = sum(Sig == "Y"),
      N = n(), 
      p = n_sig / N, 
      # convert - lower to zero
      lwr = max(0, p - 1.96 * sqrt(p * (1 - p) / N)),
      # bound upr to one
      upr = min(1, p + 1.96 * sqrt(p * (1 - p) / N))
    )
  if(check){
    return(df_ci)
  }
  ## Plot
  p  <- df_ci %>% 
    ggplot(aes(!!pred, p, col = Thyroid_test)) + 
    geom_point(position = pd) + 
    geom_errorbar(aes(ymin = lwr, ymax = upr), width = width, position = pd) + 
    theme(axis.text.x = element_text(angle = angle, hjust=0)) + 
    labs(y = "Proportion")
  return(p)
}
