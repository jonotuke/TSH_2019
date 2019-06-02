pacman::p_load(glue)
get_cov  <- function(cov){
  f1  <- as.formula(glue("Sig ~ Thyroid_test * {cov} + (1 | Paper)"))
  M1  <- glmer(f1, data = TSH_long, family = binomial)
  f2  <- as.formula(glue("Sig ~ Thyroid_test + {cov} + (1 | Paper)"))
  M2  <- glmer(f2, data = TSH_long, family = binomial)
  f3  <- as.formula(glue("Sig ~ Thyroid_test + (1 | Paper)"))
  M3  <- glmer(f3, data = TSH_long, family = binomial)
  f4  <- as.formula(glue("Sig ~ Thyroid_test * {cov}"))
  M4  <- glm(f4, data = TSH_long, family = binomial)
  f5  <- as.formula(glue("Sig ~ Thyroid_test + {cov}"))
  M5  <- glm(f5, data = TSH_long, family = binomial)
  f6  <- as.formula(glue("Sig ~ Thyroid_test"))
  M6  <- glm(f6, data = TSH_long, family = binomial)
  models  <- list(M1, M2, M3, M4, M5, M6)
  model_info  <- models %>% map_df(broom::glance)
  get_converge  <- function(M){
    if("glm" %in% class(M)){
      return(M$converged)
    }
    code  <- M@optinfo$conv$lme4$code
    if(is.null(code)){
      return(TRUE)
    }
    return(FALSE)
  }
  model_info$converge  <- models %>% map_lgl(get_converge)
  model_info$models  <- c("interaction + RI", "test + main + RI", "test + RI", 
                          "interaction", "test + main", "test")
  return(model_info)
}
