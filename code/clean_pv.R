pacman::p_load(tidyverse, testthat, readxl)

#' Extracts information from PV column
#'
#' @param x string vector
#' @param check will give enteries that did not parse
#'
#' @return data frame with pvalue, OR, and count. 
#' @export
#'
#' @examples
clean_pv  <- function(x, check = FALSE){
  type  <- str_match(quo_name(enquo(x)), "\\$(.*)_PV")[, 2]
  origin_x  <- x
  # Set non stated to NA
  x[str_detect(x,"not stated")]  <- NA
  # Get bounds on PV
  bounded  <- ifelse(str_detect(x, "<"), TRUE, FALSE)
  # Remove `<`
  x  <- str_replace(x, "<","")
  # Get pv. Uses side effect of as numeric - anything containing ( or ) get
  # convert to NA
  pv  <- as.numeric(x)
  ## Get count
  count  <- str_match(x, "(\\d+/\\d+)\\((.*)\\)")
  count_estimate  <- count[, 2]
  count_CI  <- count[, 3]
  x[!is.na(count_estimate)]  <- NA
  ## Get OR
  x  <- str_replace_all(x, " ", "")
  OR  <- str_match(x, "(.*)\\((.*)\\)")
  OR_estimate  <- as.numeric(OR[, 2])
  OR_CI  <- OR[, 3]
  ## Results
  df  <- tibble(
    origin_x  = origin_x,
    !! str_c(type, "_pv") := pv, 
    !! str_c(type, "_bounded") := bounded,
    !! str_c(type, "_OR") := OR_estimate,
    !! str_c(type, "_ORCI") := OR_CI,
    !! str_c(type, "_count") := count_estimate,
    !! str_c(type, "_countCI") := count_CI
  )
  if(check){
    index  <- which(apply(df, 1, function(x){sum(is.na(x))}) > 4)
    tmp  <- df[index, ]
    tmp  <- tmp[!is.na(tmp[,1]),]
    tmp  <- tmp[!str_detect(tmp$origin_x, "not stated"),]
    return(tmp)
  }
  return(df)
}
df  <- tibble(
  x_PV = c("0.001", "<0.001", "1.1(0.9-1.2)", "not stated", "11/100(1-2)", "1(0.9-1.2)")
  )
results  <- clean_pv(df$x_PV)
results
expect_equal(results$x_pv, c(0.001, 0.001, NA, NA, NA, NA))
expect_equal(results$x_bounded, c(F, T, F, NA, F, F))
expect_equal(results$x_OR, c(NA, NA, 1.1, NA, NA, 1))
expect_equal(results$x_ORCI, c(NA, NA, "0.9-1.2", NA, NA, "0.9-1.2"))
expect_equal(results$x_count, c(NA, NA, NA, NA, "11/100", NA))
expect_equal(results$x_countCI, c(NA, NA, NA, NA, "1-2", NA))
