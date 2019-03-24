get_label  <- function(ext = "rds"){
  # Add date
  date  <- str_c(
    year(today()),
    str_pad(month(today()), width = 2, side = "left", pad = "0"),
    day(today())
  )
  # Get contain file
  filename  <- knitr:::knit_concord$get("infile")
  filename  <- str_replace(filename, "\\.rmd","")
  return(glue("{date}-{filename}.{ext}"))
}