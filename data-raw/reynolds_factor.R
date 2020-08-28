
  library(tidyverse)
  library(here)

  reynolds_factor <- read_csv(here::here("data-raw", "fr.csv"),
    col_types = cols(problem_typ =
                      col_factor(levels = c("selection", "flow", "pressure"))))

  usethis::use_data(reynolds_factor,
                    compress = "xz",
                    overwrite = TRUE)

  # https://www.r-bloggers.com/interpolation-and-smoothing-functions-in-base-r/

  # reynolds_factor %>%
  #   filter(problem_typ == "selection") %>%
  #   with(spline(reynolds, fr, xout = c(340)))$y

