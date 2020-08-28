
  library(tidyverse)
  library(here)

  # https://www.r-bloggers.com/interpolation-and-smoothing-functions-in-base-r/

  reynolds_data <- read_csv(here::here("data-raw", "reynolds_factor.csv"))

  vec <- 10^(seq(-2,5,.025))

  pred_flow <- data.frame(
    with( reynolds_data, spline(reynolds, pred_flow, xout = vec)))

  selecting <- data.frame(
    with( reynolds_data, spline(reynolds, selecting, xout = vec)))

  pred_pressure <- data.frame(
    with( reynolds_data, spline(reynolds, pred_pressure, xout = vec)))

  reynolds_factor <- tibble( reynolds      = pred_flow$x,
                             pred_flow     = pred_flow$y,
                             selecting     = selecting$y,
                             pred_pressure = pred_pressure$y)

  reynolds_factor <- reynolds_factor %>% round(3) %>%
    filter(reynolds <= 40000) %>%
    mutate( pred_flow = replace(pred_flow, pred_flow > 1, 1),
            selecting = replace(selecting, selecting > 1, 1),
            pred_pressure = replace(pred_pressure, pred_pressure > 1, 1))

  rm(reynolds_data, vec, pred_flow, selecting, pred_pressure)

  # fr <- with( reynolds_factor, spline(reynolds, selecting, xout = c(10)))$y

  usethis::use_data(reynolds_factor, compress = "xz")

