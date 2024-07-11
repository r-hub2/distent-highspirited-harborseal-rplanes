## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  message = FALSE,
  comment = "#>"
)

## ----setup, echo=FALSE--------------------------------------------------------
library(rplanes)

more <- function (.data, fill = ".", extra_rows = 4) {
    .data <- tibble::as_tibble(.data)
    filler <- matrix(fill, ncol = ncol(.data), nrow = extra_rows)
    filler <- tibble::as_tibble(filler, .name_repair = "minimal")
    names(filler) <- names(.data)
    rbind(.data, filler)
}

## ----echo=FALSE, eval=TRUE----------------------------------------------------
hosp <- read.csv(system.file("extdata/observed/hdgov_hosp_weekly.csv", package = "rplanes"))

hosp %>%
  head(9) %>%
  more(extra_rows = 1) %>%
  knitr::kable()

## ----echo=FALSE---------------------------------------------------------------
fp <- system.file("extdata/forecast/2022-10-31-SigSci-TSENS.csv", package = "rplanes")

read.csv(fp) %>%
  head(9) %>%
  more(extra_rows = 1) %>%
  knitr::kable()

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  library(rplanes)
#  rplanes_explorer(host = "0.0.0.0", launch.browser = TRUE, port = 80)

