## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  echo=TRUE,
  comment = "#>",
  warning=FALSE,
  message=FALSE,
  fig.width = 7,
  fig.height = 5
)

## ----setup, echo=FALSE--------------------------------------------------------
library(rplanes)

## ----message = FALSE----------------------------------------------------------
library(rplanes)
library(dplyr)
library(purrr)
library(ggplot2)

## ----eval=FALSE---------------------------------------------------------------
#  hosp_all <-
#    read.csv(system.file("extdata/observed/hdgov_hosp_weekly.csv", package = "rplanes")) %>%
#    select(date, location, flu.admits) %>%
#    mutate(date = as.Date(date))
#  
#  head(hosp_all)

## ----eval = TRUE, echo=FALSE--------------------------------------------------
hosp_all <-
  read.csv(system.file("extdata/observed/hdgov_hosp_weekly.csv", package = "rplanes")) %>%
  select(date, location, flu.admits) %>%
  mutate(date = as.Date(date))

knitr::kable(head(hosp_all))

## -----------------------------------------------------------------------------
observed_signal <- to_signal(input = hosp_all, outcome = "flu.admits", type = "observed", resolution = "weeks", horizon = NULL)

## -----------------------------------------------------------------------------
prepped_seed <- plane_seed(observed_signal, cut_date = "2022-10-29")

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  forecast_fp <- system.file("extdata/forecast/2022-10-31-SigSci-TSENS.csv", package = "rplanes")
#  
#  read.csv(forecast_fp) %>%
#    head(.)

## ----echo=FALSE, eval=TRUE----------------------------------------------------
forecast_fp <- system.file("extdata/forecast/2022-10-31-SigSci-TSENS.csv", package = "rplanes")

read.csv(forecast_fp) %>% 
  head(.) %>%
  knitr::kable(.)

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  prepped_forecast <- read_forecast(forecast_fp)
#  
#  head(prepped_forecast)

## ----echo=FALSE, eval=TRUE----------------------------------------------------
prepped_forecast <- read_forecast(forecast_fp)

knitr::kable(head(prepped_forecast))

## -----------------------------------------------------------------------------
forecast_signal <- 
  prepped_forecast  %>%
  to_signal(., outcome = "flu.admits", type = "forecast", resolution = "weeks", horizon = 4)

## -----------------------------------------------------------------------------
scores <- plane_score(input = forecast_signal, seed = prepped_seed)

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  res <-
#    scores$scores_summary %>%
#    map_df(., as_tibble)
#  
#  head(res)

## ----echo=FALSE, eval=TRUE----------------------------------------------------
res <-
  scores$scores_summary %>%
  map_df(., as_tibble)
  
knitr::kable(head(res))

## ----fig.align = 'center'-----------------------------------------------------
res %>%
  count(n_flags) %>%
  mutate(n_flags = as.character(n_flags)) %>%
  ggplot(aes(n_flags,n)) +
  geom_col() +
  labs(x = "Number of flags raised", y = "Count")

## ----eval=FALSE---------------------------------------------------------------
#  hosp_pre23 <-
#    read.csv(system.file("extdata/observed/hdgov_hosp_weekly.csv", package = "rplanes")) %>%
#    select(date, location, flu.admits) %>%
#    mutate(date = as.Date(date)) %>%
#    filter(date < as.Date("2023-01-01"))
#  
#  head(hosp_pre23)

## ----eval = TRUE, echo=FALSE--------------------------------------------------
hosp_pre23 <-
  read.csv(system.file("extdata/observed/hdgov_hosp_weekly.csv", package = "rplanes")) %>%
  select(date, location, flu.admits) %>%
  mutate(date = as.Date(date)) %>%
  filter(date < as.Date("2023-01-01"))

knitr::kable(head(hosp_pre23))

## -----------------------------------------------------------------------------
observed_signal <- to_signal(input = hosp_pre23, outcome = "flu.admits", type = "observed", resolution = "weeks", horizon = NULL)

## -----------------------------------------------------------------------------
prepped_seed <- plane_seed(observed_signal, cut_date = "2022-12-24")

## -----------------------------------------------------------------------------
scores <- plane_score(observed_signal, seed = prepped_seed, components = c("repeat","diff"))

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  res <-
#    scores$scores_summary %>%
#    map_df(., as_tibble)
#  
#  head(res)

## ----echo=FALSE, eval=TRUE----------------------------------------------------
res <-
  scores$scores_summary %>%
  map_df(., as_tibble)
  
knitr::kable(head(res))

## ----fig.align = 'center'-----------------------------------------------------
res %>%
  count(n_flags) %>%
  mutate(n_flags = as.character(n_flags)) %>%
  ggplot(aes(n_flags,n)) +
  geom_col() +
  labs(x = "Number of flags raised", y = "Count")

