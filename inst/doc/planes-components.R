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

library(magrittr)
library(dplyr)

## ----eval=TRUE, echo=FALSE----------------------------------------------------
tibble(
  `Component` = c("Difference","Coverage","Taper","Repeat","Trend", "Shape", "Zero"),
  `Description` = c("Point-to-point difference","Prediction interval covers the most recent observation","Prediction interval narrows as horizon increases","Values repeat more than expected","Signal exhibits change in trend compared to recent observations", "Shape of signal trajectory has not been observed in seed data", "Zeros found in signal when not in seed"),
  `Function` = c("plane_diff()","plane_cover()","plane_taper()","plane_repeat()","plane_trend()", "plane_shape()", "plane_zero()"),
  `Forecast` = c("YES","YES","YES","YES","YES", "YES", "YES"),
  `Observed` = c("YES","NO","NO","YES","NO", "NO", "YES"),
  `Parameters` = c("None","None","None","Tolerated number of repeats; Number of observations to prepend","Significance level for trend change", "Method used to identify unique shapes (sdiff or dtw)", "None")
) %>%
  knitr::kable()

## ----message = FALSE----------------------------------------------------------
## load packages
library(rplanes)
library(dplyr)
library(ggplot2)

## read in observed data
hosp_all <-
  read.csv(system.file("extdata/observed/hdgov_hosp_weekly.csv", package = "rplanes")) %>%
  select(date, location, flu.admits) %>%
  mutate(date = as.Date(date))

## prepare observed signal
observed_signal <- to_signal(input = hosp_all, outcome = "flu.admits", type = "observed", resolution = "weeks", horizon = NULL)

## create seed with cut date
prepped_seed <- plane_seed(observed_signal, cut_date = "2022-06-04")

## -----------------------------------------------------------------------------
point_est <- c(100, 120, 140, 160)

prepped_forecast <-
  tibble(
    location = "01",
    date = seq(as.Date("2022-06-11"), as.Date("2022-07-02"), by = 7),
    horizon = 1:4,
    lower = point_est - c(10, 20, 30, 40),
    ## make a large jump in hospitalizations to trigger diff component
    point = point_est,
    upper = point_est + c(10, 20, 30, 40),
  ) %>%
  to_signal(outcome = "flu.admits", type = "forecast", horizon = 4)

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  prepped_forecast$data

## ----echo=FALSE, eval=TRUE----------------------------------------------------
prepped_forecast$data %>%
  knitr::kable(.)

## -----------------------------------------------------------------------------
prepped_seed$`01`$last_value

prepped_seed$`01`$diff$max

## -----------------------------------------------------------------------------
plane_diff(location = "01", input = prepped_forecast, seed = prepped_seed)

## -----------------------------------------------------------------------------
diff_dat <- 
  prepped_forecast$data %>%
  mutate(type = "forecast") %>%
  rename(flu.admits = point) %>%
  bind_rows(observed_signal$data %>% filter(location == "01") %>% filter(date <= "2022-06-04") %>% mutate(type="observed"), . )

diff_flags <-
  diff_dat %>%
  filter(type == "forecast") %>%
  filter(date == min(date))

diff_dat %>%
  ggplot(mapping = aes(x = date, y = flu.admits)) +
  geom_line(lty = "dotted") +
  geom_line(aes(colour = type)) +
  geom_point(aes(colour = type)) +
  geom_point(data = diff_flags, mapping = aes(x = date, y = flu.admits), shape=23, size=4, color = "black") +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = type), alpha = 0.2) +
  xlab("Date") +
  ylab("Flu hospitalizations") +
  ggtitle("Difference component\nFlagged")

## -----------------------------------------------------------------------------
point_est <- c(28, 31, 34, 37)

prepped_forecast <-
  tibble(
    location = "01",
    date = seq(as.Date("2022-06-11"), as.Date("2022-07-02"), by = 7),
    horizon = 1:4,
    lower = point_est - c(5, 10, 15, 20),
    point = point_est,
    upper = point_est + c(5, 10, 15, 20),
  ) %>%
  to_signal(outcome = "flu.admits", type = "forecast", horizon = 4)

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  prepped_forecast$data

## ----echo=FALSE, eval=TRUE----------------------------------------------------
prepped_forecast$data %>%
  knitr::kable(.)

## -----------------------------------------------------------------------------
prepped_seed$`01`$last_value

prepped_seed$`01`$diff$max

## -----------------------------------------------------------------------------
plane_diff(location = "01", input = prepped_forecast, seed = prepped_seed)

## -----------------------------------------------------------------------------
diff_dat <- 
  prepped_forecast$data %>%
  mutate(type = "forecast") %>%
  rename(flu.admits = point) %>%
  bind_rows(observed_signal$data %>% filter(location == "01") %>% filter(date <= "2022-06-04") %>% mutate(type="observed"), . )

diff_dat %>%
  ggplot(mapping = aes(x = date, y = flu.admits)) +
  geom_line(lty = "dotted") +
  geom_line(aes(colour = type)) +
  geom_point(aes(colour = type)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = type), alpha = 0.2) +
  xlab("Date") +
  ylab("Flu hospitalizations") +
  ggtitle("Difference component\nNot flagged")

## -----------------------------------------------------------------------------
## make sure the 1 week-ahead point estimate and PI do not cover the last reported obs
point_est <- c(60, 62, 64, 66)

prepped_forecast <-
  tibble(
    location = "01",
    date = seq(as.Date("2022-06-11"), as.Date("2022-07-02"), by = 7),
    horizon = 1:4,
    lower = point_est - c(2, 4, 6, 8),
    point = point_est,
    upper = point_est + c(2, 4, 6, 8)
  ) %>%
  to_signal(outcome = "flu.admits", type = "forecast", horizon = 4)

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  prepped_forecast$data

## ----echo=FALSE, eval=TRUE----------------------------------------------------
prepped_forecast$data %>%
  knitr::kable(.)

## -----------------------------------------------------------------------------
prepped_seed$`01`$last_value

## -----------------------------------------------------------------------------
plane_cover(location = "01", input = prepped_forecast, seed = prepped_seed)

## -----------------------------------------------------------------------------
cover_dat <- 
  prepped_forecast$data %>%
  mutate(type = "forecast") %>%
  rename(flu.admits = point) %>%
  bind_rows(observed_signal$data %>% filter(location == "01") %>% filter(date <= "2022-06-04") %>% mutate(type="observed"), . )

cov_flags <-
  cover_dat %>%
  filter(type == "observed") %>%
  filter(date == max(date))

ggplot(data = cover_dat, mapping = aes(x = date, y = flu.admits)) +
  geom_line(lty="dotted") +
  geom_line(aes(colour = type)) +
  geom_point(aes(colour = type)) +
  geom_point(data = cov_flags, mapping = aes(x = date, y = flu.admits), shape=23, size=4, color = "black") +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = type), alpha = 0.2) +
  xlab("Date") +
  ylab("Flu hospitalizations") +
  ggtitle(paste("Coverage component\nFlagged"))

## -----------------------------------------------------------------------------
## make sure the 1 week-ahead point estimate and PI cover the last reported obs
point_est <- c(28, 31, 34, 37)

prepped_forecast <-
  tibble(
    location = "01",
    date = seq(as.Date("2022-06-11"), as.Date("2022-07-02"), by = 7),
    horizon = 1:4,
    lower = point_est - 28,
    point = point_est,
    upper = point_est + 28
    ) %>%
  to_signal(outcome = "flu.admits", type = "forecast", horizon = 4)

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  prepped_forecast$data

## ----echo=FALSE, eval=TRUE----------------------------------------------------
prepped_forecast$data %>%
  knitr::kable(.)

## -----------------------------------------------------------------------------
prepped_seed$`01`$last_value

## -----------------------------------------------------------------------------
plane_cover(location = "01", input = prepped_forecast, seed = prepped_seed)

## -----------------------------------------------------------------------------
cover_dat <- 
  prepped_forecast$data %>%
  mutate(type = "forecast") %>%
  rename(flu.admits = point) %>%
  bind_rows(observed_signal$data %>% filter(location == "01") %>% filter(date <= "2022-06-04") %>% mutate(type="observed"), . )

ggplot(data = cover_dat, mapping = aes(x = date, y = flu.admits)) +
  geom_line(lty="dotted") +
  geom_line(aes(colour = type)) +
  geom_point(aes(colour = type)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = type), alpha = 0.2) +
  xlab("Date") +
  ylab("Flu hospitalizations") +
  ggtitle(paste("Coverage component\nNot flagged"))

## -----------------------------------------------------------------------------
point_est <- c(30, 33, 36, 39)

prepped_forecast <-
  tibble(
    location = "01",
    date = seq(as.Date("2022-06-11"), as.Date("2022-07-02"), by = 7),
    horizon = 1:4,
    ## make the lower and upper bounds get narrower as horizon increases
    lower = point_est - c(20, 15, 10, 5),
    point = point_est,
    upper = point_est + c(20, 15, 10, 5)
  ) %>%
  to_signal(outcome = "flu.admits", type = "forecast", horizon = 4)

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  prepped_forecast$data

## ----echo=FALSE, eval=TRUE----------------------------------------------------
prepped_forecast$data %>%
  knitr::kable(.)

## -----------------------------------------------------------------------------
plane_taper(location = "01", input = prepped_forecast, seed = prepped_seed)

## -----------------------------------------------------------------------------
taper_dat <- 
  prepped_forecast$data %>%
  mutate(type = "forecast") %>%
  rename(flu.admits = point) %>%
  bind_rows(observed_signal$data %>% filter(location == "01") %>% filter(date <= "2022-06-04") %>% mutate(type="observed"), . )

taper_flags <-
  taper_dat %>%
  filter(type == "forecast")

taper_dat %>%
  ggplot(data = taper_dat, mapping = aes(x = date, y = flu.admits)) +
  geom_line(lty="dotted") +
  geom_line(aes(colour = type)) +
  geom_point(aes(colour = type)) +
  geom_point(data = taper_flags, mapping = aes(x = date, y = flu.admits), shape=23, size=4, color = "black") +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = type), alpha = 0.2) +
  xlab("Date") +
  ylab("Flu Hospital Admissions") +
  ggtitle(paste("Taper component\nFlagged"))

## -----------------------------------------------------------------------------
point_est <- c(30, 33, 36, 39)

prepped_forecast <-
  tibble(
    location = "01",
    date = seq(as.Date("2022-06-11"), as.Date("2022-07-02"), by = 7),
    horizon = 1:4,
    ## make the lower and upper bounds get wider as horizon increases
    lower = point_est - c(5, 10, 15, 20),
    point = point_est,
    upper = point_est + c(5, 10, 15, 20)
  ) %>%
  to_signal(outcome = "flu.admits", type = "forecast", horizon = 4)

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  prepped_forecast$data

## ----echo=FALSE, eval=TRUE----------------------------------------------------
prepped_forecast$data %>%
  knitr::kable(.)

## -----------------------------------------------------------------------------
plane_taper(location = "01", input = prepped_forecast, seed = prepped_seed)

## -----------------------------------------------------------------------------
taper_dat <- 
  prepped_forecast$data %>%
  mutate(type = "forecast") %>%
  rename(flu.admits = point) %>%
  bind_rows(observed_signal$data %>% filter(location == "01") %>% filter(date <= "2022-06-04") %>% mutate(type="observed"), . )

taper_dat %>%
  ggplot(data = taper_dat, mapping = aes(x = date, y = flu.admits)) +
  geom_line(lty="dotted") +
  geom_line(aes(colour = type)) +
  geom_point(aes(colour = type)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = type), alpha = 0.2) +
  xlab("Date") +
  ylab("Flu Hospital Admissions") +
  ggtitle(paste("Taper component\nNot flagged"))

## -----------------------------------------------------------------------------
## make sure the point estimates repeat
point_est <- c(55, 55, 55, 55)

prepped_forecast <-
  tibble(
    location = "01",
    date = seq(as.Date("2022-06-11"), as.Date("2022-07-02"), by = 7),
    horizon = 1:4,
    lower = point_est - c(5, 10, 15, 20),
    point = point_est,
    upper = point_est + c(5, 10, 15, 20)
    ) %>%
  to_signal(outcome = "flu.admits", type = "forecast", horizon = 4)

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  prepped_forecast$data

## ----echo=FALSE, eval=TRUE----------------------------------------------------
prepped_forecast$data %>%
  knitr::kable(.)

## -----------------------------------------------------------------------------
prepped_seed$`01`$max_repeats

## -----------------------------------------------------------------------------
plane_repeat(location = "01", input = prepped_forecast, seed = prepped_seed, tolerance = NULL, prepend = NULL)

## -----------------------------------------------------------------------------
repeat_dat <- 
  prepped_forecast$data %>%
  mutate(type = "forecast") %>%
  rename(flu.admits = point) %>%
  bind_rows(observed_signal$data %>% filter(location == "01") %>% filter(date <= "2022-06-04") %>% mutate(type="observed"), . )

repeat_flags <-
  repeat_dat %>%
  filter(type == "forecast")

repeat_dat %>%
  ggplot(data = repeat_dat, mapping = aes(x = date, y = flu.admits)) +
  geom_line(lty="dotted") +
  geom_line(aes(colour = type)) +
  geom_point(aes(colour = type)) +
  geom_point(data = repeat_flags, mapping = aes(x = date, y = flu.admits), shape=23, size=4, color = "black") +
  geom_ribbon(aes(ymin = lower, ymax = upper,fill = type), alpha = 0.2) +
  xlab("Date") +
  ylab("Flu hospitalizations") +
  ggtitle(paste("Repeat component\nFlagged"))

## -----------------------------------------------------------------------------
plane_repeat(location = "01", input = prepped_forecast, seed = prepped_seed, tolerance = 4, prepend = NULL)

## -----------------------------------------------------------------------------
## make sure the point estimates do not repeat
point_est <- c(55, 57, 59, 61)

prepped_forecast <-
  tibble(
    location = "01",
    date = seq(as.Date("2022-06-11"), as.Date("2022-07-02"), by = 7),
    horizon = 1:4,
    lower = point_est - c(5, 10, 15, 20),
    point = point_est,
    upper = point_est + c(5, 10, 15, 20)
    ) %>%
    to_signal(outcome = "flu.admits", type = "forecast", horizon = 4)

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  prepped_forecast$data

## ----echo=FALSE, eval=TRUE----------------------------------------------------
prepped_forecast$data %>%
  knitr::kable(.)

## -----------------------------------------------------------------------------
prepped_seed$`01`$max_repeats

## -----------------------------------------------------------------------------
plane_repeat(location = "01", input = prepped_forecast, seed = prepped_seed, tolerance = NULL, prepend = NULL)

## -----------------------------------------------------------------------------
repeat_dat <- 
  prepped_forecast$data %>%
  mutate(type = "forecast") %>%
  rename(flu.admits = point) %>%
  bind_rows(observed_signal$data %>% filter(location == "01") %>% filter(date <= "2022-06-04") %>% mutate(type="observed"), . )

repeat_dat %>%
  ggplot(data = repeat_dat, mapping = aes(x = date, y = flu.admits)) +
  geom_line(lty="dotted") +
  geom_line(aes(colour = type)) +
  geom_point(aes(colour = type)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = type), alpha = 0.2) +
  xlab("Date") +
  ylab("Flu hospitalizations") +
  ggtitle(paste("Repeat component\nNot flagged"))

## -----------------------------------------------------------------------------
point_est <- c(25, 50, 100, 200)

prepped_forecast <-
  tibble(
    location = "01",
    date = seq(as.Date("2022-06-11"), as.Date("2022-07-02"), by = 7),
    horizon = 1:4,
    lower = point_est - c(5, 10, 20, 40),
    point = point_est,
    upper = point_est + c(5, 10, 20, 40),
  ) %>%
  to_signal(outcome = "flu.admits", type = "forecast", horizon = 4)

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  prepped_forecast$data

## ----echo=FALSE, eval=TRUE----------------------------------------------------
prepped_forecast$data %>%
  knitr::kable(.)

## -----------------------------------------------------------------------------
plane_trend(location = "01", input = prepped_forecast, seed = prepped_seed, sig_lvl = 0.1)

## -----------------------------------------------------------------------------
trend_dat <- 
  prepped_forecast$data %>%
  mutate(type = "forecast") %>%
  rename(flu.admits = point) %>%
  bind_rows(observed_signal$data %>% filter(location == "01") %>% filter(date <= "2022-06-04") %>% mutate(type="observed"), . )

trend_flags <-
  plane_trend(location = "01", input = prepped_forecast, seed = prepped_seed, sig_lvl = 0.1)$output %>%
  filter(Changepoint == TRUE)


ggplot(data = trend_dat, mapping = aes(x = date, y = flu.admits)) +
  geom_line(lty="dotted") +
  geom_line(aes(colour = type)) +
  geom_point(aes(colour = type)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = type), alpha = 0.2) +
  geom_point(data = trend_flags, mapping = aes(x = Date, y = Value), shape=23, size=4) +
  xlab("Date") +
  ylab("Flu hospitalizations") +
  ggtitle(paste("Trend component\nFlagged"))

## -----------------------------------------------------------------------------
plane_trend(location = "01", input = prepped_forecast, seed = prepped_seed, sig_lvl = 0.001)

## -----------------------------------------------------------------------------
point_est <- c(40, 41, 40, 43)

prepped_forecast <-
  tibble(
    location = "01",
    date = seq(as.Date("2022-06-11"), as.Date("2022-07-02"), by = 7),
    horizon = 1:4,
    lower = point_est - c(5, 10, 15, 20),
    point = point_est,
    upper = point_est + c(5, 10, 15, 20),
  ) %>%
  to_signal(outcome = "flu.admits", type = "forecast", horizon = 4)

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  prepped_forecast$data

## ----echo=FALSE, eval=TRUE----------------------------------------------------
prepped_forecast$data %>%
  knitr::kable(.)

## -----------------------------------------------------------------------------
plane_trend(location = "01", input = prepped_forecast, seed = prepped_seed, sig_lvl = 0.1)

## -----------------------------------------------------------------------------
trend_dat <- 
  prepped_forecast$data %>%
  mutate(type = "forecast") %>%
  rename(flu.admits = point) %>%
  bind_rows(observed_signal$data %>% filter(location == "01") %>% filter(date <= "2022-06-04") %>% mutate(type="observed"), . )

trend_flags <-
  plane_trend(location = "01", input = prepped_forecast, seed = prepped_seed, sig_lvl = 0.1)$output %>%
  filter(Changepoint == TRUE)

ggplot(data = trend_dat, mapping = aes(x = date, y = flu.admits)) +
  geom_line(lty="dotted") +
  geom_line(aes(colour = type)) +
  geom_point(aes(colour = type)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = type), alpha = 0.2) +
  geom_point(data = trend_flags, mapping = aes(x = Date, y = Value), shape=23, size=4) +
  xlab("Date") +
  ylab("Flu hospitalizations") +
  ggtitle(paste("Trend component\nNot flagged"))

## -----------------------------------------------------------------------------
## create seed with cut date
prepped_seed2 <- plane_seed(observed_signal, cut_date = "2022-10-29")
point_est <- c(40, 41, 40, 43)

prepped_forecast <-
  tibble(
    location = "06",
    date = seq(as.Date("2022-11-05"), as.Date("2022-11-26"), by = 7),
    horizon = 1:4,
    lower = point_est - c(5, 10, 15, 20),
    point = point_est,
    upper = point_est + c(5, 10, 15, 20),
  ) %>%
  to_signal(outcome = "flu.admits", type = "forecast", horizon = 4)

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  prepped_forecast$data

## ----echo=FALSE, eval=TRUE----------------------------------------------------
prepped_forecast$data %>%
  knitr::kable(.)

## -----------------------------------------------------------------------------
plane_trend(location = "06", input = prepped_forecast, seed = prepped_seed2, sig_lvl = 0.1)

## -----------------------------------------------------------------------------
trend_dat <- 
  prepped_forecast$data %>%
  mutate(type = "forecast") %>%
  rename(flu.admits = point) %>%
  bind_rows(observed_signal$data %>% filter(location == "06") %>% filter(date <= "2022-10-29") %>% mutate(type="observed"), . )

trend_flags <-
  plane_trend(location = "06", input = prepped_forecast, seed = prepped_seed2, sig_lvl = 0.1)$output %>%
  filter(Changepoint == TRUE)

ggplot(data = trend_dat, mapping = aes(x = date, y = flu.admits)) +
  geom_line(lty="dotted") +
  geom_line(aes(colour = type)) +
  geom_point(aes(colour = type)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = type), alpha = 0.2) +
  geom_point(data = trend_flags, mapping = aes(x = Date, y = Value), shape=23, size=4) +
  xlab("Date") +
  ylab("Flu hospitalizations") +
  ggtitle(paste("Trend component\nNot flagged but change point in seed"))

## -----------------------------------------------------------------------------
point_est <- c(60, 60, 60, 10)

prepped_forecast <-
  tibble(
    location = "01",
    date = seq(as.Date("2022-06-11"), as.Date("2022-07-02"), by = 7),
    horizon = 1:4,
    lower = point_est - 10,
    ## make an unusual shape in hospitalizations to trigger shape component
    point = point_est,
    upper = point_est + 10,
  ) %>%
  to_signal(outcome = "flu.admits", type = "forecast", horizon = 4)

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  prepped_forecast$data

## ----echo=FALSE, eval=TRUE----------------------------------------------------
prepped_forecast$data %>%
  knitr::kable(.)

## -----------------------------------------------------------------------------
plane_shape(location = "01", input = prepped_forecast, seed = prepped_seed)

## -----------------------------------------------------------------------------
shape_dat <- 
  prepped_forecast$data %>%
  mutate(type = "forecast") %>%
  rename(flu.admits = point) %>%
  bind_rows(observed_signal$data %>% filter(location == "01") %>% filter(date <= "2022-06-04") %>% mutate(type="observed"), . )

shape_flags <-
  shape_dat %>%
  filter(type == "forecast")

shape_dat %>%
  ggplot(mapping = aes(x = date, y = flu.admits)) +
  geom_line(lty = "dotted") +
  geom_line(aes(colour = type)) +
  geom_point(aes(colour = type)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = type), alpha = 0.2) +
  geom_point(data = shape_flags, mapping = aes(x = date, y = flu.admits), shape=23, size=4, color = "black") +
  xlab("Date") +
  ylab("Flu hospitalizations") +
  ggtitle("Shape component\nFlagged")

## -----------------------------------------------------------------------------
point_est <- c(28, 18, 30, 20)

prepped_forecast <-
  tibble(
    location = "01",
    date = seq(as.Date("2022-06-11"), as.Date("2022-07-02"), by = 7),
    horizon = 1:4,
    lower = point_est - 10,
    ## make a familiar shape in hospitalizations to not trigger shape component
    point = point_est,
    upper = point_est + 10,
  ) %>%
  to_signal(outcome = "flu.admits", type = "forecast", horizon = 4)

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  prepped_forecast$data

## ----echo=FALSE, eval=TRUE----------------------------------------------------
prepped_forecast$data %>%
  knitr::kable(.)

## -----------------------------------------------------------------------------
plane_shape(location = "01", input = prepped_forecast, seed = prepped_seed)

## -----------------------------------------------------------------------------
shape_dat <- 
  prepped_forecast$data %>%
  mutate(type = "forecast") %>%
  rename(flu.admits = point) %>%
  bind_rows(observed_signal$data %>% filter(location == "01") %>% filter(date <= "2022-06-04") %>% mutate(type="observed"), . )


shape_dat %>%
  ggplot(mapping = aes(x = date, y = flu.admits)) +
  geom_line(lty = "dotted") +
  geom_line(aes(colour = type)) +
  geom_point(aes(colour = type)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = type), alpha = 0.2) +
  xlab("Date") +
  ylab("Flu hospitalizations") +
  ggtitle("Shape component\nNot Flagged")

## -----------------------------------------------------------------------------
point_est <- c(31, 30, 31, 0)

prepped_forecast <-
  tibble(
    location = "01",
    date = seq(as.Date("2022-06-11"), as.Date("2022-07-02"), by = 7),
    horizon = 1:4,
    lower = c(26,24,24,0),
    ## add zeros in hospitalizations to trigger zero component
    point = point_est,
    upper = c(36,36,38,15)
  ) %>%
  to_signal(outcome = "flu.admits", type = "forecast", horizon = 4)

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  prepped_forecast$data

## ----echo=FALSE, eval=TRUE----------------------------------------------------
prepped_forecast$data %>%
  knitr::kable(.)

## -----------------------------------------------------------------------------
prepped_seed$`01`$any_zeros

## -----------------------------------------------------------------------------
plane_zero(location = "01", input = prepped_forecast, seed = prepped_seed)

## -----------------------------------------------------------------------------
zero_dat <- 
  prepped_forecast$data %>%
  mutate(type = "forecast") %>%
  rename(flu.admits = point) %>%
  bind_rows(observed_signal$data %>% filter(location == "01") %>% filter(date <= "2022-06-04") %>% mutate(type="observed"), . )

zero_flags <- zero_dat %>%
  filter(flu.admits == 0)

zero_dat %>%
  ggplot(mapping = aes(x = date, y = flu.admits)) +
  geom_line(lty = "dotted") +
  geom_line(aes(colour = type)) +
  geom_point(data = zero_flags, mapping = aes(x = date, y = flu.admits), shape=23, size=4, color = "black") +
  geom_point(aes(colour = type)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = type), alpha = 0.2) +
  xlab("Date") +
  ylab("Flu hospitalizations") +
  ggtitle("Zero component\nFlagged")

## -----------------------------------------------------------------------------
point_est <- c(0, 6, 2, 3)

prepped_forecast <-
  tibble(
    location = "02",
    date = seq(as.Date("2022-06-11"), as.Date("2022-07-02"), by = 7),
    horizon = 1:4,
    lower = c(0,5,0,1),
    ## add zeros in hospitalizations
    point = point_est,
    upper = c(1,7,4,5),
  ) %>%
  to_signal(outcome = "flu.admits", type = "forecast", horizon = 4)

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  prepped_forecast$data

## ----echo=FALSE, eval=TRUE----------------------------------------------------
prepped_forecast$data %>%
  knitr::kable(.)

## -----------------------------------------------------------------------------
prepped_seed$`02`$any_zeros

## -----------------------------------------------------------------------------
plane_zero(location = "02", input = prepped_forecast, seed = prepped_seed)

## -----------------------------------------------------------------------------
zero_dat <- 
  prepped_forecast$data %>%
  mutate(type = "forecast") %>%
  rename(flu.admits = point) %>%
  bind_rows(observed_signal$data %>% filter(location == "02") %>% filter(date <= "2022-06-04") %>% mutate(type="observed"), . )

zero_flags <- zero_dat %>%
  filter(flu.admits == 0)

zero_dat %>%
  ggplot(mapping = aes(x = date, y = flu.admits)) +
  geom_line(lty = "dotted") +
  geom_line(aes(colour = type)) +
  geom_point(data = zero_flags, mapping = aes(x = date, y = flu.admits), shape=23, size=4, color = "black") +
  geom_point(aes(colour = type)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = type), alpha = 0.2) +
  xlab("Date") +
  ylab("Flu hospitalizations") +
  ggtitle("Zero component\nNot Flagged")

