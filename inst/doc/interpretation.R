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
  `Component` = c("Difference", "Repeat", "Shape", "Zero"),
  `Description` = c("Point-to-point difference", "Values repeat more than expected", "Shape of signal trajectory has not been observed in seed data", "Zeros found in signal when not in seed"),
  `Issue` = c("Without enough point-to-point differences evaluated in the seed data, a true and reasonable jump in the signal may be flagged as implausible.", "If there are no repeats in the seed, a single repeat will not be tolerated and will be flagged. Decreasing the prepend length and/or increasing the repeat tolerance should help mitigate this.", "A short seed object is comprised of fewer signal trajectory shapes that can be compared to the signal, so a reasonable trajectory might be erroneously flagged.", "If there are no zeros in the seed, a single zero in the signal will not be tolerated and will be flagged.")
) %>%
  knitr::kable()

