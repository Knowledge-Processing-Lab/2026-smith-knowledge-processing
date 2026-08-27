# INFO -------------------------------------------------------------------

# PROJECT
## Paper: Knowledge and Ignorance Processing is Faster than Belief Processing
## Authors: Smith, A., Peney, T., Tidoni, E., O'Connor, R. J., & Riggs, K.

# R Script
## Purpose: This script contains several analysis functions used throughout the project.
## Authors: Peney, T.

# Setup ------------------------------------------------------------------
library(dplyr)
library(rstatix)

# Function Definitions ---------------------------------------------------
aggregate_mean <- function(data, measure, stat_func = mean, .by = p_id) {
  data |>
    summarise(
      {{ measure }} := stat_func({{ measure }}),
      n = n(),
      .by = c({{ .by }})
    )
}

rt_summary <- function(data, .by) {
  data |>
    summarise(
      mean_rt = mean(rt),
      lower_ci = t.test(rt)$conf.int[[1]],
      upper_ci = t.test(rt)$conf.int[[2]],
      n = n(),
      .by = {{ .by }}
    )
}

ttest_wrapper <- function(data, alternative, ...) {
  ttest <- rstatix::pairwise_t_test(data, alternative = alternative, ...)
  cohens_d <- rstatix::cohens_d(data, ...)
  left_join(
    ttest,
    cohens_d,
    by = intersect(names(ttest), names(cohens_d))
  )
}
