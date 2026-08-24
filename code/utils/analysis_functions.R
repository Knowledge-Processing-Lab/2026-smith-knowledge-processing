library(tidyverse)

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

report_ttest <- function(res) {
  print(glue::glue(
    "{res$group1} + {res$group2} t({res$df}) = {abs(round(res$statistic, 2))}, p = {round(res$p, 3)}, d = {abs(round(res$effsize, 3))}"
  ))
  return(res)
}

report_rt_means <- function(rt_means, test_var = "statement_type") {
  print(glue::glue(
    "{rt_means[[test_var]]}- mean = {round(rt_means$mean_rt, 2)} s, \\ 
    95% CI [{round(rt_means$lower_ci, 2)}, {round(rt_means$upper_ci, 2)}]"
  ))
  return(rt_means)
}
