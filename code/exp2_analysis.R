# Setup ------------------------------------------------------------------

library(tidyverse)
library(here)
library(rstatix)

here::i_am("code/exp2_analysis.R")

source(here("code/utils/exclusions.R"))
source(here("code/utils/analysis_functions.R"))

base_data <- read_rds(here("data/processed/clean_data.rds"))$E2

# Data Preparation -------------------------------------------------------

trimmed_data <- base_data |>
  # Accuracy Exclusions
  exclude_low_accuracy(threshold = .75) |>
  filter(experimental) |>
  exclude_participant_accuracy_outliers(z_threshold = 2.5) |>
  report_trial_count("trials submitted to RT exclusions") |>

  # RT Exclusions
  exclude_incorrect_responses() |>
  exclude_rts(`>`, 5) |>
  exclude_trial_rt_outliers(z_threshold = 2.5) |>
  exclude_participant_rt_outliers(z_threshold = 2.5) |>
  report_exclusions()

rt_data <- trimmed_data |>
  aggregate_mean(
    measure = rt,
    stat_func = mean,
    .by = c(p_id, statement_type)
  )

accuracy_data <- trimmed_data |>
  restore_excluded_trials() |>
  aggregate_mean(measure = accuracy, .by = c(p_id, statement_type))

write_rds(
  rt_data,
  here("data/processed/exp2_confirmatory_data.rds")
)

# Descriptive Statistics -------------------------------------------------

# Experimental Trial Accuracy
accuracy_data |> aggregate_mean(measure = accuracy, .by = c(statement_type))

# Experimental Trial RT
rt_data |> rt_summary(.by = statement_type)

# Confirmatory Analysis --------------------------------------------------

# One-way RM ANOVA - Statement Type
rt_data |>
  rstatix::anova_test(dv = rt, wid = p_id, within = statement_type) |>
  rstatix::get_anova_table(correction = "auto")

# Paired Samples T-Tests - Statement Type
rt_data |>
  ttest_wrapper(
    formula = rt ~ statement_type,
    paired = TRUE,
    alternative = "two.sided"
  )
