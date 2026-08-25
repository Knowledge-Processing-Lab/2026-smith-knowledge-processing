# INFO -------------------------------------------------------------------

# PROJECT
## Paper: Knowledge and Ignorance Processing is Faster than Belief Processing
## Authors: Smith, A., Peney, T., Tidoni, E., O'Connor, R. J., & Riggs, K.

# R Script
## Purpose: Run exclusion criteria, generate descriptive statistics and compute
# confirmatory statistics for Experiment 2. Write results to file.
## Inputs: data/processed/clean_data.rds
## Outputs: outputs/exp2_results.rds
## Authors: Peney, T.

# Setup ------------------------------------------------------------------
library(tidyverse)
library(here)
library(rstatix)

here::i_am("code/exp2_analysis.R")

source(here("code/utils/exclusions.R"))
source(here("code/utils/analysis_functions.R"))

base_data <- read_rds(here("data/processed/clean_data.rds"))$E2
results <- list()

# Data Preparation -------------------------------------------------------

trimmed_data <- base_data |>
  # Accuracy Exclusions
  exclude_low_accuracy(threshold = .75) |>
  filter(experimental) |>
  exclude_participant_accuracy_outliers(z_threshold = 2.5) |>

  # RT Exclusions
  exclude_incorrect_responses() |>
  exclude_rts(`>`, 5) |>
  exclude_trial_rt_outliers(z_threshold = 2.5) |>
  exclude_participant_rt_outliers(z_threshold = 2.5)

results$exclusions <- report_exclusions(trimmed_data)

# Trial Aggregation ------------------------------------------------------
rt_data <- trimmed_data |>
  aggregate_mean(
    measure = rt,
    stat_func = mean,
    .by = c(p_id, statement_type)
  )

accuracy_data <- trimmed_data |>
  restore_excluded_trials() |>
  aggregate_mean(measure = accuracy, .by = c(p_id, statement_type))

results$rt_data <- rt_data

# Descriptive Statistics -------------------------------------------------

# Experimental Trial Accuracy
results$descriptives$accuracy <- accuracy_data |>
  aggregate_mean(measure = accuracy, .by = c(statement_type))

# Experimental Trial RT
results$descriptives$rt <- rt_data |> rt_summary(.by = statement_type)

# Confirmatory Analysis --------------------------------------------------

# One-way RM ANOVA - Statement Type
results$anova <- rt_data |>
  rstatix::anova_test(dv = rt, wid = p_id, within = statement_type) |>
  rstatix::get_anova_table(correction = "auto")

# Paired Samples T-Tests - Statement Type
results$ttests <- rt_data |>
  ttest_wrapper(
    formula = rt ~ statement_type,
    paired = TRUE,
    alternative = "two.sided"
  )

# Write results to file --------------------------------------------------
write_rds(
  results,
  here("outputs/exp2_results.rds")
)
