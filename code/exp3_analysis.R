# INFO -------------------------------------------------------------------

# PROJECT
## Paper: Knowledge and Ignorance Processing is Faster than Belief Processing
## Authors: Smith, A., Peney, T., Tidoni, E., O'Connor, R. J., & Riggs, K.

# R Script
## Purpose: Run exclusion criteria, generate descriptive statistics and compute
# confirmatory statistics for Experiment 3. Write results to file.
## Inputs: data/processed/clean_data.rds
## Outputs: outputs/exp3_results.rds
## Authors: Peney, T.

# Setup ------------------------------------------------------------------
library(tidyverse)
library(here)
library(rstatix)

here::i_am("code/exp3_analysis.R")

source(here("code/utils/exclusions.R"))
source(here("code/utils/analysis_functions.R"))

base_data <- read_rds(here("data/processed/clean_data.rds"))$E3
results <- list()

# Data Preparation -------------------------------------------------------

trimmed_data <- base_data |>
  # Manual Exclusions
  exclude_manual(
    create_exclusion_log(participants = "018", reason = "Experimenter Error")
  ) |>

  # Accuracy Exclusions
  exclude_low_accuracy(threshold = .75) |>
  filter(experimental) |>
  exclude_participant_accuracy_outliers(z_threshold = 2.5) |>

  # Trial Exclusions
  exclude_incorrect_responses() |>
  exclude_rts(`>`, 5) |>
  exclude_trial_rt_outliers(z_threshold = 2.5) |>
  exclude_low_trial_count(
    statement_types = c("Knowledge", "Belief"),
    threshold = 12
  ) |>
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
  here("outputs/exp3_results.rds")
)
