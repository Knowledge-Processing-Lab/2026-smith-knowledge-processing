# INFO -------------------------------------------------------------------

# PROJECT
## Paper: Knowledge and Ignorance Processing is Faster than Belief Processing
## Authors: Smith, A., Peney, T., Tidoni, E., O'Connor, R. J., & Riggs, K.

# R Script
## Purpose: Conduct preregistered supplementary analysis for Experiment 2.
# Only includes participants who contributed to the confirmatory analysis of Experiment 2.
# Exclusions are recalculated per analysis. Repeated analysis pipeline is captured as a function.

## Inputs: outputs/exp2_results.rds$rt_data
## Outputs: TODO
## Authors: Peney, T.

# Setup ------------------------------------------------------------------
library(here)
library(readr)
library(dplyr)
library(rstatix)

here::i_am("code/exp2_supplementary_analysis.R")

source(here("code/utils/exclusions.R"))
source(here("code/utils/analysis_functions.R"))

results <- list()

# Load Data --------------------------------------------------------------

base_data <- read_rds(here("data/processed/clean_data.rds"))$E2
confirmatory_data <- read_rds(here("outputs/exp2_results.rds"))$rt_data

# Filter data to only those participants who contributed to the confirmatory analysis
exploratory_data <- base_data |>
  filter(p_id %in% confirmatory_data$p_id)

# Define Generic Functions -----------------------------------------------

apply_exclusions <- function(data) {
  data |>
    exclude_participant_accuracy_outliers(z_threshold = 2.5) |>
    exclude_incorrect_responses() |>
    exclude_rts(`>`, 5) |>
    exclude_trial_rt_outliers(z_threshold = 2.5) |>
    exclude_participant_rt_outliers(z_threshold = 2.5)
}

run_rt_ttest <- function(data, test_var, .by) {
  results <- list()
  rt_data <- data |>
    aggregate_mean(
      measure = rt,
      stat_func = mean,
      .by = c(p_id, {{ test_var }}, {{ .by }})
    )

  results$rt_data <- rt_data

  accuracy_data <- data |>
    restore_excluded_trials() |>
    aggregate_mean(measure = accuracy, .by = c(p_id, {{ test_var }}, {{ .by }}))

  results$accuracy <- accuracy_data |>
    aggregate_mean(measure = accuracy, .by = c({{ test_var }}, {{ .by }}))

  results$rt <- rt_data |>
    rt_summary(.by = c({{ test_var }}, {{ .by }}))

  ttest_formula <- rlang::new_formula(sym("rt"), rlang::ensym(test_var))
  results$ttest <- rt_data |>
    group_by({{ .by }}) |>
    ttest_wrapper(
      formula = ttest_formula,
      paired = TRUE,
      alternative = "two.sided"
    )
  return(results)
}

# Supplementary Test 1 ---------------------------------------------------

# Comparing RT between Knowledge statements that are False
# and Belief statements that are False. FB-Ig Scenario trials.

results$S1 <- exploratory_data |>
  filter(
    scenario == "FB-Ig",
    statement_type %in% c("Knowledge", "Belief"),
    stimuli_type == "T_B_T",
    statement_number == 2,
    truth_value == FALSE
  ) |>
  apply_exclusions() |>
  run_rt_ttest(statement_type)

# Supplementary Test 2 ---------------------------------------------------

# Comparing RT between Knowledge statements that are False
# and Belief statements that are True. FB-Ig Scenario trials.

results$S2 <- exploratory_data |>
  filter(
    scenario == "FB-Ig",
    stimuli_type == "T_B_T",
    ((statement_type == "Knowledge" &
      truth_value == FALSE &
      statement_number == 2) |
      (statement_type == "Belief" &
        truth_value == TRUE &
        statement_number == 1))
  ) |>
  apply_exclusions() |>
  run_rt_ttest(statement_type)

# Supplementary Test 3 ---------------------------------------------------

# Comparing RT between
# (a) mean RT for False K statements / mean RT for False R statements
# (b) mean RT for True B statements / mean RT for True R statements

means_data <- exploratory_data |>
  filter(
    scenario == "FB-Ig",
    stimuli_type == "T_B_T",
    ((statement_type == "Knowledge" &
      truth_value == FALSE &
      statement_number == 2) |
      (statement_type == "Belief" &
        truth_value == TRUE &
        statement_number == 1) |
      (statement_type == "Reality"))
  ) |>
  apply_exclusions() |>
  summarise(rt = mean(rt), .by = c(p_id, statement_type, truth_value))

# Calcuate reality-adjusted RT means
reality_data <- means_data |>
  filter(statement_type == "Reality") |>
  select(p_id, truth_value, reality_rt = rt)

reality_adjusted_data <- means_data |>
  filter(statement_type != "Reality") |>
  left_join(reality_data, by = c("p_id", "truth_value")) |>
  mutate(rt = rt / reality_rt)

# RT Summary Data
results$S3$rt <- reality_adjusted_data |>
  rt_summary(.by = statement_type)

results$S3$ttest <- reality_adjusted_data |>
  ttest_wrapper(
    formula = rt ~ statement_type,
    paired = TRUE,
    alternative = "two.sided"
  )

# Supplementary Test 4 ---------------------------------------------------

# Check that the number of cubes referred to in the statement does not
# produce a difference in RTs. No Barrier: TB-K scenario trials only.

results$S4 <- exploratory_data |>
  filter(
    scenario == "TB-K",
    statement_number > 0
  ) |>
  apply_exclusions() |>
  run_rt_ttest(statement_number, .by = truth_value)

# Write results to file --------------------------------------------------
write_rds(
  results,
  here("outputs/exp2_supplementary_results.rds")
)
