# INFO -------------------------------------------------------------------

# PROJECT
## Paper: Knowledge and Ignorance Processing is Faster than Belief Processing
## Authors: Smith, A., Peney, T., Tidoni, E., O'Connor, R. J., & Riggs, K.

# R Script
## Purpose: This script imports raw data, tidies and aggregates it into one standardised dataframe per experiment.
## Inputs: data/primary/*_data.csv
## Outputs: data/processed/clean_data.rds
## Authors: Peney, T.

# Setup ------------------------------------------------------------------
library(here)
library(dplyr)
library(readr)
library(purrr)
library(forcats)
library(janitor)
library(stringr)

here::i_am("code/data_import.R")

# Data integrity check ---------------------------------------------------
source(here("code/utils/checksum.R"))
#checksum_verify()

# Import raw data --------------------------------------------------------
raw_data <- c("experiment_1", "experiment_2", "experiment_3") |>
  map(
    \(dir_name) {
      path <- here("data/primary", dir_name)
      files <- list.files(path, full.names = TRUE, pattern = "_data\\.csv$")
      map(
        files,
        read_csv,
        show_col_types = FALSE,
        na = c("", "NA", "None"),
        name_repair = "unique_quiet",
        col_select = !starts_with("..."),
        # Explicitly specify col types for those which are ambigous to col_guess
        col_types = cols(
          participant = col_character(),
          session = col_character(),
          .default = col_guess()
        )
      )
    }
  ) |>
  set_names(c("E1", "E2", "E3")) |>
  map(bind_rows)

# Clean Data -------------------------------------------------------------
clean_data <- raw_data |>
  map(janitor::clean_names) |>
  # Rename Columns
  modify_at(
    c("E1", "E2"),
    rename,
    accuracy = exp_response_corr,
    rt = exp_response_rt,
    p_id = participant,
    exp_routine_statement_primer_started = exp_primer_text_started,
    exp_routine_statement_primer_stopped = exp_primer_text_stopped,
    exp_routine_statement_started = exp_text_started,
    exp_routine_image_primer_started = exp_image_primer_started,
    exp_routine_image_primer_stopped = exp_image_primer_stopped,
    exp_routine_image_started = exp_image_started,
    exp_routine_key_resp_started = exp_response_started
  ) |>
  modify_at(
    "E1",
    rename,
    why_false = why_no,
    statement_number = num_of_blocks_asked,
  ) |>
  modify_at(
    "E2",
    rename,
    statement_type = condition,
    statement_number = statement_num,
    truth_value = true_false,
    stimuli_type = stim_type,
  ) |>
  modify_at(
    "E3",
    rename,
    accuracy = exp_routine_key_resp_corr,
    rt = exp_routine_key_resp_rt,
    p_id = participant,
    why_false = why_false
  ) |>

  # Remove non-trial rows
  map(filter, !is.na(statement)) |>

  # Create missing columns from E1
  modify_at(
    "E1",
    mutate,
    statement_type = case_when(
      str_detect(statement, fixed("KNOWS")) ~ "Knowledge",
      str_detect(statement, fixed("REALITY")) ~ "Reality",
      str_detect(statement, fixed("THINKS")) ~ "Belief",
    ),
    truth_value = str_detect(cor_ans_odd, fixed("h")),
    # Create stimuli_type shortcode from filepath name
    stimuli_type = stimulus |>
      basename() |>
      # Regex pattern - remove e.g., "_1.png"
      str_remove("_\\d.*$") |>
      str_replace_all(c("Square" = "T", "Triangle" = "D")) |>
      # Regex pattern - collapse e.g., T_T to TT
      str_replace_all("(?<=[TD])_(?=[TD])", ""),
    # Fix erroneously labelled data
    scenario = if_else(
      condition == 'KnowledgeY' &
        type_of_stimuli %in% c("FB-Ig", "TB-Ig"),
      "TB-K-B",
      type_of_stimuli
    )
  ) |>

  # Specify if a trial is experimental or filler
  modify_at(
    c("E1", "E2"),
    mutate,
    experimental = scenario == 'TB-K' & truth_value == TRUE
  ) |>
  modify_at(
    "E3",
    mutate,
    experimental = experimental == "Y",
    negated = reg_neg == "Neg"
  ) |>

  # Fix Column Types
  modify_at(
    "E3",
    mutate,
    statement_number = statement_number |>
      recode_values(
        "Zero" ~ 0,
        "One" ~ 1,
        "Two" ~ 2
      ),
    scenario = replace_values(scenario, "TB-K" ~ "TB-K-B")
  ) |>

  map(
    mutate,
    statement_type = fct(
      statement_type,
      levels = c("Reality", "Knowledge", "Belief")
    ),
    statement_number = as.integer(statement_number)
  ) |>

  # Remove unused columns
  map(
    select,
    p_id,
    statement_type,
    rt,
    accuracy,
    experimental,
    scenario,
    stimuli_type,
    stimulus,
    truth_value,
    statement_number,
    statement,
    why_false,
    any_of("negated"), # E3 only
    starts_with("exp_routine"),
    psychopy_version,
    frame_rate
  ) |>

  write_rds("data/processed/clean_data.rds")
