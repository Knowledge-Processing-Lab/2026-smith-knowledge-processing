library(tidyverse)

# Helper Functions -------------------------------------------------------

create_exclusion_log <- function(participants = NULL, reason = NULL, statistics = NULL) {
  tibble(p_id = participants, reason = reason, statistic = statistics)
}

log_excluded_participants <- function(data_source, exclusion_data) {
  attr(data_source, "excluded_participants") <- bind_rows(
    attr(data_source, "excluded_participants") %||% create_exclusion_log(),
    exclusion_data
  )
  return(data_source)
}

log_excluded_trials <- function(data_source, excluded_rows, reason) {
  excluded_rows <- mutate(excluded_rows, reason = reason)
  attr(data_source, "excluded_trials") <- bind_rows(
    attr(data_source, "excluded_trials"),
    excluded_rows
  )
  return(data_source)
}

report_trial_count <- function(data, additional_message = NULL) {
  cat(nrow(data), additional_message, "\n\n")
  return(data)
}

report_exclusions <- function(data) {
  exc_pid <- attr(data, "excluded_participants")
  exc_trials <- attr(data, "excluded_trials")

  print_reason_counts <- \(df) df |> count(reason) |> print()
  print_reason_counts(exc_pid)
  print_reason_counts(exc_trials)

  n_exc_trials <- exc_trials |> filter(!p_id %in% exc_pid$p_id) |> nrow()
  total_trials <- n_exc_trials + nrow(data)
  percent_kept <- 100 * (1 - n_exc_trials / total_trials)

  message("Remaining participants contributed ", round(percent_kept, 1), "% trials")
  return(data)
}

restore_excluded_trials <- function(data) {
  exc_trials <- attr(data, "excluded_trials") |> 
    filter(p_id %in% data$p_id)

  bind_rows(data, exc_trials)
}

detect_outliers <- function(data, measure, z_threshold, .by = NULL) {
  data |>
    mutate(
      is_outlier = abs({{ measure }} - mean({{ measure }})) >
        sd({{ measure }}) * z_threshold,
      .by = {{ .by }}
    )
}

# Participant Level Exclusions -------------------------------------------

exclude_manual <- function(data, exclusion_log) {
  data |> 
    filter(!p_id %in% exclusion_log$p_id) |> 
    log_excluded_participants(exclusion_log)
}

exclude_low_accuracy <- function(data, threshold) {
  exclusions <- data |>
    summarise(overall_accuracy = mean(accuracy), .by = p_id) |>
    filter(overall_accuracy < threshold)

  data |> 
    filter(!p_id %in% exclusions$p_id) |> 
    log_excluded_participants(
      create_exclusion_log(
        participants = exclusions$p_id,
        reason = "Low Overall Accuracy",
        statistics = exclusions$overall_accuracy * 100
      )
    )
}

exclude_participant_accuracy_outliers <- function(data, z_threshold) {
  exclusions <- data |> 
    summarise(accuracy = mean(accuracy), .by = p_id) |> 
    detect_outliers(accuracy, z_threshold = z_threshold) |> 
    filter(is_outlier == TRUE)

  data |> 
    filter(!p_id %in% exclusions$p_id) |> 
    log_excluded_participants(
      create_exclusion_log(
        participants = exclusions$p_id,
        reason = "Low Experimental Accuracy",
        statistics = exclusions$accuracy * 100
      )
    )
}

exclude_participant_rt_outliers <- function(data, z_threshold) {
    exclusions <- data |> 
      summarise(rt = mean(rt), .by = p_id) |> 
      detect_outliers(rt, z_threshold = z_threshold) |> 
      filter(is_outlier)

  data |> 
    filter(!p_id %in% exclusions$p_id) |> 
    log_excluded_participants(
      create_exclusion_log(
        participants = exclusions$p_id,
        reason = "Mean RT Outlier",
        statistics = exclusions$rt
      )
    )
}

exclude_low_trial_count <- function(data, statement_types, threshold) {
  exclusions <- data |> 
    filter(statement_type %in% statement_types) |> 
    count(p_id, statement_type) |> 
    complete(p_id, statement_type = statement_types, fill = list(n = 0)) |> 
    summarise(meets_threshold = all(n >= threshold), .by = p_id) |> 
    filter(!meets_threshold)

  data |> 
    filter(!p_id %in% exclusions$p_id) |> 
    log_excluded_participants(
      create_exclusion_log(
        participants = exclusions$p_id,
        reason = "Low Trial Count"
      )
    )  
}

# Trial Level Exclusions -------------------------------------------------

exclude_incorrect_responses <- function(data) {
  exclusions <- data |>
    filter(accuracy == 0)

  data <- log_excluded_trials(
    data,
    exclusions,
    reason = "Incorrect Response"
  )

  anti_join(data, exclusions, by = names(data))
}

exclude_rts <- function(data, operator, threshold) {
  exclusions <- data |>
    filter(operator(rt, threshold))

  data <- log_excluded_trials(
    data,
    exclusions,
    reason = "RT Outside Threshold"
  )

  anti_join(data, exclusions, by = names(data))
}

exclude_trial_rt_outliers <- function(data, z_threshold, .by = p_id) {
  exclusions <- data |>
    detect_outliers(
      measure = rt,
      z_threshold = z_threshold,
      .by = {{ .by }}
    ) |>
    filter(is_outlier == TRUE) |>
    select(-is_outlier)

  data <- log_excluded_trials(data, exclusions, reason = "RT Outlier")

  anti_join(data, exclusions, by = names(data))
}
