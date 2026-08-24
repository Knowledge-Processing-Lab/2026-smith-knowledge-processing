
# Setup ------------------------------------------------------------------

library(tidyverse)
library(here)
library(rempsyc)
library(flextable)

here::i_am("code/tables.R")

if (!dir.exists(here("outputs/tables"))) {
  dir.create(here("outputs/tables"))
}

# Import Data ------------------------------------------------------------

base_data <- read_rds(here("data/processed/clean_data.rds"))

confirmatory_analysis_data <- list(
  E1 = read_rds(here("data/processed/exp1_confirmatory_data.rds")),
  E2 = read_rds(here("data/processed/exp2_confirmatory_data.rds")),
  E3 = read_rds(here("data/processed/exp3_confirmatory_data.rds"))
)

# Filter all trial data to those participants which contributed
# to the confirmatory analysis.
all_accuracy_data <- map2(
  base_data, confirmatory_analysis_data,
  \(x, y) {
    x |>
      filter(p_id %in% y$p_id) |> 
      mutate(
        scenario = scenario |> 
          recode_values(
            c("TB-K", "NB-TB-K") ~ "No Barrier: TB-Know",
            "TB-K-B" ~ "Barrier: TB-Know",
            "TB-Ig" ~ "Barrier: TB-Ig",
            "FB-Ig" ~ "Barrier: FB-Ig",
          ),
        scenario = fct(
          scenario,
          levels = c(
            "No Barrier: TB-Know",
            "Barrier: FB-Ig",
            "Barrier: TB-Know",
            "Barrier: TB-Ig"
          )
        )
      )
  }
)

# Accuracy Tables --------------------------------------------------------

create_accuracy_table <- function(data, title, preview = TRUE) {
  # Helper: Converts logicals to ordered factors so "True" always precedes "False"
  fmt_truth <- function(x) factor(x, levels = c(TRUE, FALSE), labels = c("True", "False"))

  # 1. Create accuracy summaries & pivot data wide
  tbl_wide <- data |> 
    summarise(
      M = sprintf("%.0f%%", mean(accuracy) * 100), 
      .by = c(scenario, statement_type, truth_value)
    ) |> 
    mutate(truth_value = fmt_truth(truth_value)) |> 
    arrange(scenario) |> 
    rename(Scenario = scenario) |> 
    pivot_wider(
      names_from = c(statement_type, truth_value), 
      values_from = M,
      names_sep = ".",
      names_sort = TRUE,
      values_fill = "—"
    )
  
  # Extract footnote coordinates
  exp_cells <- data |>
    filter(experimental) |>
    distinct(scenario, statement_type, truth_value) |>
    mutate(col = paste(statement_type, fmt_truth(truth_value), sep = "."))

  # Generate Table (inc flagging experimental trials as footnotes)
  tbl <- tbl_wide |> 
    rempsyc::nice_table(
      separate.header = TRUE,
      title = title,
      note = paste0(
        "TB = True Belief; FB = False Belief; Know = Knowledge; Ig = Ignorance. ",
        "Dashes indicate cells with no applicable trials."
      )
    ) |> 
    flextable::footnote(
      i = match(exp_cells$scenario, tbl_wide$Scenario),
      j = exp_cells$col,
      value = flextable::as_paragraph(" Experimental Trials."),
      ref_symbols = "a"
    )
  
  if (preview) { print(tbl) }
  return(tbl)
}

# Experiment 1
all_accuracy_data$E1 |> 
  create_accuracy_table(
    title = "Experiment 1: Mean Accuracy by Statement Type and Truth Value Across Scenarios"
  ) |> 
  flextable::save_as_docx(
    path = here("outputs/tables", "E1_accuracy.docx")
  )

# Experiment 2
all_accuracy_data$E2 |> 
  create_accuracy_table(
    title = "Experiment 2: Mean Accuracy by Statement Type and Truth Value Across Scenarios"
  ) |> 
  flextable::save_as_docx(
    path = here("outputs/tables", "E2_accuracy.docx")
  )

# Experiment 3 - Negated
all_accuracy_data$E3 |> 
  filter(
    negated,
    statement != "Reality",
    scenario != "Barrier: TB-Ig"
  ) |> 
  droplevels() |> 
  mutate(
    scenario = fct_relevel(scenario, "Barrier: FB-Ig"),
    statement_type = statement_type |> fct_recode(
      `Negated Knowledge` = "Knowledge",
      `Negated Belief` = "Belief"
    )
  ) |> 
  create_accuracy_table(
    title = "Experiment 3: Mean Accuracy by Statement Type and Truth Value Across Scenarios for Negated Statements"
  ) |> 
  flextable::save_as_docx(
    path = here("outputs/tables", "E3_accuracy_negated.docx")
  )

# Experiment 3 - Affirmed
all_accuracy_data$E3 |> 
  filter(!negated) |> 
  mutate(scenario = fct_relevel(scenario, "Barrier: FB-Ig")) |> 
  create_accuracy_table(
    title = "Experiment 3: Mean Accuracy by Statement Type and Truth Value Across Scenarios for Affirmative Statements"
  ) |> 
  flextable::save_as_docx(
    path = here("outputs/tables", "E3_accuracy_affirmed.docx")
  )
