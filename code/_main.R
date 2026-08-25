# INFO -------------------------------------------------------------------

# PROJECT
## Paper: Knowledge and Ignorance Processing is Faster than Belief Processing
## Authors: Smith, A., Peney, T., Tidoni, E., O'Connor, R. J., & Riggs, K.

# R Script
## Purpose: This script executes all other relevant analysis scripts in the correct order.
## Authors: Peney, T.

# Setup ------------------------------------------------------------------
library(here)
here::i_am("code/_main.R")

# Import and tidy data
source(here("code/data_import.R"))

# Run analysis scripts & export derived data
source(here("code/exp1_analysis.R"))
source(here("code/exp2_analysis.R"))
source(here("code/exp2_supplementary_analysis.R"))
source(here("code/exp3_analysis.R"))

# Create Table & Figure outputs
source(here("code/tables.R"))
source(here("code/figures.R"))

# Render analysis output document
system("quarto render _experiment_report.qmd")

message(
  "All analysis scripts ran. See _experiment_report.html for a printed output. For figures/tables see: outputs/figures and outputs/tables."
)
