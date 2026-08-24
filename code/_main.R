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
