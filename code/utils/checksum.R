# INFO -------------------------------------------------------------------

# PROJECT
## Paper: Knowledge and Ignorance Processing is Faster than Belief Processing
## Authors: Smith, A., Peney, T., Tidoni, E., O'Connor, R. J., & Riggs, K.

# R Script
## Purpose: Generate and verify SHA-256 checksums for a dataframe.
### Used in code/data_import.R to check that the dataframe created when re-importing
### the raw data matches the one used in the original analysis. On a mismatch in an
### interactive session, the user is prompted to continue or abort.
## Authors: Peney, T.

# Setup ------------------------------------------------------------------
library(here)
library(readr)
library(dplyr)
library(digest)

data_dir <- here("data/primary")

# Functions --------------------------------------------------------------
create_checksum <- function(dataframe) {
  digest::digest(dataframe, algo = "sha256")
}

checksum_write <- function(dataframe, name) {
  if (!dir.exists(here("tests/snapshots"))) {
    dir.create(here("tests/snapshots"), recursive = TRUE)
  }

  checksum <- create_checksum(dataframe)
  write_rds(checksum, here("tests", "snapshots", paste0(name, ".rds")))
}

checksum_verify <- function(dataframe, name) {
  path <- here("tests", "snapshots", paste0(name, ".rds"))

  if (!file.exists(path)) {
    warning(
      "No snapshot found at ",
      path,
      "\n  Run checksum_write() to create one."
    )
    return()
  }

  old <- read_rds(path)
  new <- create_checksum(dataframe)

  checksums_equal <- old == new

  if (checksums_equal) {
    message("Data validation complete. No issues found.")
  } else {
    warning(
      "Checksum mismatch: ",
      name,
      " differs from the committed reference.\n",
      "The source data on disk may not be the same as that used to conduct the original analysis.\n",
      "Consider re-importing using archived versions of the data.",
      immediate. = TRUE
    )

    if (!interactive()) {
      stop("Data validation issues found; aborting (non-interactive session).")
    } else {
      if (!confirm_continue()) {
        stop("Aborting as requested.")
      }
    }
  }
}

confirm_continue <- function(prompt = "Continue anyway?: ") {
  choice <- utils::menu(
    c("Continue", "Abort"),
    title = prompt
  )
  return(choice == 1)
}
