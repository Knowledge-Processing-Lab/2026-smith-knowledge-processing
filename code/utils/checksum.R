# INFO -------------------------------------------------------------------

# PROJECT
## Paper: Knowledge and Ignorance Processing is Faster than Belief Processing
## Authors: Smith, A., Peney, T., Tidoni, E., O'Connor, R. J., & Riggs, K.

# R Script
## Purpose: Generate and verify SHA-256 checksums for primary data files.
## Authors: Peney, T.

# Setup ------------------------------------------------------------------
library(tidyverse)
library(here)
library(digest)

data_dir <- here("data/primary")

# Functions --------------------------------------------------------------
build_manifest <- function() {
  files <- list.files(
    data_dir,
    pattern = "_data.csv",
    recursive = TRUE
  )

  manifest <- data.frame(
    file = files,
    row.names = NULL
  ) |>
    rowwise() |>
    mutate(
      sha256 = digest::digest(
        here(data_dir, file),
        algo = "sha256",
        file = TRUE
      ),
      bytes = file.size(here(data_dir, file))
    )
}

checksum_write <- function() {
  manifest <- build_manifest()
  write.csv(manifest, here(data_dir, "checksums.csv"), row.names = FALSE)
}

checksum_verify <- function() {
  manifest_path <- here(data_dir, "checksums.csv")
  if (!file.exists(manifest_path)) {
    stop(
      "No manifest found at ",
      manifest_path,
      "\n  Run checksum_write() to create one.",
      call. = FALSE
    )
  }

  old <- read_csv(here(data_dir, "checksums.csv"), col_types = c("ccd"))
  new <- build_manifest()

  shared <- merge(
    old[c("file", "sha256")],
    new[c("file", "sha256")],
    by = "file",
    suffixes = c("_old", "_new")
  )

  changed <- shared$file[shared$sha256_old != shared$sha256_new]

  result <- rbind(
    data.frame(file = changed) |>
      mutate(status = "changed"),
    data.frame(file = setdiff(old$file, new$file)) |>
      mutate(status = "missing"),
    data.frame(file = setdiff(new$file, old$file)) |>
      mutate(status = "unrecorded")
  )

  if (nrow(result) > 0L) {
    stop(
      "Data integrity check failed.\n",
      nrow(result),
      " file(s) differ from the recorded manifest:\n",
      paste0("  [", result$status, "] ", result$file, collapse = "\n"),
      call. = FALSE
    )
  } else {
    message("All ", nrow(new), " files match the checksum manifest.")
  }
}
