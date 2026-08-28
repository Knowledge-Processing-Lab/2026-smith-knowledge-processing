# Knowledge and Ignorance Processing is Faster than Belief Processing (2026)

This repository contains the code required to reproduce all data processing, analyses, figures and tables reported in the manuscript.

Authors: *Alex Smith¹, Emmanuele Tidoni¹ ², Tom Peney¹, Richard J O’Connor¹ & Kevin J Riggs¹*

¹ School of Psychology, University of Hull, Hull, UK

² School of Psychology, University of Leeds, Leeds, UK

## Preparation for reproduction

### Prerequisites

* R 4.6.1
* `renv` (for versioned package installation)
* [Optional] Quarto (for document rendering)

### Setup Instructions

Primary data is included under `data/primary`. If for whatever reason the checksum warning flags that a data validation error has occurred, data can be redownloaded from the Zenodo archive. [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.22025765.svg)](https://doi.org/10.5281/zenodo.22025765)

1. Clone the repository (or download the `.zip` and extract it):

   ```powershell
   git clone https://github.com/Knowledge-Processing-Lab/2026-smith-knowledge-and-ignorance.git
   cd 2026-smith-knowledge-and-ignorance
   ```

2. *Optional:* If your IDE uses projects (e.g., RStudio), open the new directory as a project before continuing.

3. Install `renv`, if you have not done so already.

   ```R
   install.packages("renv")
   ```

4. Restore the project library and install R dependencies:

   ```R
   renv::restore()
   ```

**Notes**

* You may be prompted to activate the project first. Once activated, re-run `renv::restore()` to begin package installation.

* If any packages fail to build, you may need [RTools](https://cran.r-project.org/bin/windows/Rtools/) (Windows only).

## Instructions for reproducing the analysis

Once the environment has been setup, run `_main.R`. This will run each analysis script in the correct order. 

A printed report `_experiment_report.html` will be created for simplified viewing of the results. Opening the file should display the report in your default web browser. 

* To avoid using Quarto, you can run each script in `/code` manually and print the `results` object to the console. 

Figures and tables are created under `outputs/figures` and `outputs/tables` respectively. 

**N.B.** The 'Denying Knowledge' result of Experiment 2 is reported under Supplementary Test 1.

## Additional Information

* Estimated time for reproduction: It should be possible to complete the above steps in under 30 minutes.
* The analysis code was run and verified on a Windows 11 x64 (build 26200) system.
* Each data directory contains a machine-readable `directory_metadata.json` file which includes definitions for all columns in the associated data files.
* This repository conforms to the [Psych-DS](https://psych-ds.github.io/) data standard.

## Funding and contact

This work was supported by the Economic and Social Research Council (UKRI559).

Questions and issues: Please contact Tom Peney ([T.Peney@hull.ac.uk](mailto:T.Peney@hull.ac.uk)) or open an issue on this repository.

## Licence

Code is released under the MIT licence. Data and materials are released under CC-BY-4.0.

## Abstract

Humans routinely track what others know, believe, and are ignorant of. Much of social cognition research, however, focuses on belief attribution, with knowledge and ignorance underexplored. Competing theories treat knowledge as either a specific form of belief (justified true belief) or as a distinct and more foundational mental state. We tested these accounts across three preregistered experiments. Participants read statements about either reality, knowledge or belief and then judged whether these statements were true of a subsequently presented image. Response times revealed a consistent hierarchy: reality judgments were fastest; followed by knowledge and then belief. Participants were also faster to deny knowledge (attribute ignorance) than to deny belief. These findings challenge traditional philosophical belief-based accounts of knowledge and suggest instead that knowledge is a more fundamental mental state not reducible to belief, with implications for theory of mind architecture and the modelling of mental states in AI systems.