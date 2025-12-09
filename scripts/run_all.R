# scripts/run_all.R
# Purpose: Run the full analysis pipeline (data cleaning, analysis, plots) and render final report
# Author(s): Ashley Eitmontas, Ella Anderson
# Date: December 2025

# -------------------------------
# Setup
# -------------------------------
set.seed(2025)        # Ensure reproducibility
library(here)         # Reproducible file paths
library(rmarkdown)    # For rendering the report

# -------------------------------
# Run pipeline scripts
# -------------------------------
source(here("scripts", "00_load_clean.R"))   # Load and clean raw data
source(here("scripts", "02_analysis.R"))     # Build models and run analysis
source(here("scripts", "03_plots.R"))        # Generate plots and figures

# -------------------------------
# Render final report
# -------------------------------
rmarkdown::render(
  input = here("docs", "report.Rmd"),
  output_file = here("docs", "report.pdf")
)
