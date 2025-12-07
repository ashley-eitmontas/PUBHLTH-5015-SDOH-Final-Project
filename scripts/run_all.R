# scripts/run_all.R
set.seed(2025)

source("scripts/00_load_clean.R")
source("scripts/02_analysis.R")
source("scripts/03_plots.R")

# Render report last
rmarkdown::render("docs/report.Rmd", output_file = "docs/report.pdf")

