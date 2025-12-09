# scripts/02_analysis.R
# Purpose: Build modeling dataset, run regression analysis, and perform backward elimination
# Author(s): Ashley Eitmontas, Ella Anderson
# Date: December 2025

# -------------------------------
# Load required packages
# -------------------------------
library(dplyr)      # Data manipulation
library(stringr)    # String operations
library(tidyr)      # Reshaping data
library(readr)      # Read/write data
library(here)       # Reproducible file paths

# -------------------------------
# Load cleaned SDOH data
# -------------------------------
SDOH <- read_rds(here("results", "clean.rds"))

# -------------------------------
# Define Appalachian counties
# -------------------------------
appalachian_counties <- c(
  "Adams","Athens","Ashtabula","Belmont","Brown","Carroll","Columbiana",
  "Coshocton","Clermont","Gallia","Guernsey","Harrison","Highland",
  "Hocking","Holmes","Jackson","Jefferson","Lawrence","Meigs","Monroe",
  "Morgan","Mahoning","Muskingum","Noble","Perry","Pike","Ross","Scioto",
  "Trumbull","Tuscarawas","Vinton","Washington"
)

# -------------------------------
# Pivot data to wide format
# -------------------------------
appalachian_wide <- SDOH %>%
  mutate(county_name = str_to_title(county_name)) %>%
  filter(county_name %in% appalachian_counties) %>%
  select(county_name, metric_name, metric_value) %>%
  group_by(county_name, metric_name) %>%
  summarise(metric_value = first(metric_value), .groups = "drop") %>%
  pivot_wider(names_from = metric_name, values_from = metric_value)

# -------------------------------
# Rename columns for clarity
# -------------------------------
appalachian_wide <- appalachian_wide %>%
  rename(
    asthma_rate = `Asthma Rate`,
    poverty_rate = `Poverty Rate`,
    uninsured_rate = `Uninsured Rate`,
    unemployment_rate = `Unemployment Rate`,
    median_household_income = `Median Household Income`,
    hs_less_than_rate = `25 Or Older With Less Than Hs Degree Rate`
  )

# -------------------------------
# Build modeling dataset
# -------------------------------
needed <- c(
  "asthma_rate","poverty_rate","uninsured_rate","unemployment_rate",
  "median_household_income","hs_less_than_rate"
)

model_df <- appalachian_wide %>%
  select(county_name, all_of(needed)) %>%
  filter(complete.cases(across(all_of(needed))))

# -------------------------------
# Define predictors
# -------------------------------
predictors <- c(
  "poverty_rate", "uninsured_rate", "unemployment_rate",
  "median_household_income", "hs_less_than_rate"
)

# -------------------------------
# Define backward elimination function
# -------------------------------
backward_elimination <- function(df, outcome, predictors, alpha = 0.05) {
  current_preds <- predictors
  elimination_path <- c()
  repeat {
    frm <- stats::as.formula(
      paste(outcome, "~", paste(current_preds, collapse = " + "))
    )
    fit <- stats::lm(frm, data = df)
    coefs <- summary(fit)$coefficients
    idx <- rownames(coefs) != "(Intercept)"
    pvals <- coefs[idx, 4]
    
    if (any(is.na(pvals))) {
      na_terms <- names(pvals)[is.na(pvals)]
      removal <- na_terms[1]
      elimination_path <- c(elimination_path, paste0("Removed (NA p): ", removal))
      current_preds <- setdiff(current_preds, removal)
      if (length(current_preds) == 0) return(list(model = fit, predictors = current_preds, path = elimination_path))
      next
    }
    
    max_p <- max(pvals)
    worst_term <- names(which.max(pvals))
    if (max_p > alpha) {
      elimination_path <- c(elimination_path, paste0("Removed: ", worst_term, " (p = ", signif(max_p, 3), ")"))
      current_preds <- setdiff(current_preds, worst_term)
      if (length(current_preds) == 0) return(list(model = fit, predictors = current_preds, path = elimination_path))
    } else {
      return(list(model = fit, predictors = current_preds, path = elimination_path))
    }
  }
}

# -------------------------------
# Fit full model
# -------------------------------
full_model <- lm(
  asthma_rate ~ poverty_rate + uninsured_rate + unemployment_rate +
    median_household_income + hs_less_than_rate,
  data = model_df
)

# -------------------------------
# Run backward elimination
# -------------------------------
be <- backward_elimination(model_df, "asthma_rate", predictors, alpha = 0.05)

# -------------------------------
# Save outputs
# -------------------------------
saveRDS(full_model, here("results", "full_model.rds"))
write_csv(model_df, here("results", "model_df.csv"))
saveRDS(be, here("results", "backward_elimination.rds"))
