# scripts/02_analysis.R
library(dplyr)
library(stringr)
library(tidyr)
library(readr)

SDOH <- read_rds("results/clean.rds")

appalachian_counties <- c("Adams","Athens","Ashtabula","Belmont","Brown","Carroll","Columbiana",
                          "Coshocton","Clermont","Gallia","Guernsey","Harrison","Highland",
                          "Hocking","Holmes","Jackson","Jefferson","Lawrence","Meigs","Monroe",
                          "Morgan","Mahoning","Muskingum","Noble","Perry","Pike","Ross","Scioto",
                          "Trumbull","Tuscarawas","Vinton","Washington")

# Pivot wide
appalachian_wide <- SDOH %>%
  mutate(county_name = str_to_title(county_name)) %>%
  filter(county_name %in% appalachian_counties) %>%
  select(county_name, metric_name, metric_value) %>%
  group_by(county_name, metric_name) %>%
  summarise(metric_value = first(metric_value), .groups = "drop") %>%
  pivot_wider(names_from = metric_name, values_from = metric_value)

# Rename columns
appalachian_wide <- appalachian_wide %>%
  rename(asthma_rate = `Asthma Rate`,
         poverty_rate = `Poverty Rate`,
         uninsured_rate = `Uninsured Rate`,
         unemployment_rate = `Unemployment Rate`,
         median_household_income = `Median Household Income`,
         hs_less_than_rate = `25 Or Older With Less Than Hs Degree Rate`)
# Define predictors
predictors <- c("poverty_rate", "uninsured_rate", "unemployment_rate",
                "median_household_income", "hs_less_than_rate")

# Define backward elimination function
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

# Run backward elimination
be <- backward_elimination(model_df, "asthma_rate", predictors, alpha = 0.05)

# Save outputs
saveRDS(full_model, "results/full_model.rds")
write_csv(model_df, "results/model_df.csv")
saveRDS(be, "results/backward_elimination.rds")

# Build modeling dataset
needed <- c("asthma_rate","poverty_rate","uninsured_rate","unemployment_rate",
            "median_household_income","hs_less_than_rate")

model_df <- appalachian_wide %>%
  select(county_name, all_of(needed)) %>%
  filter(complete.cases(across(all_of(needed))))

# Full model
full_model <- lm(asthma_rate ~ poverty_rate + uninsured_rate + unemployment_rate +
                   median_household_income + hs_less_than_rate, data = model_df)

saveRDS(full_model, "results/full_model.rds")
write_csv(model_df, "results/model_df.csv")
# After computing be <- backward_elimination(...)
saveRDS(be, "results/backward_elimination.rds")
