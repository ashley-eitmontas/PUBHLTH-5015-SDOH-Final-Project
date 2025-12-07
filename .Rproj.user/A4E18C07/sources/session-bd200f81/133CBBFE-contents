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
