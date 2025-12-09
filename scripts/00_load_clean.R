# scripts/00_load_clean.R
# Purpose: Load raw SDOH data, perform cleaning, and save cleaned dataset
# Author(s): Ashley Eitmontas, Ella Anderson
# Date: December 2025

# Set seed for reproducibility
set.seed(2025)

# Load required packages
library(tidyverse)   # Data manipulation and visualization
library(readxl)      # Read Excel files (if needed)
library(janitor)     # Clean column names
library(lubridate)   # Work with dates
library(here)        # Ensure reproducible file paths

# -------------------------------
# Load raw data
# -------------------------------
SDOH <- read_csv(
  here("data", "SDOH_Census_Tract.csv"), 
  show_col_types = FALSE
) %>%
  clean_names()

# -------------------------------
# Structure & quality checks
# -------------------------------
dim(SDOH)                        # Dimensions of dataset
names(SDOH)                      # Column names
glimpse(SDOH)                    # Quick look at structure
colSums(is.na(SDOH)) %>% 
  sort(decreasing = TRUE)         # Missing values by column
dup_count <- SDOH %>% 
  duplicated() %>% 
  sum()                           # Count of duplicate rows

# -------------------------------
# Cleaning steps
# -------------------------------
SDOH <- SDOH %>%
  # Standardize character fields
  mutate(across(where(is.character), ~ str_squish(.x) |> str_to_title())) %>%
  distinct() %>%                              # Remove duplicates
  filter(metric_value >= 0 | is.na(metric_value)) %>%  # Remove negative values
  mutate(metric_value = as.character(metric_value)) %>%
  # Handle missing codes
  mutate(metric_value = case_when(
    metric_value %in% c("Null","N/A","NA","Missing","NaN") ~ NA_character_,
    TRUE ~ metric_value
  )) %>%
  # Convert to numeric
  mutate(metric_value = readr::parse_number(metric_value)) %>%
  filter(metric_value >= 0 | is.na(metric_value)) %>%
  # Replace remaining NAs with median
  mutate(metric_value = tidyr::replace_na(metric_value, median(metric_value, na.rm = TRUE)))

# -------------------------------
# Save cleaned data
# -------------------------------
write_rds(SDOH, here("results", "clean.rds"))

