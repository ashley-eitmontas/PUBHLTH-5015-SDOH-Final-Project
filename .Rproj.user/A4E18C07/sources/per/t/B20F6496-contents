# scripts/00_load_clean.R
set.seed(2025)

library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)

# Load raw data
SDOH <- read_csv("data/SDOH_Census_Tract.csv", show_col_types = FALSE) %>%
  clean_names()

# Structure & quality checks
dim(SDOH)
names(SDOH)
glimpse(SDOH)
colSums(is.na(SDOH)) %>% sort(decreasing = TRUE)
dup_count <- SDOH %>% duplicated() %>% sum()

# Cleaning
SDOH <- SDOH %>%
  mutate(across(where(is.character), ~ str_squish(.x) |> str_to_title())) %>%
  distinct() %>%
  filter(metric_value >= 0 | is.na(metric_value)) %>%
  mutate(metric_value = as.character(metric_value)) %>%
  mutate(metric_value = case_when(
    metric_value %in% c("Null","N/A","NA","Missing","NaN") ~ NA_character_,
    TRUE ~ metric_value
  )) %>%
  mutate(metric_value = readr::parse_number(metric_value)) %>%
  filter(metric_value >= 0 | is.na(metric_value)) %>%
  mutate(metric_value = tidyr::replace_na(metric_value, median(metric_value, na.rm = TRUE)))

# Save cleaned data
write_rds(SDOH, "results/clean.rds")
