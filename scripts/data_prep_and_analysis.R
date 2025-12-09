
library(here)
library(ggplot2)
library(readr)
library(dplyr)
library(purrr)
library(broom)
library(tidyr)
library(stringr)

# Load data set from ODH

census <- read.csv(here::here("data", "SDOH_Census_Tract.csv"))

# Filter out non-Appalachian Ohio counties

exclude_nonapp <- c(
  "Allen","Ashland","Auglaize","Butler","Champaign",
  "Clark","Clinton","Crawford","Cuyahoga","Darke",
  "Defiance","Delaware","Erie","Fairfield","Fayette",
  "Franklin","Fulton","Geauga","Greene","Hamilton",
  "Hancock","Hardin","Henry","Huron","Knox",
  "Lake","Licking","Logan","Lorain","Lucas",
  "Madison","Marion","Medina","Mercer","Miami",
  "Montgomery","Morrow","Ottawa","Paulding","Pickaway",
  "Portage","Preble","Putnam","Richland","Sandusky",
  "Seneca","Shelby","Stark","Summit","Union",
  "Van Wert","Warren","Wayne","Williams","Wood",
  "Wyandot"
)

app_data <- census %>%
  filter(!(county_name %in% exclude_nonapp))

# ----- County average asthma regression model - median household income ----

# 1st step: filter out just the asthma rate and median household data for each county, and find the mean for each county.
app_asthma <- app_data %>%
  filter(metric_name == "Asthma Rate") %>%
  mutate(metric_value = as.numeric(gsub("%", "", metric_value))) %>%
  group_by(county_name) %>%
  summarise(mean_asthma = mean(metric_value, na.rm = TRUE))

app_income <- app_data %>%
  filter(metric_name == "Median Household Income") %>%
  mutate(
    metric_value = gsub("[^0-9.]", "", metric_value),  
    metric_value = ifelse(metric_value == "", NA, metric_value),
    metric_value = as.numeric(metric_value)
  ) %>%
  group_by(county_name) %>%
  summarise(mean_income = mean(metric_value, na.rm = TRUE), .groups = "drop")

# Combine the data, since in the excel sheet each metric is a separate data point
app_combined_asthma<- inner_join(app_income, app_asthma, by = "county_name")

# Run the regression model using the combined data set
app_model_income <- lm(mean_income~ mean_asthma, data = app_combined_asthma)


# ---- Compare the Appalachian county regression with all Ohio counties -----


asthma <- census %>%
  filter(metric_name == "Asthma Rate") %>%
  mutate(metric_value = as.numeric(gsub("%", "", metric_value))) %>%
  group_by(county_name) %>%
  summarise(mean_asthma = mean(metric_value, na.rm = TRUE))

income <- census %>%
  filter(metric_name == "Median Household Income") %>%
  mutate(
    metric_value = gsub("[^0-9.]", "", metric_value),  
    metric_value = ifelse(metric_value == "", NA, metric_value),
    metric_value = as.numeric(metric_value)
  ) %>%
  group_by(county_name) %>%
  summarise(mean_income = mean(metric_value, na.rm = TRUE), .groups = "drop")

combined_asthma<- inner_join(income, asthma, by = "county_name")

combined_asthma <- combined_asthma %>%
  mutate(highlight = ifelse(county_name %in% c(  "Adams", "Athens", "Belmont", "Brown", "Carroll", 
                                                 "Clermont", "Coshocton", "Gallia", "Guernsey", "Harrison",
                                                 "Hocking", "Jackson", "Jefferson", "Lawrence", "Meigs",
                                                 "Monroe", "Morgan", "Muskingum", "Noble", "Perry",
                                                 "Pike", "Ross", "Scioto", "Vinton", "Washington"), "Appalachian Counties", "Other"))
asthma_model_all_income <- lm(mean_income~ mean_asthma, data = combined_asthma)

# ---- Poverty Rate - County Average -----

# filter out poverty rate data
app_poverty <- app_data %>%
  filter(metric_name == "Poverty Rate") %>%
  mutate(
    metric_value = gsub("[^0-9.]", "", metric_value),  
    metric_value = ifelse(metric_value == "", NA, metric_value),
    metric_value = as.numeric(metric_value)
  ) %>%
  group_by(county_name) %>%
  summarise(mean_poverty = mean(metric_value, na.rm = TRUE), .groups = "drop")

# combine poverty rate and asthma rate
app_combined_asthma_poverty<- inner_join(app_poverty, app_asthma, by = "county_name")

# run linear regression for poverty rate and asthma rate
app_model_poverty <- lm(mean_poverty~ mean_asthma, data = app_combined_asthma_poverty)

# ---- Uninsured Rate - County Average -----

app_uninsured <- app_data %>%
  filter(metric_name == "Uninsured Rate") %>%
  mutate(
    metric_value = gsub("[^0-9.]", "", metric_value),  
    metric_value = ifelse(metric_value == "", NA, metric_value),
    metric_value = as.numeric(metric_value)
  ) %>%
  group_by(county_name) %>%
  summarise(mean_uninsured = mean(metric_value, na.rm = TRUE), .groups = "drop")

asthma_uninsured <- inner_join(app_uninsured, app_asthma, by = "county_name")
model_uninsured <- lm(mean_uninsured ~ mean_asthma, data = asthma_uninsured)

# Remove outlier to clean data (Holmes County)

asthma_uninsured_clean <- asthma_uninsured %>%
  filter(county_name != "Holmes")

model_uninsured_clean <- lm(mean_uninsured ~ mean_asthma, data = asthma_uninsured_clean)

# ---- Unemployed Rate - County Average -----

app_unemployed <- app_data %>%
  filter(metric_name == "Unemployment Rate") %>%
  mutate(
    metric_value = gsub("[^0-9.]", "", metric_value),  
    metric_value = ifelse(metric_value == "", NA, metric_value),
    metric_value = as.numeric(metric_value)
  ) %>%
  group_by(county_name) %>%
  summarise(mean_unemployed = mean(metric_value, na.rm = TRUE), .groups = "drop")
asthma_unemployed <- inner_join(app_unemployed, app_asthma, by = "county_name")
unemployed_model <- lm(mean_unemployed ~ mean_asthma, data = asthma_unemployed)

# ---- 25 or Older with Less than a HS Degree - County Average ----

app_education <- app_data %>%
  filter(metric_name == "25 or Older With Less Than HS Degree Rate") %>%
  mutate(
    metric_value = gsub("[^0-9.]", "", metric_value),  
    metric_value = ifelse(metric_value == "", NA, metric_value),
    metric_value = as.numeric(metric_value)
  ) %>%
  group_by(county_name) %>%
  summarise(mean_education = mean(metric_value, na.rm = TRUE), .groups = "drop")
education_asthma <- inner_join(app_education, app_asthma, by = "county_name")
education_model <- lm(mean_education ~ mean_asthma, data = education_asthma)


# ---- Regressions for each metric and each county using geo_ID (for the heatmap) ---

# Define predictors
predictors <- c("Poverty Rate", "Median Household Income",
                "25 or Older With Less Than HS Degree Rate",
                "Uninsured Rate", "Unemployment Rate")

# Make sure the data set is able to be used
clean_data <- app_data %>%
  filter(metric_name %in% c("Asthma Rate", predictors)) %>%
  mutate(
    metric_value = gsub("[%,\\$]", "", metric_value),
    metric_value = as.numeric(metric_value)
  ) %>%
  select(county_name, geo_id, metric_name, metric_value) %>%
  pivot_wider(
    names_from = metric_name,
    values_from = metric_value
  )
county_long <- clean_data %>%
  pivot_longer(
    cols = all_of(predictors),
    names_to = "metric",
    values_to = "value"
  )

# Run regressions

county_models <- county_long %>%
  group_by(county_name, metric) %>%
  group_map(~ lm(`Asthma Rate` ~ value, data = .x))

names(county_models) <- county_long %>%
  group_by(county_name, metric) %>%
  group_keys() %>%
  mutate(name = paste(county_name, metric, sep = " - ")) %>%
  pull(name)

# Get model results and determine significance
model_results <- map_df(
  names(county_models), function(name) {
    broom::tidy(county_models[[name]]) %>%
      filter(term == "value") %>% # the slope
      mutate(
        county_name = sub(" - .*", "", name),
        metric = sub(".* - ", "", name)
      )
  }
) %>%
  mutate(significant = ifelse(p.value < 0.05, "Significant", "Not Significant"))

# Assign results for plotting
model_results <- model_results %>%
  mutate(
    county_name = factor(county_name, levels = sort(unique(county_name))),
    metric = factor(metric, levels = predictors)
  )
model_results <- model_results %>%
  mutate(metric_wrapped = str_wrap(metric, width = 15))

# --- Individual County Data Prep (Lawerence, Meigs, Muskingum, Scioto, Tuscarawas) ---

app_data <- app_data %>%
  filter(metric_name %in% c("Asthma Rate", "Median Household Income", "Poverty Rate", "25 or Older With Less Than HS Degree Rate", "Uninsured Rate", "Unemployment Rate")) %>%
  mutate(
    metric_value = gsub("[%,\\$]", "", metric_value),
    metric_value = ifelse(metric_value == "", NA, metric_value),  
    metric_value = as.numeric(metric_value)                       
  )
app_data <- app_data %>% filter(!is.na(metric_value))

# Function to process individual county data
process_county_data <- function(county_name) {
  app_data %>%
    filter(county_name == !!county_name) %>%
    select(geo_id, metric_name, metric_value) %>%
    tidyr::pivot_wider(
      names_from = metric_name,
      values_from = metric_value
    )
}

lawrence <- process_county_data("Lawrence")
meigs <- process_county_data("Meigs")
muskingum <- process_county_data("Muskingum")
scioto <- process_county_data("Scioto")
tuscarawas <- process_county_data("Tuscarawas")

# --- Regression Models and Results for Individual Counties ---

# Function to run individual county models and tidy results
run_county_models <- function(data) {
  county_long <- data %>%
    pivot_longer(cols = all_of(predictors), names_to = "metric", values_to = "value")
  
  county_models <- county_long %>%
    group_by(metric) %>%
    group_map(~ lm(`Asthma Rate` ~ value, data = .x), .keep = TRUE)
  
  names(county_models) <- unique(county_long$metric)
  
  results <- map_df(
    names(county_models), function(metric_name) {
      broom::tidy(county_models[[metric_name]]) %>%
        filter(term == "value") %>% 
        mutate(metric = metric_name)
    }
  )
  return(list(long_data = county_long, models = county_models, results = results))
}

lawrence_data <- run_county_models(lawrence)
meigs_data <- run_county_models(meigs)
muskingum_data <- run_county_models(muskingum)
scioto_data <- run_county_models(scioto)
tuscarawas_data <- run_county_models(tuscarawas)

