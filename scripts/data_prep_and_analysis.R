
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

# Create scatter plot
income_asthma_plot<- ggplot(app_combined_asthma, aes(x = mean_income, y = mean_asthma)) +
  geom_point(color = "darkgreen", size = 3, alpha = 0.7) +  
  geom_smooth(method = "lm", color = "red", se = TRUE) +    
  labs(
    title = "Asthma Rate vs Mean Income in App. Counties",
    x = "Mean Income ($)",
    y = "Mean Asthma Rate (%)"
  ) +
  theme_minimal(base_size = 14)
income_asthma_plot
ggsave(
  filename = here::here("figs", "income_vs_asthma_county_avg.png"), 
  plot = income_asthma_plot, 
  width = 8, 
  height = 5,
  units = "in"
)

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

combined_income_asthma_plot <- ggplot(combined_asthma, aes(x = mean_income, y = mean_asthma, color = highlight)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  scale_color_manual(values = c("Other" = "steelblue", "Appalachian Counties" = "darkgreen")) +
  labs(
    title = "Asthma Rate vs Mean Income in Ohio Counties",
    x = "Mean Income ($)",
    y = "Mean Asthma Rate (%)",
    color = "County Type"
  ) +
  theme_minimal(base_size = 14)

ggsave(
  filename = here::here("figs", "income_vs_asthma_county_avg_combined.png"), 
  plot = combined_income_asthma_plot, 
  width = 8, 
  height = 5,
  units = "in"
)

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

poverty_asthma_plot<- ggplot(app_combined_asthma_poverty, aes(x = mean_poverty, y = mean_asthma)) +
  geom_point(color = "darkgreen", size = 3, alpha = 0.7) +  
  geom_smooth(method = "lm", color = "red", se = TRUE) +    
  labs(
    title = "Asthma Rate vs Poverty in App. Counties",
    x = "Mean Poverty ($)",
    y = "Mean Asthma Rate (%)"
  ) +
  theme_minimal(base_size = 14)

ggsave(
  filename = here::here("figs", "poverty_vs_asthma_county_avg.png"), 
  plot = poverty_asthma_plot, 
  width = 8, 
  height = 5,
  units = "in"
)

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

uninsured_asthma_plot <- ggplot(asthma_uninsured, aes(x = mean_uninsured, y = mean_asthma)) +
  geom_point(color = "darkgreen", size = 3, alpha = 0.7) +  
  geom_smooth(method = "lm", color = "red", se = TRUE) +    
  labs(
    title = "Asthma Rate vs Mean Uninsured Rate in App. Counties",
    x = "Mean Uninsured Rate (%)",
    y = "Mean Asthma Rate (%)"
  ) +
  theme_minimal(base_size = 14)

ggsave(
  filename = here::here("figs", "uninsured_vs_asthma_county_avg.png"), 
  plot = uninsured_asthma_plot, 
  width = 8, 
  height = 5,
  units = "in"
)

# Remove outlier to clean data (Holmes County)

asthma_uninsured_clean <- asthma_uninsured %>%
  filter(county_name != "Holmes")

model_uninsured_clean <- lm(mean_uninsured ~ mean_asthma, data = asthma_uninsured_clean)

clean_uninsured_asthma_plot <- ggplot(asthma_uninsured_clean, aes(x = mean_uninsured, y = mean_asthma)) +
  geom_point(color = "darkgreen", size = 3, alpha = 0.7) +  
  geom_smooth(method = "lm", color = "red", se = TRUE) +    
  labs(
    title = "Asthma Rate vs Mean Uninsured Rate in App. Counties w/o outlier",
    x = "Mean Uninsured Rate (%)",
    y = "Mean Asthma Rate (%)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, margin = margin(b = 10)),
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
  )

ggsave(
  filename = here::here("figs", "clean_uninsured_vs_asthma_county_avg.png"), 
  plot = clean_uninsured_asthma_plot, 
  width = 8, 
  height = 5,
  units = "in"
)

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

unemployed_asthma_plot <- ggplot(asthma_unemployed, aes(x = mean_unemployed, y = mean_asthma)) +
  geom_point(color = "darkgreen", size = 3, alpha = 0.7) +  
  geom_smooth(method = "lm", color = "red", se = TRUE) +    
  labs(
    title = "Asthma Rate vs Mean Unemployment Rate in App. Counties",
    x = "Mean Unemployment Rate (%)",
    y = "Mean Asthma Rate (%)"
  ) +
  theme_minimal(base_size = 14)
ggsave(
  filename = here::here("figs", "unemployment_vs_asthma_county_avg.png"), 
  plot = unemployed_asthma_plot, 
  width = 8, 
  height = 5,
  units = "in"
)

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
education_asthma_plot <- ggplot(education_asthma, aes(x = mean_education, y = mean_asthma)) +
  geom_point(color = "darkgreen", size = 3, alpha = 0.7) +  
  geom_smooth(method = "lm", color = "red", se = TRUE) +    
  labs(
    title = "Asthma Rate vs Education Level in App. Counties",
    x = "Mean % 25+ With < HS Degree",
    y = "Mean Asthma Rate (%)"
  ) +
  theme_minimal(base_size = 14)
ggsave(
  filename = here::here("figs", "education_vs_asthma_county_avg.png"), 
  plot = education_asthma_plot, 
  width = 8, 
  height = 5,
  units = "in"
)

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

# Lawrence County
lawrence_plot <- ggplot(lawrence_data$long_data, aes(x = value, y = `Asthma Rate`)) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  facet_wrap(~ metric, scales = "free_x") +  # one plot per metric
  labs(title = "Lawrence County Asthma Rate vs Each Metric",
       x = "Metric Value",
       y = "Asthma Rate") +
  theme_minimal()
ggsave(
  filename = here::here("figs", "lawrence.png"), 
  plot = lawrence_plot, 
  width = 9, 
  height = 6,
  units = "in"
)

# Meigs County
meigs_plot <- ggplot(meigs_data$long_data, aes(x = value, y = `Asthma Rate`)) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  facet_wrap(
    ~ metric,
    scales = "free_x",
    labeller = label_wrap_gen(width = 15)   
  ) +
  labs(
    title = "Meigs County Asthma Rate vs Each Metric",
    x = "Metric Value",
    y = "Asthma Rate"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(size = 10))
ggsave(
  filename = here::here("figs", "meigs.png"), 
  plot = meigs_plot, 
  width = 9, 
  height = 6,
  units = "in"
)
# Muskingum
muskingum_plot <- ggplot(muskingum_data$long_data, aes(x = value, y = `Asthma Rate`)) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  facet_wrap(
    ~ metric,
    scales = "free_x",
    labeller = label_wrap_gen(width = 15)   # <-- wraps long names
  ) +
  labs(
    title = "Muskingum County Asthma Rate vs Each Metric",
    x = "Metric Value",
    y = "Asthma Rate"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(size = 10))
ggsave(
  filename = here::here("figs", "muskingum.png"), 
  plot = muskingum_plot, 
  width = 9, 
  height = 6,
  units = "in"
)

# Scioto
scioto_plot <- ggplot(scioto_data$long_data, aes(x = value, y = `Asthma Rate`)) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  facet_wrap(
    ~ metric,
    scales = "free_x",
    labeller = label_wrap_gen(width = 15)
  ) +
  labs(
    title = "Scioto County Asthma Rate vs Each Metric",
    x = "Metric Value",
    y = "Asthma Rate"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(size = 10))
ggsave(
  filename = here::here("figs", "scioto.png"), 
  plot = scioto_plot, 
  width = 9, 
  height = 6,
  units = "in"
)


# Tuscarawas
tuscarawas_plot <- ggplot(tuscarawas_data$long_data, aes(x = value, y = `Asthma Rate`)) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  facet_wrap(
    ~ metric,
    scales = "free_x",
    labeller = label_wrap_gen(width = 15)   # <-- wraps long names
  ) +
  labs(
    title = "Tuscarawas County Asthma Rate vs Each Metric",
    x = "Metric Value",
    y = "Asthma Rate"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(size = 10))
ggsave(
  filename = here::here("figs", "tuscarawas.png"), 
  plot = tuscarawas_plot, 
  width = 9, 
  height = 6,
  units = "in"
)

heatmap <- ggplot(model_results, aes(x = metric_wrapped, y = county_name, fill = significant)) +
  geom_tile(color = "white", width = 0.9, height = 1.2) +
  scale_fill_manual(values = c("Significant" = "blue", "Not Significant" = "grey")) +
  labs(
    title = "Significant Predictors of Asthma Rate by County",
    x = "Predictor",
    y = "County",
    fill = "Significant"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),  
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 13),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    panel.grid = element_blank()
  )
ggsave(
  filename = here::here("figs", "significance_heatmap.png"), 
  plot = heatmap, 
  width = 9, 
  height = 6,
  units = "in"
)

