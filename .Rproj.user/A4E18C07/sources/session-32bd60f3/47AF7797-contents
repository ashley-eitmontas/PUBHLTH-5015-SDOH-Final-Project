# scripts/01_figures.R
# Purpose: Generate choropleth map of asthma outcomes in Appalachian Ohio counties
# Author(s): Ashley Eitmontas, Ella Anderson
# Date: December 2025

# -------------------------------
# Load required packages
# -------------------------------
library(sf)         # Spatial data handling
library(tigris)     # Census shapefiles
library(dplyr)      # Data manipulation
library(ggplot2)    # Plotting
library(stringr)    # String operations
library(here)       # Reproducible file paths

# Cache shapefiles locally for efficiency
options(tigris_use_cache = TRUE)

# -------------------------------
# Load cleaned SDOH data
# -------------------------------
SDOH <- read_rds(here("results", "clean.rds"))

# -------------------------------
# Define Appalachian counties in Ohio
# -------------------------------
appalachian_ohio <- c(
  "Adams","Athens","Ashtabula","Belmont","Brown","Carroll","Columbiana",
  "Coshocton","Clermont","Gallia","Guernsey","Harrison","Highland",
  "Hocking","Holmes","Jackson","Jefferson","Lawrence","Meigs","Monroe",
  "Morgan","Mahoning","Muskingum","Noble","Perry","Pike","Ross","Scioto",
  "Trumbull","Tuscarawas","Vinton","Washington"
)

# -------------------------------
# Filter asthma data from SDOH
# -------------------------------
asthma_data <- SDOH %>%
  filter(str_detect(metric_name, regex("asthma", ignore_case = TRUE))) %>%
  mutate(county_name = str_to_title(county_name)) %>%
  select(county_name, asthma_rate = metric_value)

# -------------------------------
# Join asthma data to Ohio counties shapefile
# -------------------------------
ohio_counties <- counties(state = "OH", cb = TRUE, class = "sf") %>%
  mutate(is_appalachian = if_else(NAME %in% appalachian_ohio, TRUE, FALSE)) %>%
  left_join(asthma_data, by = c("NAME" = "county_name"))

# -------------------------------
# Create choropleth map
# -------------------------------
p <- ggplot() +
  geom_sf(data = ohio_counties, fill = "grey90", color = "white") +
  geom_sf(data = filter(ohio_counties, is_appalachian), 
          aes(fill = asthma_rate), color = "white") +
  geom_sf_text(data = filter(ohio_counties, is_appalachian), 
               aes(label = NAME), size = 2, color = "black") +
  scale_fill_viridis_c(option = "plasma", na.value = "grey80") +
  labs(
    title = "Asthma Outcomes in Appalachian Counties (Ohio)",
    fill = "Asthma Rate"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

# -------------------------------
# Save figure to figs/ folder
# -------------------------------
ggsave(
  filename = here("figs", "asthma_map.png"),
  plot = p,
  width = 8,
  height = 6
)
