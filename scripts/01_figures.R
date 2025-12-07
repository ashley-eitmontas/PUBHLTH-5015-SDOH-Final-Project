# scripts/01_figures.R
library(sf)
library(tigris)
library(dplyr)
library(ggplot2)
library(stringr)

options(tigris_use_cache = TRUE)

SDOH <- read_rds("results/clean.rds")

# Appalachian counties list
appalachian_ohio <- c("Adams","Athens","Ashtabula","Belmont","Brown","Carroll","Columbiana",
                      "Coshocton","Clermont","Gallia","Guernsey","Harrison","Highland",
                      "Hocking","Holmes","Jackson","Jefferson","Lawrence","Meigs","Monroe",
                      "Morgan","Mahoning","Muskingum","Noble","Perry","Pike","Ross","Scioto",
                      "Trumbull","Tuscarawas","Vinton","Washington")

# Filter asthma data
asthma_data <- SDOH %>%
  filter(str_detect(metric_name, regex("asthma", ignore_case = TRUE))) %>%
  mutate(county_name = str_to_title(county_name)) %>%
  select(county_name, asthma_rate = metric_value)

# Join asthma data to counties
ohio_counties <- counties(state = "OH", cb = TRUE, class = "sf") %>%
  mutate(is_appalachian = if_else(NAME %in% appalachian_ohio, TRUE, FALSE)) %>%
  left_join(asthma_data, by = c("NAME" = "county_name"))

# Plot
p <- ggplot() +
  geom_sf(data = ohio_counties, fill = "grey90", color = "white") +
  geom_sf(data = filter(ohio_counties, is_appalachian), aes(fill = asthma_rate), color = "white") +
  geom_sf_text(data = filter(ohio_counties, is_appalachian), aes(label = NAME), size = 2, color = "black") +
  scale_fill_viridis_c(option = "plasma", na.value = "grey80") +
  labs(title = "Asthma Outcomes in Appalachian Counties (Ohio)", fill = "Asthma Rate") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

ggsave("figs/asthma_map.png", p, width = 8, height = 6)
