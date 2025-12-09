# scripts/03_plots.R
# Purpose: Generate and save maps, scatterplots, diagnostics, and backward elimination path figures
# Author(s): Ashley Eitmontas, Ella Anderson
# Date: December 2025

# -------------------------------
# Load required packages
# -------------------------------
library(dplyr)      # Data manipulation
library(stringr)    # String operations
library(tidyr)      # Reshaping data
library(readr)      # Read/write data
library(ggplot2)    # Plotting
library(tigris)     # Census shapefiles
library(viridis)    # Color scales
library(here)       # Reproducible file paths

# -------------------------------
# Load inputs produced earlier by the pipeline
# -------------------------------
# These are created by scripts/02_analysis.R
model_df <- read_csv(here("results", "model_df.csv"), show_col_types = FALSE)
full_model <- readRDS(here("results", "full_model.rds"))

# Optional: load backward elimination object if saved
# be <- readRDS(here("results", "backward_elimination.rds"))

# -------------------------------
# Variables used for plotting
# -------------------------------
numeric_vars <- c(
  "asthma_rate", "poverty_rate", "uninsured_rate",
  "unemployment_rate", "median_household_income",
  "hs_less_than_rate"
)

# -------------------------------
# Appalachian counties list
# -------------------------------
appalachian_counties <- c(
  "Adams","Athens","Ashtabula","Belmont","Brown","Carroll","Columbiana",
  "Coshocton","Clermont","Gallia","Guernsey","Harrison","Highland",
  "Hocking","Holmes","Jackson","Jefferson","Lawrence","Meigs","Monroe",
  "Morgan","Mahoning","Muskingum","Noble","Perry","Pike","Ross","Scioto",
  "Trumbull","Tuscarawas","Vinton","Washington"
)

# Ensure output directory exists
dir.create(here("figs"), showWarnings = FALSE)

# ============================================
# 1) Geographic Choropleth (Appalachian subset)
# ============================================
options(tigris_use_cache = TRUE)
ohio_counties <- counties(state = "OH", cb = TRUE)

map_data <- ohio_counties %>%
  filter(NAME %in% appalachian_counties) %>%
  left_join(model_df, by = c("NAME" = "county_name"))

p_map <- ggplot(map_data) +
  geom_sf(aes(fill = asthma_rate), color = "white") +
  scale_fill_viridis_c(option = "plasma", na.value = "grey80") +
  labs(
    title = "Asthma rate in Appalachian Ohio counties",
    fill = "Asthma rate"
  ) +
  theme_minimal()

ggsave(
  filename = here("figs", "asthma_choropleth.png"),
  plot = p_map, width = 8, height = 6, dpi = 300
)

# ============================================
# 2) Predictor vs Outcome Scatterplots
# ============================================
pred_vars <- setdiff(numeric_vars, "asthma_rate")

for (var in pred_vars) {
  p_scatter <- ggplot(model_df, aes_string(x = var, y = "asthma_rate")) +
    geom_point(color = "blue", alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    labs(
      title = paste("Asthma rate vs", var),
      x = var, y = "Asthma rate"
    ) +
    theme_minimal()
  
  safe_var <- gsub("[^A-Za-z0-9_]+", "_", var)
  out_file <- here("figs", paste0("scatter_asthma_vs_", safe_var, ".png"))
  
  ggsave(filename = out_file, plot = p_scatter, width = 7, height = 5, dpi = 300)
}

# ============================================
# 3) Model diagnostics (Residuals, QQ, Scale-Location, Cook's)
# ============================================
png(filename = here("figs", "model_diagnostics.png"), width = 1200, height = 900, res = 150)
par(mfrow = c(2, 2))
plot(full_model)
dev.off()

# ============================================
# 4) Backward elimination path visualization
# ============================================
if (file.exists(here("results", "backward_elimination.rds"))) {
  be <- readRDS(here("results", "backward_elimination.rds"))
  elim_steps <- be$path
} else {
  terms_retained <- attr(terms(full_model), "term.labels")
  elim_steps <- paste("Retained:", terms_retained)
}

elim_df <- data.frame(step = seq_along(elim_steps), action = elim_steps)

p_elim <- ggplot(elim_df, aes(x = step, y = step)) +
  geom_point(color = "darkred", size = 2) +
  geom_text(aes(label = action), vjust = -0.6, hjust = 0, size = 3) +
  labs(
    title = "Backward elimination path",
    x = "Step", y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

ggsave(
  filename = here("figs", "backward_elimination_path.png"),
  plot = p_elim, width = 9, height = 6, dpi = 300
)
