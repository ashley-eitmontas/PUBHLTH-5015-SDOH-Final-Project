# scripts/03_plots.R
# Generate and save maps, scatterplots, diagnostics, and elimination path figures

# --- Libraries ---
library(dplyr)
library(stringr)
library(tidyr)
library(readr)
library(ggplot2)
library(tigris)
library(viridis)  # for viridis scale
# corrplot used only if you add correlation plots; not used below
# library(corrplot)

# --- Load inputs produced earlier by the pipeline ---
# These are created by scripts/02_analysis.R
model_df <- readr::read_csv("results/model_df.csv", show_col_types = FALSE)
full_model <- readRDS("results/full_model.rds")

# Optional: load backward elimination object if you saved it
# be <- readRDS("results/backward_elimination.rds")
# If not saved, we’ll build a text path from model terms (fallback below)

# --- Variables used for plotting ---
numeric_vars <- c(
  "asthma_rate", "poverty_rate", "uninsured_rate",
  "unemployment_rate", "median_household_income",
  "hs_less_than_rate"  # renamed in your analysis script
)

# --- Appalachian counties list ---
appalachian_counties <- c(
  "Adams","Athens","Ashtabula","Belmont","Brown","Carroll","Columbiana",
  "Coshocton","Clermont","Gallia","Guernsey","Harrison","Highland",
  "Hocking","Holmes","Jackson","Jefferson","Lawrence","Meigs","Monroe",
  "Morgan","Mahoning","Muskingum","Noble","Perry","Pike","Ross","Scioto",
  "Trumbull","Tuscarawas","Vinton","Washington"
)

# Ensure output directory exists
dir.create("figs", showWarnings = FALSE)

# ============================================
# 1) Geographic Choropleth (Appalachian subset)
# ============================================

options(tigris_use_cache = TRUE)
ohio_counties <- counties(state = "OH", cb = TRUE)

map_data <- ohio_counties %>%
  dplyr::filter(NAME %in% appalachian_counties) %>%
  dplyr::left_join(model_df, by = c("NAME" = "county_name"))

p_map <- ggplot(map_data) +
  geom_sf(aes(fill = asthma_rate), color = "white") +
  scale_fill_viridis_c(option = "plasma", na.value = "grey80") +
  labs(title = "Asthma rate in Appalachian Ohio counties",
       fill = "Asthma rate") +
  theme_minimal()

ggsave(filename = "figs/asthma_choropleth.png",
       plot = p_map, width = 8, height = 6, dpi = 300)

# ============================================
# 2) Predictor vs Outcome Scatterplots (saved per variable)
# ============================================

pred_vars <- setdiff(numeric_vars, "asthma_rate")

for (var in pred_vars) {
  # Use aes_string for dynamic aesthetics (works for non-standard names)
  p_scatter <- ggplot(model_df, aes_string(x = var, y = "asthma_rate")) +
    geom_point(color = "blue", alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    labs(title = paste("Asthma rate vs", var),
         x = var, y = "Asthma rate") +
    theme_minimal()
  
  # Safe file name (replace spaces with underscores)
  safe_var <- gsub("[^A-Za-z0-9_]+", "_", var)
  out_file <- file.path("figs", paste0("scatter_asthma_vs_", safe_var, ".png"))
  
  ggsave(filename = out_file, plot = p_scatter, width = 7, height = 5, dpi = 300)
}

# ============================================
# 3) Model diagnostics (Residuals, QQ, Scale-Location, Cook's)
# ============================================

# Base R plots: open a PNG device, plot, then close
png(filename = "figs/model_diagnostics.png", width = 1200, height = 900, res = 150)
par(mfrow = c(2, 2))
plot(full_model)
dev.off()

# ============================================
# 4) Backward elimination path visualization
# ============================================

if (file.exists("results/backward_elimination.rds")) {
  be <- readRDS("results/backward_elimination.rds")
  elim_steps <- be$path
} else {
  # Fallback placeholder: list retained terms in final model
  terms_retained <- attr(terms(full_model), "term.labels")
  elim_steps <- paste("Retained:", terms_retained)
}

elim_df <- data.frame(step = seq_along(elim_steps), action = elim_steps)

p_elim <- ggplot(elim_df, aes(x = step, y = step)) +
  geom_point(color = "darkred", size = 2) +
  geom_text(aes(label = action), vjust = -0.6, hjust = 0, size = 3) +
  labs(title = "Backward elimination path",
       x = "Step", y = NULL) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

ggsave(filename = "figs/backward_elimination_path.png",
       plot = p_elim, width = 9, height = 6, dpi = 300)
