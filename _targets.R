# _targets.R
# Targets pipeline for Rotorua Lake Modelling project
# Author: Tadhg Moore
# Last updated: 2025-11-03

# Load packages
library(targets)
library(tarchetypes)    # optional, for dynamic branching / commands
library(dplyr)
library(ggplot2)
library(readr)

# Source R scripts (functions)
lapply(list.files("R", full.names = TRUE, pattern = "\\.R$"), source)
dir.create(here::here("data", "aeme"), showWarnings = FALSE, recursive = TRUE)

# Set target options
tar_option_set(
  packages = c("dplyr", "ggplot2", "readr"), # packages needed in your functions
  format = "rds" # default storage format
)

# Pipeline definition
list(
  
  # ---------------------------
  # 1. Load data
  # ---------------------------
  tar_target(
    aeme_rds,
    get_aeme_rds(lid = 11133),
    format = "file"
  ),
  
  tar_target(
    lake_data_raw,
    read_csv("data/lake_data.csv"),
    format = "file"
  ),
  
    tar_target(
    lake_data_raw,
    read_csv("data/lake_data.csv"),
    format = "file"
  ),
  
  tar_target(
    inflow_data_raw,
    read_csv("data/inflow_data.csv"),
    format = "file"
  ),
  
  tar_target(
    climate_scenarios,
    read_rds("data/climate_scenarios.rds")
  ),
  
  # ---------------------------
  # 2. Preprocess / clean data
  # ---------------------------
  tar_target(
    lake_data,
    preprocess_lake_data(lake_data_raw)
  ),
  
  tar_target(
    inflow_data,
    preprocess_inflow_data(inflow_data_raw)
  ),
  
  # ---------------------------
  # 3. Model execution
  # ---------------------------
  tar_target(
    glm_aed_model,
    run_glm_aed(lake_data, inflow_data),
    pattern = map(climate_scenarios)  # optional branching by scenario
  ),
  
  tar_target(
    alum_module_results,
    simulate_alum_module(glm_aed_model, inflow_data)
  ),
  
  # ---------------------------
  # 4. Scenario analysis
  # ---------------------------
  tar_target(
    climate_scenario_results,
    analyze_climate_scenarios(glm_aed_model, climate_scenarios)
  ),
  
  tar_target(
    combined_results,
    combine_results(list(alum_module_results, climate_scenario_results))
  ),
  
  # ---------------------------
  # 5. Visualization
  # ---------------------------
  tar_target(
    plots,
    create_plots(combined_results),
    format = "file" # saves plots as image files
  ),
  
  # ---------------------------
  # 6. Reporting / Quarto rendering
  # ---------------------------
  tar_target(
    report,
    quarto_render("website/index.qmd"),
    format = "file"
  )
)
