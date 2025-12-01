# _targets.R
# Targets pipeline for Rotorua Lake Modelling project
# Author: Tadhg Moore
# Last updated: 2025-11-03

# Load packages
library(targets)
library(tarchetypes)    # optional, for dynamic branching / commands
library(crew)
tar_option_set(
  controller = crew_controller_local(workers = 10)
)
# Source R scripts (functions)
tar_source(
  c(
    here::here("R", "summarise_gcm_spatial.R"),
    here::here("R", "gather_cmip6_metadata.R"),
    here::here("R", "process_cmip6_shp.R"),
    here::here("R", "get_vcsn_grid_points.R"),
    here::here("R", "utils-nc.R"),
    here::here("R", "process_cmip6.R"),
    here::here("R", "summarise_gcm_spatial.R"),
    here::here("R", "summarise_gcm_ts.R")
  )
)
# Set target options
tar_option_set(
  # packages = c("dplyr", "ggplot2", "aemetools"), # packages needed in your functions
  format = "rds" # default storage format
)

# Pipeline definition
list(
  
  tar_target(
    rotorua_catchment_bbox_file, here::here("data", "processed", "rotorua_area.rds")
  ),
  
  tar_target(
    lake_id, 11133
  ),
  
  tar_target(
    cmip_vars, c("tas", "hurs", "pr", "rsds", "sfcWind")
  ),
  
  tar_target(
    cmip_gcm, c("ACCESS-CM2", "AWI-CM-1-1-MR", "CNRM-CM6-1", "EC-Earth3", 
                "GFDL-ESM4", "NZESM", "NorESM2-MM")
  ),
  
  tar_target(
    cmip_scenarios, c("ssp126", "ssp245", "ssp370", "ssp585")
  ),
  
  tar_target(
    time_periods, list(
      "2015-2040" = c(2015, 2040),
      "2041-2070" = c(2041, 2070),
      "2071-2100" = c(2071, 2100)
    )
  ),
  
  tar_target(
    vcsn_grid_points, get_vcsn_grid_points()
  ),
  
  # ---------------------------
  # 1. Load data
  # ---------------------------
  tar_target(
    cmip6_metadata, gather_cmip6_metadata()
  ),
  
  tar_target(
    lernzmp_aeme, aemetools::get_aeme(id = lake_id,
                                          api_key = Sys.getenv("LERNZMP_API"))
  ),
  
  tar_target(
    lake_shape, 
    aemetools::get_lake_shape(id = lake_id, 
                              api_key = Sys.getenv("LERNZMP_API"))
  ),
  
  tar_target(
    rotorua_catchment_bbox, readRDS(rotorua_catchment_bbox_file)
  ),

  
  # ---------------------------
  # 2. Preprocess / clean data
  # ---------------------------
  
  # CMIP6 processing
  tar_target(
    cmip6_processed,
    process_cmip6_shp(
      x = rotorua_catchment_bbox,
      variable = cmip_vars,
      vcsn_grid_points = vcsn_grid_points,
      metadata = cmip6_metadata,
      out_dir = here::here("data", "processed", "rotorua_cmip6"),
      overwrite = FALSE
    )
  ),
  
  tar_target(
    cmip6_files,
    cmip6_processed$outfile,
    format = "file"
  ),
  
  tar_target(
    gcm_spatial_summary,
    summarise_gcm_spatial(variable = cmip_vars, gcm = cmip_gcm,
                          time_periods = time_periods, files = cmip6_files),
    pattern = cross(cmip_vars, cmip_gcm),
    iteration = "list"
  ),
  tar_target(
    gcm_spatial_df, dplyr::bind_rows(gcm_spatial_summary)
  ),
  
  tar_target(
    gcm_ts_summary,
    summarise_gcm_ts(variable = cmip_vars, gcm = cmip_gcm, files = cmip6_files),
    pattern = cross(cmip_vars, cmip_gcm),
    iteration = "list"
  ),
  tar_target(
    gcm_ts_df, dplyr::bind_rows(gcm_ts_summary)
  ),
  
  
  # ---------------------------
  # 3. Model execution
  # ---------------------------
  
  # ---------------------------
  # 4. Scenario analysis
  # ---------------------------
  
  # ---------------------------
  # 5. Visualization
  # ---------------------------
  
  #* GCM Data
  tar_target(
    gcm_spatial_plot, 
    {
      p <- plot_gcm_spatial(gcm_spatial_df, variable = cmip_vars)
      ggsave(
        filename = here::here("man", "figures", paste0("gcm_spatial_", 
                                                           cmip_vars, ".png")),
        plot = p,
        width = 10,
        height = 6
      )
      p
    },
    pattern = map(cmip_vars),
    format = "file"
  ),
  tar_target(
    gcm_ts_plot, 
    {
      p <- plot_gcm_ts(gcm_ts_df, variable = cmip_vars)
      ggsave(
        filename = here::here("man", "figures", paste0("gcm_ts_", 
                                                           cmip_vars, ".png")),
        plot = p,
        width = 10,
        height = 6
      )
      p
    },
    pattern = map(cmip_vars),
    format = "file"
  )
  
  
  # ---------------------------
  # 6. Reporting / Quarto rendering
  # ---------------------------

)
