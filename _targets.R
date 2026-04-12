# _targets.R
# Targets pipeline for Rotorua Lake Modelling project
# Author: Tadhg Moore
# Last updated: 2025-11-03

# Load packages
library(targets)
library(tarchetypes)    # optional, for dynamic branching / commands
library(crew)
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
    here::here("R", "summarise_gcm_ts.R"),
    here::here("R", "plot_gcm_spatial.R"),
    here::here("R", "plot_var_ts.R"),
    here::here("R", "get_lake_wqprofiler.R"),
    here::here("R", "format_buoy_for_aeme.R"),
    here::here("R", "update_hyps.R"),
    here::here("R", "get_point_data.R"),
    here::here("R", "extract_lake_level.R"),
    here::here("R", "read_ctd.R"),
    here::here("R", "standardise_to_gregorian.R"),
    here::here("R", "estimate_sed_zones.R"),
    here::here("R", "buoy_to_aeme_met.R"),
    here::here("R", "read_niwa_climate_file.R"),
    here::here("R", "niwa_to_aeme_met.R")
  )
)
# Set target options
tar_option_set(
  controller = crew_controller_local(workers = 5), 
  error = "continue", 
  storage = "worker", 
  retrieval = "worker",
  packages = c("dplyr", "ggplot2", "AEME"), # packages needed in your functions
  format = "rds" # default storage format
)

# Pipeline definition
list(
  
  # 0. Define constants ----
  tar_target(
    # rotorua_catchment_bbox_file, here::here("data", "processed", "rotorua_area.rds")
    rotorua_catchment_bbox_file, here::here("data", "processed", "rotorua_lakes_area.rds")
  ),
  tar_target(
    tutira_bbox_coords, c(xmin = 176.85762, ymin = -39.24869, xmax = 176.93763, 
                          ymax = -39.20032)
  ),
  tar_target(
    #list.files(here::here("data", "raw", "niwa_climate"), pattern = "daily", full.names = TRUE),
    niwa_met_daily_files, c(
      here::here("data", "raw", "niwa_climate", "1770__Evaporation__Penman-Open-Water-Evaporation__daily.csv"),
      here::here("data", "raw", "niwa_climate", "1770__Evaporation__Penman-PET__daily.csv"),
      here::here("data", "raw", "niwa_climate", "1770__Evaporation__Priestly-Taylor-PET__daily.csv"),
      here::here("data", "raw", "niwa_climate", "1770__Radiation__Global__daily.csv"),
      here::here("data", "raw", "niwa_climate", "1770__Rain__daily.csv"),
      here::here("data", "raw", "niwa_climate", "1770__Temperature__daily.csv"),
      here::here("data", "raw", "niwa_climate", "1770__Wind__daily.csv")
    )
  ),
  tar_target(
    niwa_met_hourly_files, list.files(here::here("data", "raw", "niwa_climate"), 
                                      pattern = "hourly", full.names = TRUE),
    format = "file"
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
    all_scenarios , c("historical", cmip_scenarios)
  ),
  
  tar_target(
    time_periods, list(
      "2015-2040" = c(2015, 2040),
      "2041-2070" = c(2041, 2070),
      "2071-2100" = c(2071, 2100)
    )
  ),
  
  #* Simulation periods ----
  tar_target(
    sim_periods, list(
      "historical" = list(start = "1984-07-01", stop = "2014-06-30",
                          spin_up = (365 * 10)),
      "ssp" = list(start = "2015-07-01", stop = "2099-06-30",
                   spin_up = (365 * 10)
      )
    )
  ),
  
  tar_target(
    vcsn_grid_points, get_vcsn_grid_points()
  ),
  
  tar_target(
    model, "glm_aed"
  ),
  tar_target(
    aed_models, c("aed_sedflux", "aed_oxygen", "aed_silica", "aed_nitrogen",
                  "aed_phosphorus", "aed_organic_matter", "aed_phytoplankton",
                  # "aed_zooplankton", "aed_macrophyte",
                  "aed_totals")
  ),
  
  tar_target(
    path, "aeme"
  ),
  
  tar_target(ext_elev, 3),
  
  tar_target(
    sim_grid,
    tidyr::crossing(
      gcm      = cmip_gcm,
      scenario = all_scenarios,
      model    = model,
      lake_id  = lake_id
    ) |> 
      dplyr::filter(!(scenario == "ssp585" & gcm == "NZESM")) # NZESM doesn't have ssp585 data
  ),
  
  tar_target(
    bop_lake_level_zip_folder, here::here("data", "raw",
                                          "BulkExport-FL150407-20251215152116.zip")
  ),
  
  # CTD Excel file
  tar_target(
    ctd_excel_file, 
    here::here("data", "raw", "bop_wq",
               "Lake Rotorua CTD Profile Data - Full Record .xlsx")  
  ),
  
  # Lake Chemistry data
  tar_target(
    chem_excel_file,
    here::here("data", "raw", "bop_wq", "Lakes Data 1989_2025.xlsx")
  ),
  
  # 1. Load data ----
  #* Unzip lake level folder and load data
  tar_target(
    lake_level, extract_lake_level(bop_lake_level_zip_folder)
  ),
  tar_target(
    median_lake_level_masl, median(lake_level$Lake_Level_m)
  ),
  
  # NIWA met data
  tar_target(
    niwa_met_daily,
    read_niwa_climate_file(niwa_met_daily_files),
    pattern = map(niwa_met_daily_files)
  ),
  
  
  tar_target(
    cmip6_metadata, {
      gather_cmip6_metadata()
    }, 
    cue = tar_cue(mode = "never")
  ),
  
  tar_target(
    gcm_metadata, {
      cmip6_metadata |> 
        dplyr::select(gcm, scenario, spatial_res, calendar) |> 
        dplyr::filter(!is.na(calendar)) |> 
        dplyr::distinct()
    }
  ),
  tar_target(
    gcm_metadata_file, {
      out_file <- here::here("data", "processed", "cmip6_gcm_metadata.csv")
      readr::write_csv(gcm_metadata, file = out_file)
      out_file
    },
  ),
  tar_target(
    lernzmp_aeme, aemetools::get_aeme(id = lake_id,
                                      api_key = Sys.getenv("LERNZMP_API"))
  ),
  
  tar_target(
    lake_meta, AEME::lake(lernzmp_aeme) |> 
      as.data.frame()
  ),
  
  tar_target(
    lake_shape, 
    aemetools::get_lake_shape(id = lake_id, 
                              api_key = Sys.getenv("LERNZMP_API"))
  ),
  
  tar_target(
    rotorua_catchment_bbox, readRDS(rotorua_catchment_bbox_file)
  ),
  tar_target(
    tutira_catchment_bbox, {
      bb <- tutira_bbox_coords |> 
        sf::st_bbox() |> 
        sf::st_as_sfc() |> 
        sf::st_as_sf()
      sf::st_crs(bb) <- 4326
      sf::st_geometry(bb) <- "geometry"
      bb
    }
  ),
  
  tar_target(
    rotorua_buoy_pro_data, get_lake_wqprofiler(type = "pro",
                                               api_key = Sys.getenv("LERNZMP_API")), 
    # cue = tar_cue(mode = "always")
  ),
  tar_target(
    rotorua_buoy_met_data, get_lake_wqprofiler(type = "met",
                                               api_key = Sys.getenv("LERNZMP_API")), 
    # cue = tar_cue(mode = "always")
  ),
  tar_target(
    rotorua_buoy_met_aeme, buoy_to_aeme_met(rotorua_buoy_met_data)
  ),
  # NIWA met to AEME data
  tar_target(
    niwa_met_aeme, {
      niwa_met_daily |>
        dplyr::bind_rows() |>
        niwa_to_aeme_met()
    }
  ),
  tar_target(
    ctd_data, read_ctd(file = ctd_excel_file)
  ),
  
  # 2. Preprocess / clean data ----
  
  # Subset lake level data
  tar_target(
    sub_lake_level, {
      browser()
      surf_elev <- corr_hyps |> 
        dplyr::filter(depth == 0) |>
        dplyr::pull(elev)
      
      lake_level |> 
        dplyr::filter(qc_code > 200,
                      Date >= as.Date("2014-06-12")) |> 
        dplyr::group_by(Date) |>
        dplyr::summarise(
          elev = mean(Lake_Level_m, na.rm = TRUE)
        ) |>
        dplyr::mutate(
          var_aeme = "LKE_lvlwtr",
          value = elev # - surf_elev
        ) |> 
        dplyr::select(Date, var_aeme, value)
    }
  ),
  
  # Fix Rotorua hypsograph in aeme
  tar_target(
    corr_hyps, fix_hyps(aeme_base, lake_elev = median_lake_level_masl)
  ),
  tar_target(
    hyps_no_hole, {
      hyp <- corr_hyps |> 
        dplyr::filter(depth >= -21.5) |> 
        dplyr::mutate(
          area = ifelse(depth == -21.5, 0, area)
        )
    }
  ),
  tar_target(
    aeme_base_hyps, {
      aeme <- update_hyps(aeme_base, hyps = corr_hyps)
      return(aeme)
    }
  ),
  # Estimate sediment zones & params
  tar_target(
    glm_sed_param, estimate_sed_zones(aeme_base_hyps)
  ),
  
  tar_target(sub_metadata, {
    cmip6_metadata |> 
      dplyr::filter(variable %in% cmip_vars)
  }
  ),
  
  # CMIP6 processing
  tar_target(
    cmip6_files, {
      process_cmip6_shp(
        x = rotorua_catchment_bbox,
        vcsn_grid_points = vcsn_grid_points,
        metadata = sub_metadata,
        out_dir = here::here("data", "processed", "rotorua_lakes_cmip6"),
        overwrite = FALSE
      )
    }, 
    pattern = map(sub_metadata),
    format = "file",
    # deployment = "main"
    # cue = tar_cue(mode = "never")
  ),
  # tar_target(
  #   tutira_cmip6_files, {
  #     process_cmip6_shp(
  #       x = tutira_catchment_bbox,
  #       vcsn_grid_points = vcsn_grid_points,
  #       metadata = sub_metadata,
  #       out_dir = here::here("data", "processed", "tutira_cmip6"),
  #       overwrite = FALSE
  #     )
  #   }, 
  #   pattern = map(sub_metadata),
  #   format = "file",
  #   # deployment = "main"
  #   cue = tar_cue(mode = "never")
  # ),
  # tar_target(
  #   cmip6_files, {
  #     cmip6_processed$outfile
  #   },
  #   format = "file"
  # ),
  
  # GCM Spatial & Temporal summaries
  # tar_target(
  #   gcm_spatial_summary,
  #   summarise_gcm_spatial(variable = cmip_vars, gcm = cmip_gcm,
  #                         time_periods = time_periods, files = cmip6_files),
  #   pattern = cross(cmip_vars, cmip_gcm),
  #   iteration = "list"
  # ),
  # tar_target(
  #   gcm_spatial_df, dplyr::bind_rows(gcm_spatial_summary)
  # ),
  
  tar_target(
    gcm_ts_summary, {
      # browser()
      summarise_gcm_ts(variable = cmip_vars, gcm = cmip_gcm, 
                       files = cmip6_files)
    },
    pattern = cross(cmip_vars, cmip_gcm),
    iteration = "list", 
    cue = tar_cue(mode = "never")
  ),
  tar_target(
    gcm_ts_df, dplyr::bind_rows(gcm_ts_summary)
  ),
  
  # Buoy data 
  tar_target(
    aeme_buoy_data, {
      rotorua_buoy_pro_data |> 
        format_buoy_for_aeme()
    }
  ),
  
  # CTD data
  
  # Add buoy data to AEME
  tar_target(
    aeme_base, {
      browser()
      aeme <- lernzmp_aeme
    } 
  ),
  
  # Prepare GCM data for model input
  tar_target(
    gcm_point_data, {
      get_point_data(lat = lake_meta$latitude,
                     lon = lake_meta$longitude,
                     lakename = lake_meta$name,
                     gcm = sim_grid$gcm,
                     cmip_vars = cmip_vars,
                     scenario = sim_grid$scenario,
                     cmip6_files = cmip6_files, 
                     cmip6_metadata = cmip6_metadata)
    },
    pattern = map(sim_grid),
    iteration = "list",
    cue = tar_cue(mode = "never")
  ),
  tar_target(
    gcm_point_data_std, {
      gcm_point_data_df |> 
        dplyr::filter(gcm == sim_grid$gcm,
                      scenario == sim_grid$scenario) |> 
        standardise_to_gregorian(date_col = "date",
                                 metadata = cmip6_metadata) |> 
        dplyr::mutate(
          gcm = sim_grid$gcm,
          scenario = sim_grid$scenario
        )
    },
    pattern = map(sim_grid),
    iteration = "list"
  ),
  tar_target(
    gcm_point_data_std_df, dplyr::bind_rows(gcm_point_data_std)
  ),
  
  tar_target(
    gcm_point_data_df, {
      sub <- lapply(gcm_point_data, \(x) {
        x |> 
          dplyr::select(gcm, scenario, variable, date_char, value) |> 
          dplyr::rename(date = date_char) |>
          dplyr::mutate(value = signif(value, digits = 6))
      }) |> 
        dplyr::bind_rows()
    }
  ),
  
  tar_target(
    gcm_point_csv_file, {
      out_file <- here::here("data", "processed", 
                             paste0("gcm_point_data_lake_", lake_id, ".csv"))
      readr::write_csv(gcm_point_data_df, file = out_file)
      out_file
    },
    format = "file"
  ),
  
  # tar_target(
  #   sim_build,
  #   setup_aeme(
  #     aeme     = aeme_base_hyps, 
  #     gcm      = sim_grid$gcm,
  #     scenario = sim_grid$scenario,
  #     model    = sim_grid$model
  #   ),
  #   pattern = map(sim_grid)
  # ),
  # 
  # tar_target(
  #   sim_run,
  #   run_lake_model(sim_build),
  #   pattern = map(sim_build)
  # ),
  
  
  
  
  # 3. Model execution ----
  tar_target(
    baseline_aeme, {
      browser()
      # inp <- AEME::input(aeme_base_hyps)
      lake_data <- ctd_data |> 
        dplyr::bind_rows(aeme_buoy_data) 
      aeme <- aeme_base_hyps |>
        AEME::add_param(glm_sed_param) |>
        AEME::add_obs(lake = lake_data, level = sub_lake_level) |>
        AEME::remove_inflow(all = TRUE) |>
        AEME::reset_wbal_param() |> 
        AEME::set_precip(type = "precip_as_inflow") |> 
        AEME::add_param(param = glm_sed_param) |> 
        AEME::build_aeme(model = model, ext_elev = ext_elev, wb_method = 2,
                         use_bgc = TRUE, path = path)
      
      AEME::plot_weir_calibration(aeme)
      AEME::plot_est_wbal(aeme)
      # wb_comp <- AEME::get_wbal_components(aeme)
      # AEME::plot_wbal_comp(wb_comp)
      
      # AEME::plot_wbal(aeme)
      
      aeme <- aeme |> 
        AEME::set_glm_aed_models(aed_models = aed_models) |> 
        AEME::set_aed_totals() |> 
        # AEME::set_aed_sed_const2d(path = path) |> 
        AEME::run_aeme()
      # AEME::plot_wbal_annual(aeme, lake_frac = T)
      AEME::plot_wlev(aeme, remove_spin_up = F)
      return(aeme)
      aeme |> 
        # AEME::add_obs(level = sub_lake_level) |> 
        AEME::plot_wlev()
      AEME::plot_wbal(aeme, cumulative = T)
      AEME::plot_output(aeme, remove_spin_up = F)
      AEME::plot_output(aeme, var_sim = "CHM_oxy")
      AEME::plot_output(aeme, var_sim = "PHY_tchla")
    }
  ),
  # tar_target(
  #   aeme_sims, {
  #     if (grepl("ssp", sim_grid$scenario)) {
  #       sel_scen <- c("historical", sim_grid$scenario)
  #     } else if (grepl("historical", sim_grid$scenario)) {
  #       sel_scen <- "historical"
  #     }
  #     browser()
  #     scen <- ifelse(grepl("ssp", sim_grid$scenario), "ssp", "historical")
  #     met <- gcm_point_data_std_df |>
  #       dplyr::filter(
  #         gcm == sim_grid$gcm,
  #         scenario %in% sel_scen
  #       ) |> 
  #       dplyr::rename(
  #         Date = date,
  #         MET_humrel = hurs,
  #         MET_tmpair = tas,
  #         MET_pprain = pr,
  #         MET_radswd = rsds,
  #         MET_wndspd = sfcWind
  #       )
  #     met <- met |>
  #       dplyr::filter(
  #         Date >= as.Date(sim_periods[[scen]]$start) - (sim_periods[[scen]]$spin_up + 6) &
  #           Date <= as.Date(sim_periods[[scen]]$stop)
  #       )
  #     
  #     # AEME::expand_met(lat = lake_meta$latitude,
  #     #                  lon = lake_meta$longitude, elev = lake_meta$elevation)
  #     aeme <- aeme_base_hyps |>
  #       AEME::add_met(met = met) |>
  #       AEME::set_time(
  #         start = sim_periods[[scen]]$start,
  #         stop = sim_periods[[scen]]$stop,
  #         spin_up = sim_periods[[scen]]$spin_up
  #       ) |> 
  #       AEME::build_aeme(model = model, ext_elev = ext_elev,
  #                        use_bgc = FALSE, wb_method = 3, path  = path)
  #     aeme
  #     # AEME::run_aeme(aeme, model, path = path, verbose = T)
  #   },
  #   pattern = map(sim_grid),
  #   iteration = "list"
  # ),
  
  # 4. Scenario analysis ----
  
  # 5. Visualization ----
  
  #* Lake observations
  # tar_target(
  #   aeme_obs_plot, {
  #     
  #   }
  # )
  
  #* GCM Data
  # tar_target(
  #   gcm_spatial_plot, 
  #   {
  #     # browser()
  #     p <- plot_gcm_spatial(df = gcm_spatial_df, variable = cmip_vars, 
  #                           gcm = cmip_gcm, metadata = cmip6_metadata, 
  #                           x = lake_shape)
  #     out_file <- here::here("website", "www", "plots", paste0("gcm_spatial_", 
  #                                                              cmip_vars, "_", 
  #                                                              cmip_gcm, 
  #                                                              ".png"))
  #     ggsave(
  #       filename = out_file,
  #       plot = p,
  #       width = 10,
  #       height = 6
  #     )
  #     out_file
  #   },
  #   pattern = cross(cmip_vars, cmip_gcm),
  #   format = "file",
  #   cue = tar_cue(mode = "never")
  # ),
  tar_target(
    gcm_ts_plot, 
    {
      # browser()
      p <- plot_var_ts(gcm_ts_df = gcm_ts_df, variable = cmip_vars,
                       metadata = cmip6_metadata)
      out_file <- here::here("website", "www", "plots", paste0("gcm_ts_", 
                                                               cmip_vars, 
                                                               ".png"))
      ggsave(
        filename = out_file,
        plot = p,
        width = 10,
        height = 6
      )
      out_file
    },
    pattern = map(cmip_vars),
    format = "file"
  )
  
  
  # 6. Reporting / Quarto rendering
  
)
