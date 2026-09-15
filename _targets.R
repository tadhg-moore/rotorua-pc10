# _targets.R
# Targets pipeline for Rotorua Lake Modelling project
# Author: Tadhg Moore
# Last updated: 2025-11-03

# Load packages
library(targets)
library(tarchetypes)    # optional, for dynamic branching / commands
library(crew)
# Source R scripts (functions) ----
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
    here::here("R", "niwa_to_aeme_met.R"),
    here::here("R", "format_dyresm_inflow.R"),
    here::here("R", "render_data_inventory.R"),
    here::here("R", "read_alum_dosing.R"),
    here::here("R", "calc_alum_inflow.R") 
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
    tutira_bbox_coords, c(xmin = 176.75423, ymin = -39.29087, xmax = 177.00423, 
                          ymax = -39.15557)
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
  # From Chris McBride previous load modelling work
  tar_target(
    rotorua_inflow_file, here::here("data", "raw", "flows", 
                                    "Rotorua_inf_final.csv"),
    format = "file"
  ),
  tar_target(
    rotorua_inflow_id_file, here::here("data", "raw", "flows",
                                       "Rotorua_infID.csv"),
    format = "file"
  ),
  tar_target(
    rotorua_inflow_key, here::here("data", "raw", "flows",
                                   "rotorua_inflow_key.csv"),
    format = "file"
  ),
  tar_target(
    alum_dosing_file, here::here("data", "raw", "alum_dosing",
                                 "Rotorua alum dose data February 2026.xlsx"),
    format = "file"
  ),
  tar_target(
    bop_lake_level_zip_folder, here::here("data", "raw",
                                          "BulkExport-FL150407-20251215152116.zip"),
    format = "file"
  ),
  
  # CTD Excel file
  tar_target(
    ctd_excel_file, 
    here::here("data", "raw", "bop_wq",
               "Lake Rotorua CTD Profile Data - Full Record .xlsx"),
    format = "file"
  ),
  
  # Lake Chemistry data
  tar_target(
    chem_excel_file,
    here::here("data", "raw", "bop_wq", "Lakes Data 1989_2025.xlsx"), 
    format = "file"
  ),
  
  # Constants
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
    calib_period, list(start = "2016-07-01", stop = "2022-06-29", 
                       spin_up = (365 * 3))
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
    aed_models_alum, c("aed_sedflux", "aed_noncohesive", "aed_oxygen", "aed_silica", 
                       "aed_nitrogen", "aed_phosphorus", "aed_organic_matter",
                       "aed_phytoplankton", "aed_oasim", "aed_totals", 
                       "aed_alum")
  ),
  
  tar_target(
    inflows_alum_dosed, c("Puarenga", "Utuhina")
  ),
  
  tar_target(
    ratio_al_p, c(
      # eta	Al:P molar	Interpretation
      # 1.4, #	0.7:1	Rotorua jar best case — only defensible if you also simulate the pH depression via fpH
      1.0 #	1:1	central / Puarenga jar / operational reference
      # 0.5, #	2:1	mild field de-rating (Moawhitu-like)
      # 0.3, #	3.3:1	moderate competition + mixing losses
      # 0.15, #	~7:1	full field losses — McDowell & Hawke operational figure
    )
  ),
  
  tar_target(
    path, "."
  ),
  tar_target(
    glm4_path, "glm4"
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
  
  # 1. Load data ----
  #* Load alum data ----
  tar_target(
    alum_dosing, read_alum_dosing(alum_dosing_file)
  ),
  
  #* Unzip lake level folder and load data
  tar_target(
    lake_level, extract_lake_level(bop_lake_level_zip_folder)
  ),
  tar_target(
    median_lake_level_masl, median(lake_level$Lake_Level_m)
  ),
  
  #* Rotorua chemistry data
  tar_target(
    chem_sites, {
      readxl::read_excel(chem_excel_file, sheet = "Sites") |> 
        dplyr::filter(grepl("Rotorua", LocationName))
    }
  ),
  tar_target(
    chem_sites_sf, {
      chem_sites |> 
        dplyr::distinct(Easting, Northing, .keep_all = TRUE) |> 
        sf::st_as_sf(coords = c("Easting", "Northing"), crs = 2193)
    }
  ),
  tar_target(
    chem_data, {
      readxl::read_excel(chem_excel_file, sheet = "All_data") |> 
        dplyr::filter(AquariusID %in% chem_sites$Site)
    }
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
    depth_contours, 
    aemetools::get_depth_contours(id = lake_id, 
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
    rotorua_buoy_met_aeme_dly, buoy_to_aeme_met(rotorua_buoy_met_data)
  ),
  tar_target(
    rotorua_buoy_met_aeme_hr, buoy_to_aeme_met(rotorua_buoy_met_data, unit = "hour")
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
  tar_target(
    ctd_par, format_par_ts(ctd_data)
  ),
  tar_target(
    rotorua_inflow_list, {
      format_dyresm_inflow(rotorua_inflow_file, rotorua_inflow_id_file,
                           rotorua_inflow_key)
      
    }
  ),
  
  tar_target(
    rotorua_inflow_list_matrix, {
      # Add ALU_ala and ALU_alp to inflows, all 0 except for dosed streams
      # browser()
      flows <- lapply(names(rotorua_inflow_list), \(nm) {
        if (nm %in% inflows_alum_dosed) {
          sel_alum_dosing <- alum_dosing |> 
            dplyr::select(dplyr::contains(c("Date", nm))) |> 
            # rename to "alum"
            dplyr::rename(
              alum = dplyr::contains(nm)
            )
          # chk <- unique(diff(rotorua_inflows[[nm]][["Date"]]))
          df <- rotorua_inflow_list[[nm]] |> 
            dplyr::left_join(sel_alum_dosing, by = "Date") |> 
            dplyr::mutate(
              alum = dplyr::if_else(is.na(alum), 0, alum),
              alum = dplyr::if_else(alum < 0, 0, alum)
            ) |> 
            dplyr::mutate(
              ALU_ala = calc_alum_inflow(alum_vol_L_day = alum, 
                                         flow_m3s = (HYD_flow / 86400)) * ratio_al_p,
              ALU_alp = 0
            ) |> 
            dplyr::select(-alum)
          
        } else {
          df <- rotorua_inflow_list[[nm]] |> 
            dplyr::mutate(
              ALU_ala = 0,
              ALU_alp = 0
            )
        }
        df |> 
          dplyr::mutate(Date = as.Date(Date)) 
      })
      
      names(flows) <- names(rotorua_inflow_list)
      return(flows)
    }, 
    pattern = map(ratio_al_p)
  ),
  
  #* Data inventory ----
  tar_target(
    data_inventory_html, {
      objs <- list(
        # ── Observations ──────────────────────────────────────────────────
        lake_level             = lake_level,
        rotorua_inflow         = rotorua_inflow,
        rotorua_inflow_list    = rotorua_inflow_list,
        alum_dosing            = alum_dosing,
        rotorua_buoy_pro_data  = rotorua_buoy_pro_data,
        rotorua_buoy_met_data  = rotorua_buoy_met_data,
        ctd_data               = ctd_data,
        ctd_par                = ctd_par,
        chem_data              = chem_data,
        chem_sites             = chem_sites,
        light_data             = light_data,

        # ── Climate ───────────────────────────────────────────────────────
        niwa_met_daily         = niwa_met_daily,
        niwa_met_hourly_files  = niwa_met_hourly_files,  # character vector of paths

        # ── GCM / CMIP6 ───────────────────────────────────────────────────
        cmip6_metadata         = cmip6_metadata,
        gcm_point_data_df      = gcm_point_data_df,
        gcm_point_data_std_df  = gcm_point_data_std_df,
        gcm_ts_df              = gcm_ts_df,

        # ── Spatial ───────────────────────────────────────────────────────
        lake_shape             = lake_shape,
        lake_meta              = lake_meta,
        depth_contours         = depth_contours,
        rotorua_catchment_bbox = rotorua_catchment_bbox,
        tutira_catchment_bbox  = tutira_catchment_bbox,
        vcsn_grid_points       = vcsn_grid_points,

        # ── Model config ──────────────────────────────────────────────────
        aeme_base_hyps         = aeme_base_hyps,
        corr_hyps              = corr_hyps,
        glm_sed_param          = glm_sed_param,
        glm_sed_param_meas     = glm_sed_param_meas,
        meas_param             = meas_param,
        aed_alum_params        = aed_alum_params
      )
      
      inv <- build_inventory(objs, annotations = inventory_annotations())
      
      render_data_inventory(
        inventory_df = inv,
        out_file     = here::here("website", "www", "data_inventory.html"),
        project      = "Rotorua Lake Modelling"
      )
    },
    format = "file"
  ),
  
  
  
  # 2. Preprocess / clean data ----
  # Subset lake level data
  tar_target(
    sub_lake_level, {
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
  
  tar_target(
    rotorua_inflow, {
      dplyr::bind_rows(rotorua_inflow_list, .id = "name")
    }
  ),
  # tar_target(
  #   rotorua_aeme_inflow, convert_dyresm_to_aeme(rotorua_inflow)
  # ),
  
  # Fix Rotorua hypsograph in aeme
  tar_target(
    corr_hyps, fix_hyps(aeme_base, lake_elev = median_lake_level_masl)
  ),
  tar_target(
    corr_hyps_file, {
      out_file <- here::here("data", "processed", "rotorua_data_sh",
                             "rotorua_hypsograph.csv")
      readr::write_csv(corr_hyps, file = out_file)
      out_file
    },
    format = "file"
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
  tar_target(
    zone_heights, c(23.65, 30.15, 35.15, 39.65, 54)
  ),
  tar_target(
    glm_sed_param_meas, AEME::glm_sed_params(n_zones = length(zone_heights),
                                             zone_heights = zone_heights) |> 
      dplyr::filter(!grepl("sed_temp|n_zones", name)) |> 
      dplyr::mutate(
        file = dplyr::case_when(
          file == "glm3.nml" ~ "glm4.nml",
          .default = file
        )
      )
  ),
  tar_target(
    meas_param, {
      tibble::tribble(
        ~model,    ~file,      ~name,                          ~value,  ~min,    ~max,   ~group,        ~index,
        # Sediment nutrient/oxygen flux values below: Kaylee Campbell internal loading
        # study (repeat of Bergen 2004) -- confirm citation before publishing.
        # --- AED 2-D sediment fluxes: &aed_sed_const2d  (zone 1 = deepest, 24-47.65 m pit) ---
        "glm_aed", "aed.nml",  "aed_sed_const2d/fsed_oxy",       -5.8,  -12,     -2,     NA_character_,  1L,
        "glm_aed", "aed.nml",  "aed_sed_const2d/fsed_oxy",       -9.6,  -18,     -5,     NA_character_,  2L,
        "glm_aed", "aed.nml",  "aed_sed_const2d/fsed_oxy",      -15.3,  -24,     -9,     NA_character_,  3L,
        "glm_aed", "aed.nml",  "aed_sed_const2d/fsed_oxy",      -26.9,  -45,    -14,     NA_character_,  4L,
        "glm_aed", "aed.nml",  "aed_sed_const2d/fsed_oxy",      -16.1,  -32,     -5,     NA_character_,  5L,
        
        "glm_aed", "aed.nml",  "aed_sed_const2d/fsed_amm",       0.011,  0.004,   0.05,  NA_character_,  1L,
        "glm_aed", "aed.nml",  "aed_sed_const2d/fsed_amm",       0.018,  0.007,   0.05,  NA_character_,  2L,
        "glm_aed", "aed.nml",  "aed_sed_const2d/fsed_amm",       0.029,  0.012,   0.08,  NA_character_,  3L,
        "glm_aed", "aed.nml",  "aed_sed_const2d/fsed_amm",       0.039,  0.015,   0.12,  NA_character_,  4L,
        "glm_aed", "aed.nml",  "aed_sed_const2d/fsed_amm",       0.023,  0.009,   0.07,  NA_character_,  5L,
        
        "glm_aed", "aed.nml",  "aed_sed_const2d/fsed_nit",       0,     -0.05,    0.05,  NA_character_,  1L,
        "glm_aed", "aed.nml",  "aed_sed_const2d/fsed_nit",       0,     -0.05,    0.05,  NA_character_,  2L,
        "glm_aed", "aed.nml",  "aed_sed_const2d/fsed_nit",       0,     -0.05,    0.05,  NA_character_,  3L,
        "glm_aed", "aed.nml",  "aed_sed_const2d/fsed_nit",       0,     -0.05,    0.05,  NA_character_,  4L,
        "glm_aed", "aed.nml",  "aed_sed_const2d/fsed_nit",       0,     -0.05,    0.05,  NA_character_,  5L,
        
        "glm_aed", "aed.nml",  "aed_sed_const2d/fsed_frp",       0.0014, 0.0005,  0.02,  NA_character_,  1L,
        "glm_aed", "aed.nml",  "aed_sed_const2d/fsed_frp",       0.0024, 0.001,   0.008, NA_character_,  2L,
        "glm_aed", "aed.nml",  "aed_sed_const2d/fsed_frp",       0.0038, 0.0015,  0.012, NA_character_,  3L,
        "glm_aed", "aed.nml",  "aed_sed_const2d/fsed_frp",       0.0093, 0.004,   0.03,  NA_character_,  4L,
        "glm_aed", "aed.nml",  "aed_sed_const2d/fsed_frp",       0.0056, 0.002,   0.02,  NA_character_,  5L,
        
        # --- GLM sediment heat model: &sediment  (replace *_mean/amplitude/peak_doy with calc_sed_temp() output) ---
        "glm_aed", "glm4.nml", "sediment/n_zones",        5,   5,    5,  NA_character_,  1L,
        "glm_aed", "glm4.nml", "sediment/sed_temp_mean",        14.6,   13.0,    16.5,  NA_character_,  1L,
        "glm_aed", "glm4.nml", "sediment/sed_temp_mean",        15.0,   13.5,    16.5,  NA_character_,  2L,
        "glm_aed", "glm4.nml", "sediment/sed_temp_mean",        15.4,   14.0,    17.0,  NA_character_,  3L,
        "glm_aed", "glm4.nml", "sediment/sed_temp_mean",        15.7,   14.0,    17.5,  NA_character_,  4L,
        "glm_aed", "glm4.nml", "sediment/sed_temp_mean",        16.0,   14.5,    17.5,  NA_character_,  5L,
        
        "glm_aed", "glm4.nml", "sediment/sed_temp_amplitude",    2.5,    1.0,     4.5,  NA_character_,  1L,
        "glm_aed", "glm4.nml", "sediment/sed_temp_amplitude",    3.3,    1.5,     5.0,  NA_character_,  2L,
        "glm_aed", "glm4.nml", "sediment/sed_temp_amplitude",    3.9,    2.0,     5.5,  NA_character_,  3L,
        "glm_aed", "glm4.nml", "sediment/sed_temp_amplitude",    4.3,    2.5,     6.0,  NA_character_,  4L,
        "glm_aed", "glm4.nml", "sediment/sed_temp_amplitude",    4.6,    2.5,     6.5,  NA_character_,  5L,
        
        "glm_aed", "glm4.nml", "sediment/sed_temp_peak_doy",    90,     55,     140,    NA_character_,  1L,
        "glm_aed", "glm4.nml", "sediment/sed_temp_peak_doy",    62,     40,      95,    NA_character_,  2L,
        "glm_aed", "glm4.nml", "sediment/sed_temp_peak_doy",    52,     35,      75,    NA_character_,  3L,
        "glm_aed", "glm4.nml", "sediment/sed_temp_peak_doy",    48,     30,      70,    NA_character_,  4L,
        "glm_aed", "glm4.nml", "sediment/sed_temp_peak_doy",    46,     30,      65,    NA_character_,  5L,
        
        "glm_aed", "glm4.nml", "sediment/sed_vwc",               0.85,   0.60,    0.95, NA_character_,  1L,
        "glm_aed", "glm4.nml", "sediment/sed_vwc",               0.85,   0.60,    0.95, NA_character_,  2L,
        "glm_aed", "glm4.nml", "sediment/sed_vwc",               0.85,   0.55,    0.95, NA_character_,  3L,
        "glm_aed", "glm4.nml", "sediment/sed_vwc",               0.60,   0.40,    0.90, NA_character_,  4L,
        "glm_aed", "glm4.nml", "sediment/sed_vwc",               0.40,   0.30,    0.70, NA_character_,  5L,
        
        # --- AED scalar sediment modifiers (not zoned) ---
        "glm_aed", "aed.nml",  "aed_oxygen/Ksed_oxy",           25,     10,      50,    NA_character_,  NA_integer_,
        "glm_aed", "aed.nml",  "aed_oxygen/theta_sed_oxy",       1.06,   1.04,    1.10, NA_character_,  NA_integer_,
        "glm_aed", "aed.nml",  "aed_phosphorus/Ksed_frp",       45,     20,      80,    NA_character_,  NA_integer_,
        "glm_aed", "aed.nml",  "aed_phosphorus/theta_sed_frp",   1.06,   1.03,    1.10, NA_character_,  NA_integer_,
        "glm_aed", "aed.nml",  "aed_nitrogen/Ksed_amm",         30,     10,      60,    NA_character_,  NA_integer_,
        "glm_aed", "aed.nml",  "aed_nitrogen/theta_sed_amm",     1.08,   1.05,    1.10, NA_character_,  NA_integer_,
        "glm_aed", "aed.nml",  "aed_nitrogen/theta_sed_nit",     1.08,   1.05,    1.12, NA_character_,  NA_integer_
      )
    }
  ),
  
  tar_target(
    aed_alum_params, {
      # AED alum parameters for Lake Rotorua
      # Sources: Sanila (2026) MSc thesis, University of Waikato
      # Notes:
      #   - w_ala: from video settling measurements (89 flocs); 0.5 m/d is conservative
      #     central estimate; range 0.3-1.5 m/d defensible from data
      #   - k_w: binding appeared complete within ~20 min in jar tests; no explicit
      #     rate constant fitted — value retained from default, flagged as uncertain
      #   - K_P: not directly measured; default retained; not contradicted by data
      #   - S_ala: derived from mean operational dose (158.1 kg Al/day) converted via
      #     0.7:1 Al:P molar ratio, distributed over lake surface (80.6 km2)
      #   - pH_opt: raised from default 6.0 to 7.0 to reflect open-lake ambient pH
      #   - simResuspension: thesis strongly supports enabling; k_res not quantified
      #   - k_age_w/s, k_bur: not constrained by thesis; defaults retained
      
      tibble::tribble(
        ~model,    ~file,       ~name,                          ~value,  ~min,   ~max,   ~group,   ~index,
        "glm_aed", "aed.nml",  "aed_alum/w_ala",               0.5,     0.3,    1.5,    NA_character_,   NA_integer_,
        "glm_aed", "aed.nml",  "aed_alum/k_w",                 0.5,     0.2,    1.0,    NA_character_,   NA_integer_,
        "glm_aed", "aed.nml",  "aed_alum/k_s",                 0.1,     0.05,   0.3,    NA_character_,   NA_integer_,
        "glm_aed", "aed.nml",  "aed_alum/K_P",                 0.3,     0.1,    0.6,    NA_character_,   NA_integer_,
        "glm_aed", "aed.nml",  "aed_alum/k_age_w",             0.0998,  0.05,   0.2,    NA_character_,   NA_integer_,
        "glm_aed", "aed.nml",  "aed_alum/k_age_s",             0.0998,  0.05,   0.2,    NA_character_,   NA_integer_,
        "glm_aed", "aed.nml",  "aed_alum/k_bur",               0.01,    0.005,  0.05,   NA_character_,   NA_integer_,
        "glm_aed", "aed.nml",  "aed_alum/k_res",               0.0,     0.0,    0.1,    NA_character_,   NA_integer_,
        "glm_aed", "aed.nml",  "aed_alum/S_ala",               0.00,    0.0,   0.0,   NA_character_,   NA_integer_,
        "glm_aed", "aed.nml",  "aed_alum/pH_opt",              7.0,     6.0,    8.0,    NA_character_,   NA_integer_,
        "glm_aed", "aed.nml",  "aed_alum/sigma_pH",            1.0,     0.5,    2.0,    NA_character_,   NA_integer_
      )
      
      # Confidence notes (not part of AEME format — for reference only):
      # w_ala    : Good       — directly measured from 89 flocs via video settling
      # k_w      : Uncertain  — qualitative support only; no rate constant fitted
      # k_s      : Uncertain  — not measured; default retained
      # K_P      : Low        — not measured; default not contradicted
      # k_age_w  : Unknown    — not constrained by thesis
      # k_age_s  : Unknown    — not constrained by thesis
      # k_bur    : Unknown    — not constrained by thesis
      # k_res    : Unknown    — resuspension confirmed qualitatively; rate not measured
      # S_ala    : Derived    — from operational dosing records + 0.7:1 Al:P molar ratio
      # pH_opt   : Moderate   — raised from default 6.0 to reflect open-lake ambient pH
      # sigma_pH : Moderate   — consistent with observed ±1-2 unit swings during dosing
      
    }
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
  tar_target(
    tutira_cmip6_files, {
      process_cmip6_shp(
        x = tutira_catchment_bbox,
        vcsn_grid_points = vcsn_grid_points,
        metadata = sub_metadata,
        out_dir = here::here("data", "processed", "tutira_cmip6"),
        overwrite = FALSE
      )
    },
    pattern = map(sub_metadata),
    format = "file",
    # deployment = "main"
    cue = tar_cue(mode = "never")
  ),
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
  
  # Light data
  tar_target(
    light_data, {
      chla_data <- chem_data |> 
        dplyr::select(AquariusID, Date, DepthFrom, DepthTo, `CHLA (mg/m^3)`, `Turbidity (NTU)`, `Secchi (m)`) |> 
        dplyr::rename(chla = `CHLA (mg/m^3)`, turbidity = `Turbidity (NTU)`, secchi = `Secchi (m)`)
      
      ctd_par |> 
        dplyr::left_join(chla_data, by = "Date") |> 
        dplyr::group_by(Date) |>
        dplyr::summarise(
          par_n = dplyr::first(n),
          Kd = mean(Kd, na.rm = TRUE),
          chla = mean(chla, na.rm = TRUE),
          secchi = mean(secchi, na.rm = TRUE),
          turbidity = mean(turbidity, na.rm = TRUE)
        ) |> 
        dplyr::mutate(
          Kd_est = 1.7 / secchi
        )
    }
  ),
  
  tar_target(
    light_data_file, {
      file <- here::here("data", "processed", "rotorua_light_data.csv")
      readr::write_csv(light_data, file = file)
      file
    }, 
    format = "file"
  ),
  
  # 3. Model execution ----
  tar_target(
    aeme_glm4, {
      
      params <- dplyr::bind_rows(glm_sed_param_meas, meas_param, 
                                 aed_alum_params)
      aeme <- aeme_base |> 
        AEME::remove_inflow(all = TRUE) |> 
        AEME::add_inflows(data = rotorua_inflow_list_matrix) |>
        AEME::add_obs(lake = aeme_buoy_data, level = sub_lake_level) |> 
        AEME::add_param(params) |>
        AEME::set_time(start = calib_period$start, stop = calib_period$stop,
                      spin_up = calib_period$spin_up) |> 
      # aeme_time <- AEME::time(aeme)
      
        AEME::build_aeme(path = glm4_path, model = model, ext_elev = ext_elev,
                         use_bgc = TRUE)
      # aeme <- AEME::run_aeme(aeme, verbose = TRUE)
      # AEME::plot_output(aeme)
      return(aeme)
    },
    pattern = map(rotorua_inflow_list_matrix)
  ),
  
  # tar_target(
  #   baseline_aeme, {
  #     # inp <- AEME::input(aeme_base_hyps)
  #     lake_data <- ctd_data |> 
  #       dplyr::bind_rows(aeme_buoy_data) 
  #     aeme <- aeme_base_hyps |>
  #       AEME::add_param(glm_sed_param) |>
  #       AEME::add_obs(lake = lake_data, level = sub_lake_level) |>
  #       AEME::remove_inflow(all = TRUE) |>
  #       AEME::reset_wbal_param() |> 
  #       AEME::set_precip(type = "precip_as_inflow") |> 
  #       AEME::add_param(param = glm_sed_param) |> 
  #       AEME::set_time(start = "2021-07-01", stop = "2023-06-30",
  #                      spin_up = 2*365) |> 
  #       AEME::build_aeme(model = model, ext_elev = ext_elev, wb_method = 2,
  #                        use_bgc = TRUE, path = path)
  #     
  #     AEME::plot_weir_calibration(aeme)
  #     AEME::plot_est_wbal(aeme)
  #     # wb_comp <- AEME::get_wbal_components(aeme)
  #     # AEME::plot_wbal_comp(wb_comp)
  #     
  #     # AEME::plot_wbal(aeme)
  #     browser()
  #     
  #     AEME::plot_glm_config(aeme)
  #     aeme <- aeme |> 
  #       AEME::set_glm_aed_models(aed_models = aed_models) |> 
  #       # AEME::set_aed_totals() |> 
  #       AEME::set_aed_sed_const2d(path = path) 
  #     
  #     aeme <- aeme |> 
  #       AEME::run_aeme(verbose = TRUE)
  #     
  #     diag <- AEME::plot_glm_diagnostics(aeme)
  #     diag$oxy
  #     diag$physical
  #     
  #     aeme |> 
  #       AEME::plot_output(var_sim = "CHM_oxy", add_obs = FALSE, remove_spin_up = FALSE)
  #     
  #     # AEME::plot_wbal_annual(aeme, lake_frac = T)
  #     AEME::plot_wlev(aeme, remove_spin_up = F)
  #     return(aeme)
  #     aeme |> 
  #       # AEME::add_obs(level = sub_lake_level) |> 
  #       AEME::plot_wlev()
  #     AEME::plot_wbal(aeme, cumulative = T)
  #     AEME::plot_output(aeme, remove_spin_up = F)
  #     AEME::plot_output(aeme, var_sim = "CHM_oxy", add_obs = FALSE)
  #     AEME::plot_output(aeme, var_sim = "PHY_tchla")
  #     AEME::plot_output(aeme, var_sim = "PHS_tp")
  #     AEME::plot_output(aeme, var_sim = "NIT_tn")
  #   }
  # ),
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
