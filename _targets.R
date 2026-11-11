# _targets.R
# Targets pipeline for Rotorua Lake Modelling project
# Author: Tadhg Moore
# Last updated: 2025-11-03

# Load packages
library(targets)
library(tarchetypes)    # optional, for dynamic branching / commands
library(crew)

# download.file(
#   "https://raw.githubusercontent.com/limnotrack/f_rotorua/main/R/qc_funs.R",
#   destfile = "R/qc_funs.R",
#   mode = "wb"
# )
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
    here::here("R", "extract_pc10_data.R"),
    here::here("R", "compare_met_sources.R"),
    here::here("R", "read_ctd.R"),
    here::here("R", "standardise_to_gregorian.R"),
    here::here("R", "estimate_sed_zones.R"),
    here::here("R", "buoy_to_aeme_met.R"),
    here::here("R", "read_niwa_climate_file.R"),
    here::here("R", "niwa_to_aeme_met.R"),
    here::here("R", "niwa_hourly_to_aeme_met.R"),
    here::here("R", "format_dyresm_inflow.R"),
    here::here("R", "read_pc10_loads.R"),
    here::here("R", "pc10_loads_helpers.R"),
    here::here("R", "render_data_inventory.R"),
    here::here("R", "read_alum_dosing.R"),
    here::here("R", "calc_alum_inflow.R"),
    here::here("R", "sync_aeme_onedrive.R"),
    here::here("R", "scenario_climate_workflow.R"),
    here::here("R", "plot_gcm_delta_variability.R"),
    here::here("R", "summarise_climate_extremes.R"),
    here::here("R", "plot_climate_extremes.R"),
    here::here("R", "plot_gcm_transient_deltas.R"),
    here::here("R", "plot_bias_correction.R"),
    here::here("R", "qc_funs.R"),
    here::here("R", "load_old_buoy_data.R"),
    here::here("R", "read_met_service_xl.R"),
    here::here("R", "make_met_data_zip.R"),
    here::here("R", "read_bop_csv_url.R"),
    here::here("R", "read_bop_daily_flow.R"),
    here::here("R", "enso_helpers.R"),
    here::here("R", "read_bop_lab_csv_url.R"),
    here::here("R", "est_glm_weir_params.R"),
    here::here("R", "build_calib_base_aeme.R"),
    here::here("R", "read_chem_data.R"),
    here::here("R", "read_electro_fishing.R"),
    here::here("R", "download_arcgis_layer.R"),
    here::here("R", "read_river_wq.R"),
    here::here("R", "wq_synthesis_helpers.R")
  )
)
# Set target options
cores <- parallel::detectCores(logical = FALSE) - 1
workers <- pmin(cores, 5)
tar_option_set(
  controller = crew_controller_local(workers = workers), 
  error = "continue", 
  storage = "worker", 
  retrieval = "worker",
  packages = c("dplyr", "ggplot2", "AEME", "metscale"), # packages needed in your functions
  format = "rds" # default storage format
)

# Pipeline definition
list(
  
  # -1. Sync data from OneDrive ----
  # Freshness lives on OneDrive, not in any locally-hashable input, so these
  # always re-run and their results are re-hashed each tar_make().
  tar_target(
    data_raw_dir,
    sync_onedrive_folder("rotorua-pc10/data/raw", here::here("data", "raw")),
    format = "file",
    # Always re-sync in CI (a fresh checkout has none of this on disk, and
    # a restored _targets cache can't be trusted alone — see comment on
    # aeme_onedrive_rds below); locally, sync once then trust the cache so
    # repeat renders don't re-download every time.
    cue = if (identical(Sys.getenv("CI"), "true")) tar_cue(mode = "always") else tar_cue(mode = "never"),
    deployment = "main"
  ),
  tar_target(
    aeme_onedrive_rds,
    sync_onedrive_file("rotorua-pc10/LID11133_rotorua/aeme.rds",
                       here::here("LID11133_rotorua", "aeme_download.rds")),
    format = "file",
    # Always in CI: a restored _targets cache only tracks the file's
    # hash/path, not its content — on a fresh checkout the actual file
    # isn't there yet even if the cache says it's "already downloaded",
    # so CI must always re-fetch. Locally, sync once then trust the cache.
    cue = if (identical(Sys.getenv("CI"), "true")) tar_cue(mode = "always") else tar_cue(mode = "never"),
    deployment = "main"
  ),
  tar_target(
    aeme_onedrive_lake_rotorua_dir,
    sync_onedrive_folder("rotorua-pc10/LakeRotorua", here::here("website", "LakeRotorua")),
    format = "file",
    cue = if (identical(Sys.getenv("CI"), "true")) tar_cue(mode = "always") else tar_cue(mode = "never"),
    deployment = "main"
  ),
  tar_target(
    aeme_onedrive_bin_dir,
    sync_onedrive_folder("rotorua-pc10/bin", here::here("website", "bin")),
    format = "file",
    cue = if (identical(Sys.getenv("CI"), "true")) tar_cue(mode = "always") else tar_cue(mode = "never"),
    deployment = "main"
  ),
  # Report figures/tables made by the local-only calibration, PEST and ensemble
  # runs (calibration/07_export_report_data.R etc.). Too large to keep in git,
  # so they live on OneDrive (push with website/scripts/push_assets.R).
  tar_target(
    website_assets_images_dir,
    sync_onedrive_folder("rotorua-pc10/website_assets/images", here::here("website", "images")),
    format = "file",
    cue = if (identical(Sys.getenv("CI"), "true")) tar_cue(mode = "always") else tar_cue(mode = "never"),
    deployment = "main"
  ),
  tar_target(
    website_assets_data_dir,
    sync_onedrive_folder("rotorua-pc10/website_assets/data", here::here("website", "data")),
    format = "file",
    cue = if (identical(Sys.getenv("CI"), "true")) tar_cue(mode = "always") else tar_cue(mode = "never"),
    deployment = "main"
  ),
  tar_target(
    aeme_onedrive_r_dir,
    sync_onedrive_folder("rotorua-pc10/R", here::here("website", "R")),
    format = "file",
    cue = if (identical(Sys.getenv("CI"), "true")) tar_cue(mode = "always") else tar_cue(mode = "never"),
    deployment = "main"
  ),
  
  # BOPRC Rotorua nutrient-management catchment polygons (ArcGIS Hub) ----
  tar_target(
    boprc_lake_catchments_url,
    "https://services1.arcgis.com/TWsoAGve3wb3oFHK/arcgis/rest/services/Rotorua_Lakes_Catchment___Nutrient_Management/FeatureServer"
  ),
  tar_target(
    boprc_lake_catchments_file,
    download_arcgis_layer(
      boprc_lake_catchments_url,
      here::here("data", "processed", "boprc_gis", "rotorua_lakes_surface_water_catchments.geojson")
    ),
    format = "file"
  ),
  tar_target(
    boprc_lake_catchments,
    sf::read_sf(boprc_lake_catchments_file)
  ),
  tar_target(
    boprc_gw_subcatchments_url,
    "https://services1.arcgis.com/TWsoAGve3wb3oFHK/arcgis/rest/services/GNS___Groundwater_Sub_catchments_Rotorua/FeatureServer"
  ),
  tar_target(
    boprc_gw_subcatchments_file,
    download_arcgis_layer(
      boprc_gw_subcatchments_url,
      here::here("data", "processed", "boprc_gis", "rotorua_groundwater_subcatchments.geojson")
    ),
    format = "file"
  ),
  tar_target(
    boprc_gw_subcatchments,
    sf::read_sf(boprc_gw_subcatchments_file)
  ),

  # Puarenga and Utuhina stream centrelines (NZ River Centrelines, Topo 1:50k) ----
  # Shapefile: all component files are tracked so edits to any of them re-run the readers.
  tar_target(
    stream_centrelines_files,
    list.files(
      file.path(data_raw_dir, "Puarenga and Utuhina streams"),
      pattern = "^Puarenga and Utuhina[.](shp|shx|dbf|prj|cpg|sbn|sbx)$",
      full.names = TRUE
    ),
    format = "file"
  ),
  tar_target(
    stream_centrelines, {
      shp <- grep("[.]shp$", stream_centrelines_files, value = TRUE)
      x <- sf::st_cast(sf::read_sf(shp), "LINESTRING")
      x <- sf::st_transform(sf::st_zm(x), 4326)   # drop the Z (KML export) dimension
      # The file has no stream names: label each segment by the surface-water
      # sub-catchment it lies in (all 34 segments fall wholly in Puarenga or Utuhina).
      sub <- boprc_lake_catchments
      sub$SubCatchment <- trimws(sub$SubCatchment)
      sub <- sub[sub$SubCatchment %in% c("Puarenga", "Utuhina"), "SubCatchment"]
      s2 <- sf::sf_use_s2(FALSE); on.exit(sf::sf_use_s2(s2), add = TRUE)
      idx <- suppressMessages(sf::st_intersects(x, sub))
      x$stream <- vapply(idx, function(i) if (length(i)) sub$SubCatchment[i[1]] else NA_character_, "")
      x[!is.na(x$stream), c("stream", "geometry")]
    }
  ),

  tar_target(
    rotorua_old_buoy_data_url,"https://github.com/limnotrack/f_rotorua/releases/download/v0.0.1/rotorua_data_qc.zip"
  ),
  tar_target(
    rotorua_old_buoy_data_zip, {
      dest <- here::here("data", "processed")
      download.file(rotorua_old_buoy_data_url, destfile = file.path(dest, "rotorua_data_qc.zip"))
      file.path(dest, "rotorua_data_qc.zip")
    },
    format = "file"
  ),
  tar_target(
    rotorua_old_buoy_data, load_old_buoy_data(rotorua_old_buoy_data_zip)
  ),
  tar_target(old_buoy_met_hr_aeme, old_buoy_met_data(rotorua_old_buoy_data, 1.5)),
  tar_target(old_buoy_wq, old_buoy_wq_data(rotorua_old_buoy_data)),
  
  # level data
  tar_target(
    rotorua_level_data_url, "https://envdata.boprc.govt.nz/Export/BulkExport?DateRange=EntirePeriodOfRecord&TimeZone=12&Calendar=CALENDARYEAR&Interval=PointsAsRecorded&Step=1&ExportFormat=csv&TimeAligned=True&RoundData=False&IncludeGradeCodes=True&IncludeApprovalLevels=True&IncludeQualifiers=True&IncludeInterpolationTypes=False&IncludeNotes=undefined&Datasets[0].DatasetName=Lake%20Level.Moturiki%40FL150407&Datasets[0].Calculation=Instantaneous&Datasets[0].UnitId=122&_=1791861502718"
  ),
  tar_target(
    rotorua_level_data, read_bop_csv_url(rotorua_level_data_url)
  ),
  
  tar_target(
    rotorua_flow_data_url, "https://envdata.boprc.govt.nz/Export/BulkExport?DateRange=EntirePeriodOfRecord&TimeZone=12&Calendar=CALENDARYEAR&Interval=PointsAsRecorded&Step=1&ExportFormat=csv&TimeAligned=True&RoundData=False&IncludeGradeCodes=True&IncludeApprovalLevels=True&IncludeQualifiers=True&IncludeInterpolationTypes=False&IncludeNotes=undefined&Datasets[0].DatasetName=Discharge.Primary%40FL150407&Datasets[0].Calculation=Instantaneous&Datasets[0].UnitId=141&_=1791861409538"
  ),
  tar_target(
    rotorua_flow_data, read_bop_csv_url(rotorua_flow_data_url)
  ),

  # Ohau Channel at SH33 (FL230406) lab results: NH4-N, TN, DRP, NNN, TP, TSS and water temperature
  tar_target(
    ohau_wq_url, "https://envdata.boprc.govt.nz/Export/BulkExport?DateRange=EntirePeriodOfRecord&TimeZone=12&Calendar=CALENDARYEAR&Interval=PointsAsRecorded&Step=1&ExportFormat=csv&TimeAligned=True&RoundData=False&IncludeGradeCodes=True&IncludeApprovalLevels=True&IncludeQualifiers=True&IncludeInterpolationTypes=False&IncludeNotes=undefined&Datasets[0].DatasetName=NH4-N.LabResult%40FL230406&Datasets[0].Calculation=Instantaneous&Datasets[0].UnitId=71&Datasets[1].DatasetName=TN.LabResult%40FL230406&Datasets[1].Calculation=Instantaneous&Datasets[1].UnitId=71&Datasets[2].DatasetName=DRP.LabResult%40FL230406&Datasets[2].Calculation=Instantaneous&Datasets[2].UnitId=71&Datasets[3].DatasetName=NNN.LabResult%40FL230406&Datasets[3].Calculation=Instantaneous&Datasets[3].UnitId=71&Datasets[4].DatasetName=TP.LabResult%40FL230406&Datasets[4].Calculation=Instantaneous&Datasets[4].UnitId=71&Datasets[5].DatasetName=TSS.LabResult%40FL230406&Datasets[5].Calculation=Instantaneous&Datasets[5].UnitId=71&Datasets[6].DatasetName=Water%20Temp.LabResult%40FL230406&Datasets[6].Calculation=Instantaneous&Datasets[6].UnitId=242&_=1793315629865"
  ),
  tar_target(
    ohau_wq_data, read_bop_lab_csv_url(ohau_wq_url)
  ),
  # Site location from the BOPRC WebPortal location summary page
  # (https://envdata.boprc.govt.nz/Data/Location/Summary/Location/FL230406/Interval/Latest)
  # Lat/lon (WGS84) -38.04399712, 176.33084006; NZTM 1892304 E, 5784064 N; elevation 280 m (as listed on the portal)
  tar_target(
    ohau_site_sf,
    sf::st_as_sf(
      data.frame(site_code = "FL230406", site_name = "Ohau Channel at SH33",
                 lon = 176.33084006, lat = -38.04399712),
      coords = c("lon", "lat"), crs = 4326
    )
  ),
  
  # Ohau Channel weir (lake outflow control structure), WGS84
  tar_target(
    ohau_weir_sf,
    sf::st_as_sf(
      data.frame(site_name = "Ohau Channel weir",
                 lon = 176.323916, lat = -38.046224),
      coords = c("lon", "lat"), crs = 4326
    )
  ),

  tar_target(
    ohau_flow_level, {
      dplyr::left_join(rotorua_flow_data, rotorua_level_data,
                       by = c("Timestamp (UTC+12:00)")) |>
        dplyr::rename(
          Date = `Timestamp (UTC+12:00)`,
          level = `Value (m)`,
          flow = `Value (m^3/s)`
        ) |>  
        dplyr::select(Date, level, flow)
    }
  ),
  # Measured inflow discharge (BOPRC Aquarius, entire period of record) for the ENSO analysis.
  # Instantaneous exports are large, so only the daily-mean summary is kept.
  tar_target(ngongotaha_flow_url, "https://envdata.boprc.govt.nz/Export/BulkExport?DateRange=EntirePeriodOfRecord&TimeZone=12&Calendar=CALENDARYEAR&Interval=PointsAsRecorded&Step=1&ExportFormat=csv&TimeAligned=True&RoundData=False&IncludeGradeCodes=True&IncludeApprovalLevels=True&IncludeQualifiers=True&IncludeInterpolationTypes=False&IncludeNotes=undefined&Datasets[0].DatasetName=Discharge.Primary%40DK982984&Datasets[0].Calculation=Instantaneous&Datasets[0].UnitId=141&_=1793833415800"),
  tar_target(ngongotaha_flow_daily, read_bop_daily_flow(ngongotaha_flow_url, "DK982984", "Ngongotaha at SH5")),
  tar_target(puarenga_sh30_flow_url, "https://envdata.boprc.govt.nz/Export/BulkExport?DateRange=EntirePeriodOfRecord&TimeZone=12&Calendar=CALENDARYEAR&Interval=PointsAsRecorded&Step=1&ExportFormat=csv&TimeAligned=True&RoundData=False&IncludeGradeCodes=True&IncludeApprovalLevels=True&IncludeQualifiers=True&IncludeInterpolationTypes=False&IncludeNotes=undefined&Datasets[0].DatasetName=Discharge.Primary%40EK630244&Datasets[0].Calculation=Instantaneous&Datasets[0].UnitId=141&_=1793833457235"),
  tar_target(puarenga_sh30_flow_daily, read_bop_daily_flow(puarenga_sh30_flow_url, "EK630244", "Puarenga at SH30")),
  tar_target(waingaehe_flow_url, "https://envdata.boprc.govt.nz/Export/BulkExport?DateRange=EntirePeriodOfRecord&TimeZone=12&Calendar=CALENDARYEAR&Interval=PointsAsRecorded&Step=1&ExportFormat=csv&TimeAligned=True&RoundData=False&IncludeGradeCodes=True&IncludeApprovalLevels=True&IncludeQualifiers=True&IncludeInterpolationTypes=False&IncludeNotes=undefined&Datasets[0].DatasetName=Discharge.Primary%40FK014535&Datasets[0].Calculation=Instantaneous&Datasets[0].UnitId=141&_=1793833511623"),
  tar_target(waingaehe_flow_daily, read_bop_daily_flow(waingaehe_flow_url, "FK014535", "Waingaehe at SH30")),
  tar_target(kuirau_flow_url, "https://envdata.boprc.govt.nz/Export/BulkExport?DateRange=EntirePeriodOfRecord&TimeZone=12&Calendar=CALENDARYEAR&Interval=PointsAsRecorded&Step=1&ExportFormat=csv&TimeAligned=True&RoundData=False&IncludeGradeCodes=True&IncludeApprovalLevels=True&IncludeQualifiers=True&IncludeInterpolationTypes=False&IncludeNotes=undefined&Datasets[0].DatasetName=Discharge.Primary%40EK406461&Datasets[0].Calculation=Instantaneous&Datasets[0].UnitId=141&_=1793833569369"),
  tar_target(kuirau_flow_daily, read_bop_daily_flow(kuirau_flow_url, "EK406461", "Kuirau at Tarewa Rd")),
  tar_target(
    measured_inflow_daily,
    dplyr::bind_rows(ngongotaha_flow_daily, puarenga_sh30_flow_daily,
                     waingaehe_flow_daily, kuirau_flow_daily)
  ),

  # ENSO indices (see website/enso-lake-rotorua.qmd, calibration/enso/).
  # Live downloads, like the BOPRC URL targets above: tar_invalidate() to refresh.
  #  - enso_indices: NOAA CPC SOI, ONI; NOAA PSL IPO tripole index; CPC AAO (SAM proxy); monthly
  #  - statsnz_soi: Stats NZ / Earth Sciences NZ 3-month SOI and official phases, 1990-2025
  #  - soi_long: Stats NZ SOI from 1990, NOAA SOI rescaled to it before 1990, with phases
  tar_target(enso_indices, get_enso_indices(cache_dir = NULL)),
  tar_target(
    statsnz_soi_url,
    "https://www.stats.govt.nz/assets/Uploads/Environment-indicators-2026/El-Nino-Southern-Oscillation-Data-to-2025/Download-data/soi-3-month-rolling-average-and-de-trended-temperature-anomaly-1990-2025.csv"
  ),
  tar_target(statsnz_soi, read_statsnz_soi(statsnz_soi_url)),
  tar_target(soi_long, splice_soi(enso_indices, statsnz_soi)),

  tar_target(ohau_weir_width, 26.0),
  
  tar_target(
    glm_weir_params, est_glm_weir_params(ohau_flow_level, ohau_weir_width)
  ),
  
  tar_target(
    weir_params, format_weir_params(crest_elev = glm_weir_params$crest_elev,
                                    crest_factor = glm_weir_params$crest_factor,
                                    crest_width = ohau_weir_width)
  ),
  
  # 0. Define constants ----
  tar_target(
    rotorua_catchment_bbox_file,
    sync_onedrive_file("rotorua-pc10/data/processed/rotorua_lakes_area.rds",
                       here::here("data", "processed", "rotorua_lakes_area.rds")),
    format = "file",
    cue = if (identical(Sys.getenv("CI"), "true")) tar_cue(mode = "always") else tar_cue(mode = "never"),
    deployment = "main"
  ),
  tar_target(
    rotorua_era5_37yr_hr_file,
    sync_onedrive_file("rotorua-pc10/data/processed/rotorua_era5_hourly_1988-2024.csv",
                       here::here("data", "processed", "rotorua_era5_hourly_1988-2024.csv")),
    format = "file",
    cue = if (identical(Sys.getenv("CI"), "true")) tar_cue(mode = "always") else tar_cue(mode = "never"),
    deployment = "main"
  ),
  tar_target(
    rotorua_era5_50yr_hr_file,
    sync_onedrive_file("rotorua-pc10/data/processed/rotorua_era5_hourly_1976-2025.csv",
                       here::here("data", "processed", "rotorua_era5_hourly_1976-2025.csv")),
    format = "file",
    cue = if (identical(Sys.getenv("CI"), "true")) tar_cue(mode = "always") else tar_cue(mode = "never"),
    deployment = "main"
  ),
  tar_target(
    rotorua_era5_hr_file,
    sync_onedrive_file("rotorua-pc10/data/processed/rotorua_era5_hourly.csv",
                       here::here("data", "processed", "rotorua_era5_hourly.csv")),
    format = "file",
    cue = if (identical(Sys.getenv("CI"), "true")) tar_cue(mode = "always") else tar_cue(mode = "never"),
    deployment = "main"
  ),
  tar_target(
    tutira_bbox_coords, c(xmin = 176.75423, ymin = -39.29087, xmax = 177.00423, 
                          ymax = -39.15557)
  ),
  tar_target(
    #list.files(file.path(data_raw_dir, "niwa_climate"), pattern = "daily", full.names = TRUE),
    niwa_met_daily_files, c(
      file.path(data_raw_dir, "niwa_climate", "1770__Evaporation__Penman-Open-Water-Evaporation__daily.csv"),
      file.path(data_raw_dir, "niwa_climate", "1770__Evaporation__Penman-PET__daily.csv"),
      file.path(data_raw_dir, "niwa_climate", "1770__Evaporation__Priestly-Taylor-PET__daily.csv"),
      file.path(data_raw_dir, "niwa_climate", "1770__Radiation__Global__daily.csv"),
      file.path(data_raw_dir, "niwa_climate", "1770__Rain__daily.csv"),
      file.path(data_raw_dir, "niwa_climate", "1770__Temperature__daily.csv"),
      file.path(data_raw_dir, "niwa_climate", "1770__Wind__daily.csv")
    )
  ),
  tar_target(
    niwa_met_hourly_files, list.files(file.path(data_raw_dir, "niwa_climate"),
                                      pattern = "hourly", full.names = TRUE),
    format = "file"
  ),
  # From Chris McBride previous load modelling work
  tar_target(
    rotorua_inflow_file, file.path(data_raw_dir, "flows",
                                   "Rotorua_inf_final.csv"),
    format = "file"
  ),
  tar_target(
    rotorua_inflow_id_file, file.path(data_raw_dir, "flows",
                                      "Rotorua_infID.csv"),
    format = "file"
  ),
  tar_target(
    rotorua_inflow_key, file.path(data_raw_dir, "flows",
                                  "rotorua_inflow_key.csv"),
    format = "file"
  ),
  tar_target(
    alum_dosing_file, file.path(data_raw_dir, "alum_dosing",
                                "Rotorua alum dose data February 2026.xlsx"),
    format = "file"
  ),
  tar_target(
    bop_lake_level_zip_folder, file.path(data_raw_dir,
                                         "BulkExport-FL150407-20251215152116.zip"),
    format = "file"
  ),
  tar_target(
    bop_pc10_zip_folder, file.path(data_raw_dir, "PC10_data_Jun26.zip"),
    format = "file"
  ),
  tar_target(
    era5_daily_rds_file, file.path(data_raw_dir, "LID_11133.rds"),
    format = "file"
  ),
  tar_target(
    pc10_data_dir, unzip_pc10_data(bop_pc10_zip_folder),
    format = "file"
  ),
  tar_target(
    pc10_climate_data, extract_pc10_climate(pc10_data_dir)
  ),
  tar_target(
    pc10_climate_met, pc10_climate_to_aeme_met(pc10_climate_data)
  ),
  # Utuhina electrofishing data
  tar_target(
    electro_fishing_file, file.path(data_raw_dir, "electro_fishing",
                                    "Historic Utuhina electrofishing dates and CPUE.xlsx")
  ),
  tar_target(
    electro_fishing, read_electro_fishing(electro_fishing_file)
  ),
  # Met service data
  tar_target(
    airport_met_service_file, file.path(data_raw_dir, "met_service",
                                "Hourly_NZROA_Jan1993_Jun2023.xlsx"),
    format = "file"
  ),
  
  tar_target(
    airport_met, read_met_service_xl(airport_met_service_file, tz = scenario_tz)
  ),
  tar_target(airport_met_aeme, airport_met_to_aeme_met(airport_met)),

  # CTD Excel file
  tar_target(
    ctd_excel_file,
    file.path(data_raw_dir, "bop_wq",
              "Lake Rotorua CTD Profile Data - Full Record .xlsx"),
    format = "file"
  ),
  
  # Lake Chemistry data
  tar_target(
    chem_excel_file,
    file.path(data_raw_dir, "bop_wq", "Lakes Data 1989_2025.xlsx"),
    format = "file"
  ),
  
  # Alkalinity, total hardness and DOC (discrete lake samples; used with pH on website/ph-data.qmd)
  tar_target(
    alk_excel_file,
    file.path(data_raw_dir, "bop_wq", "alkalinity, tot hardness & diss org Carbon.xlsx"),
    format = "file"
  ),
  tar_target(
    alk_data, {
      readxl::read_excel(alk_excel_file, sheet = 1) |>
        dplyr::transmute(
          site_id = `Site ID`, location = `Location Name`, datetime = Time,
          alkalinity_mgl_caco3 = `Alkalinity (g/m^3)`, doc_gm3 = `DOC (g/m^3)`,
          hardness_mgl_caco3 = `Tot Hardness (g/m^3)`
        )
    }
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
    alum_dosing_sites,
    data.frame(
      site = c("Utuhina", "Puarenga"),
      lat  = c(-38.134566, -38.143958),
      long = c(176.237772, 176.267716)
    ) |> 
      sf::st_as_sf(coords = c("long", "lat"), crs = 4326) |> 
      sf::st_transform(crs = 2193)
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
  tar_target(
    chem_aeme, format_chem_for_aeme(chem_data)
  ),
  
  # NIWA met data
  tar_target(
    niwa_met_daily,
    read_niwa_climate_file(niwa_met_daily_files),
    pattern = map(niwa_met_daily_files)
  ),
  
  
  # gather_cmip6_metadata() reads from a hardcoded local network drive
  # ("Z:") that only exists on the machine it was written for — it isn't
  # reachable from CI. Its output is already committed to git as
  # data/processed/niwa_cmip6_metadata.csv, so read that directly in CI
  # instead of trying (and failing) to rebuild it from the network drive.
  tar_target(
    cmip6_metadata, {
      # gather_cmip6_metadata()
      readr::read_csv(here::here("data", "processed", "niwa_cmip6_metadata.csv"),
                      col_types = readr::cols())
    }
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
    lernzmp_aeme, {
      aeme <- aemetools::get_aeme(id = lake_id,
                                  api_key = Sys.getenv("LERNZMP_KEY")) |> 
        AEME::upgrade_aeme()
      # Rename inflow variable "Rainfall" to "precip" to match AEME variable names
      inf <- inflows(aeme)
      names(inf$data)[names(inf$data) == "Rainfall"] <- "precip"
      inflows(aeme) <- inf
      aeme
    }
  ),
  
  tar_target(
    lake_meta, AEME::lake(lernzmp_aeme) |> 
      as.data.frame()
  ),
  
  tar_target(
    lake_shape, 
    ltapi::lt_fetch(
      table  = "lernzmp_lakes",
      filter = ltapi::lt_filter(lernzmp_id == "LID11133")
    )
  ),
  tar_target(
    depth_contours, 
    ltapi::lt_fetch(
      table  = "lake_contours",
      filter = ltapi::lt_filter(lernzmp_id == "LID11133")
    )
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
    buoy_metadata, {
      ltapi::lt_fetch("sites", filter = ltapi::lt_filter(site_id == "Rotorua"))
    }
  ),
  tar_target(
    buoy_sf, sf::st_as_sf(buoy_metadata, coords = c("longitude", "latitude"),
                          crs = 4326)
  ),
  
  tar_target(
    rotorua_buoy_pro_data, get_lake_wqprofiler(type = "pro",
                                               api_key = Sys.getenv("LERNZMP_KEY")), 
    # cue = tar_cue(mode = "always")
  ),
  tar_target(
    rotorua_buoy_met_data, {
      met <- get_lake_wqprofiler(type = "met",
                          api_key = Sys.getenv("LERNZMP_KEY")) |> 
        dplyr::mutate(
          DateTime = lubridate::as_datetime(DateTime, tz = scenario_tz)
        )
      # Before 2023-10-23 windpseed and rainfall were collected in different units
      met_pre <- met |>
        dplyr::filter(
          DateTime < as.POSIXct("2023-10-23 00:00:00", tz = scenario_tz)
        ) |> 
        dplyr::mutate(
          PpRain = PpRain / 10, # mm/10min -> mm
          WndSpd = WndSpd * 0.514444 # knots -> m/s
        )
      met_post <- met |> 
        dplyr::filter(
          DateTime >= as.POSIXct("2023-10-23 00:00:00", tz = scenario_tz)
        )
      dplyr::bind_rows(met_pre, met_post) |> 
        dplyr::arrange(DateTime)
      
    }, 
    # cue = tar_cue(mode = "always")
  ),
  tar_target(
    buoy_met_height, 2.0 # m above water surface
  ),
  tar_target(
    rotorua_buoy_met_aeme_dly, buoy_to_aeme_met(rotorua_buoy_met_data,
                                                buoy_met_height)
  ),
  tar_target(
    rotorua_buoy_met_aeme_hr, buoy_to_aeme_met(rotorua_buoy_met_data,
                                               buoy_met_height, unit = "hour")
  ),
  
  tar_target(
    all_buoy_met_data, dplyr::bind_rows(old_buoy_met_hr_aeme,
                                        rotorua_buoy_met_aeme_hr)
  ),
  # Buoy record used as the *reference* when fitting bias corrections: buoy
  # relative humidity is restricted to 2022-01-01 .. 2023-12-31 (new buoy
  # only; everything else set to NA). The old-buoy RH reads ~5-9% below ERA5
  # and the MetService airport, and the new buoy's RH drifts from 2024 on
  # (ERA5 - buoy RH bias: +2-3% in 2022-23, +10% 2024, +13% 2025, +48% 2026),
  # whereas in 2022-23 it agrees with the MetService airport to <1%. See
  # met-source-comparison.qmd. Other variables and the raw buoy record
  # (all_buoy_met_data) are unchanged.
  tar_target(
    buoy_met_fit_ref, {
      ref <- all_buoy_met_data
      rh_ok <- ref$Date >= as.POSIXct("2022-01-01", tz = "Etc/GMT-12") &
        ref$Date < as.POSIXct("2024-01-01", tz = "Etc/GMT-12")
      ref$MET_humrel[!rh_ok] <- NA_real_
      ref
    }
  ),
  
  # NIWA met to AEME data
  tar_target(
    niwa_met_aeme, {
      niwa_met_daily |>
        dplyr::bind_rows() |>
        niwa_to_aeme_met()
    }
  ),
  # NIWA hourly met, combined into the same Date + MET_* wide layout as
  # rotorua_buoy_met_aeme_hr / era5_hourly_met so it can also be handed to
  # metscale::fit_met_bias_correction() as a (longer-record) alternative or
  # supplement to the buoy when bias-correcting era5_hourly_met.
  tar_target(
    niwa_met_hourly_aeme,
    niwa_hourly_to_aeme_met(niwa_met_hourly_files, tz = scenario_tz)
  ),
  tar_target(
    ctd_data, read_ctd(file = ctd_excel_file)
  ),
  tar_target(
    ctd_par, format_par_ts(ctd_data)
  ),
  tar_target(
    ctd_aeme, format_ctd_for_aeme(ctd_data)
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
    pattern = map(ratio_al_p),
    iteration = "list"
  ),

  #* PC10 loads 2026 branch ----
  # Parallel chain that uses the 2026 PC10 loads (data/raw/pc10_loads_2026) instead of
  # rotorua_inflows_populated_noalum.csv. The targets above are untouched; everything
  # here is suffixed _pc10. Change pc10_loads_set to switch among the 12 sets
  # (<alum|noalum>_<storm|nostorm>_<central|lower|upper>); noalum_* matches the
  # project convention (alum module on, in-stream binding handled in the model).
  tar_target(
    pc10_loads_set, "noalum_storm_central"
  ),
  tar_target(
    pc10_loads_files,
    list.files(here::here("data", "raw", "pc10_loads_2026", pc10_loads_set),
               full.names = TRUE),
    format = "file"
  ),
  # The loads run to 2026-06-30 but the corrected ERA5 forcing ends earlier, so the
  # inflows are cut at the end of the forcing.
  tar_target(
    pc10_loads_stop, as.Date(max(era5_corrected_hourly$Date))
  ),
  tar_target(
    rotorua_inflow_list_pc10, {
      pc10_loads_files  # dependency
      read_pc10_loads(pc10_loads_set, include_rain = FALSE, to = pc10_loads_stop)
    }
  ),
  tar_target(
    rotorua_inflow_list_matrix_pc10,
    add_alum_to_inflows(rotorua_inflow_list_pc10, alum_dosing,
                        dosed = inflows_alum_dosed, ratio_al_p = ratio_al_p),
    pattern = map(ratio_al_p),
    iteration = "list"
  ),
  tar_target(
    aeme_base_pc10_file,
    build_calib_base_aeme(
      aeme_download_file = aeme_onedrive_rds,
      hyps_no_hole = hyps_no_hole,
      era5_corrected_hourly = era5_corrected_hourly,
      inflows = rotorua_inflow_list_matrix_pc10[[1]],
      zone_heights = zone_heights,
      weir_params = weir_params,
      aed_models_alum = aed_models_alum,
      model = model,
      ext_elev = ext_elev,
      alum_nml_file = file.path(aeme_onedrive_lake_rotorua_dir, "aed", "aed_original.nml"),
      out_file = here::here("aeme_base_pc10.rds"),
      build_path = here::here("calibration", "base_build_pc10"),
      old_buoy_wq = old_buoy_wq
    ),
    format = "file"
  ),
  tar_target(
    aeme_baseline_pc10, readRDS(aeme_base_pc10_file)
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
        niwa_met_hourly_aeme   = niwa_met_hourly_aeme,
        
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
  # tar_target(
  #   corr_hyps_file, {
  #     out_file <- here::here("data", "processed", "rotorua_data_sh",
  #                            "rotorua_hypsograph.csv")
  #     readr::write_csv(corr_hyps, file = out_file)
  #     out_file
  #   },
  #   format = "file"
  # ),
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
  # process_cmip6_shp() takes >6 days locally across the full grid/variable/
  # scenario/period combination set — far beyond what a CI runner can do.
  # In CI (Sys.getenv("CI") == "true", set automatically by GitHub Actions),
  # download the already-computed outputs from OneDrive instead of
  # recomputing them. Push fresh outputs up with R/sync_cmip6_onedrive.R
  # after running tar_make() locally.
  if (identical(Sys.getenv("CI"), "true")) {
    tar_target(
      cmip6_files,
      list.files(
        sync_onedrive_folder("rotorua-pc10/data/processed/rotorua_lakes_cmip6",
                             here::here("data", "processed", "rotorua_lakes_cmip6")),
        full.names = TRUE, recursive = TRUE
      ),
      format = "file",
      # Combined with the _targets cache in CI: download once, then skip on
      # every subsequent run. Re-run R/sync_cmip6_onedrive.R after a local
      # recompute, then tar_invalidate(cmip6_files) to force a fresh pull.
      cue = tar_cue(mode = "always"),
      deployment = "main"
    )
  } else {
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
      # cue = tar_cue(mode = "always")
    )
  },
  if (identical(Sys.getenv("CI"), "true")) {
    tar_target(
      tutira_cmip6_files,
      list.files(
        sync_onedrive_folder("rotorua-pc10/data/processed/tutira_cmip6",
                             here::here("data", "processed", "tutira_cmip6")),
        full.names = TRUE, recursive = TRUE
      ),
      format = "file",
      cue = tar_cue(mode = "always"),
      deployment = "main"
    )
  } else {
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
    )
  },
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
  
  # gcm_ts_summary takes >1 day locally (summarise_gcm_ts() over every
  # variable x GCM combination) — far too long for CI. In CI, skip it
  # entirely and download the already-computed gcm_ts_df from OneDrive
  # instead (push fresh results up with R/sync_cmip6_onedrive.R after
  # running tar_make() locally).
  if (identical(Sys.getenv("CI"), "true")) {
    tar_target(
      gcm_ts_df,
      readRDS(sync_onedrive_file("rotorua-pc10/data/processed/gcm_ts_df.rds",
                                 here::here("data", "processed", "gcm_ts_df.rds"))),
      cue = tar_cue(mode = "always"),
      deployment = "main"
    )
  } else {
    list(
      tar_target(
        gcm_ts_summary, {
          summarise_gcm_ts(variable = cmip_vars, gcm = cmip_gcm,
                           files = cmip6_files)
        },
        pattern = cross(cmip_vars, cmip_gcm),
        iteration = "list",
        cue = tar_cue(mode = "never")
      ),
      tar_target(
        gcm_ts_df, dplyr::bind_rows(gcm_ts_summary)
      )
    )
  },
  
  # Buoy data 
  tar_target(
    aeme_buoy_data, {
      rotorua_buoy_pro_data |> 
        format_buoy_for_aeme()
    }
  ),
  
  # pH time series (old + new buoy), all values with a QC flag; used by website/ph-data.qmd
  tar_target(
    ph_data, {
      dplyr::bind_rows(
        old_buoy_wq |> dplyr::filter(var_aeme == "CHM_ph") |>
          dplyr::transmute(datetime = Date, depth_m = depth, ph = value, source = "old buoy"),
        aeme_buoy_data |> dplyr::filter(var_aeme == "CHM_ph") |>
          dplyr::transmute(datetime = Date, depth_m = depth, ph = value, source = "new buoy")
      ) |>
        dplyr::filter(is.finite(ph)) |>
        dplyr::arrange(source, datetime, depth_m) |>
        dplyr::mutate(date = as.Date(datetime), qc_in_range = ph > 4 & ph < 11) |>
        dplyr::select(datetime, date, depth_m, ph, source, qc_in_range)
    }
  ),

  # Daily mean profiler data (0.5 m depth bins) for pH and the variables measured alongside it
  tar_target(
    buoy_profile_daily, {
      rotorua_buoy_pro_data |>
        dplyr::mutate(date = as.Date(DateTime), depth_m = round(DptSns * 2) / 2) |>
        dplyr::group_by(date, depth_m) |>
        dplyr::summarise(
          dplyr::across(
            c(ph = pH, temp_c = TmpWtr, chl_fl = FlChlr, do_mgl = DOconc, do_sat = DOpsat,
              turb = TurbRT, spcond = SpCond, orp_mv = ORP),
            \(x) mean(x, na.rm = TRUE)),
          .groups = "drop") |>
        dplyr::mutate(dplyr::across(where(is.numeric), \(x) ifelse(is.nan(x), NA_real_, x))) |>
        dplyr::filter(!is.na(ph), ph > 4, ph < 11)
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
    deployment = "main",
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
    iteration = "list", 
    deployment = "main"
  ),
  tar_target(
    gcm_point_data_std_df,
    dplyr::bind_rows(Filter(is.data.frame, gcm_point_data_std))
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
      # The downloaded Aeme carries a 3-zone sediment set under glm3.nml (n_zones, zone_heights,
      # sed_temp_*, ...). Left in place next to the 5-zone glm4.nml set it gives an inconsistent
      # glm4.nml (n_zones = 3 but 5 sed_temp_deep values), which AEME's nml check rejects.
      old_param <- AEME::parameters(aeme_base)
      stale_sed <- unique(old_param$name[old_param$file == "glm3.nml" & grepl("^sediment/", old_param$name)])
      aeme <- aeme_base |> 
        AEME::remove_inflow(all = TRUE) |> 
        AEME::add_inflows(data = rotorua_inflow_list_matrix) |>
        AEME::add_obs(lake = aeme_buoy_data, level = sub_lake_level) |> 
        AEME::remove_param(name = stale_sed, file = "glm3.nml") |> 
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

  # Baseline GLM-AED object for the staged PEST++ calibration (calibration/).
  # The calibration itself is NOT a target: see calibration/README.md.
  tar_target(
    aeme_base_file,
    build_calib_base_aeme(
      # depend on the OneDrive sync targets and use absolute paths: on CI the working directory is
      # website/, and these files only exist after the sync targets have run
      aeme_download_file = aeme_onedrive_rds,
      hyps_no_hole = hyps_no_hole,
      era5_corrected_hourly = era5_corrected_hourly,
      inflows = rotorua_inflow_list_matrix[[1]],
      zone_heights = zone_heights,
      weir_params = weir_params,
      aed_models_alum = aed_models_alum,
      model = model,
      ext_elev = ext_elev,
      alum_nml_file = file.path(aeme_onedrive_lake_rotorua_dir, "aed", "aed_original.nml"),
      out_file = here::here("aeme_base.rds"),
      build_path = here::here("calibration", "base_build"),
      old_buoy_wq = old_buoy_wq
    ),
    format = "file"
  ),
  tar_target(
    aeme_baseline, readRDS(aeme_base_file)
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
        height = 6,
        create.dir = TRUE
      )
      out_file
    },
    pattern = map(cmip_vars),
    deployment = "main",
    format = "file"
  ),

  # 4c. Climate-scenario meteorology (metscale bias-correction -> delta-change
  #     -> disaggregation, see vignette("scenario-workflow", package = "metscale")) ----

  #* Reference/future windows for delta-change (matches scenario_batch_rotorua.R) ----
  # 30-year reference (WMO normal), ending at the last CMIP6 historical year. Changed from
  # 1995:2014 (20 years) on 2026-11-09. The ERA5 baseline only starts in 1988, so the
  # reference-period GCM-direct (QDM) mapping, which needs both series, cannot use 1985-2014.
  tar_target(
    scenario_ref_years, 1985:2014
  ),
  tar_target(
    scenario_tz, "Etc/GMT-12"
  ),
  
  # Era5 daily data
  tar_target(
    era5_daily, readRDS(era5_daily_rds_file)
  ),

  #* 1-2. Bias-correct hourly ERA5 against the buoy; bias-corrected daily baseline ----
  tar_target(
    era5_hourly_met,
    # 1988-2024 file, extended to 2025 from the older 2011-2025 file
    read_era5_hourly_met(rotorua_era5_50yr_hr_file, lat = lake_meta$latitude,
                         lon = lake_meta$longitude, tz = scenario_tz,
                         extend_file = rotorua_era5_hr_file)
  ),
  # Rain is corrected in two stages (see met-source-comparison.qmd): ERA5
  # rain is "wet" in ~41% of hours vs ~15% on the buoy, which a monthly scale
  # cannot fix. Stage 1 quantile-maps rain onto the buoy distribution (eqm:
  # fixes wet-hour frequency and intensity); stage 2 (inside
  # era5_bias_correction) puts the total back on the buoy with a monthly
  # scale. All downstream targets use era5_hourly_met_rainqm, not the raw
  # ERA5, as the input to era5_bias_correction.
  tar_target(
    era5_rain_eqm_correction,
    metscale::fit_met_bias_correction(
      era5_hourly_met, all_buoy_met_data, vars = "MET_pprain",
      method = "eqm", nquantiles = 100, verbose = FALSE)
  ),
  tar_target(
    era5_hourly_met_rainqm,
    metscale::apply_met_bias_correction(
      era5_hourly_met, era5_rain_eqm_correction, verbose = FALSE)
  ),
  # Relative humidity is corrected against the MetService airport record
  # (airport_met_aeme, 1993-Jun 2023) instead of the buoy: buoy RH is
  # unreliable (old buoy ~5-9% below ERA5 and the airport; new buoy drifts
  # from 2024). See met-source-comparison.qmd. Applied as a pre-stage, like
  # rain, so era5_bias_correction (buoy) no longer corrects MET_humrel.
  tar_target(
    era5_humrel_correction,
    metscale::fit_met_bias_correction(
      era5_hourly_met, airport_met_aeme, vars = "MET_humrel",
      method = "scale", by = "doy-loess", verbose = FALSE)
  ),
  tar_target(
    era5_hourly_met_stage1,
    metscale::apply_met_bias_correction(
      era5_hourly_met_rainqm, era5_humrel_correction, verbose = FALSE)
  ),
  tar_target(
    era5_bias_correction,
    metscale::fit_met_bias_correction(
      era5_hourly_met_stage1, buoy_met_fit_ref,
      vars = c("MET_tmpair", "MET_wndspd", "MET_radswd",
               "MET_prsttn", "MET_pprain"),
      method = "scale", by = "doy-loess", verbose = FALSE)
  ),
  tar_target(
    era5_corrected_hourly,
    metscale::apply_met_bias_correction(
      era5_hourly_met_stage1, era5_bias_correction, expand = TRUE,
      lat = lake_meta$latitude, lon = lake_meta$longitude,
      elev = lake_meta$elevation, tz = scenario_tz, verbose = FALSE)
  ),

  #* Bias-correction diagnostics: raw vs. corrected vs. buoy, time series and
  #  distribution, for the two variables most consequential for the
  #  scenario workflow -- wind speed (drives storm/mixing indices) and
  #  rainfall (drives heavy-rain indices, and known to be sensitive to
  #  the buoy's own data quality -- see climate-extreme-events.qmd) ----
  tar_target(
    era5_bc_timeseries_wind, {
      out_file <- here::here("website", "www", "plots", "era5_bc_timeseries_wind.png")
      p <- plot_bias_correction_timeseries(era5_hourly_met, era5_corrected_hourly,
                                           all_buoy_met_data, "MET_wndspd")
      ggsave(filename = out_file, plot = p, width = 10, height = 4, dpi = 150,
            create.dir = TRUE)
      out_file
    },
    format = "file",
    deployment = "main"
  ),
  tar_target(
    era5_bc_distribution_wind, {
      out_file <- here::here("website", "www", "plots", "era5_bc_distribution_wind.png")
      p <- plot_bias_correction_distribution(era5_hourly_met, era5_corrected_hourly,
                                             all_buoy_met_data, "MET_wndspd")
      ggsave(filename = out_file, plot = p, width = 7, height = 4.5, dpi = 150,
            create.dir = TRUE)
      out_file
    },
    format = "file",
    deployment = "main"
  ),
  tar_target(
    era5_bc_timeseries_rain, {
      out_file <- here::here("website", "www", "plots", "era5_bc_timeseries_rain.png")
      p <- plot_bias_correction_timeseries(era5_hourly_met, era5_corrected_hourly,
                                           all_buoy_met_data, "MET_pprain")
      ggsave(filename = out_file, plot = p, width = 10, height = 4, dpi = 150,
            create.dir = TRUE)
      out_file
    },
    format = "file",
    deployment = "main"
  ),
  tar_target(
    era5_bc_distribution_rain, {
      out_file <- here::here("website", "www", "plots", "era5_bc_distribution_rain.png")
      p <- plot_bias_correction_distribution(era5_hourly_met, era5_corrected_hourly,
                                             all_buoy_met_data, "MET_pprain",
                                             wet_only = TRUE, log1p_transform = TRUE)
      ggsave(filename = out_file, plot = p, width = 7, height = 4.5, dpi = 150,
            create.dir = TRUE)
      out_file
    },
    format = "file",
    deployment = "main"
  ),

  #* Extend the buoy record using the airport station's long history ----
  # The buoy only spans ~5 years, which is thin for a doy-loess seasonal
  # correction. Fit a land (airport) -> lake (buoy) transfer function over
  # their short overlap window, then apply it to the full airport record to
  # synthesize a lake-equivalent series spanning decades instead of 5 years
  # -- giving fit_met_bias_correction() a longer, still lake-representative
  # reference to bias-correct ERA5 against. See era5_corrected_hourly_extended
  # below for validation against the buoy-only correction before relying on
  # this outside the buoy's own overlap window.
  tar_target(
    niwa_buoy_overlap_window,
    range(all_buoy_met_data$Date, na.rm = TRUE)
  ),
  tar_target(
    niwa_to_buoy_bias_correction, {
      niwa_overlap <- niwa_met_hourly_aeme |>
        dplyr::filter(Date >= niwa_buoy_overlap_window[1],
                      Date <= niwa_buoy_overlap_window[2])
      metscale::fit_met_bias_correction(
        niwa_overlap, buoy_met_fit_ref,
        vars = c("MET_tmpair", "MET_wndspd", "MET_radswd", "MET_humrel", "MET_prsttn"),
        method = "scale", by = "doy-loess", verbose = FALSE)
    }
  ),
  # expand = FALSE here: expand_met() derives extra atmospheric variables
  # (cloud cover, downwelling longwave, etc.) that this intermediate target
  # doesn't need -- it's only used as a fit_met_bias_correction() reference
  # below, not as forcing data. expand = TRUE also requires MET_radswd,
  # MET_tmpair and MET_pprain simultaneously non-NA on every row, which
  # this station's data can't satisfy: the airport's radiation sensor and
  # temperature sensor only ever overlap on 44 calendar days across the
  # entire 44-year record (radiation stopped in 2020; temperature's own
  # "Mean Temperature" field wasn't populated until 2023), so that filter
  # collapsed to zero rows.
  tar_target(
    niwa_met_hourly_lake_equiv,
    metscale::apply_met_bias_correction(
      niwa_met_hourly_aeme, niwa_to_buoy_bias_correction, verbose = FALSE)
  ),

  #* Bias-correct ERA5 against the extended (buoy-quality, airport-length)
  #  lake-equivalent series instead of the raw 5-year buoy record ----
  # Note: MET_wndspd and MET_radswd aren't in niwa_to_buoy_bias_correction's
  # models (the airport's wind and radiation sensors have ~no valid readings
  # during the buoy's 2022-2026 window -- 0 radiation pairs, 2 wind pairs),
  # so those two columns pass through this fit as raw, buoy-uncorrected
  # airport values rather than lake-equivalent ones. Treat those two
  # variables' results here cautiously versus era5_bias_correction.
  tar_target(
    era5_bias_correction_extended,
    metscale::fit_met_bias_correction(
      era5_hourly_met, niwa_met_hourly_lake_equiv,
      vars = c("MET_tmpair", "MET_wndspd", "MET_radswd", "MET_humrel",
              "MET_prsttn", "MET_pprain"),
      method = "scale", by = "doy-loess", verbose = FALSE)
  ),
  tar_target(
    era5_corrected_hourly_extended,
    metscale::apply_met_bias_correction(
      era5_hourly_met, era5_bias_correction_extended, expand = TRUE,
      lat = lake_meta$latitude, lon = lake_meta$longitude,
      elev = lake_meta$elevation, tz = scenario_tz, verbose = FALSE)
  ),

  #* Compare all four raw met sources (buoy, airport, ERA5, PC10) against
  #  each other -- a QA check on this pipeline's existing bias-correction
  #  chain, and a way to see whether PC10's longer temp/RH/rain records
  #  are worth using to extend it the way the airport already is. ----
  tar_target(
    met_source_comparison,
    compare_met_sources(buoy = all_buoy_met_data,
                        airport = niwa_met_hourly_aeme,
                        metservice = airport_met_aeme,
                        era5 = era5_hourly_met,
                        pc10 = pc10_climate_met,
                        vars = c("MET_tmpair", "MET_humrel", "MET_pprain",
                                "MET_wndspd", "MET_radswd", "MET_prsttn"))
  ),
  # Downloadable met data (buoy, ERA5 original, ERA5 bias-corrected + column
  # metadata CSV) served from the website. The airport data are not included.
  tar_target(
    met_data_zip,
    make_met_data_zip(
      old_buoy_met_hr_aeme, rotorua_buoy_met_aeme_hr,
      era5_hourly_met, era5_corrected_hourly,
      out_zip = here::here("website", "downloads", "rotorua_met_data.zip")),
    format = "file"
  ),
  tar_target(
    met_source_comparison_plot,
    plot_met_source_comparison(
      met_source_comparison,
      window = range(all_buoy_met_data$Date, na.rm = TRUE),
      vars = c("MET_tmpair", "MET_humrel", "MET_pprain", "MET_wndspd",
               "MET_radswd"))
  ),

  tar_target(
    era5_corrected_daily_baseline, {
      baseline <- metscale::bias_correct_daily_baseline(
        era5_hourly_met_stage1, era5_bias_correction, lat = lake_meta$latitude,
        lon = lake_meta$longitude, elev = lake_meta$elevation,
        tz = scenario_tz, verbose = FALSE)
      ok <- stats::complete.cases(baseline[c("MET_radswd", "MET_tmpair", "MET_pprain",
                                             "MET_wndspd", "MET_humrel")])
      baseline[ok, ]
    }
  ),

  #* 3. CMIP6 monthly delta-change factors, every GCM x scenario x window ----
  # Lightweight point extraction (one GCM's already-cropped netCDFs at a
  # time) -- unlike gcm_point_data/gcm_point_data_std_df above (raw absolute
  # GCM values), this is the vignette's delta-change quantity: per-month
  # future-vs-historical change factors, additive for temperature/shortwave
  # and multiplicative for the bounded/skewed fields.
  tar_target(
    gcm_monthly_deltas,
    compute_gcm_monthly_deltas(
      gcm = cmip_gcm, cmip6_files = cmip6_files, lon = lake_meta$longitude,
      lat = lake_meta$latitude, scenarios = cmip_scenarios,
      ref_years = scenario_ref_years, future_windows = time_periods),
    pattern = map(cmip_gcm),
    iteration = "list"
  ),
  tar_target(
    gcm_monthly_deltas_df, dplyr::bind_rows(gcm_monthly_deltas)
  ),
  tar_target(
    gcm_monthly_deltas_file, {
      out_file <- here::here("data", "processed", "gcm_monthly_deltas.csv")
      readr::write_csv(gcm_monthly_deltas_df, file = out_file)
      out_file
    },
    format = "file"
  ),

  #* GCM-variability figure: spread of monthly deltas across GCMs, per variable ----
  tar_target(
    gcm_delta_variability_plot,
    {
      out_file <- here::here("website", "www", "plots",
                             "gcm_delta_variability_2071-2100.png")
      p <- plot_gcm_delta_variability(gcm_monthly_deltas_df, window = "2071-2100")
      ggsave(filename = out_file, plot = p, width = 11, height = 7, dpi = 150,
            create.dir = TRUE)
      out_file
    },
    format = "file",
    deployment = "main"
  ),
  tar_target(
    gcm_delta_variability_plot_midcentury,
    {
      out_file <- here::here("website", "www", "plots",
                             "gcm_delta_variability_2041-2070.png")
      p <- plot_gcm_delta_variability(gcm_monthly_deltas_df, window = "2041-2070")
      ggsave(filename = out_file, plot = p, width = 11, height = 7, dpi = 150,
            create.dir = TRUE)
      out_file
    },
    format = "file",
    deployment = "main"
  ),

  #* GCM-consensus figure: box-and-whisker of each GCM's 12 monthly deltas,
  #  to distinguish agreement between models from within-model seasonality ----
  tar_target(
    gcm_delta_boxplot,
    {
      out_file <- here::here("website", "www", "plots",
                             "gcm_delta_boxplot_2071-2100.png")
      p <- plot_gcm_delta_boxplot(gcm_monthly_deltas_df, window = "2071-2100")
      ggsave(filename = out_file, plot = p, width = 11, height = 7, dpi = 150,
            create.dir = TRUE)
      out_file
    },
    format = "file",
    deployment = "main"
  ),
  tar_target(
    gcm_delta_boxplot_midcentury,
    {
      out_file <- here::here("website", "www", "plots",
                             "gcm_delta_boxplot_2041-2070.png")
      p <- plot_gcm_delta_boxplot(gcm_monthly_deltas_df, window = "2041-2070")
      ggsave(filename = out_file, plot = p, width = 11, height = 7, dpi = 150,
            create.dir = TRUE)
      out_file
    },
    format = "file",
    deployment = "main"
  ),

  #* 4. Delta-changed daily baseline + hourly disaggregation, per GCM x
  #     scenario x window -- 7 GCM x 4 SSP x 3 windows (minus NZESM/ssp585)
  #     = 83 combinations, each disaggregating ~20-30 years to hourly. Follows
  #     the same "expensive, skip by default" convention as gcm_ts_summary /
  #     tutira_cmip6_files above: set cue = tar_cue(mode = "always") locally
  #     (or tar_make(names = "scenario_hourly_met")) to actually run it.
  tar_target(
    scenario_delta_grid, {
      tidyr::crossing(gcm = cmip_gcm, scenario = cmip_scenarios,
                      window = names(time_periods)) |>
        dplyr::filter(!(scenario == "ssp585" & gcm == "NZESM"))
    }
  ),
  tar_target(
    scenario_daily_met, {
      deltas <- gcm_monthly_deltas_df |>
        dplyr::filter(gcm == scenario_delta_grid$gcm,
                      scenario == scenario_delta_grid$scenario,
                      window == scenario_delta_grid$window)
      apply_gcm_delta_to_baseline(era5_corrected_daily_baseline, deltas,
                                  lat = lake_meta$latitude,
                                  lon = lake_meta$longitude,
                                  elev = lake_meta$elevation, tz = scenario_tz)
    },
    pattern = map(scenario_delta_grid),
    iteration = "list",
    cue = tar_cue(mode = "never")
  ),
  tar_target(
    scenario_hourly_met, {
      # `scenario_daily_met` no longer carries MET_wnduvu/MET_wnduvv/MET_wnddir
      # (see apply_gcm_delta_to_baseline()); strip them from the donor too, so
      # disaggregate_met_to_hourly() treats MET_wndspd as a plain scalar
      # mean-conserved variable (fragments-shaped from the donor's own wind-speed
      # pattern) rather than taking its wind-vector code path -- direction is
      # not needed for this lake model and the vector recomputation was
      # distorting the disaggregated hourly wind speed (see
      # website/climate-extreme-events.qmd).
      donor <- era5_corrected_hourly
      donor[c("MET_wnddir", "MET_wnduvu", "MET_wnduvv")] <- NULL
      metscale::disaggregate_met_to_hourly(
        scenario_daily_met, donor = donor,
        method = "fragments", swr = "clearsky", lat = lake_meta$latitude,
        lon = lake_meta$longitude, elev = lake_meta$elevation, tz = scenario_tz,
        seed = 42, expand = TRUE, verbose = FALSE)
    },
    pattern = map(scenario_daily_met),
    iteration = "list",
    cue = tar_cue(mode = "never")
  ),

  #* 4b. TRANSIENT scenarios: one continuous series per GCM x SSP (plus a
  #      no-delta control), with the delta-change factors evolving year by
  #      year along each GCM's historical -> SSP trajectory. For long-term
  #      nutrient-loading questions (e.g. N reduction from 2032) the lake
  #      response is path-dependent, so climate has to evolve alongside the
  #      loads rather than sit at three fixed time slices (section 4 above,
  #      kept as the fixed-climate sensitivity set). See
  #      website/climate-scenario-workflow.qmd. ----
  # 2005-2099 = the 10-year spin-up before sim_periods$ssp$start (2015-07-01)
  # plus the run to 2099-06-30.
  tar_target(
    transient_years, 2005:2099
  ),
  tar_target(
    transient_window_length, 30
  ),
  tar_target(
    transient_weather_seed, 42
  ),
  tar_target(
    transient_year_map,
    build_transient_year_map(transient_years, complete_years(era5_corrected_daily_baseline),
                             seed = transient_weather_seed)
  ),
  tar_target(
    gcm_transient_deltas,
    compute_gcm_transient_deltas(
      gcm = cmip_gcm, cmip6_files = cmip6_files, lon = lake_meta$longitude,
      lat = lake_meta$latitude, scenarios = cmip_scenarios,
      ref_years = scenario_ref_years, target_years = transient_years,
      window_length = transient_window_length),
    pattern = map(cmip_gcm),
    iteration = "list"
  ),
  tar_target(
    gcm_transient_deltas_df, dplyr::bind_rows(gcm_transient_deltas)
  ),
  tar_target(
    gcm_transient_delta_plot, {
      out_file <- here::here("website", "www", "plots", "gcm_transient_deltas.png")
      p <- plot_gcm_transient_deltas(gcm_transient_deltas_df)
      ggsave(filename = out_file, plot = p, width = 11, height = 7, dpi = 150,
            create.dir = TRUE)
      out_file
    },
    format = "file",
    deployment = "main"
  ),
  tar_target(
    transient_delta_grid, {
      tidyr::crossing(gcm = cmip_gcm, scenario = cmip_scenarios) |>
        dplyr::filter(!(scenario == "ssp585" & gcm == "NZESM")) |>
        dplyr::bind_rows(data.frame(gcm = "none", scenario = "control"))
    }
  ),
  # Daily is cheap, so it keeps the default cue (always current with the
  # code); the hourly disaggregation below is the expensive step and is gated.
  tar_target(
    scenario_transient_daily_met,
    build_transient_daily_met(
      era5_corrected_daily_baseline, transient_year_map, gcm_transient_deltas_df,
      gcm = transient_delta_grid$gcm, scenario = transient_delta_grid$scenario,
      lat = lake_meta$latitude, lon = lake_meta$longitude,
      elev = lake_meta$elevation, tz = scenario_tz),
    pattern = map(transient_delta_grid),
    iteration = "list"
  ),
  # ~95 years x 28 branches disaggregated to hourly. Build explicitly with
  # tar_make(names = "scenario_transient_hourly_met"); after any change to the
  # daily met, tar_invalidate("scenario_transient_hourly_met") first.
  tar_target(
    scenario_transient_hourly_met,
    disaggregate_scenario_met(
      scenario_transient_daily_met, donor = era5_corrected_hourly,
      lat = lake_meta$latitude, lon = lake_meta$longitude,
      elev = lake_meta$elevation, tz = scenario_tz),
    pattern = map(scenario_transient_daily_met),
    iteration = "list",
    cue = tar_cue(mode = "never")
  ),

  #* 5. Extreme/storm-event indices from the hourly met, baseline vs each
  #     GCM x scenario x window branch -- thresholds fixed from a
  #     historical reference so frequency changes are directly interpretable.
  #     Rainfall thresholds use the 125-year Whakarewarewa record
  #     (`pc10_climate_met`) rather than the 20-year ERA5 window -- far more
  #     robust for a p95/p99 estimate, and independent of the reanalysis
  #     chain the scenarios themselves are built from. Wind/heat thresholds
  #     still come from the bias-corrected ERA5 hourly baseline, which has
  #     no long-record alternative at this site. ----
  tar_target(
    whaka_rain_record, extract_long_rain_record(pc10_climate_met)
  ),
  tar_target(
    whaka_rain_thresholds, compute_long_rain_thresholds(whaka_rain_record)
  ),
  tar_target(
    whaka_rain_return_levels, compute_rain_return_levels(whaka_rain_record)
  ),
  tar_target(
    extreme_thresholds,
    compute_extreme_thresholds(era5_corrected_hourly, scenario_ref_years,
                               rain_thresholds = whaka_rain_thresholds)
  ),
  tar_target(
    baseline_extreme_indices, {
      day <- as.Date(era5_corrected_hourly$Date)
      yr  <- as.integer(format(day, "%Y"))
      ann <- compute_annual_extreme_indices(
        era5_corrected_hourly[yr %in% scenario_ref_years, ], extreme_thresholds)
      as.list(dplyr::summarise(ann, dplyr::across(-year, \(x) mean(x, na.rm = TRUE))))
    }
  ),
  tar_target(
    scenario_extreme_indices,
    summarise_branch_extremes(
      scenario_hourly_met, extreme_thresholds,
      gcm = scenario_delta_grid$gcm, scenario = scenario_delta_grid$scenario,
      window = scenario_delta_grid$window),
    pattern = map(scenario_hourly_met, scenario_delta_grid),
    iteration = "list"
  ),
  tar_target(
    scenario_extreme_indices_df, dplyr::bind_rows(scenario_extreme_indices)
  ),
  tar_target(
    scenario_extreme_indices_file, {
      out_file <- here::here("data", "processed", "scenario_extreme_indices.csv")
      readr::write_csv(scenario_extreme_indices_df, file = out_file)
      out_file
    },
    format = "file"
  ),
  tar_target(
    extreme_indices_plot, {
      out_file <- here::here("website", "www", "plots", "extreme_indices_summary.png")
      p <- plot_extreme_index_summary(
        scenario_extreme_indices_df, baseline_extreme_indices,
        indices = c("rx1day", "heavy_rain_days", "extreme_rain_days",
                   "max_wind", "storm_wind_hours", "storm_events",
                   "hot_days", "wsdi"))
      ggsave(filename = out_file, plot = p, width = 12, height = 9, dpi = 150,
            create.dir = TRUE)
      out_file
    },
    format = "file",
    deployment = "main"
  ),

  # 5b. Lake water quality synthesis ----
  # Inputs and summaries for website/lake-water-quality-synthesis.qmd.
  #* River WQ (BOPRC, 12 sites, 1984-2026) ----
  tar_target(
    river_wq_file,
    file.path(data_raw_dir, "flows", "BOPRC - River WQ Data Rotorua.xlsx"),
    format = "file"
  ),
  tar_target(river_wq_sites, read_river_wq_sites(river_wq_file)),
  tar_target(river_wq_data, read_river_wq(river_wq_file)),

  #* Extended lake chemistry and CTD (PC10 Jun-26 workbooks, to 2026-06) ----
  tar_target(
    lake_chem_2026_file,
    file.path(pc10_data_dir, "Rotorua S2 & S5 data 1989_2026.xlsx"),
    format = "file"
  ),
  tar_target(lake_chem_2026, read_lake_chem_2026(lake_chem_2026_file)),
  tar_target(
    lake_ctd_2026_file,
    file.path(pc10_data_dir, "Lake Rotorua S2 & S5 Profile data 2001_2026.xlsx"),
    format = "file"
  ),
  tar_target(lake_ctd_2026, read_lake_ctd_2026(lake_ctd_2026_file)),

  #* Alum: daily Al dose (2006-2026), 2022-26 dose rates, Puarenga compliance ----
  tar_target(
    alum_2026_file,
    file.path(data_raw_dir, "Alum",
              "Lake Rotorua alum dosing data Jan 2022 to Jul 2026 incl Puarenga DRP data .xlsx"),
    format = "file"
  ),
  tar_target(alum_dose_2026, read_alum_dose_2026(alum_2026_file)),
  tar_target(puarenga_compliance, read_puarenga_compliance(alum_2026_file)),
  tar_target(alum_al_daily, read_alum_al_kg(alum_dosing_file)),

  #* Lake chemistry summaries ----
  tar_target(wq_lake_long, lake_chem_long(lake_chem_2026)),
  tar_target(wq_periods, list(
    full = c("1990-07-01", "2026-06-30"),
    pre_alum = c("1990-07-01", "2006-06-30"),
    alum_era = c("2006-07-01", "2026-06-30"),
    last_10y = c("2016-07-01", "2026-06-30")
  )),
  tar_target(wq_annual, annual_summary(wq_lake_long, site, layer, variable)),
  tar_target(wq_climatology, seasonal_climatology(wq_lake_long, site, layer, variable)),
  tar_target(wq_trends, trend_table(wq_lake_long, site, layer, variable,
                                    periods = wq_periods)),
  tar_target(
    wq_changepoints,
    wq_lake_long |>
      dplyr::filter(variable %in% c("tp", "tn", "drp", "nh4", "nnn", "chla", "secchi", "pp", "org_n")) |>
      dplyr::group_by(site, layer, variable) |>
      dplyr::group_modify(\(d, k) detect_changepoints(d$date, d$value)) |>
      dplyr::ungroup()
  ),
  tar_target(lake_tli_annual, tli_annual(lake_chem_2026)),

  #* Profiles: CTD (2003-2026) and buoys (2007-2026) ----
  tar_target(
    ctd_cast_metrics,
    lake_ctd_2026 |>
      dplyr::filter(var %in% c("temp_c", "do_mgl", "chl", "turb")) |>
      dplyr::select(site, date, depth_m, var, value) |>
      tidyr::pivot_wider(names_from = var, values_from = value, values_fn = mean) |>
      dplyr::mutate(source = site) |>
      profile_daily_metrics(surf_max = 1, bot_min = 18) |>
      dplyr::rename(site = source)
  ),
  tar_target(
    buoy_profile_long, {
      dplyr::bind_rows(
        old_buoy_wq |> dplyr::mutate(source = "old buoy"),
        aeme_buoy_data |> dplyr::mutate(source = "new buoy")
      ) |>
        dplyr::filter(var_aeme %in% c("HYD_temp", "CHM_oxy"), is.finite(value)) |>
        dplyr::transmute(source, date = as.Date(Date), depth_m = depth,
                         var = c(HYD_temp = "temp_c", CHM_oxy = "do_mgl")[var_aeme], value) |>
        tidyr::pivot_wider(names_from = var, values_from = value, values_fn = mean)
    }
  ),
  tar_target(buoy_daily_metrics, profile_daily_metrics(buoy_profile_long)),
  tar_target(
    buoy_events, {
      d <- buoy_daily_metrics |>
        dplyr::group_by(date) |>
        dplyr::summarise(dplyr::across(c(t_surf, t_bot, do_bot, d_temp), \(x) mean(x, na.rm = TRUE)),
                         .groups = "drop") |>
        dplyr::mutate(dplyr::across(-date, \(x) ifelse(is.nan(x), NA_real_, x)))
      hw <- detect_heatwaves(d$date, d$t_surf)
      list(
        daily = d,
        heatwaves = hw$events,
        heatwave_daily = hw$daily,
        anoxia = run_events(d$date, d$do_bot < 2, min_len = 3, max_gap = 2),
        hypoxia = run_events(d$date, d$do_bot < 4, min_len = 3, max_gap = 2),
        stratified = run_events(d$date, d$d_temp > 1, min_len = 3, max_gap = 2)
      )
    }
  ),

  #* Thermal structure and internal-load flux estimates ----
  tar_target(lake_hyps, hyps_below_surface(corr_hyps)),
  tar_target(
    ctd_thermal,
    lake_ctd_2026 |>
      dplyr::filter(var == "temp_c") |>
      dplyr::transmute(site, date, depth_m, temp_c = value) |>
      thermal_structure(lake_hyps, site)
  ),
  tar_target(
    buoy_thermal,
    buoy_profile_long |>
      dplyr::select(source, date, depth_m, temp_c) |>
      thermal_structure(lake_hyps, source)
  ),
  tar_target(hyp_flux, hypolimnetic_flux(wq_lake_long, ctd_thermal, lake_hyps)),
  tar_target(
    hod_events, {
      th <- buoy_thermal |>
        dplyr::group_by(date) |>
        dplyr::summarise(thermo_depth = mean(thermo_depth, na.rm = TRUE), .groups = "drop")
      oxygen_depletion(buoy_events$daily, th, buoy_events$stratified, lake_hyps)
    }
  ),

  #* Meteorology events and annual summaries (ERA5 bias-corrected, 1976-) ----
  tar_target(
    met_events, {
      d <- era5_corrected_daily_baseline |>
        dplyr::transmute(date = as.Date(Date), tair = MET_tmpair,
                         rain_mm = MET_pprain * 1000, wind = MET_wndspd)
      hw <- detect_heatwaves(d$date, d$tair, base_years = 1991:2020)
      cs <- detect_heatwaves(d$date, -d$tair, base_years = 1991:2020)  # cold spells
      calm_thr <- stats::quantile(d$wind, 0.1, na.rm = TRUE)
      list(
        daily = d,
        storms = detect_storms(d$date, d$rain_mm, window = 3, pct = 0.99, wind = d$wind),
        storms_1day = detect_storms(d$date, d$rain_mm, window = 1, pct = 0.995, wind = d$wind),
        air_heatwaves = hw$events,
        cold_spells = cs$events,
        calm_spells = run_events(d$date, d$wind < calm_thr, min_len = 4),
        calm_threshold = calm_thr
      )
    }
  ),
  tar_target(met_annual_era5, met_annual(era5_corrected_daily_baseline)),

  #* PC10 2026 modelled loads ----
  tar_target(
    pc10_loads_central_files,
    list.files(here::here("data", "raw", "pc10_loads_2026", "alum_storm_central"),
               full.names = TRUE),
    format = "file"
  ),
  tar_target(
    pc10_check_loads,
    readr::read_csv(here::here("data", "raw", "pc10_loads_2026", "check_annual_loads.csv"),
                    show_col_types = FALSE)
  ),
  tar_target(
    pc10_stream_loads, {
      pc10_loads_central_files  # dependency
      # outflow_ohau.csv holds flow only (no concentrations), so it is not a load
      inflow_annual_loads(read_pc10_loads("alum_storm_central", include_rain = TRUE))
    }
  ),
  tar_target(
    pc10_daily_flow, {
      pc10_loads_central_files  # dependency
      dir <- here::here("data", "raw", "pc10_loads_2026", "alum_storm_central")
      rd <- function(f, nm) readr::read_csv(file.path(dir, f), show_col_types = FALSE) |>
        dplyr::transmute(date = as.Date(time), inflow = nm, flow_m3s = FLOW)
      key <- pc10_inflow_key()
      dplyr::bind_rows(
        lapply(seq_len(nrow(key)), \(i) rd(key$file[i], key$name[i])),
        rd("inflow_all_combined.csv", "All inflows"),
        rd("outflow_ohau.csv", "Ohau outflow"))
    }
  ),

  #* River WQ summaries ----
  tar_target(
    river_wq_annual,
    river_wq_data |>
      dplyr::filter(variable %in% c("TN", "NNN", "NH4-N", "TP", "DRP", "TSS", "Water Temp")) |>
      annual_summary(site_id, location, variable, min_months = 4)
  ),
  tar_target(
    river_wq_trends,
    river_wq_data |>
      dplyr::filter(variable %in% c("TN", "NNN", "NH4-N", "TP", "DRP", "TSS")) |>
      trend_table(location, variable, periods = wq_periods)
  ),

  #* Annual driver - lake response table and correlations ----
  tar_target(
    driver_lake_annual, {
      lake <- wq_annual |>
        dplyr::filter(layer == "INT", variable %in% c("tp", "tn", "drp", "nh4", "nnn", "chla", "secchi")) |>
        dplyr::group_by(hy, variable) |>
        dplyr::summarise(value = mean(mean), .groups = "drop") |>
        tidyr::pivot_wider(names_from = variable, values_from = value, names_prefix = "lake_")
      bot <- wq_annual |>
        dplyr::filter(layer == "BOT", variable %in% c("tp", "nh4", "drp")) |>
        dplyr::group_by(hy, variable) |>
        dplyr::summarise(value = mean(max), .groups = "drop") |>
        tidyr::pivot_wider(names_from = variable, values_from = value, names_prefix = "bot_max_")
      tli <- lake_tli_annual |>
        dplyr::filter(site == "Lake (mean of sites)") |>
        dplyr::select(hy, tli3)
      ctd <- ctd_cast_metrics |>
        dplyr::mutate(hy = wq_hy(date)) |>
        dplyr::group_by(hy) |>
        dplyr::summarise(ctd_min_do_bot = min(do_bot, na.rm = TRUE),
                         ctd_frac_hypoxic = mean(do_bot < 2, na.rm = TRUE), .groups = "drop") |>
        dplyr::filter(is.finite(ctd_min_do_bot))
      loads <- pc10_check_loads |>
        dplyr::filter(scenario == "alum_storm_central") |>
        dplyr::select(hy, load_tn = tn, load_tp = tp, load_frp = frp)
      flow <- pc10_stream_loads |>
        dplyr::filter(!inflow %in% c("Rainfall")) |>
        dplyr::group_by(hy) |>
        dplyr::summarise(inflow_Mm3 = sum(flow), load_pp = sum(pp), .groups = "drop")
      alum <- alum_al_daily |>
        dplyr::mutate(hy = wq_hy(date)) |>
        dplyr::group_by(hy) |>
        dplyr::summarise(alum_t_al = sum(total_kg_al) / 1000, .groups = "drop")
      lake |>
        dplyr::full_join(bot, by = "hy") |>
        dplyr::full_join(tli, by = "hy") |>
        dplyr::full_join(ctd, by = "hy") |>
        dplyr::left_join(met_annual_era5, by = "hy") |>
        dplyr::left_join(loads, by = "hy") |>
        dplyr::left_join(flow, by = "hy") |>
        dplyr::left_join(alum, by = "hy") |>
        dplyr::mutate(alum_t_al = dplyr::coalesce(alum_t_al, 0)) |>
        dplyr::filter(hy >= 1991, hy <= 2026) |>
        dplyr::arrange(hy)
    }
  ),
  tar_target(
    driver_lake_correlations,
    driver_correlations(
      driver_lake_annual,
      responses = c("lake_tp", "lake_tn", "lake_drp", "lake_nh4", "lake_chla",
                    "lake_secchi", "tli3", "bot_max_nh4", "bot_max_drp", "ctd_min_do_bot"),
      drivers = c("tair", "tair_djf", "wind", "wind_djf", "swr_djf", "rain_mm",
                  "inflow_Mm3", "load_tn", "load_tp", "load_frp", "load_pp", "alum_t_al"),
      lags = 0:1)
  )

  # 6. Reporting / Quarto rendering

)
