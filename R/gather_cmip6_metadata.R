gather_cmip6_metadata <- function(out_dir = here::here("data", "processed"), 
                                  shared_drive = "Z:") {
  # Dynamical downscaling CMIP6 models over New Zealand: added value of climatology and extremes (2024)
  # https://link.springer.com/article/10.1007/s00382-024-07337-5
  
  
  
  fils <- list.files(shared_drive)
  # out_dir <- here::here("data", "processed")
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Extract metadata from filenames "variable_scenario_model_region_timestep_spatialres_biascorrection.nc"
  fils_metadata <- fils |> 
    tibble::as_tibble() |> 
    dplyr::mutate(basename = tools::file_path_sans_ext(value)) |>
    tidyr::separate(basename, into = c("variable", "scenario", "gcm", "model", 
                                       "timestep", "spatial_res", "biascorrection"), 
                    sep = "_", remove = FALSE) |> 
    dplyr::rename(filename = value)
  
  fils_metadata |> 
    # dplyr::filter(biascorrection == "bc") |> 
    dplyr::distinct(variable, biascorrection, spatial_res) 
  
  # https://climatedata.environment.govt.nz/ 
  csv_file <- "https://climatedata.environment.govt.nz/data/core_public_data/netcdf_file_manifest.csv"
  manifest <- readr::read_csv(csv_file, col_types = readr::cols())
  meta_vars <- manifest |> 
    dplyr::filter(data_type == "base_data") |> 
    dplyr::select(variable_name, variable_name_long, variable_units) |>
    dplyr::distinct()
  
  # Get Daily data metadata
  url <- "https://climatedata.environment.govt.nz/daily-data.html"
  
  # Scrape two tables
  page <- rvest::read_html(url)
  daily_vars <- page |> 
    rvest::html_nodes("table") |> 
    rvest::html_table() |> 
    dplyr::bind_rows() |> 
    dplyr::rename(variable_name = "Variable ID",
                  daily_variable_name_long = "Description"
                  ) 
  
  # GCM calendars
  # Open and check all netCDF calendars
  fils_metadata <- fils_metadata |> 
    dplyr::arrange(gcm) |>
    dplyr::mutate(calendar = NA_character_, units = NA_character_)
  for (i in seq_len(nrow(fils_metadata))) {
    print(i)
    var <- fils_metadata$variable[i]
    file <- file.path(shared_drive, fils_metadata$filename[i])
    if (!is.na(fils_metadata$calendar[i]) | var == "PETsrad") {
      next
    }
    if (!file.exists(file)) {
      cli::cli_alert_warning("File {file} does not exist, skipping")
      next
    }
    nc <- ncdf4::nc_open(file, readunlim = FALSE)
    time_calendar <- ncdf4::ncatt_get(nc, varid = "time", attname = "calendar")$value
    fils_metadata$units[i] <- ncdf4::ncatt_get(nc, varid = fils_metadata$variable[i], attname = "units")$value
    fils_metadata$calendar[i] <- time_calendar
    ncdf4::nc_close(nc)
  }
  
  fils_metadata |> 
    dplyr::distinct(gcm, variable, calendar, units)
  
  # Conformal Cubic Atmospheric Model (CCAM) CCAM https://research.csiro.au/ccam/
  metadata <- fils_metadata |> 
    dplyr::left_join(meta_vars, by = c("variable" = "variable_name")) |> 
    dplyr::left_join(daily_vars, by = c("variable" = "variable_name")) |> 
    dplyr::mutate(
      variable_name_long = dplyr::if_else(is.na(variable_name_long), 
                                          daily_variable_name_long, 
                                          variable_name_long)
    )
  
  daily_meta <- fils_metadata |> 
    dplyr::filter(variable %in% daily_vars$variable_name) |>
    dplyr::left_join(daily_vars, by = c("variable" = "variable_name"))
  
  metadata |> 
    dplyr::distinct(gcm, scenario)
  
  out_file <- here::here(out_dir, "niwa_cmip6_metadata.csv")
  readr::write_csv(metadata, out_file)
  return(metadata)
}





# parquet_file <- "https://climatedata.environment.govt.nz/data/core_public_data/aggregates/parquet/CMIP6_Climate_Projections_All_Models_FPs_Regional_Council_2025_07_30.parquet"
# parquet_file <- "https://climatedata.environment.govt.nz/data/core_public_data/vector/NorESM2-MM_raw_vector_data_wf.parquet"
# 
# data <- arrow::read_parquet(parquet_file)
# 
# 
# 
# 
# 
# # Scrape html table from online:
# url <- "https://link.springer.com/article/10.1007/s00382-024-07337-5/tables/1"
# page <- rvest::read_html(url)
# table <- page |> 
#   rvest::html_node("table") |> 
#   rvest::html_table()
