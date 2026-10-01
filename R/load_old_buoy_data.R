load_old_buoy_data <- function(zip_folder) {
  
  # unzip the zip folder
  target_dir <- tempdir()
  unzip(zip_folder, exdir = target_dir)
  dir <- file.path(target_dir, tools::file_path_sans_ext(basename(zip_folder)))
  
  # list all the files in the unzipped folder
  files <- list.files(dir, full.names = TRUE)
  
  # Load all the data
  site <- readr::read_csv(file.path(dir, "sites.csv"),
                          col_types = readr::cols())
  site_events <- readr::read_csv(file.path(dir, "site_events.csv"),
                                 col_types = readr::cols())
  site_devices <- readr::read_csv(file.path(dir, "site_devices.csv"), 
                                  col_types = readr::cols())
  device_var <- readr::read_csv(file.path(dir, "device_variable.csv"), 
                                col_types = readr::cols())
  device_position <- readr::read_csv(file.path(dir, "device_position.csv"), 
                                     col_types = readr::cols())
  sensor_reference <- readr::read_csv(file.path(dir, "sensor_reference.csv"), 
                                      col_types = readr::cols())
  sensor_calibrations <- readr::read_csv(file.path(dir, "sensor_calibrations.csv"),
                                         col_types = readr::cols())
  sensor_scaling <- readr::read_csv(file.path(dir, "sensor_scaling.csv"), 
                                    col_types = readr::cols())
  variable_ref <- readr::read_csv(file.path(dir, "variables.csv"), 
                                  col_types = readr::cols())
  qc_filters <- readr::read_csv(file.path(dir, "qc_filters.csv"), 
                                col_types = readr::cols())
  
  data_wide <- readr::read_csv(file.path(dir, "rotorua_qc.csv"), show_col_types = FALSE) |> 
    dplyr::mutate(
      datetime = lubridate::force_tz(datetime, tzone = "Etc/GMT-12")
    )
  
  data <- data_wide |> 
    tidyr::pivot_longer(
      cols = tidyselect::matches("^(qc_value|qc_code|qc_flag)_"),
      names_to = c(".value", "var_ref_id"),
      names_pattern = "^(qc_value|qc_code|qc_flag)_(.+)$"
    )
  
  # Map site devices to data
  data <- data |> 
    map_data_to_devices(site_devices = site_devices,
                        device_var = device_var,
                        device_position = device_position,
                        variables = variable_ref
    ) 
  head(data)
  return(data)
}

old_buoy_met_data <- function(data, height = 1.5) {
  met_vars <- c("h_rh", "pr_baro", "r_clrsky", "r_swd", "t_air", "w_dir",
                "w_gust", "w_spd", "pp_rain")
  met <- data |> 
    # dplyr::mutate(
    #   datetime = lubridate::with_tz(datetime, tzone = "UTC")
    # ) |>
    dplyr::filter(var_abbr %in% met_vars, !is.na(qc_value)) |>
    dplyr::select(datetime, var_abbr, qc_value) |>
    tidyr::pivot_wider(names_from = var_abbr, values_from = qc_value)
  
  met <- met |> 
    dplyr::mutate(
      MET_wndspd = w_spd * 0.514444, # convert knots to m/s
      MET_wndspd = metscale::wind_at_height(MET_wndspd, from = height, 10),
      pr_baro = (pr_baro * 100), # - 3509
      pr_baro = dplyr::case_when(
        datetime > as.POSIXct("2008-06-15") ~ pr_baro - 3509,
      ),
      pp_rain = pp_rain / 1000
    ) |> 
    dplyr::select(-w_spd) |>
    dplyr::rename(
      Date = datetime,
      MET_tmpair = t_air,
      MET_humrel = h_rh,
      MET_pprain = pp_rain,
      MET_prsttn = pr_baro,
      MET_wnddir = w_dir
    )
  return(met)
}
