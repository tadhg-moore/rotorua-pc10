source("https://raw.githubusercontent.com/limnotrack/f_rotorua/refs/heads/main/R/qc_funs.R")

get_rotorua_data_product_zip <- function() {
  
  dest <- here::here("data", "processed")
  piggyback::pb_download(
    file = "rotorua_data_qc.zip",
    dest = dest,
    repo = "limnotrack/f_rotorua",
    tag = "v0.0.1"
  )
  
  outfile <- file.path(dest, "rotorua_data_qc.zip")
  
  return(outfile)
}

unzip_rotorua_data_product <- function(zip_file) {

  out_dir <- here::here("data", "processed", "rotorua_data_qc")
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  unzip(zip_file, exdir = out_dir)
  
  return(out_dir)
}

get_rotorua_data_product <- function(out_dir) {
  
  files <- list.files(out_dir, pattern = "\\.csv$", recursive = TRUE, 
                      full.names = TRUE)
  out_names <- gsub(".csv", "", basename(files))
  
  out <- lapply(files, readr::read_csv, col_types = readr::cols())
  names(out) <- out_names
  
  
  data <- out$rotorua_qc |> 
    tidyr::pivot_longer(
      cols = matches("^(qc_value|qc_code|qc_flag)_"),
      names_to = c(".value", "var_ref_id"),
      names_pattern = "^(qc_value|qc_code|qc_flag)_(.+)$"
    )
  
  # Map site devices to data
  data <- data |> 
    map_data_to_devices(site_devices = out$site_devices,
                        device_var = out$device_var,
                        device_position = out$device_position,
                        variables = out$variable_ref
    ) 
  return(data)
}

get_rotorua_data_product_met <- function(data) {
  met <- data |> 
    dplyr::filter(grepl("t_air|pr_baro|h_rh|w_spd|w_dir|pp_rain", var_abbr),
                  !is.na(qc_value))
  met <- met |> 
    dplyr::select(datetime, var_abbr, qc_value) |> 
    tidyr::pivot_wider(names_from = var_abbr, values_from = qc_value)
  
  met2 <- met |> 
    AEME::standardise_met()
  
}