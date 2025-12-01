summarise_gcm_spatial <- function(variable = "tas", gcm = "ACCESS-CM2_CCAM",
                                  time_periods = list(
                                    "2015-2040" = c(2015, 2040),
                                    "2041-2070" = c(2041, 2070),
                                    "2071-2100" = c(2071, 2100)
                                  ), files) {
  # dir <- here::here("data", "processed", "rotorua_cmip6")
  # Example file name: "tas_historical_ACCESS-CM2_CCAM_daily_NZ5km_bc.nc"
  # files <- list.files(dir, paste0("^", variable, "_", ".*_", gcm, ".*\\.nc$"),
  #                     full.names = TRUE)
  # files <- files[grepl(paste0("^", variable, "_"), basename(files)) &
  #                  grepl(gcm, basename(files))]
  historical_file <- files[grepl(paste0("^", variable, "_"), basename(files)) &
                             grepl("historical", files) & grepl(gcm, files)]
  
  if (length(historical_file) == 0) {
    cli::cli_abort("No historical file found for variable '{variable}' and GCM '{gcm}'")
  } else if (length(historical_file) > 1) {
    cli::cli_warn("Multiple historical files found for variable '{variable}' and GCM '{gcm}'. Using the first one.")
    historical_file <- historical_file[1]
  }
  
  scenario_files <- files[grepl(paste0("^", variable, "_"), basename(files)) &
                            grepl("ssp", files) & grepl(gcm, files)]
  scenarios <- unique(gsub(paste0(variable, "_(ssp[0-9]+)_", gcm, ".*\\.nc$"), 
                           "\\1", basename(scenario_files))) |> toupper()
  
  hist <- load_nc(historical_file)
  if (variable == "pr") {
    hist_mean <- calc_pr_mean(hist)
  } else {
    hist_mean <- terra::app(hist, fun = mean, na.rm = TRUE)
  }
  terra::plot(hist_mean, main = paste0(variable, " mean for ", gcm))
  hist_df <- as.data.frame(hist_mean, xy = TRUE) |> 
    dplyr::rename(lon = x, lat = y, value = mean) |> 
    dplyr::mutate(
      scenario = "Historical",
      gcm = gcm,
      period = "1960-2014"
    )
  
  # future::plan(future::multisession)
  # scens <- future.apply::future_lapply(scenario_files, \(f) {
  scens <- lapply(scenario_files, \(f) {
    scenario_label <- strsplit(basename(f), "_")[[1]][2]
    scen <- load_nc(f)
    # Split into 3 time periods and calculate change from historical mean
    scen_means <- lapply(time_periods, \(tp) {
      start_year <- tp[1]
      end_year <- tp[2]
      time_vals <- terra::time(scen)
      time_idx <- which(format(time_vals, "%Y") >= start_year & 
                          format(time_vals, "%Y") <= end_year)
      scen_subset <- terra::subset(scen, time_idx)
      if (variable == "pr") {
        scen_mean <- calc_pr_mean(scen_subset)
      } else {
        scen_mean <- terra::app(scen_subset, fun = mean, na.rm = TRUE)
      }
      df <- as.data.frame(scen_mean, xy = TRUE) |> 
        dplyr::rename(lon = x, lat = y, value = mean) |> 
        dplyr::mutate(
          scenario = toupper(scenario_label),
          gcm = gcm,
          period = paste0(start_year, "-", end_year)
        )
      return(df)
    }) |> 
      dplyr::bind_rows()
  }) |> 
    dplyr::bind_rows()
  
  all_df <- dplyr::bind_rows(hist_df, scens) |> 
    dplyr::mutate(variable = variable)
  return(all_df)
}
