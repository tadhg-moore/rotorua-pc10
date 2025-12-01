summarise_gcm_ts <- function(variable, gcm, files) {
  # gcms <- c("ACCESS-CM2", "AWI-CM-1-1-MR", "CNRM-CM6-1", "EC-Earth3", 
  #           "GFDL-ESM4", "NZESM", "NorESM2-MM")
  # Separate historical and scenario files
  historical_files <- files[grepl(paste0("^", variable, "_"), basename(files)) &
                             grepl("historical", files) & grepl(gcm, files)]
  scenario_files <- files[grepl(paste0("^", variable, "_"), basename(files)) &
                            grepl("ssp", files) & grepl(gcm, files)]
  
  fun <- ifelse(variable == "pr", sum, mean)
  
  hist_all <- lapply(gcm, \(g) {
    historical_file <- historical_files[grepl(g, historical_files)]
    # Load historical data
    hist <- load_nc(historical_file)
    hist_mean <- fun(terra::values(hist), na.rm = TRUE)
    
    # Calculate annual means
    hist_time <- terra::time(hist)
    hist_years <- unique(format(hist_time, "%Y"))
    
    hist_annual_means <- sapply(hist_years, function(yr) {
      time_idx <- which(format(hist_time, "%Y") == yr)
      r_subset <- terra::subset(hist, time_idx)
      mean_val <- fun(terra::values(r_subset), na.rm = TRUE)
      return(mean_val)
    })
    
    hist_df <- data.frame(
      year = as.integer(hist_years),
      value = hist_annual_means,
      scenario = "Historical",
      gcm = g
    )
    return(hist_df)
  }) |> 
    dplyr::bind_rows()
  
  
  scen_all <- lapply(gcm, \(g) {
    sub_files <- scenario_files[grepl(g, scenario_files)]
    # Load scenario data and calculate annual means
    scen_dfs <- lapply(sub_files, function(f) {
      
      scen_label <- strsplit(basename(f), "_")[[1]][2]
      scen <- load_nc(f)
      scen_time <- terra::time(scen)
      scen_years <- unique(format(scen_time, "%Y"))
      
      scen_annual_means <- sapply(scen_years, function(yr) {
        time_idx <- which(format(scen_time, "%Y") == yr)
        r_subset <- terra::subset(scen, time_idx)
        mean_val <- fun(terra::values(r_subset), na.rm = TRUE)
        return(mean_val)
      })
      
      df <- data.frame(
        year = as.integer(scen_years),
        value = scen_annual_means,
        scenario = scen_label,
        gcm = g
      )
      return(df)
    }) |> 
      dplyr::bind_rows()
  }) |> 
    dplyr::bind_rows()
  
  
  
  all_df <- dplyr::bind_rows(hist_all, scen_all) |> 
    dplyr::select(year, value, scenario, gcm)
  return(all_df)
}
