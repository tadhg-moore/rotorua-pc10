load_nc <- function(file) {
  file_name_splt <- strsplit(basename(file), "_")[[1]]
  sel_var <- file_name_splt[1]
  scen <- file_name_splt[2]
  r <- terra::rast(file)
  if (sel_var == "tas") {
    r <- r - 273.15
  }
  return(r)
}

calc_pr_mean <- function(r) {
  
  # First calculate annual sum
  years <- unique(format(terra::time(r), "%Y"))
  annual_sums <- lapply(years, function(yr) {
    time_vals <- terra::time(r)
    time_idx <- which(format(time_vals, "%Y") == yr)
    r_subset <- terra::subset(r, time_idx)
    annual_sum <- terra::app(r_subset, fun = sum, na.rm = TRUE)
    return(annual_sum)
  })
  
  annual_stack <- terra::rast(annual_sums)
  # Then calculate mean of annual sums
  mean_annual_sum <- terra::app(annual_stack, fun = mean, na.rm = TRUE)
  # terra::plot(mean_annual_sum, main = "Mean Annual Precipitation (historical)")
  return(mean_annual_sum)
  
}
