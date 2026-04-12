source("R/get_point_data.R")

cmip_vars <- c("tas", "hurs", "pr", "rsds", "sfcWind")
cmip6_metadata <- readr::read_csv("data/processed/cmip6_gcm_metadata.csv")
cmip6_files <- list.files("data/processed/rotorua_lakes_cmip6/", full.names = TRUE)

gcm_scenario_var <- cmip6_metadata |> 
  dplyr::select(gcm, scenario) |> 
  dplyr::distinct() |> 
  tidyr::crossing(variable = cmip_vars)

# CMIP6 data combinations (170 files)
gcm_scenario_var


# Directory to write files
out_dir <- "data/processed/cmip6_point_data/"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Extraction point details Rotorua, New Zealand
lat <- -38.1368
lon <- 176.2497
lakename <- "Rotorua"
method <- "simple" # Can be "simple" or "bilinear"
overwrite <- FALSE # Set to TRUE to overwrite existing files, FALSE to skip if file exists

# Loop through each GCM x SSP combination and extract point data for each variable
for (i in seq_len(nrow(cmip6_metadata))) {
  out_file <- file.path(out_dir, paste0(lakename, "_", cmip6_metadata$gcm[i], 
                                        "_", cmip6_metadata$scenario[i], 
                                        "_point_data.csv")) |> 
    tolower() # Good practice to use lowercase for file names
  
  
  if (file.exists(out_file) && !overwrite) {
    message(paste("File already exists, skipping:", out_file))
    next
  }
  
  
  df <- get_point_data(lat = lat, lon = lon, lakename = lakename, 
                       gcm = cmip6_metadata$gcm[i], 
                       scenario = cmip6_metadata$scenario[i], 
                       cmip_vars = cmip_vars, cmip6_files = cmip6_files,
                       cmip6_metadata = cmip6_metadata, method = method)
  
  # df is a data.frame with columns: date_char, value, variable, gcm, scenario
  
  # Save the extracted point data to a CSV file
  readr::write_csv(df, out_file)
  message("Saved point data to:", out_file, " [", format(Sys.time()), "]")
}
