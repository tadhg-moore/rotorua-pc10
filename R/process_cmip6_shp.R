process_cmip6_shp <- function(x, vcsn_grid_points, metadata, out_dir, 
                              overwrite = FALSE) {
  # x <- mapedit::drawFeatures()
  # saveRDS(x, here::here("data", "processed", "rotorua_area.rds"))
  
  # out_dir <- here::here("data", "processed", "rotorua_cmip6")
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  metadata <- metadata |> 
    dplyr::arrange(gcm, scenario)
  
  for (i in seq_len(nrow(metadata))) {
    
    outfile <- here::here(out_dir, metadata$filename[i])
    if (file.exists(outfile) & !overwrite) {
      cli::cli_alert_info("File {outfile} exists, skipping")
      next
    }
    cli::cli_alert_info("Processing {metadata$filename[i]}")
    
    file <- file.path("Z:", metadata$filename[i])
    
    outfile <- process_cmip6(x = x, vcsn_grid_points = vcsn_grid_points,
                             file = file, outfile = outfile)
    
    cli::cli_alert_success("Saved processed file to {outfile}")
  }
  
}

source(here::here("R", "process_cmip6.R"))
rotorua_area <- readRDS(here::here("data", "processed", "rotorua_area.rds"))
vcsn_grid_points <- sf::st_read(here::here("data", "processed",
                                           "vcsn_grid_points.gpkg"))
cmip6_metadata <- readr::read_csv(here::here("data", "processed",
                                             "niwa_cmip6_metadata.csv"), 
                                  col_types = readr::cols())
process_cmip6_shp(x = rotorua_area,
                  vcsn_grid_points = vcsn_grid_points,
                  metadata = cmip6_metadata,
                  out_dir = here::here("data", "processed", "rotorua_cmip6"))
