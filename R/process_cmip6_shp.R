process_cmip6_shp <- function(x, variable = NULL, vcsn_grid_points, metadata, 
                              out_dir, overwrite = FALSE) {
  # x <- mapedit::drawFeatures()
  # saveRDS(x, here::here("data", "processed", "rotorua_area.rds"))
  
  # out_dir <- here::here("data", "processed", "rotorua_cmip6")
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  metadata <- metadata |> 
    dplyr::arrange(gcm, scenario)
  if (!is.null(variable)) {
    metadata <- metadata |> 
      dplyr::filter(variable %in% !!variable)
  }
  results <- vector("list", nrow(metadata))
  max_files <- nrow(metadata)
  for (i in seq_len(nrow(metadata))) {
    
    outfile <- here::here(out_dir, metadata$filename[i])
    if (file.exists(outfile) & !overwrite) {
      cli::cli_alert_info("File {outfile} exists, skipping")
      results[[i]] <- list(
        filename = metadata$filename[i],
        outfile  = outfile,
        ok       = TRUE,
        message  = "skipped"
      )
      next
    }
    cli::cli_alert_info("Processing {metadata$filename[i]}")
    
    file <- file.path("Z:", metadata$filename[i])
    
    new_outfile <- process_cmip6(x = x, vcsn_grid_points = vcsn_grid_points,
                                 file = file, outfile = outfile)
    
    ok <- !is.null(new_outfile)
    
    results[[i]] <- list(
      filename = metadata$filename[i],
      outfile  = outfile,
      ok       = ok,
      message  = if (ok) "success" else "failed"
    )
    
    if (ok) {
      cli::cli_alert_success("Saved processed file to {outfile}
                          ({i} of {max_files})")
    } else {
      cli::cli_alert_danger("Processing failed for {metadata$filename[i]}")
    }
  }
  results_df <- dplyr::bind_rows(results)
  return(results_df)
}

# source(here::here("R", "process_cmip6.R"))
# rotorua_area <- readRDS(here::here("data", "processed", "rotorua_area.rds"))
# vcsn_grid_points <- sf::st_read(here::here("data", "processed",
#                                            "vcsn_grid_points.gpkg"))
# metadata <- readr::read_csv(here::here("data", "processed",
#                                        "niwa_cmip6_metadata.csv"), 
#                             col_types = readr::cols())
# 
# variable <- c("hurs", "pr", "rsds", "sfcWind", "tas")
# process_cmip6_shp(x = rotorua_area, variable = variable,
#                   vcsn_grid_points = vcsn_grid_points,
#                   metadata = metadata,
#                   out_dir = here::here("data", "processed", "rotorua_cmip6"))
