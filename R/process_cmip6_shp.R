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
  # results <- vector("list", nrow(metadata))
  # max_files <- nrow(metadata)
  outfile <- here::here(out_dir, metadata$filename)
  if (file.exists(outfile) & !overwrite) {
    cli::cli_alert_info("File {outfile} exists, skipping")
    return(outfile)
  }
  cli::cli_alert_info("Processing {metadata$filename}")
  
  file <- file.path("Z:", metadata$filename)
  
  new_outfile <- process_cmip6(x = x, vcsn_grid_points = vcsn_grid_points,
                               file = file, outfile = outfile)
  return(new_outfile)
  
  # tst <- stars::read_ncdf(new_outfile)

}
