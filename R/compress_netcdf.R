infile <- "C:/Users/mooret/Downloads/hurs_historical_ACCESS-CM2_CCAM_daily_NZ5km_raw.nc"
outfile <- "C:/Users/mooret/Downloads/hurs_historical_ACCESS-CM2_CCAM_daily_NZ5km_compressed.nc"

compress_netcdf <- function(infile, outfile, overwrite = FALSE) {
  r <- terra::rast(infile)
  
  # Write to a new NetCDF with internal DEFLATE compression
  t0 <- Sys.time()
  terra::writeCDF(
    r,
    filename = outfile,
    # filetype = "CDF",
    overwrite = overwrite,
    gdal = c("COMPRESS=DEFLATE", "ZLEVEL=4", "SHUFFLE=YES")
  )
  t1 <- Sys.time()
  tdiff <- round(difftime(t1, t0, units = "mins"), 1)
  cli::cli_alert_success("Compression completed in {tdiff} minutes.")
  
  invisible(outfile)
}
