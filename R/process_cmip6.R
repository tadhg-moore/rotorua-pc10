process_cmip6 <- function(x, vcsn_grid_points, file, outfile) {
  # x <- mapedit::drawFeatures()
  
  # file <- list.files(
  #   path = shared_drive,
  #   pattern = paste0(var,"_", scenario, "_", gcm, ".*\\.nc$"),
  #   full.names = TRUE
  # )
  # 
  # if (length(files) == 0) {
  #   cli::cli_abort("No files found for the specified scenario, and GCM.")
  # }
  # if (length(files) > 1) {
  #   cli::cli_abort("Multiple files found for the specified variable, scenario, and GCM.")
  # }
  if (!file.exists(file)) {
    cli::cli_abort("File {file} does not exist.")
  }
  
  # Make sure x is in WGS84
  if (sf::st_crs(x)$epsg != 4326) {
    x <- sf::st_transform(x, crs = 4326)
  }
  
  # Ensure vcsn_grid_points is in WGS84
  if (sf::st_crs(vcsn_grid_points)$epsg != 4326) {
    vcsn_grid_points <- sf::st_transform(vcsn_grid_points, crs = 4326)
  }
  
  sel_grid_points <- sf::st_intersection(vcsn_grid_points, x)
  coords <- sel_grid_points |> 
    sf::st_drop_geometry() |> 
    dplyr::select(coord_index) |> 
    tidyr::separate(col = coord_index, into = c("x", "y"), sep = "_", 
                    convert = TRUE)
  x_start <- min(coords$x)
  x_count <- max(coords$x) - x_start + 1
  x_end <- x_start + x_count - 1
  y_start <- min(coords$y)
  y_count <- max(coords$y) - y_start + 1
  y_end <- y_start + y_count - 1
  
  # Read nc file
  # Get dimensions
  nc <- ncdf4::nc_open(file, readunlim = FALSE)
  on.exit(ncdf4::nc_close(nc))
  # Extract variable data in chunks
  varid <- names(nc$var)[[1]]
  lon <- ncdf4::ncvar_get(nc, varid = "longitude", start = x_start, 
                          count = x_count)
  lat <- ncdf4::ncvar_get(nc, varid = "latitude", start = y_start, 
                          count = y_count)
  
  
  chunk_size <- 100
  
  
  time_raw <- ncdf4::ncvar_get(nc, varid = "time")
  time_units <- ncdf4::ncatt_get(nc, varid = "time", attname = "units")$value
  time_calendar <- ncdf4::ncatt_get(nc, varid = "time", 
                                    attname = "calendar")$value
  # Extract origin date
  origin_str <- sub(".*since ", "", time_units)
  origin <- as.POSIXct(origin_str, tz = "UTC")
  
  var_att <- ncdf4::ncatt_get(nc, varid)
  
  # Determine multiplier from units
  if (grepl("minutes", time_units)) {
    time_sec <- time_raw * 60
  } else if (grepl("hours", time_units)) {
    time_sec <- time_raw * 3600
  } else if (grepl("days", time_units)) {
    time_sec <- time_raw * 86400
  } else {
    stop("Unknown time units")
  }
  
  # Treat as model 365_day calendar
  time_pcict <- PCICt::as.PCICt(time_sec, cal = time_calendar,
                                origin = origin, tz = "UTC")
  
  tmax <- nc$dim$time$len
  chunks <- seq(1, tmax, by = chunk_size)
  out <- vector("list", length(chunks))
  
  t0 <- Sys.time()
  for (k in seq_along(chunks)) {
    t_start <- chunks[k]
    t_count <- min(chunk_size, tmax - t_start + 1)
    
    curr_time <- format(Sys.time())
    cli::cli_inform(c("i" = paste0("Reading chunk ", k, " of ", length(chunks),
                                   " [", curr_time, "]")))
    
    out[[k]] <- ncdf4::ncvar_get(
      nc = nc,
      varid = varid,
      start = c(x_start, y_start, t_start),
      count = c(x_count, y_count, t_count)
    )
  }
  # ncdf4::nc_close(nc)
  
  t1 <- Sys.time()
  tdiff <- round(difftime(t1, t0, units = "sec"), 1)
  print(tdiff)
  
  out <- abind::abind(out, along = 3)
  dim(out)
  
  
  lon_idx <- x_start:x_end
  lat_idx <- y_start:y_end
  # time_idx <- t_start:t_end
  
  lon_new <- lon[lon_idx]
  lat_new <- lat[lat_idx]
  time_new <- time_raw
  
  dim_lon <- ncdf4::ncdim_def("longitude", "degrees_east", lon)
  dim_lat <- ncdf4::ncdim_def("latitude", "degrees_north", lat)
  dim_time <- ncdf4::ncdim_def("time", time_units, time_new,
                               calendar = time_calendar)
  
  
  var_def <- ncdf4::ncvar_def(name = varid, units = var_att$units,  
                              longname = var_att$long_name, 
                              # standard_name = var_att$standard_name,
                              dim = list(dim_lon, dim_lat, dim_time),
                              missval = var_att$missing_value, 
                              compression = 5, 
                              chunksizes = c(1, 1, 100))
  
  dst <- ncdf4::nc_create(outfile, var_def)
  
  ncdf4::ncvar_put(dst, varid = varid, vals = out)
  ncdf4::nc_close(dst)
  
  # Return data array
  # r <- terra::rast("subset.nc")
  # terra::plot(r)
  
  # nc <- ncdf4::nc_open(outfile)
  # ncdf4::nc_close(nc)
  
  # dat <- ncdf4::ncvar_get(nc = nc, varid = vars[1], 
  #                         start = c(x_start, y_start, 1), 
  #                         count = c(x_count, y_count, -1))
  return(outfile)
}