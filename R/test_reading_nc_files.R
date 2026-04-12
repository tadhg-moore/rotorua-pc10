library(ncdf4)
library(dplyr)

#-------------------------------------------
# Benchmark helper
#-------------------------------------------
benchmark_read <- function(ncfile, varname, x_range, y_range, t_range) {
  nc <- nc_open(ncfile)
  
  t0 <- Sys.time()
  dat <- ncvar_get(
    nc,
    varid = varname,
    start = c(x_range[1], y_range[1], t_range[1]),
    count = c(diff(x_range) + 1, diff(y_range) + 1, diff(t_range) + 1)
  )
  t1 <- Sys.time()
  
  nc_close(nc)
  
  as.numeric(difftime(t1, t0, units = "secs"))
}

#-------------------------------------------
# Main benchmark function
#-------------------------------------------
benchmark_nc_chunking <- function(
    ncfile,
    varname = NULL,
    reps = 3,
    t_chunk_sizes = c(1, 10, 100, 1000),
    spatial_sizes = c(1, 5, 20, 50),
    seed_point = c(120, 130)  # lon idx, lat idx
) {
  nc <- nc_open(ncfile)
  
  if (is.null(varname)) {
    varname <- names(nc$var)[1]
  }
  
  nx <- nc$dim$longitude$len
  ny <- nc$dim$latitude$len
  nt <- nc$dim$time$len
  
  nc_close(nc)
  
  results <- list()
  
  #-----------------------
  # 1) Time-based chunk tests
  #-----------------------
  for (tc in t_chunk_sizes) {
    t_end <- min(tc, nt)
    message(sprintf("Testing time chunk: %d", tc))
    
    times <- replicate(reps, {
      benchmark_read(
        ncfile, varname,
        x_range = c(1, nx),
        y_range = c(1, ny),
        t_range = c(1, t_end)
      )
    })
    
    results[[paste0("time_", tc)]] <- data.frame(
      test = paste0("Time chunk = ", tc),
      mean_sec = mean(times),
      sd_sec = sd(times),
      t = t_end
    )
  }
  
  #-----------------------
  # 2) Spatial block tests (full time)
  #-----------------------
  for (sz in spatial_sizes) {
    message(sprintf("Testing spatial block: %dx%d", sz, sz))
    
    x1 <- seed_point[1]
    y1 <- seed_point[2]
    
    x_range <- c(x1, min(x1 + sz - 1, nx))
    y_range <- c(y1, min(y1 + sz - 1, ny))
    
    times <- replicate(reps, {
      benchmark_read(
        ncfile, varname,
        x_range = x_range,
        y_range = y_range,
        t_range = c(1, 100)
      )
    })
    
    results[[paste0("space_", sz)]] <- data.frame(
      test = paste0("Spatial block = ", sz, "x", sz),
      mean_sec = mean(times),
      sd_sec = sd(times),
      t = 100
    )
  }
  
  #-----------------------
  # 3) Full time series at single point
  #-----------------------
  # message("Testing full time at single point")
  # 
  # times <- replicate(reps, {
  #   benchmark_read(
  #     ncfile, varname,
  #     x_range = seed_point[1],
  #     y_range = seed_point[2],
  #     t_range = c(1, nt)
  #   )
  # })
  # 
  # results[["point_full_time"]] <- data.frame(
  #   test = "Single point, full time series",
  #   mean_sec = mean(times),
  #   sd_sec = sd(times)
  # )
  
  # Combine and return
  bind_rows(results)
}

#-------------------------------------------
# Run the benchmark
#-------------------------------------------

ncfile <- "Z:/hurs_historical_ACCESS-CM2_CCAM_daily_NZ5km_raw.nc"

results <- benchmark_nc_chunking(
  ncfile,
  reps = 3,
  t_chunk_sizes = c(1, 10, 100, 250, 400, 500, 600, 750, 1000),
  spatial_sizes = c(1, 5, 20, 50)
)

results$t <- c(1, 10, 100, 250, 400, 500, 600, 750, 1000, 100, 100, 100, 100)

results <- results |> 
  dplyr::mutate(
    time_per_time_step = mean_sec / t
  ) |> 
  dplyr::arrange(time_per_time_step)

print(results)

library(ggplot2)

ggplot(results, aes(x = test, y = mean_sec)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_sec - sd_sec, ymax = mean_sec + sd_sec), width = 0.2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "NetCDF Read Benchmark Results", x = "Test", y = "Mean Time (seconds)")

ggplot(results, aes(x = t, y = time_per_time_step, color = test)) +
  # geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
  geom_line() +
  geom_point() +
  labs(title = "NetCDF Read Benchmark Results", x = "Time Dimension Size", y = "Mean Time (seconds)") +
  theme_minimal()
