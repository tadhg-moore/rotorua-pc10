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
    message("Processing file: ", basename(historical_file))
    # Load historical data
    hist <- load_nc(historical_file)
    hist_mean <- fun(terra::values(hist), na.rm = TRUE)
    
    if (g == "NZESM") {
      hist_time <- get_nc_time(ncfile = historical_file)
    } else {
      hist_time <- terra::time(hist)
    }
    
    # Get season + season year
    ts <- get_season_year(hist_time)
    season_groups <- unique(data.frame(
      season = ts$season,
      season_year = ts$season_year
    ))
    
    # Filter to years that have 4 seasons
    complete_years <- season_groups |>
      dplyr::group_by(season_year) |>
      dplyr::summarise(n_seasons = dplyr::n(), .groups = "drop") |>
      dplyr::filter(n_seasons == 4) |>
      dplyr::pull(season_year)
    
    season_groups <- season_groups |>
      dplyr::filter(season_year %in% complete_years, 
                    season_year > 1960) 
    
    # Calculate seasonal means
    hist_seasonal_df <- calc_seasonal_fun(time = hist_time, r = hist, fun) |> 
      dplyr::mutate(
        scenario = "historical",
        gcm = g
      )
    
    hist_annual_df <- calc_annual_fun(time = hist_time, r = hist, fun) |> 
      dplyr::mutate(
        season = "annual",
        scenario = "historical",
        gcm = g
      )
    
    hist_df <- dplyr::bind_rows(hist_seasonal_df, hist_annual_df) |> 
      dplyr::arrange(year, season)
    
    return(hist_df)
  }) |> 
    dplyr::bind_rows()
  
  
  scen_all <- lapply(gcm, \(g) {
    sub_files <- scenario_files[grepl(g, scenario_files)]
    # Load scenario data and calculate annual means
    scen_dfs <- lapply(sub_files, function(f) {
      
      message("Processing file: ", basename(f))
      scen_label <- strsplit(basename(f), "_")[[1]][2]
      scen <- load_nc(f)
      scen_time <- get_nc_time(f)
      
      scen_seasonal <- calc_seasonal_fun(time = scen_time, r = scen, fun) |> 
        dplyr::mutate(
          scenario = scen_label,
          gcm = g
        )

      scen_annual <- calc_annual_fun(time = scen_time, r = scen, fun) |> 
        dplyr::mutate(
          season = "annual",
          scenario = scen_label,
          gcm = g
        )
      
      df <- dplyr::bind_rows(scen_seasonal, scen_annual) |> 
        dplyr::arrange(year, season)
      
      return(df)
    }) |> 
      dplyr::bind_rows()
  }) |> 
    dplyr::bind_rows()
  
  
  
  all_df <- dplyr::bind_rows(hist_all, scen_all) |> 
    dplyr::mutate(variable = variable) |>
    dplyr::select(gcm, scenario, year, season, variable, value)
  return(all_df)
}

get_season_year <- function(dates) {
  m <- as.integer(format(as.Date(dates), "%m"))
  y <- as.integer(format(as.Date(dates), "%Y"))
  
  season <- ifelse(m %in% c(12, 1, 2), "DJF",
                   ifelse(m %in% 3:5, "MAM",
                          ifelse(m %in% 6:8, "JJA", "SON")))
  
  # DJF belongs to the YEAR of Jan–Feb (Dec gets shifted +1)
  season_year <- ifelse(m == 12, y + 1, y)
  
  data.frame(
    season = season,
    season_year = season_year
  )
}

calc_seasonal_fun <- function(time, r, fun) {
  
  time <- adj_360_day_time(time)
  
  # Get season + season year
  ts <- get_season_year(time)
  season_groups <- unique(data.frame(
    season = ts$season,
    season_year = ts$season_year
  ))
  
  # Filter to years that have 4 seasons
  complete_years <- season_groups |>
    dplyr::group_by(season_year) |>
    dplyr::summarise(n_seasons = dplyr::n(), .groups = "drop") |>
    dplyr::filter(n_seasons == 4) |>
    dplyr::pull(season_year)
  
  season_groups <- season_groups |>
    dplyr::filter(season_year %in% complete_years, 
                  season_year > 1960) 
  
  # Calculate seasonal means
  seasonal_df <- apply(season_groups, 1, function(row) {
    
    s <- row[["season"]]
    sy <- row[["season_year"]]
    
    idx <- which(ts$season == s & ts$season_year == sy)
    time[idx]
    if (length(idx) < 90) {
      return(data.frame(
        year = as.integer(sy),
        value = NA,
        season = s
      ))
    }
    
    r_subset <- terra::subset(r, idx)
    
    precip <- identical(fun, sum)
    if (precip) {
      # Calculate sum for all cells then median of raster
      sum_res <- terra::app(r_subset, fun = sum, na.rm = TRUE)
      mean_val <- mean(terra::values(sum_res), na.rm = TRUE)
    } else {
      mean_val <- fun(terra::values(r_subset), na.rm = TRUE)
    }
    
    data.frame(
      year = as.integer(sy),
      value = mean_val,
      season = s
    )
  }) |> 
    dplyr::bind_rows()
  return(seasonal_df)
}


calc_annual_fun <- function(time, r, fun) {
  
  time <- adj_360_day_time(time)
  
  years <- unique(format(as.Date(time), "%Y"))
  
  # remove NA's from NZESM
  years <- years[!is.na(years)]
  
  annual_means <- sapply(years, function(yr) {
    time_idx <- which(format(as.Date(time), "%Y") == yr)
    r_subset <- terra::subset(r, time_idx)
    precip <- identical(fun, sum)
    if (precip) {
      # Calculate sum for all cells then median of raster
      sum_res <- terra::app(r_subset, fun = sum, na.rm = TRUE)
      mean_val <- mean(terra::values(sum_res), na.rm = TRUE)
    } else {
      mean_val <- fun(terra::values(r_subset), na.rm = TRUE)
    }
    return(mean_val)
  })
  
  annual_df <- data.frame(
    year = as.integer(years),
    value = annual_means
  )
  
  return(annual_df)
}

adj_360_day_time <- function(time) {
  # Adjust dates 29/2 and 30/2 to 28/2 for non-leap years (NZESM has some leap day issues
  non_dates <- grepl("02-29|02-30", time)
  if (any(non_dates)) {
    upd_yrs <- strsplit(time[non_dates], "-") |> 
      sapply(function(x) x[1]) |> 
      as.integer( "")
    time[non_dates] <- paste0(upd_yrs, "-02-28")
  }
  return(time)
}
