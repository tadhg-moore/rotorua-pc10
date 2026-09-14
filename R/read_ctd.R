read_ctd <- function(file) {
  
  sheets <- readxl::excel_sheets(file)
  
  coord_sheet <- sheets[grepl("Coordinates", sheets, ignore.case = TRUE)]
  ctd_sheets <- sheets[grepl("CTD", sheets, ignore.case = TRUE)]
  
  coords <- readxl::read_excel(file, sheet = coord_sheet) |> 
    sf::st_as_sf(coords = c("easting", "northing"), crs = 2193)
  
  # tm_shape(coords) +
  #   tm_dots()
  
  ctd_data <- lapply(ctd_sheets, function(sheet) {
    readxl::read_excel(file, sheet = sheet)
  }) |> 
    dplyr::bind_rows() |> 
    dplyr::mutate(Date = as.Date(Time))
  
  df_long <- ctd_data |> 
    dplyr::select(-Time, -LocationName) |> 
    tidyr::pivot_longer(
      cols = -c(Site, Date, `Depth (m)`),
      names_to = "variable",
      values_to = "value"
    ) |> 
    dplyr::filter(!is.na(value), value >= 0)
  return(df_long)
}

format_ctd_for_aeme <- function(df) {
  
  
  # AEME::generate_var_map_code(data = df_long, var_col_name = "variable")
  var_map <- tibble::tribble(
    ~var_aeme, ~name, ~unit,
    "CHM_oxy", "Dissolved oxygen (g/m^3)", "mg/L",
    # "HYD_dens", "Conductivity (uS/cm)", "kg/m3",
    "HYD_temp", "Water Temperature (degC)", "degC",
    "PHY_tchla", "Chlorophyll (ug/l)", "mg/m^3",
    "CHM_oxysat", "Dissolved oxygen - field percentage saturation (%)", "%"
    # "NA", "Photosynthetically Active Radiation (umol/m^2/s)", "NA",
    # "NA", "Specific Conductance (uS/cm)", "NA",
    # "NA", "Turbidity, nephelometric turbidity units (NTU) (_NTU)", "NA",
  )
  
  aeme_obs <- AEME::lake_obs_to_aeme(data = df,
                                     datetime_col_name = "Date",
                                     lake_id_col = "Site",
                                     depth_col_name = "Depth (m)", 
                                     var_col_name = "variable",
                                     value_col_name = "value",
                                     var_map = var_map)
  
  # aeme_obs |> 
  #   dplyr::group_by(lake_id, var_aeme) |>
  #   dplyr::summarise(
  #     mean = mean(value, na.rm = TRUE),
  #     sd = sd(value, na.rm = TRUE),
  #     min = min(value, na.rm = TRUE),
  #     max = max(value, na.rm = TRUE),
  #     uniq_depths = length(unique(depth_from)),
  #     n_na = sum(is.na(value)),
  #     n_dates = length(unique(Date)),
  #     n_values = dplyr::n()
  #   ) 
  
  return(aeme_obs)
  
}

format_par_ts <- function(df) {
  par_profiles <- df |> 
    dplyr::filter(variable == "Photosynthetically Active Radiation (umol/m^2/s)") |> 
    dplyr::mutate(depth = `Depth (m)`)
  
  kd_fits <- par_profiles |>
    dplyr::filter(depth > 0.1) |>
    dplyr::group_by(Date) |>
    dplyr::filter(dplyr::n() >= 3) |>
    dplyr::summarise(
      fit = list(stats::lm(log(value) ~ depth)),
      n = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      Kd = -sapply(fit, function(f) stats::coef(f)[["depth"]]),
      r2 = sapply(fit, function(f) stats::summary.lm(f)$r.squared)
    ) |> 
    dplyr::filter(r2 > 0.8) |>
    dplyr::select(Date, Kd, r2, n)
  return(kd_fits)
  plot(kd_fits$Date, kd_fits$Kd, type = "b", pch = 19, xlab = "Date", ylab = "Kd (1/m)")
  
  kd_fits <- kd_fits |> 
    dplyr::mutate(
      month = factor(lubridate::month(Date), levels = 1:12, labels = month.abb),
      year = lubridate::year(Date)
    )
  kd_seasonal <- kd_fits |>
    dplyr::group_by(month) |>
    dplyr::summarise(
      Kd_mean = mean(Kd, na.rm = TRUE),
      Kd_sd = sd(Kd, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    )
  
  library(ggplot2)
  ggplot() +
    geom_boxplot(data = kd_fits, aes(x = month, y = Kd)) +
    # geom_line(data = kd_seasonal, aes(x = month, y = Kd_mean)) +
    geom_point(data = kd_seasonal, aes(x = month, y = Kd_mean), col = "red", size = 2) +
    geom_smooth(data = kd_seasonal, aes(x = as.numeric(month), y = Kd_mean), method = "loess", col = "blue", se = FALSE) +
    # geom_errorbar(data = kd_seasonal, aes(x = month, ymin = Kd_mean - Kd_sd, ymax = Kd_mean + Kd_sd), width = 0.2) +
    # scale_x_continuous(breaks = 1:12) +
    scale_y_reverse(limits = c(2, 0)) +
    labs(x = "Month", y = "Mean Kd (1/m)", title = "Seasonal variation of Kd") +
    theme_classic()
  
  
  # Interpolate to daily coverage over the sim period before writing
  daily <- base::data.frame(time = base::seq(sim_start, sim_end, by = "day"))
  daily$Kd <- stats::approx(kd_fits$time, kd_fits$Kd, xout = daily$time, rule = 2)$y
  utils::write.csv(daily, "kd_timeseries.csv", row.names = FALSE)
}
