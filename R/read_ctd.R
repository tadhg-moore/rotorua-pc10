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

#' Fit Kd to a single PAR profile (one site, one date)
#'
#' The PAR sensor bottoms out at a dark floor (~0.04 umol/m2/s in 2003-09, ~0 in
#' 2010-19, ~0.13 in 2020-24). Fitting log(PAR) ~ depth through that plateau
#' flattens the slope and biases Kd low, so: (1) estimate the floor as the median
#' of the deepest 5 readings, only if they have plateaued; (2) drop the 0 m
#' reading and profiles with PAR(1 m) < `i0_min` (night/very dull casts);
#' (3) fit log(PAR - floor) ~ depth from 1 m down to the first depth where PAR
#' drops below `frac` x PAR(1 m) or `floor_x` x floor. The 95% CI is the
#' t-interval on the slope; it ignores serial correlation in the residuals, so
#' it is a lower bound on the profile uncertainty.
fit_kd_profile <- function(depth, value, i0_min = 10, frac = 0.01,
                           floor_x = 10, min_n = 4) {
  na <- data.frame(n_fit = NA_integer_, floor = NA_real_, Kd = NA_real_,
                   Kd_lo = NA_real_, Kd_hi = NA_real_, Kd_se = NA_real_,
                   r2 = NA_real_, zmax = NA_real_)
  o <- order(depth); depth <- depth[o]; value <- value[o]
  deep <- utils::tail(value, 5)
  floor <- if (diff(range(deep)) <= 0.2 * stats::median(deep) + 1e-6) stats::median(deep) else 0
  na$floor <- floor
  keep <- depth >= 1
  depth <- depth[keep]; value <- value[keep]
  if (length(value) < min_n || value[1] < i0_min) return(na)
  ok <- value > frac * value[1] & value > floor_x * floor
  last <- if (all(ok)) length(ok) else which(!ok)[1] - 1
  na$n_fit <- last
  if (last < min_n) return(na)
  z <- depth[seq_len(last)]; y <- log(value[seq_len(last)] - floor)
  m <- stats::lm(y ~ z); s <- summary(m)
  ci <- stats::confint(m)["z", ]
  data.frame(n_fit = last, floor = floor, Kd = -stats::coef(m)[["z"]],
             Kd_lo = -ci[[2]], Kd_hi = -ci[[1]],
             Kd_se = s$coefficients["z", 2], r2 = s$r.squared, zmax = max(z))
}

#' Per-site Kd fits for every CTD PAR profile
fit_kd_sites <- function(df, ...) {
  df |>
    dplyr::filter(variable == "Photosynthetically Active Radiation (umol/m^2/s)") |>
    dplyr::group_by(Date, Site) |>
    dplyr::summarise(fit = list(fit_kd_profile(`Depth (m)`, value, ...)),
                     .groups = "drop") |>
    tidyr::unnest(fit)
}

#' Per-date Kd with 95% CI
#'
#' Inverse-variance mean of the site fits on each date. The SE adds the
#' between-site variance to the within-profile SE, so the CI widens when the two
#' sites disagree. `n` is the total number of depths used in the fits.
format_par_ts <- function(df, ...) {
  kd_fits <- fit_kd_sites(df, ...) |>
    dplyr::filter(!is.na(Kd)) |>
    dplyr::group_by(Date) |>
    dplyr::summarise(
      n_sites = dplyr::n(),
      n = sum(n_fit),
      r2 = min(r2),
      se_w = sqrt(1 / sum(1 / Kd_se^2)),
      sd_between = if (dplyr::n() > 1) stats::sd(Kd) else 0,
      Kd = sum(Kd / Kd_se^2) / sum(1 / Kd_se^2),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      Kd_se = sqrt(se_w^2 + sd_between^2 / n_sites),
      Kd_lo = Kd - 1.96 * Kd_se,
      Kd_hi = Kd + 1.96 * Kd_se
    ) |>
    dplyr::select(Date, Kd, Kd_lo, Kd_hi, Kd_se, n_sites, n, r2)
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
