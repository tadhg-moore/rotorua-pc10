estimate_sed_zones <- function(aeme) {
  
  inp <- AEME::input(aeme)
  lke <- AEME::lake(aeme)
  
  Kw <- inp$Kw
  z_eu <- 4.6 / Kw
  
  hyps <- inp$hypsograph |> 
    dplyr::arrange(dplyr::desc(depth)) |>
    dplyr::mutate(
      sed_height = abs(min(depth) - depth)
    )
  
  n_zones <- estimate_n_zones_hyps(hyps, max_zones = 5)
  
  zone_bp <- get_zone_breakpoints(hyps, n_zones = n_zones)
  min_elev <- min(hyps$elev)
  
  sed_heights <- hyps |> 
    dplyr::mutate(
      zone = cut(depth, breaks = c(-Inf, zone_bp, Inf), labels = FALSE)
    ) |> 
    dplyr::group_by(zone) |>
    dplyr::summarise(
      zone_height = max(sed_height),
      min_depth = min(depth),
      max_depth = max(depth),
      elev = elev[which.max(sed_height)]
    ) |> 
    dplyr::mutate(
      max_depth = ifelse(max_depth >= 0, Inf, max_depth),
      meas_depth = -max_depth
    ) |> 
    dplyr::select(zone, zone_height, meas_depth) |> 
    dplyr::arrange(zone)
  
  obs_temp <- AEME::get_obs(aeme, var_sim = "HYD_temp")
  summary(obs_temp)
  
  breaks <- sed_heights |>
    dplyr::arrange(meas_depth) |>
    dplyr::pull(meas_depth)
  
  zones <- sed_heights |>
    dplyr::arrange(meas_depth) |>
    dplyr::pull(zone)
  
  obs <- obs_temp |>
    dplyr::mutate(
      sed_zone = zones[
        findInterval(depth_from, vec = breaks)
      ]
    )
  
  # Assign depth_from based on depth bins in the sed_heights input
  obs_temp_summ <- obs |> 
    # dplyr::mutate(
    #   depth_bin = cut(depth_from, breaks = c(-Inf, abs(sed_heights$max_depth)), labels = FALSE)
    # ) |> 
    dplyr::mutate(
      month = lubridate::month(Date),
      adj_year = ifelse(month >= 7, lubridate::year(Date) + 1, lubridate::year(Date)),
      adj_doy = ifelse(month >= 7, lubridate::yday(Date) - 182, lubridate::yday(Date) + 182),
      doy = lubridate::yday(Date)
    ) |> 
    dplyr::group_by(adj_year, sed_zone) |>
    dplyr::summarise(
      avg_depth = mean(depth_from, na.rm = TRUE),
      sed_temp_mean = mean(value, na.rm = TRUE),
      sed_temp_amplitude = sd(value, na.rm = TRUE),
      max_temp = max(value, na.rm = TRUE),
      sed_temp_peak_doy = doy[which.max(value)],
      n = dplyr::n(),
      .groups = "drop"
    ) |> 
    dplyr::filter(n >= 10)
  
  sed_pars <- obs_temp_summ |> 
    tidyr::pivot_longer(cols = c(sed_temp_mean, sed_temp_amplitude, sed_temp_peak_doy),
                        names_to = "param", values_to = "value")
  
  
  # Box plot showing value across zone faceted by param
  ggplot() +
    geom_boxplot(data = sed_pars, aes(x = factor(sed_zone), y = value)) +
    facet_wrap(~param, scales = "free_y") +
    labs(x = "Sediment Zone", y = "Value") +
    theme_bw()
  
  sel_pars <- sed_pars |> 
    dplyr::group_by(param, sed_zone) |>
    dplyr::summarise(
      min = min(value, na.rm = TRUE),
      max = max(value, na.rm = TRUE),
      value = median(value, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    ) |> 
    dplyr::arrange(param, sed_zone) |> 
    dplyr::left_join(sed_heights, by = c("sed_zone" = "zone")) |> 
    dplyr::mutate(
      model = "glm_aed",
      file = "glm3.nml",
      name = paste0("sediment/", param),
      group = NA_character_, 
      index = sed_zone,
      module = "sediment"
    ) |> 
    dplyr::select(model, file, name, group, index, value, min, max, module)
  
  glm_sed_pars <- AEME::glm_sed_params(n_zones = max(sed_heights$zone), 
                                       zone_heights = sed_heights$zone_height) |> 
    dplyr::filter(!name %in% sel_pars$name) |>
    dplyr::mutate(
      min = dplyr::case_when(
        grepl("Ksoil|sed_temp_depth|zone_heights|sed_reflectivity|sed_roughness" , name) ~ value, 
        .default = min
      ),
      max = dplyr::case_when(
        grepl("Ksoil|sed_temp_depth|zone_heights|sed_reflectivity|sed_roughness" , name) ~ value, 
        .default = max
      )
    ) |> 
    dplyr::bind_rows(sel_pars) 
  
  return(glm_sed_pars)
  
  
  ggplot() +
    geom_path(data = obs_temp_summ, 
              aes(x = mean_temp, y = avg_depth, colour = adj_year)) +
    geom_point(data = obs_temp_summ, 
               aes(x = mean_temp, y = avg_depth, shape = factor(sed_zone), group = adj_year)) +
    geom_hline(data = sed_heights, aes(yintercept = -max_depth), linetype = "dashed") +
    scale_y_reverse() 
  
  # Boxplot by depth_bin
  ggplot(obs_temp_summ, aes(x = factor(sed_zone), y = mean_temp)) +
    geom_boxplot() +
    # geom_hline(data = sed_heights, aes(yintercept = max_temp), linetype = "dashed") +
    labs(x = "Depth Bin", y = "Mean Temperature (°C)") +
    theme_minimal()
  
  
  obs_td <- obs_temp |> 
    dplyr::group_by(Date) |> 
    dplyr::summarise(
      value = rLakeAnalyzer::thermo.depth(wtr = value, depths = depth_from)
    )
  
  obs_thmcln <- AEME::get_obs(aeme, var_sim = "HYD_thmcln")
  
  thermo_ann_summ <- obs_td |> 
    dplyr::filter(!is.na(value), value < 0.9 * lke$depth) |>
    dplyr::mutate(
      month = lubridate::month(Date),
      adj_year = ifelse(month >= 7, lubridate::year(Date) + 1, lubridate::year(Date))
    ) |> 
    dplyr::group_by(adj_year) |>
    dplyr::summarise(
      mean = mean(value),
      sd = sd(value),
      median = median(value),
      max = max(value),
      n = dplyr::n()
    )  
  plot(thermo_ann_summ$adj_year, thermo_ann_summ$max)
  thermo_summ <- thermo_ann_summ |> 
    dplyr::filter(n >= 10) |>
    dplyr::summarise(
      mean_mn = mean(mean),
      mean_mx = mean(max),
      median = median(max),
      q25 = quantile(max, 0.25),
      q75 = quantile(max, 0.75),
      n = dplyr::n()
    )
  mean_thermo <- thermo_summ$mean_mn
  
  # zmax <- abs(min(hyps$depth))
  
  zone1_top <- zmax - mean_thermo
  zone3_bottom <- zmax - z_eu
  
  zones <- data.frame(
    zone = c("deep", "mid", "littoral"),
    bottom = c(0,
               zone1_top,
               zone3_bottom),
    top = c(zone1_top,
            zone3_bottom,
            zmax)
  )
  
  
}

estimate_n_zones_hyps <- function(hyps, max_zones = 5, plot = TRUE) {
  
  stopifnot(all(c("depth", "area") %in% names(hyps)))
  
  hyps <- hyps[order(hyps$depth), ]
  
  # Compute slope |dA/dz|
  dA <- diff(hyps$area)
  dz <- diff(hyps$depth)
  slope <- abs(dA / dz)
  
  dat <- data.frame(
    depth = hyps$depth[-1],
    slope = slope
  )
  
  # Remove any infinite or NA values
  dat <- dat[is.finite(dat$slope), ]
  
  # ----- Determine optimal number of clusters -----
  wss <- sapply(1:max_zones, function(k) {
    kmeans(scale(dat$slope), centers = k, nstart = 20)$tot.withinss
  })
  
  # Elbow detection using second derivative
  d1 <- diff(wss)
  d2 <- diff(d1)
  opt_k <- which.min(d2) + 1
  opt_k <- max(2, opt_k)  # at least 2 zones
  return(opt_k)
}

get_zone_breakpoints <- function(hyps, n_zones = 3) {
  
  stopifnot(all(c("depth", "area") %in% names(hyps)))
  
  hyps <- hyps[order(hyps$depth), ] |> 
    dplyr::filter(depth <= 0)
  
  # Compute slope |dA/dz|
  dA <- diff(hyps$area)
  dz <- diff(hyps$depth)
  slope <- abs(dA / dz)
  
  dat <- data.frame(
    depth = hyps$depth[-1],
    slope = slope
  )
  
  
  m <- lm(slope ~ depth, data = dat)
  seg <- segmented::segmented(m, seg.Z = ~depth, npsi = (n_zones - 1))  
  
  summary(seg)
  plot(seg)
  
  # Extract breakpoints
  breakpoints <- seg$psi[, "Est."] |> sort()
  return(breakpoints)
}

