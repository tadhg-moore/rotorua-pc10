# Sediment zones for GLM-AED and the sediment flux data used to constrain them.
#
# Zones are depth bands below the median lake surface, numbered from the deepest (zone 1), as GLM numbers them.
# The default 5-zone layout gives each chamber depth its own zone:
#   zone 1 17.5 m - bed  chambers 20 m (2026), Burger 20 m (2003-04)
#   zone 2 13 - 17.5 m   chambers 15 m, Burger 14 m
#   zone 3 8 - 13 m      chambers 10 m
#   zone 4 3.5 - 8 m     Burger 7 m
#   zone 5 0 - 3.5 m     no flux data
# GLM's zone_heights are the tops of the zones as heights above the bed: bed_depth + level_offset - top depth.
# On the GNS model hypsograph the bed is at 25 m and the median modelled level 0.5 m above the hypsograph
# surface (run_gnssodp_best lake.csv), so the 13 m boundary is 12.5 m above the bed, as in the 4-zone base.

define_sed_zones <- function(hyps, breaks = c(3.5, 8, 13, 17.5), level_offset = 0.5, top_height = 36.35) {
  bed <- abs(min(hyps$depth))
  top <- c(rev(breaks), 0)                         # zone 1 (deepest) first
  bot <- c(bed, rev(breaks))
  h <- hyps[hyps$depth <= 0, ]
  a <- stats::approx(-h$depth, h$area, xout = c(top, bot), rule = 2)$y
  n <- length(top)
  tibble::tibble(
    zone = seq_len(n), top_m = top, bot_m = bot,
    mid_m = (top + bot) / 2,
    area_km2 = (a[seq_len(n)] - a[n + seq_len(n)]) / 1e6,
    zone_height = c(bed + level_offset - top[-n], top_height),
    label = sprintf("Z%d %g-%g m", seq_len(n), top, bot)
  )
}

assign_sed_zone <- function(depth, zones) {
  vapply(depth, function(d) { k <- which(d >= zones$top_m & d < zones$bot_m); if (length(k)) zones$zone[k[1]] else NA_integer_ }, integer(1))
}

# Sediment flux observations in one table: one row per chamber (2026) or chamber x period (Burger et al. 2007).
#   value: NH4 mg N/m2/d, PO4 mg P/m2/d (positive = release), SOD g O2/m2/d (positive = uptake).
#   do_mg_l / temp_c: the water above the sediment at the time (2026: buoy casts interpolated to the chamber depth,
#   mean of the deployment; Burger: bottom water on the day).
#   Burger NH4 is the secondary (12-48 h) rate, which the authors take as the natural release; the initial
#   burst is likely a disturbance artefact. Burger values are light and dark chamber means per site and period.
sed_flux_obs <- function(chamber, burger_flux_file, burger_bw_file, zones) {
  date26 <- mean(chamber$dates)
  lc <- chamber$lake_conditions |> dplyr::group_by(depth_m) |>
    dplyr::summarise(do = mean(do_mg_l), t = mean(temp_c), .groups = "drop")
  cond <- function(d) list(do = stats::approx(lc$depth_m, lc$do, d, rule = 2)$y, t = stats::approx(lc$depth_m, lc$t, d, rule = 2)$y)
  c26 <- dplyr::bind_rows(
    chamber$nutrient_flux |> dplyr::filter(variable %in% c("NH4", "PO4")) |>
      dplyr::transmute(depth_m, light, chamber, variable, value = flux_mg_m2_d, r2),
    chamber$sod |> dplyr::transmute(depth_m, light, chamber, variable, value = -sod_mg_m2_d / 1000, r2)
  ) |>
    dplyr::mutate(source = "chamber_2026", date = date26, period = "Mar 2026",
                  do_mg_l = cond(depth_m)$do, temp_c = cond(depth_m)$t)
  bf <- utils::read.csv(burger_flux_file); bw <- utils::read.csv(burger_bw_file)
  b07 <- bf |> dplyr::left_join(bw |> dplyr::select(site, date, do_mg_L, temp_C), by = c("site", "date")) |>
    dplyr::mutate(NH4 = nh4_phase2_mg_m2_d, PO4 = srp_mg_m2_d, SOD = sod_g_m2_d) |>
    tidyr::pivot_longer(c(NH4, PO4, SOD), names_to = "variable", values_to = "value") |>
    dplyr::transmute(source = "burger_2007", date = as.Date(date), period, depth_m, light = tools::toTitleCase(chamber),
                     chamber = paste0("site", site, "_n", n), variable, value, r2 = NA_real_,
                     do_mg_l = do_mg_L, temp_c = temp_C) |>
    dplyr::filter(!is.na(value))
  dplyr::bind_rows(c26, b07) |>
    dplyr::mutate(zone = assign_sed_zone(depth_m, zones), .after = depth_m) |>
    dplyr::select(source, date, period, depth_m, zone, light, chamber, variable, value, r2, do_mg_l, temp_c)
}

# AED Constant2d flux at given water conditions (aed_oxygen / aed_nitrogen / aed_phosphorus), mmol/m2/d:
#   SOD = fsed_oxy * O2 / (ksed_oxy + O2) * theta^(T-20); NH4 / FRP = fsed * ksed / (ksed + O2) * theta^(T-20); O2 in mmol/m3.
aed_sed_factor <- function(variable, do_mg_l, temp_c, k) {
  o2 <- do_mg_l * 1000 / 32
  dplyr::case_when(
    variable == "SOD" ~ o2 / (k$ksed_oxy + o2) * k$theta_sed_oxy^(temp_c - 20),
    variable == "NH4" ~ k$ksed_amm / (k$ksed_amm + o2) * k$theta_sed_amm^(temp_c - 20),
    variable == "PO4" ~ k$ksed_frp / (k$ksed_frp + o2) * k$theta_sed_frp^(temp_c - 20))
}
SED_MW <- c(NH4 = 14.007, PO4 = 30.97, SOD = 32)
SED_K_SODP <- list(ksed_oxy = 74.46, theta_sed_oxy = 1.1015, ksed_amm = 29.23, theta_sed_amm = 1.069,   # out_gns_sodp best (it3_094)
                   ksed_frp = 2.566, theta_sed_frp = 1.1156)

# Zone summary of the observations (log-scale centre and spread, conditions).
sed_flux_summary <- function(obs) {
  obs |> dplyr::filter(value > 0) |>
    dplyr::group_by(source, zone, depth_m, variable) |>
    dplyr::summarise(n = dplyr::n(), median = stats::median(value), gmean = exp(mean(log(value))),
                     sd_log = if (dplyr::n() > 1) stats::sd(log(value)) else NA_real_,
                     min = min(value), max = max(value),
                     do_mg_l = stats::median(do_mg_l), temp_c = stats::median(temp_c), .groups = "drop")
}

# Per-zone fsed (AED maximum rate, mmol/m2/d, 20 deg C, anoxic for NH4/FRP) implied by the observations: each
# observation divided by the AED oxygen and temperature factor at its own conditions, then the zone median.
# The result depends on ksed / theta (default: the out_gns_sodp best). Zones without data take the nearest zone
# with data (filled_from). fsed_nit has no measurement and keeps fsed_nit_default.
# Returned in the AEME parameter-table layout (model, file, name, value, min, max, group, index) plus provenance.
sed_fsed_anchor <- function(obs, zones, k = SED_K_SODP, sources = "chamber_2026", fsed_nit_default = -0.05,
                            range_mult = c(0.25, 4)) {
  d <- obs |> dplyr::filter(source %in% sources, value > 0, !is.na(zone)) |>
    dplyr::mutate(fsed = value / dplyr::if_else(variable == "SOD", 1e-3, 1) / SED_MW[variable] /
                    aed_sed_factor(variable, do_mg_l, temp_c, k))
  z <- d |> dplyr::group_by(variable, zone) |> dplyr::summarise(value = stats::median(fsed), n = dplyr::n(), .groups = "drop")
  full <- tidyr::expand_grid(variable = c("SOD", "NH4", "PO4"), zone = zones$zone) |>
    dplyr::left_join(z, by = c("variable", "zone")) |>
    dplyr::group_by(variable) |>
    dplyr::mutate(filled_from = vapply(zone, function(q) { have <- zone[!is.na(value)]; if (q %in% have) NA_integer_ else have[which.min(abs(have - q))] }, integer(1)),
                  value = dplyr::if_else(is.na(value), value[match(filled_from, zone)], value),
                  n = dplyr::if_else(is.na(n), 0L, n)) |> dplyr::ungroup()
  nm <- c(SOD = "aed_sed_const2d/fsed_oxy", NH4 = "aed_sed_const2d/fsed_amm", PO4 = "aed_sed_const2d/fsed_frp")
  out <- full |> dplyr::mutate(value = dplyr::if_else(variable == "SOD", -value, value),
                               name = nm[variable],
                               min = pmin(value * range_mult[1], value * range_mult[2]), max = pmax(value * range_mult[1], value * range_mult[2]))
  nit <- tibble::tibble(name = "aed_sed_const2d/fsed_nit", zone = zones$zone, value = fsed_nit_default,
                        min = -2, max = -0.002, n = 0L, filled_from = NA_integer_, variable = "NO3")
  dplyr::bind_rows(out, nit) |>
    dplyr::transmute(model = "glm_aed", file = "aed.nml", name, value, min, max, group = NA_character_, index = as.integer(zone),
                     variable, n_obs = n, filled_from, sources = paste(sources, collapse = "+")) |>
    dplyr::arrange(name, index)
}

# GLM &sediment rows for the zones (AEME parameter layout): zone_heights and n_zones.
sed_zone_glm_params <- function(zones) {
  dplyr::bind_rows(
    tibble::tibble(name = "sediment/n_zones", value = nrow(zones), index = NA_integer_),
    tibble::tibble(name = "sediment/zone_heights", value = zones$zone_height, index = zones$zone)) |>
    dplyr::mutate(model = "glm_aed", file = "glm4.nml", min = value, max = value, group = NA_character_) |>
    dplyr::select(model, file, name, value, min, max, group, index)
}

# Benthic chamber sites, March 2026 (WGS84 decimal degrees, as supplied with the data), with NZTM 2000 (EPSG:2193)
# coordinates and the GNS bathymetry depth at each site (m below the raster surface, full 2 m resolution).
sediment_chamber_sites <- function(bathy_file) {
  s <- tibble::tibble(site = c("10 m", "15 m", "20 m"), depth_m = c(10, 15, 20),
                      lat = -c(38.086070, 38.084423, 38.078386), lon = c(176.237777, 176.253026, 176.274771))
  v <- terra::project(terra::vect(s, geom = c("lon", "lat"), crs = "EPSG:4326", keep = TRUE), "EPSG:2193")
  xy <- terra::crds(v)
  r <- terra::rast(bathy_file)
  bathy <- terra::global(r, "max", na.rm = TRUE)[[1]] - terra::extract(r, v)[[2]]
  dplyr::mutate(s, x = xy[, 1], y = xy[, 2], bathy_depth_m = round(bathy, 2))
}

# Depth raster (m below the surface) from the GNS bathymetry, as get_gns_hyps() reads it, aggregated for mapping.
bathy_depth_raster <- function(file, fact = 10) {
  r <- terra::rast(file)
  d <- terra::global(r, "max", na.rm = TRUE)[[1]] - r
  names(d) <- "depth"
  if (fact > 1) d <- terra::aggregate(d, fact = fact, fun = "mean", na.rm = TRUE)
  d
}

# Map of the sediment zones (and depth contours) on the bathymetry. Returns a ggplot.
plot_sed_zone_map <- function(depth, zones, sites = NULL, contours = c(5, 10, 15, 20, 25)) {
  df <- as.data.frame(depth, xy = TRUE, na.rm = TRUE)
  df$zone <- assign_sed_zone(pmin(df$depth, max(zones$bot_m) - 1e-6), zones)
  df$zl <- factor(zones$label[df$zone], levels = zones$label)
  ggplot2::ggplot(df, ggplot2::aes(x, y)) +
    ggplot2::geom_raster(ggplot2::aes(fill = zl)) +
    ggplot2::geom_contour(ggplot2::aes(z = depth), breaks = contours, colour = "white", linewidth = 0.25) +
    ggplot2::scale_fill_viridis_d(option = "mako", direction = -1, begin = 0.15, end = 0.95, name = "sediment zone") +
    add_chamber_sites(sites) +
    ggplot2::coord_equal(expand = FALSE) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_void(11) + ggplot2::theme(legend.position = "right")
}

# Chamber site markers and labels for a map in NZTM coordinates (sediment_chamber_sites()).
add_chamber_sites <- function(sites) {
  if (is.null(sites)) return(NULL)
  list(ggplot2::geom_point(data = sites, ggplot2::aes(x, y), inherit.aes = FALSE, shape = 21, size = 3.2, stroke = 1.1, fill = "#eda100", colour = "black"),
       ggplot2::geom_label(data = sites, ggplot2::aes(x, y, label = site), inherit.aes = FALSE, size = 3, nudge_y = 450,
                           label.size = 0.15, label.padding = ggplot2::unit(0.12, "lines"), fill = "white", alpha = 0.9))
}
