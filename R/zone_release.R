# Depth-zoned sediment oxygen demand and nutrient release for Lake Rotorua
# (website/lake-water-quality-synthesis.qmd, #sec-zone-release; targets zone_o2_demand, zone_nutrient_flux).
# Needs hyps_vol_below() / hyps_area() / is_stratified() from R/wq_synthesis_helpers.R.

#' Sediment area per unit volume of a depth layer [z0, z1] from the hypsograph (1/m)
layer_alpha <- function(hyps, z0, z1) {
  a0 <- stats::approx(hyps$depth, hyps$area, z0, rule = 2)$y; a1 <- stats::approx(hyps$depth, hyps$area, z1, rule = 2)$y
  zz <- seq(z0, z1, length.out = 21); v <- sum(diff(zz) * utils::head(stats::approx(hyps$depth, hyps$area, zz, rule = 2)$y, -1) / 2 +
                                              diff(zz) * utils::tail(stats::approx(hyps$depth, hyps$area, zz, rule = 2)$y, -1) / 2)
  c(alpha = (a0 - a1) / v, area = a0 - a1, volume = v)
}

#' Zone sediment oxygen demand from depth-resolved DO during stratified periods
#'
#' Livingstone & Imboden (2009): within each depth layer below the thermocline, the DO
#' depletion rate is J(z) = Jv + Ja * alpha(z), with Jv the water-column (volumetric) demand,
#' Ja the sediment (areal) demand and alpha(z) the sediment area per unit volume of the layer
#' from the hypsograph. Per stratified period (>= `min_days`, new buoy profiles at 0.5 m from
#' Feb 2022): DO is averaged in 1 m layers; layers that stay below the thermocline for the whole
#' period (below the 90th percentile of the daily thermocline depth, plus `gap` m) get a linear
#' depletion rate over the days before DO falls to `do_floor`; Jv and Ja come from a regression
#' of the rates on alpha across layers. A per-layer areal demand (rate - Jv) / alpha then allows
#' depth zones. Also returned: the "all-sediment" demand rate / alpha, an upper bound that assigns
#' all depletion in the layer to its sediment.
#' @param do_prof long DO profiles (date, depth, value), daily
#' @param temp_prof long temperature profiles (date, depth, value), daily
#' @param thermal buoy_thermal (source, date, thermo_depth)
#' @param stratified buoy_events$stratified (start, end, duration)
zone_oxygen_demand <- function(do_prof, temp_prof, thermal, stratified, hyps, from = as.Date("2022-02-21"),
                               min_days = 7, gap = 1, do_floor = 1, min_pts = 5, z_max = 21) {
  ev <- stratified |> dplyr::filter(start >= from, duration >= min_days)
  lay <- function(p) p |> dplyr::mutate(z = floor(depth)) |> dplyr::filter(z < z_max) |>
    dplyr::group_by(date, z) |> dplyr::summarise(v = mean(value, na.rm = TRUE), .groups = "drop")
  dl <- lay(do_prof); tl <- lay(temp_prof)
  th <- thermal |> dplyr::filter(source == "new buoy")
  out_l <- list(); out_e <- list()
  for (i in seq_len(nrow(ev))) {
    e <- ev[i, ]; days <- seq(e$start, e$end, by = "day")
    zt <- stats::quantile(th$thermo_depth[th$date %in% days], 0.9, na.rm = TRUE, names = FALSE)
    if (!is.finite(zt)) next
    z_top <- ceiling(zt + gap)
    if (z_top > z_max - 3) next
    rows <- lapply(z_top:(z_max - 1), function(z) {
      d <- dl |> dplyr::filter(z == !!z, date %in% days, is.finite(v)) |> dplyr::arrange(date)
      if (nrow(d) < min_pts) return(NULL)
      k <- which(d$v <= do_floor)[1]; if (!is.na(k)) d <- d[seq_len(k), ]
      if (nrow(d) < min_pts || as.numeric(diff(range(d$date))) < min_pts - 1) return(NULL)
      t <- as.numeric(d$date - d$date[1]); f <- stats::lm(d$v ~ t)
      a <- layer_alpha(hyps, z, z + 1)
      tmp <- mean(tl$v[tl$z == z & tl$date %in% d$date], na.rm = TRUE)
      data.frame(event = i, start = e$start, end = e$end, z = z, z_mid = z + 0.5, rate = -unname(stats::coef(f)[2]),
                 r2 = summary(f)$r.squared, n = nrow(d), do0 = d$v[1], temp = tmp,
                 alpha = unname(a["alpha"]), area = unname(a["area"]), volume = unname(a["volume"]))
    })
    L <- dplyr::bind_rows(rows)
    if (nrow(L) < 3) next
    f <- stats::lm(rate ~ alpha, data = L)
    jv <- unname(stats::coef(f)[1]); ja <- unname(stats::coef(f)[2])
    L <- L |> dplyr::mutate(z_top = z_top, jv = jv, ja_layer = (rate - jv) / alpha, ja_all = rate / alpha)
    out_l[[length(out_l) + 1]] <- L
    out_e[[length(out_e) + 1]] <- data.frame(event = i, start = e$start, end = e$end, duration = e$duration, z_top = z_top,
                                             n_layers = nrow(L), jv = jv, ja = ja, r2 = summary(f)$r.squared,
                                             temp = stats::weighted.mean(L$temp, L$volume, na.rm = TRUE))
  }
  list(layers = dplyr::bind_rows(out_l), events = dplyr::bind_rows(out_e))
}

#' Aggregate per-layer areal demand to depth zones (sediment-area weighted)
#' @param zones data.frame(zone, z0, z1)
zone_summary <- function(layers, zones, value = "ja_layer") {
  dplyr::bind_rows(lapply(seq_len(nrow(zones)), function(k) {
    layers |> dplyr::filter(z >= zones$z0[k], z + 1 <= zones$z1[k]) |>
      dplyr::group_by(event, start) |>
      dplyr::summarise(v = stats::weighted.mean(.data[[value]], area), temp = stats::weighted.mean(temp, area, na.rm = TRUE),
                       n_layers = dplyr::n(), .groups = "drop") |>
      dplyr::mutate(zone = zones$zone[k])
  }))
}

#' Two-layer hypolimnetic nutrient accumulation from the monthly samples
#'
#' For consecutive sampling dates at a site (<= `max_gap` days) when the CTD shows the lake
#' stratified on both dates: an upper hypolimnion layer from the thermocline to the midpoint
#' between the HYP and BOT sampling depths (represented by the HYP sample) and a lower layer
#' from there to the bed (represented by the BOT sample). Accumulation per layer is the change
#' in C x V(layer) over the interval, per unit sediment area of that layer (mg/m2/d). Exchange
#' between the two layers is ignored, so this is net release into each layer.
#' @param sample_depths named list per site: c(hyp, bot) sampling depths (m)
two_layer_flux <- function(chem_long, ctd_thermal, hyps, vars = c("nh4", "drp", "tp"), max_gap = 45,
                           sample_depths = list(`Site 2` = c(15, 19), `Site 5` = c(18, 22))) {
  vol <- function(z0, z1) { zz <- seq(z0, z1, length.out = 41); a <- stats::approx(hyps$depth, hyps$area, zz, rule = 2)$y
    sum(diff(zz) * (utils::head(a, -1) + utils::tail(a, -1)) / 2) }
  area_at <- function(z) stats::approx(hyps$depth, hyps$area, z, rule = 2)$y
  ch <- chem_long |> dplyr::filter(layer %in% c("HYP", "BOT"), variable %in% vars) |>
    dplyr::group_by(site, date, variable, layer) |> dplyr::summarise(c = mean(value), .groups = "drop") |>
    tidyr::pivot_wider(names_from = layer, values_from = c)
  th <- ctd_thermal |> dplyr::select(site, date, thermo_depth, d_rho)
  p <- ch |> dplyr::inner_join(th, by = c("site", "date")) |> dplyr::arrange(site, variable, date) |>
    dplyr::group_by(site, variable) |>
    dplyr::mutate(date0 = dplyr::lag(date), hyp0 = dplyr::lag(HYP), bot0 = dplyr::lag(BOT), z0 = dplyr::lag(thermo_depth),
                  strat0 = is_stratified(dplyr::lag(d_rho))) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(date0), as.numeric(date - date0) <= max_gap, is_stratified(d_rho), strat0,
                  is.finite(thermo_depth), is.finite(z0), is.finite(HYP), is.finite(BOT), is.finite(hyp0), is.finite(bot0))
  if (!nrow(p)) return(tibble::tibble())
  dplyr::bind_rows(lapply(seq_len(nrow(p)), function(i) {
    r <- p[i, ]; sd <- sample_depths[[r$site]]; b <- mean(sd); z_th <- (r$thermo_depth + r$z0) / 2; days <- as.numeric(r$date - r$date0)
    bed <- max(hyps$depth[hyps$area > 0.01 * max(hyps$area)])
    out <- list()
    if (z_th < b - 1) { v <- vol(z_th, b); a <- area_at(z_th) - area_at(b)
      out$upper <- data.frame(layer = "upper", z0 = z_th, z1 = b, flux = (r$HYP - r$hyp0) * v / days / a) }
    lz <- max(b, z_th); v <- vol(lz, bed); a <- area_at(lz)
    out$lower <- data.frame(layer = "lower", z0 = lz, z1 = bed, flux = (r$BOT - r$bot0) * v / days / a)
    dplyr::bind_rows(out) |> dplyr::mutate(site = r$site, variable = r$variable, date0 = r$date0, date = r$date, days = days, z_th = z_th)
  }))
}
