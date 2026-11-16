# What starts and ends stratification on Lake Rotorua: daily stability, wind and surface heat flux
# from the buoy profiles and lake meteorology, and the onsets and breakdowns of buoy-recorded
# stratified periods. Used by the targets lake_met_daily, lake_stability_daily and
# strat_transitions, and website/lake-water-quality-synthesis.qmd (#sec-strat-drivers).
# Needs bin_profile_1m() from R/wq_synthesis_helpers.R.

#' Daily lake meteorology: buoy measurements where available, ERA5-BC otherwise
#'
#' Wind, air temperature and RH are the buoy's (15-min to 2021, hourly from 2022) on days
#' with >= 75% coverage, otherwise ERA5-BC (bias-corrected to the buoy). Shortwave and
#' longwave are always ERA5-BC: the buoy has no longwave and only patchy shortwave from 2022.
#' Wind is converted from the anemometer height (`wind_height`, assumed about 2 m) to 10 m
#' with metscale::met_wind_at_height() (neutral log profile, its default water-surface
#' z0 = 0.2 mm; factor about 1.17 at 2 m). This applies to ERA5-BC too: its wind is
#' bias-corrected to the buoy (same mean over 2007-2021, r = 0.90 daily), so it is at buoy
#' height, not 10 m.
#' `wind_rms` (root mean square of the sub-daily wind) carries the daily wind stress, and
#' `p_wind` is the mean wind energy flux rho_a Cd U10^3 (W/m2).
#' @param buoy_met all_buoy_met_data (Date, MET_wndspd, MET_tmpair, MET_humrel)
#' @param era5 era5_corrected_hourly (Date, MET_*)
lake_met_daily <- function(buoy_met, era5, wind_height = 2, cd = 1.3e-3) {
  tz <- "Etc/GMT-12"
  to10 <- function(m) metscale::met_wind_at_height(as.data.frame(m), from = wind_height, to = 10)
  b <- buoy_met |>
    dplyr::select(Date, MET_wndspd, MET_tmpair, MET_humrel) |>
    to10() |>
    dplyr::arrange(Date) |>
    dplyr::mutate(date = as.Date(Date, tz = tz), u10 = MET_wndspd) |>
    dplyr::group_by(date) |>
    dplyr::summarise(step_h = stats::median(diff(as.numeric(Date))) / 3600,
                     n_wnd = sum(is.finite(u10)), n_ta = sum(is.finite(MET_tmpair)), n_rh = sum(is.finite(MET_humrel)),
                     u = mean(u10, na.rm = TRUE), u_rms = sqrt(mean(u10^2, na.rm = TRUE)),
                     u3 = mean(u10^3, na.rm = TRUE), u_max = suppressWarnings(max(u10, na.rm = TRUE)),
                     ta = mean(MET_tmpair, na.rm = TRUE), rh = mean(MET_humrel, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(n_exp = ifelse(is.finite(step_h) & step_h > 0, 24 / step_h, 96),
                  ok_w = n_wnd >= 0.75 * n_exp, ok_t = n_ta >= 0.75 * n_exp,
                  ok_rh = n_rh >= 0.75 * n_exp & is.finite(rh) & rh > 5 & rh <= 100)
  e <- era5 |>
    dplyr::select(Date, MET_wndspd, MET_tmpair, MET_humrel, MET_radswd, MET_radlwd) |>
    to10() |>
    dplyr::mutate(date = as.Date(Date, tz = tz), u10 = MET_wndspd) |>
    dplyr::group_by(date) |>
    dplyr::summarise(e_u = mean(u10), e_u_rms = sqrt(mean(u10^2)), e_u3 = mean(u10^3),
                     e_u_max = max(u10), e_ta = mean(MET_tmpair), e_rh = mean(MET_humrel),
                     sw = mean(MET_radswd), lw = mean(MET_radlwd), .groups = "drop")
  e |>
    dplyr::left_join(b, by = "date") |>
    dplyr::mutate(ok_w = ok_w %in% TRUE, ok_t = ok_t %in% TRUE, ok_rh = ok_rh %in% TRUE) |>
    dplyr::transmute(date,
                     wind_src = ifelse(ok_w, "buoy", "ERA5-BC"),
                     wind = ifelse(ok_w, u, e_u), wind_rms = ifelse(ok_w, u_rms, e_u_rms),
                     wind_max = ifelse(ok_w, u_max, e_u_max), p_wind = 1.2 * cd * ifelse(ok_w, u3, e_u3),
                     tair = ifelse(ok_t, ta, e_ta), rh = ifelse(ok_rh, rh, e_rh), sw, lw)
}

#' Daily surface heat budget (W/m2, positive = into the lake) from daily-mean met and the
#' buoy surface water temperature. Bulk formulas: albedo 0.07, emissivity 0.97,
#' Ch = Ce = 1.3e-3 (10 m wind), station pressure 980 hPa. Daily means smooth out the
#' night-time cooling, so this is the net daily budget, not the convective peak.
surface_heat_flux <- function(t_surf, tair, rh, wind, sw, lw, ch = 1.3e-3, ce = 1.3e-3, p_hpa = 980) {
  es <- function(t) 6.112 * exp(17.67 * t / (t + 243.5))
  q <- function(e) 0.622 * e / (p_hpa - 0.378 * e)
  sw_net <- 0.93 * sw
  lw_net <- 0.97 * lw - 0.97 * 5.67e-8 * (t_surf + 273.15)^4
  sens <- -1.2 * 1005 * ch * wind * (t_surf - tair)
  lat <- -1.2 * 2.45e6 * ce * wind * (q(es(t_surf)) - q(rh / 100 * es(tair)))
  data.frame(q_sw = sw_net, q_lw = lw_net, q_sens = sens, q_lat = lat, q_net = sw_net + lw_net + sens + lat)
}

#' Daily stability of the water column from the buoy profiles, with the lake met
#'
#' Per day (the new buoy when both report): Schmidt stability (J/m2); the metalimnion
#' (rLakeAnalyzer::meta.depths on 1 m bins, NA when mixed); epilimnion and hypolimnion
#' densities; the water-side friction velocity u* of the daily rms 10 m wind; the Lake
#' number (wind can mix the hypolimnion when < 1) and the Wedderburn number (the
#' thermocline can upwell at the upwind end when < 1); and the surface heat budget.
#' @param profiles buoy_profile_long (source, date, depth_m, temp_c)
#' @param met lake_met_daily()
#' @param hyps lake_hyps (depth, area)
#' @param events_daily buoy_events$daily (date, t_surf, t_bot, d_temp)
lake_stability_daily <- function(profiles, met, hyps, events_daily) {
  bthA <- hyps$area; bthD <- hyps$depth; ao <- max(bthA)
  pr <- profiles |>
    dplyr::filter(is.finite(temp_c)) |>
    dplyr::group_by(date) |>
    dplyr::filter(source == ifelse(any(source == "new buoy"), "new buoy", "old buoy")) |>
    dplyr::ungroup()
  one <- function(p) {
    b <- bin_profile_1m(p$depth_m, p$temp_c); b <- b[b$depth <= max(bthD), ]
    out <- data.frame(date = p$date[1], source = p$source[1], st = NA_real_, meta_top = NA_real_,
                      meta_bot = NA_real_, rho_epi = NA_real_, rho_hyp = NA_real_)
    if (nrow(b) < 5 || max(b$depth) < 15) return(out)
    out$st <- suppressWarnings(rLakeAnalyzer::schmidt.stability(b$temp, b$depth, bthA, bthD))
    md <- suppressWarnings(rLakeAnalyzer::meta.depths(b$temp, b$depth, mixed.cutoff = 1))
    if (all(is.finite(md)) && md[2] > md[1] && md[2] < max(b$depth)) {
      out$meta_top <- md[1]; out$meta_bot <- md[2]
      out$rho_epi <- suppressWarnings(rLakeAnalyzer::layer.density(0, md[1], b$temp, b$depth, bthA, bthD))
      out$rho_hyp <- suppressWarnings(rLakeAnalyzer::layer.density(md[2], max(b$depth), b$temp, b$depth, bthA, bthD))
    }
    out
  }
  s <- dplyr::bind_rows(lapply(split(pr, pr$date), one)) |>
    dplyr::left_join(events_daily |> dplyr::select(date, t_surf, t_bot, d_temp), by = "date") |>
    dplyr::left_join(met, by = "date")
  rho_w <- ifelse(is.finite(s$rho_epi), s$rho_epi, 998)
  s$u_star <- rLakeAnalyzer::uStar(s$wind_rms, 10, rho_w)
  s$lake_number <- mapply(function(us, st, mt, mb, rh) {
    if (!all(is.finite(c(us, st, mt, mb, rh))) || us <= 0 || st <= 0) return(NA_real_)
    suppressWarnings(rLakeAnalyzer::lake.number(bthA, bthD, us, st, mt, mb, rh))
  }, s$u_star, s$st, s$meta_top, s$meta_bot, s$rho_hyp)
  s$wedderburn <- ifelse(is.finite(s$meta_top) & is.finite(s$u_star) & s$u_star > 0,
                         suppressWarnings(rLakeAnalyzer::wedderburn.number(s$rho_hyp - s$rho_epi, s$meta_top, s$u_star, ao, s$rho_hyp)),
                         NA_real_)
  hf <- surface_heat_flux(s$t_surf, s$tair, s$rh, s$wind, s$sw, s$lw)
  dplyr::as_tibble(dplyr::bind_cols(s, hf))
}

#' Day-of-year percentile of x among all days of the record within +-`win` days of the same day of year
doy_percentile <- function(date, x, win = 15) {
  doy <- as.integer(format(date, "%j"))
  vapply(seq_along(x), function(i) {
    if (!is.finite(x[i])) return(NA_real_)
    dd <- abs(doy - doy[i]); dd <- pmin(dd, 366 - dd)
    ref <- x[dd <= win & is.finite(x)]
    mean(ref <= x[i])
  }, numeric(1))
}

#' Onsets and breakdowns of buoy-recorded stratification, and the forcing around each
#'
#' Onset: the first day of a stratified period (buoy_events$stratified) with at least 2 of
#' the 3 previous days observed mixed (surface - bottom <= 1 C). Breakdown: the first day
#' after a period, when at least 2 of the 3 following days are observed mixed, so periods
#' that end in a data gap are left out. Day 0 is the onset day or the first mixed day.
#' Each breakdown is classed by the mean forcing over days -2 to 0 (3-day means), as
#' day-of-year percentiles of all 3-day means within +-15 days of the date in any year:
#' "wind" when the wind energy flux is at or above the 75th percentile, "cooling" when the
#' net surface heat flux is at or below the 25th percentile, "wind + cooling" for both, and
#' "neither" otherwise (no unusual forcing). By chance alone each class would occur on 25%
#' of days (wind, cooling) or less (both), which is the baseline for the shares.
#' Percentiles and anomalies make the classes insensitive to the bias of the bulk heat
#' budget (not closed: about -48 W/m2 on average).
#' @return list(events, composite = days -window..+window around each event, daily)
strat_transitions <- function(stab, stratified, window = 10) {
  roll3 <- function(x) as.numeric(stats::filter(x, rep(1 / 3, 3), sides = 1))
  d <- stab |>
    dplyr::arrange(date) |>
    tidyr::complete(date = seq(min(date), max(date), by = "day")) |>
    dplyr::mutate(p_wind3 = roll3(p_wind), q_net3 = roll3(q_net),
                  p_wind_pct = doy_percentile(date, p_wind3), q_net_pct = doy_percentile(date, q_net3),
                  ta_tw = tair - t_surf, q_net_anom = q_net - doy_mean(date, q_net),
                  wind_anom = wind - doy_mean(date, wind), ta_tw_anom = ta_tw - doy_mean(date, ta_tw))
  mixed_obs <- function(days) sum(d$d_temp[match(days, d$date)] <= 1, na.rm = TRUE)
  on <- stratified |>
    dplyr::filter(vapply(start, function(s) mixed_obs(s - 1:3) >= 2, TRUE)) |>
    dplyr::transmute(type = "onset", day0 = start, start, end, duration)
  br <- stratified |>
    dplyr::filter(vapply(end, function(e) mixed_obs(e + 1:3) >= 2, TRUE)) |>
    dplyr::transmute(type = "breakdown", day0 = end + 1, start, end, duration)
  ev <- dplyr::bind_rows(on, br) |> dplyr::mutate(id = dplyr::row_number())
  w3 <- function(e, v, f) { x <- d[[v]][match(e - 2:0, d$date)]; if (all(is.na(x))) NA_real_ else f(x, na.rm = TRUE) }
  ev <- ev |>
    dplyr::mutate(
      wind_pct = d$p_wind_pct[match(day0, d$date)],     # 3-day mean, days -2..0
      cool_pct = d$q_net_pct[match(day0, d$date)],
      wind_max = vapply(day0, w3, 0, v = "wind_max", f = max),
      q_net_3d = vapply(day0, w3, 0, v = "q_net", f = mean),
      st_last = d$st[match(day0 - 1, d$date)],
      st_peak = mapply(function(a, b) suppressWarnings(max(d$st[d$date >= a & d$date <= b], na.rm = TRUE)), start, end),
      ln_min = vapply(day0 - 1, w3, 0, v = "lake_number", f = min),
      class = dplyr::case_when(
        type != "breakdown" | is.na(wind_pct) | is.na(cool_pct) ~ NA_character_,
        wind_pct >= 0.75 & cool_pct <= 0.25 ~ "wind + cooling",
        wind_pct >= 0.75 ~ "wind", cool_pct <= 0.25 ~ "cooling", TRUE ~ "neither"))
  comp <- dplyr::bind_rows(lapply(seq_len(nrow(ev)), function(i) {
    d |>
      dplyr::filter(date >= ev$day0[i] - window, date <= ev$day0[i] + window) |>
      dplyr::transmute(id = ev$id[i], type = ev$type[i], lag = as.integer(date - ev$day0[i]), date,
                       wind, wind_anom, p_wind, tair, ta_tw, ta_tw_anom, q_net, q_net_anom, st, d_temp, lake_number)
  }))
  list(events = ev, composite = comp, daily = d)
}

#' Day-of-year mean of x (all days of the record within +-`win` days of the same day of year)
doy_mean <- function(date, x, win = 15) {
  doy <- as.integer(format(date, "%j"))
  clim <- vapply(1:366, function(j) { dd <- abs(doy - j); dd <- pmin(dd, 366 - dd); mean(x[dd <= win], na.rm = TRUE) }, 0)
  clim[doy]
}
