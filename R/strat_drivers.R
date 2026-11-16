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
#' Wind is used as supplied at 10 m: all_buoy_met_data is already converted to 10 m
#' (metscale::wind_at_height() in old_buoy_met_data() and buoy_to_aeme_met()), and ERA5-BC
#' wind is bias-corrected to that 10 m buoy wind. `wind_height` is only for input at another
#' height (converted with metscale::met_wind_at_height()).
#' `wind_rms` (root mean square of the sub-daily wind) carries the daily wind stress, and
#' `p_wind` is the mean wind energy flux rho_a Cd U10^3 (W/m2).
#' @param buoy_met all_buoy_met_data (Date, MET_wndspd, MET_tmpair, MET_humrel)
#' @param era5 era5_corrected_hourly (Date, MET_*)
#' @param rh_source "era5" (default) or "buoy". The old-buoy RH reads 5-9% low against ERA5
#'   and the airport and the new buoy RH drifts from 2024 (see buoy_met_fit_ref in _targets.R; ERA5 RH agrees with the 2022-23 buoy and airport within 2-3%),
#'   which inflates the latent heat loss; ERA5 RH is used for the heat budget.
lake_met_daily <- function(buoy_met, era5, wind_height = 10, cd = 1.3e-3, rh_source = c("era5", "buoy")) {
  rh_source <- match.arg(rh_source)
  tz <- "Etc/GMT-12"
  to10 <- function(m) if (wind_height == 10) as.data.frame(m) else
    metscale::met_wind_at_height(as.data.frame(m), from = wind_height, to = 10)
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
                     tair = ifelse(ok_t, ta, e_ta), rh = if (rh_source == "buoy") ifelse(ok_rh, rh, e_rh) else e_rh, sw, lw)
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
  # Breakdown day: the first observed mixed day in the last 3 days of the period or the day after.
  # The period itself bridges mixed gaps of up to 2 days, so a lake that mixes and then briefly
  # re-stratifies (e.g. 7-8 Feb 2015) would otherwise be dated after the event that mixed it.
  first_mixed <- function(e) { dd <- e + (-2:1); m <- d$d_temp[match(dd, d$date)] <= 1; as.numeric(dd[which(m %in% TRUE)[1]]) }
  br <- stratified |>
    dplyr::filter(vapply(end, function(e) mixed_obs(e + 1:3) >= 2, TRUE)) |>
    dplyr::mutate(day0 = as.Date(vapply(end, first_mixed, 0), origin = "1970-01-01")) |>
    dplyr::transmute(type = "breakdown", day0, start, end, duration)
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

#' Hourly view of one stratified period from the old buoy's 15-min record
#'
#' Hourly means of the water temperature at every sensor depth, DO at 1 m and 20 m, and the
#' buoy met (wind converted from 1.5 m to 10 m with metscale::wind_at_height(), as in
#' old_buoy_met_data()), with ERA5-BC radiation and ERA5 RH, and the hourly surface heat budget.
#' @param raw rotorua_old_buoy_data (var_ref_id, datetime, qc_value)
#' @param era5 era5_corrected_hourly
buoy_event_hourly <- function(raw, era5, from, to, tz = "Etc/GMT-12") {
  f <- as.POSIXct(from, tz = tz); t <- as.POSIXct(to, tz = tz)
  h <- raw |>
    dplyr::filter(datetime >= f, datetime < t, is.finite(qc_value)) |>
    dplyr::mutate(hour = lubridate::floor_date(lubridate::with_tz(datetime, tz), "hour")) |>
    dplyr::group_by(var_ref_id, hour) |>
    dplyr::summarise(v = mean(qc_value), .groups = "drop")
  wt <- h |> dplyr::filter(grepl("^t_wtr_d", var_ref_id)) |>
    dplyr::transmute(hour, depth = as.numeric(sub("t_wtr_d", "", var_ref_id)) / 100, temp = v)
  wide <- h |> dplyr::filter(!grepl("^t_wtr_d", var_ref_id)) |>
    tidyr::pivot_wider(names_from = var_ref_id, values_from = v)
  met <- wide |> dplyr::transmute(hour, wind = metscale::wind_at_height(w_spd_h150, from = 1.5, to = 10),
                                  wdir = w_dir_h150, tair = t_air_h150, rain = pp_rain_h150,
                                  do_1 = c_do_d100, do_20 = c_do_d2000)
  era <- era5 |>
    dplyr::filter(Date >= f, Date < t) |>
    dplyr::transmute(hour = lubridate::with_tz(Date, tz), sw = MET_radswd, lw = MET_radlwd, rh = MET_humrel)
  hh <- wt |>
    dplyr::group_by(hour) |>
    dplyr::summarise(t_s = temp[which.min(depth)], t_b = temp[which.max(depth)], .groups = "drop") |>
    dplyr::mutate(d_temp = t_s - t_b,
                  d_rho = rLakeAnalyzer::water.density(t_b) - rLakeAnalyzer::water.density(t_s)) |>
    dplyr::left_join(met, by = "hour") |>
    dplyr::left_join(era, by = "hour")
  hh <- dplyr::bind_cols(hh, surface_heat_flux(hh$t_s, hh$tair, hh$rh, hh$wind, hh$sw, hh$lw))
  list(hourly = hh, temp = wt)
}
