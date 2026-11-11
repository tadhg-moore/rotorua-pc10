# Helpers for the lake water quality synthesis (website/lake-water-quality-synthesis.qmd).
# Small pure functions: summaries, trend tests, change points and event detection.
# Trend tests and change points are implemented in base R so no extra packages
# are needed (trend / changepoint are not installed in this project).

# ---- Time helpers ----------------------------------------------------------

#' Austral season (Summer = DJF, Autumn = MAM, Winter = JJA, Spring = SON)
austral_season <- function(date) {
  m <- as.integer(format(date, "%m"))
  factor(c("Summer", "Summer", "Autumn", "Autumn", "Autumn", "Winter", "Winter",
           "Winter", "Spring", "Spring", "Spring", "Summer")[m],
         levels = c("Summer", "Autumn", "Winter", "Spring"))
}

#' Hydrological year (July-June) named by the year it ends (same as pc10_hy())
wq_hy <- function(date) {
  as.integer(format(date, "%Y")) + (as.integer(format(date, "%m")) >= 7)
}

# ---- Lake chemistry ---------------------------------------------------------

#' Long format of the lake chemistry with derived variables (mg/m^3)
#'
#' Derived: org_n = TKN - NH4 (organic N), din = NNN + NH4, pp = TP - DRP
#' (particulate + dissolved organic P; "non-DRP"), tn_tp = TN:TP molar ratio.
lake_chem_long <- function(chem) {
  chem |>
    dplyr::mutate(
      org_n = ifelse(tkn - nh4 >= 0, tkn - nh4, NA_real_),
      din = nnn + nh4,
      pp = ifelse(tp - drp >= 0, tp - drp, NA_real_),
      tn_tp = (tn / 14.007) / (tp / 30.974)
    ) |>
    dplyr::select(site_id, site, layer, date, tp, tn, chla, nnn, nh4, drp, tkn,
                  org_n, din, pp, tn_tp, turb, ph, secchi, al) |>
    tidyr::pivot_longer(tp:al, names_to = "variable", values_to = "value",
                        values_drop_na = TRUE) |>
    dplyr::filter(is.finite(value))
}

#' Annual (hydrological year) summaries
#'
#' @param df long data with `date`, `value` and grouping columns.
#' @param min_months keep years with at least this many sampled months.
annual_summary <- function(df, ..., min_months = 6) {
  df |>
    dplyr::mutate(hy = wq_hy(date), ym = format(date, "%Y-%m")) |>
    dplyr::group_by(..., hy) |>
    dplyr::summarise(mean = mean(value), median = stats::median(value),
                     min = min(value), max = max(value), n = dplyr::n(),
                     n_months = dplyr::n_distinct(ym), .groups = "drop") |>
    dplyr::filter(n_months >= min_months)
}

#' Seasonal (monthly) climatology: median and IQR by calendar month
seasonal_climatology <- function(df, ...) {
  df |>
    dplyr::mutate(month = as.integer(format(date, "%m"))) |>
    dplyr::group_by(..., month) |>
    dplyr::summarise(median = stats::median(value),
                     q25 = stats::quantile(value, 0.25),
                     q75 = stats::quantile(value, 0.75),
                     n = dplyr::n(), .groups = "drop")
}

# ---- Trend tests ------------------------------------------------------------

#' Seasonal Mann-Kendall test with Sen slope (Hirsch et al. 1982)
#'
#' Data are reduced to monthly medians; the test sums the Mann-Kendall S and
#' its variance (with tie correction) over the 12 months, and the slope is the
#' median of all within-month pairwise slopes. No correction for serial
#' correlation, so p-values are optimistic for strongly autocorrelated series.
#'
#' @return one-row tibble: n (months), years, slope (units/yr), pct_per_yr
#'   (slope as % of the median), z, p, direction.
seasonal_mk <- function(date, value) {
  ok <- is.finite(value) & !is.na(date)
  if (sum(ok) < 10) return(NULL)
  m <- data.frame(year = as.integer(format(date[ok], "%Y")),
                  month = as.integer(format(date[ok], "%m")),
                  value = value[ok])
  m <- stats::aggregate(value ~ year + month, data = m, FUN = stats::median)
  S <- 0; V <- 0; slopes <- numeric(0)
  for (mm in unique(m$month)) {
    x <- m[m$month == mm, ]
    x <- x[order(x$year), ]
    n <- nrow(x)
    if (n < 2) next
    ij <- utils::combn(n, 2)
    dv <- x$value[ij[2, ]] - x$value[ij[1, ]]
    dy <- x$year[ij[2, ]] - x$year[ij[1, ]]
    S <- S + sum(sign(dv))
    t <- table(x$value); t <- t[t > 1]
    V <- V + (n * (n - 1) * (2 * n + 5) - sum(t * (t - 1) * (2 * t + 5))) / 18
    slopes <- c(slopes, dv / dy)
  }
  if (V <= 0) return(NULL)
  z <- if (S > 0) (S - 1) / sqrt(V) else if (S < 0) (S + 1) / sqrt(V) else 0
  p <- 2 * stats::pnorm(-abs(z))
  slope <- stats::median(slopes)
  med <- stats::median(m$value)
  tibble::tibble(
    n = nrow(m), years = length(unique(m$year)),
    first = min(m$year), last = max(m$year),
    median = med, slope = slope,
    pct_per_yr = if (med != 0) 100 * slope / med else NA_real_,
    z = z, p = p,
    direction = dplyr::case_when(p >= 0.05 ~ "no trend",
                                 slope > 0 ~ "increasing",
                                 slope < 0 ~ "decreasing",
                                 TRUE ~ "no trend")
  )
}

#' Seasonal Mann-Kendall over several periods for each group
#'
#' @param periods named list of length-2 Date vectors (start, end).
#' @param min_years minimum number of years with data in the period.
trend_table <- function(df, ..., periods, min_years = 6) {
  grp <- dplyr::group_by(df, ...)
  dplyr::bind_rows(lapply(names(periods), function(pn) {
    pr <- as.Date(periods[[pn]])
    grp |>
      dplyr::filter(date >= pr[1], date <= pr[2]) |>
      dplyr::group_modify(\(d, k) {
        r <- seasonal_mk(d$date, d$value)
        if (is.null(r) || r$years < min_years) return(tibble::tibble())
        r
      }) |>
      dplyr::ungroup() |>
      dplyr::mutate(period = pn, .before = 1)
  }))
}

# ---- Change points ----------------------------------------------------------

#' Monthly anomalies (monthly median minus the calendar-month median)
monthly_anomaly <- function(date, value) {
  d <- data.frame(ym = as.Date(format(date, "%Y-%m-01")), value = value)
  d <- stats::aggregate(value ~ ym, data = d, FUN = stats::median)
  d$month <- as.integer(format(d$ym, "%m"))
  clim <- tapply(d$value, d$month, stats::median)
  d$anom <- d$value - clim[as.character(d$month)]
  d[order(d$ym), c("ym", "value", "anom")]
}

#' Change points in the mean by binary segmentation (normal likelihood, BIC)
#'
#' Applied to deseasonalised monthly anomalies. A split is kept when the gain
#' in log-likelihood exceeds `penalty * log(n)`; segments are at least
#' `min_seg` months long. Autocorrelation inflates detections, so the default
#' penalty (3, rather than the BIC 1) is conservative.
#'
#' @return tibble: cp_date, mean_before, mean_after, delta (in data units).
detect_changepoints <- function(date, value, min_seg = 24, max_cp = 3,
                                penalty = 3) {
  a <- monthly_anomaly(date, value)
  x <- a$anom; n <- length(x)
  if (n < 2 * min_seg) return(tibble::tibble())
  cost <- function(v) { s <- sum((v - mean(v))^2); length(v) * log(max(s, 1e-12) / length(v)) }
  segs <- list(c(1, n)); cps <- integer(0)
  repeat {
    best <- NULL
    for (s in segs) {
      i0 <- s[1]; i1 <- s[2]
      if (i1 - i0 + 1 < 2 * min_seg) next
      c0 <- cost(x[i0:i1])
      ks <- (i0 + min_seg - 1):(i1 - min_seg)
      gain <- vapply(ks, \(k) c0 - cost(x[i0:k]) - cost(x[(k + 1):i1]), numeric(1))
      j <- which.max(gain)
      if (gain[j] > penalty * log(n) && (is.null(best) || gain[j] > best$gain))
        best <- list(gain = gain[j], k = ks[j], seg = s)
    }
    if (is.null(best) || length(cps) >= max_cp) break
    cps <- c(cps, best$k)
    segs <- c(Filter(\(s) !identical(s, best$seg), segs),
              list(c(best$seg[1], best$k), c(best$k + 1, best$seg[2])))
  }
  if (!length(cps)) return(tibble::tibble())
  cps <- sort(cps); b <- c(0, cps, n)
  tibble::tibble(
    cp_date = a$ym[cps + 1],
    mean_before = vapply(seq_along(cps), \(i) mean(a$value[(b[i] + 1):b[i + 1]]), numeric(1)),
    mean_after = vapply(seq_along(cps), \(i) mean(a$value[(b[i + 1] + 1):b[i + 2]]), numeric(1))
  ) |>
    dplyr::mutate(delta = mean_after - mean_before)
}

# ---- Event detection --------------------------------------------------------

#' Runs of consecutive TRUE days (gaps of up to `max_gap` days are bridged)
#' @return tibble: start, end, duration (days)
run_events <- function(date, flag, min_len = 1, max_gap = 0) {
  d <- sort(unique(date[flag & !is.na(flag)]))
  if (!length(d)) return(tibble::tibble(start = as.Date(character()),
                                        end = as.Date(character()), duration = integer()))
  brk <- c(TRUE, diff(as.numeric(d)) > max_gap + 1)
  id <- cumsum(brk)
  tibble::tibble(date = d, id = id) |>
    dplyr::group_by(id) |>
    dplyr::summarise(start = min(date), end = max(date), .groups = "drop") |>
    dplyr::mutate(duration = as.integer(end - start) + 1L) |>
    dplyr::filter(duration >= min_len) |>
    dplyr::select(-id)
}

#' Day-of-year climatology (mean and percentile, 11-day window)
doy_climatology <- function(date, x, base_years = NULL, pct = 0.9, window = 5) {
  d <- data.frame(date = date, x = x)
  if (!is.null(base_years)) d <- d[as.integer(format(d$date, "%Y")) %in% base_years, ]
  doy <- pmin(as.integer(format(d$date, "%j")), 365)
  out <- lapply(1:365, function(j) {
    dd <- abs(doy - j); dd <- pmin(dd, 365 - dd)
    v <- d$x[dd <= window]
    c(doy = j, clim = mean(v, na.rm = TRUE),
      thresh = unname(stats::quantile(v, pct, na.rm = TRUE)),
      thresh_lo = unname(stats::quantile(v, 1 - pct, na.rm = TRUE)))
  })
  as.data.frame(do.call(rbind, out))
}

#' Heatwaves (Hobday et al. 2016): >= min_len days above the day-of-year
#' 90th percentile, gaps of <= 2 days bridged
#'
#' @return list(daily = data with clim/thresh, events = tibble of events with
#'   duration, peak date, max and mean intensity (above climatology), and
#'   Hobday category (1 moderate, 2 strong, 3 severe, 4 extreme)).
detect_heatwaves <- function(date, temp, base_years = NULL, pct = 0.9,
                             min_len = 5, max_gap = 2) {
  ok <- !is.na(temp)
  date <- as.Date(date[ok]); temp <- temp[ok]
  cl <- doy_climatology(date, temp, base_years, pct)
  doy <- pmin(as.integer(format(date, "%j")), 365)
  clim_v <- cl$clim[doy]; thresh_v <- cl$thresh[doy]
  daily <- tibble::tibble(date = date, temp = temp, clim = clim_v, thresh = thresh_v) |>
    dplyr::mutate(anom = temp - clim, above = temp > thresh)
  ev <- run_events(daily$date, daily$above, min_len = min_len, max_gap = max_gap)
  if (!nrow(ev)) return(list(daily = daily, events = ev))
  ev <- dplyr::bind_rows(lapply(seq_len(nrow(ev)), function(i) {
    w <- daily[daily$date >= ev$start[i] & daily$date <= ev$end[i], ]
    j <- which.max(w$anom)
    tibble::tibble(start = ev$start[i], end = ev$end[i], duration = ev$duration[i],
                   peak = w$date[j], max_temp = max(w$temp),
                   max_anom = w$anom[j], mean_anom = mean(w$anom),
                   category = pmin(4, floor(w$anom[j] / (w$thresh[j] - w$clim[j]))))
  })) |>
    dplyr::mutate(category = factor(pmax(category, 1), 1:4,
                                    c("Moderate", "Strong", "Severe", "Extreme")))
  list(daily = daily, events = ev)
}

#' Storm (heavy rain) events from daily rain
#'
#' Events are days whose `window`-day running total exceeds the `pct`
#' percentile of all running totals; days within `sep_days` are one event.
#' @return tibble: start, end, peak_date, total_mm (max running total), max_day_mm, rank
detect_storms <- function(date, rain_mm, window = 3, pct = 0.99, sep_days = 3,
                          wind = NULL) {
  o <- order(date); date <- as.Date(date[o]); rain_mm <- rain_mm[o]
  roll <- as.numeric(stats::filter(rain_mm, rep(1, window), sides = 1))
  thr <- stats::quantile(roll, pct, na.rm = TRUE)
  ev <- run_events(date, roll > thr, max_gap = sep_days)
  if (!nrow(ev)) return(ev)
  out <- dplyr::bind_rows(lapply(seq_len(nrow(ev)), function(i) {
    idx <- which(date >= ev$start[i] - (window - 1) & date <= ev$end[i])
    j <- idx[which.max(roll[idx])]
    tibble::tibble(start = date[min(idx)], end = ev$end[i], peak_date = date[j],
                   total_mm = roll[j], max_day_mm = max(rain_mm[idx]),
                   max_wind = if (!is.null(wind)) max(wind[o][idx], na.rm = TRUE) else NA_real_)
  }))
  out$rank <- rank(-out$total_mm, ties.method = "first")
  attr(out, "threshold_mm") <- unname(thr)
  dplyr::arrange(out, start)
}

# ---- Trophic Level Index ----------------------------------------------------

#' Burns et al. (1999/2000) Trophic Level Index from annual means
#'
#' Uses integrated (INT) samples. TLI3 = mean of chla, TP, TN components;
#' TLI4 adds Secchi. Annual means by hydrological year (July-June) per site,
#' the lake value is the mean of the two sites; `tli3_3yr` is the 3-year
#' rolling mean (the basis of the Rotorua TLI target of 4.2).
tli_annual <- function(chem, min_months = 6) {
  site_yr <- chem |>
    dplyr::filter(layer == "INT") |>
    dplyr::mutate(hy = wq_hy(date), ym = format(date, "%Y-%m")) |>
    dplyr::group_by(site, hy) |>
    dplyr::summarise(
      n_months = dplyr::n_distinct(ym),
      chla = mean(chla, na.rm = TRUE), tp = mean(tp, na.rm = TRUE),
      tn = mean(tn, na.rm = TRUE), secchi = mean(secchi, na.rm = TRUE),
      .groups = "drop") |>
    dplyr::filter(n_months >= min_months) |>
    dplyr::mutate(dplyr::across(c(chla, tp, tn, secchi), \(x) ifelse(is.nan(x), NA_real_, x)))
  comp <- function(d) d |>
    dplyr::mutate(
      tl_c = 2.22 + 2.54 * log10(chla),
      tl_p = 0.218 + 2.92 * log10(tp),
      tl_n = -3.61 + 3.01 * log10(tn),
      tl_s = 5.10 + 2.27 * log10(1 / secchi - 1 / 40),
      tli3 = (tl_c + tl_p + tl_n) / 3,
      tli4 = (tl_c + tl_p + tl_n + tl_s) / 4)
  lake <- site_yr |>
    dplyr::group_by(hy) |>
    dplyr::summarise(dplyr::across(c(chla, tp, tn, secchi), \(x) mean(x, na.rm = TRUE)),
                     n_sites = dplyr::n(), .groups = "drop") |>
    dplyr::mutate(site = "Lake (mean of sites)")
  dplyr::bind_rows(comp(site_yr), comp(lake)) |>
    dplyr::arrange(site, hy) |>
    dplyr::group_by(site) |>
    dplyr::mutate(tli3_3yr = as.numeric(stats::filter(tli3, rep(1 / 3, 3), sides = 1)),
                  tli4_3yr = as.numeric(stats::filter(tli4, rep(1 / 3, 3), sides = 1))) |>
    dplyr::ungroup()
}

# ---- Profiles: buoy and CTD --------------------------------------------------

#' Daily surface / bottom summaries from profile data
#'
#' @param df long data: date, depth_m, temp_c, do_mgl (wide on variables).
#' @param surf_max surface layer is depth <= surf_max; bottom is depth >= bot_min.
profile_daily_metrics <- function(df, surf_max = 1.5, bot_min = 18) {
  df |>
    dplyr::group_by(source, date) |>
    dplyr::summarise(
      t_surf = mean(temp_c[depth_m <= surf_max], na.rm = TRUE),
      t_bot = mean(temp_c[depth_m >= bot_min], na.rm = TRUE),
      do_surf = mean(do_mgl[depth_m <= surf_max], na.rm = TRUE),
      do_bot = mean(do_mgl[depth_m >= bot_min], na.rm = TRUE),
      max_depth = max(depth_m), .groups = "drop") |>
    dplyr::mutate(dplyr::across(t_surf:do_bot, \(x) ifelse(is.nan(x), NA_real_, x)),
                  d_temp = t_surf - t_bot,
                  d_rho = rLakeAnalyzer::water.density(t_bot) -
                    rLakeAnalyzer::water.density(t_surf))
}

# ---- Drivers ---------------------------------------------------------------

#' Annual (hydrological year) and summer (DJF) met summaries from daily ERA5-BC
met_annual <- function(met) {
  d <- met |>
    dplyr::transmute(date = as.Date(Date), tair = MET_tmpair, rain_mm = MET_pprain * 1000,
                     wind = MET_wndspd, swr = MET_radswd, rh = MET_humrel,
                     hy = wq_hy(date), season = austral_season(date))
  ann <- d |>
    dplyr::group_by(hy) |>
    dplyr::summarise(n = dplyr::n(), tair = mean(tair), rain_mm = sum(rain_mm),
                     wind = mean(wind), swr = mean(swr), rh = mean(rh), .groups = "drop") |>
    dplyr::filter(n >= 360)
  # Summer of hy y is Dec (y-1) - Feb (y), all inside the same hydrological year
  sum_ <- d |>
    dplyr::filter(season == "Summer") |>
    dplyr::group_by(hy) |>
    dplyr::summarise(n_s = dplyr::n(), tair_djf = mean(tair), wind_djf = mean(wind),
                     swr_djf = mean(swr), rain_djf_mm = sum(rain_mm), .groups = "drop") |>
    dplyr::filter(n_s >= 85) |>
    dplyr::select(-n_s)
  dplyr::left_join(ann, sum_, by = "hy") |> dplyr::select(-n)
}

#' Spearman correlations between annual responses and drivers, with lags
#'
#' Both raw and first-differenced (year-to-year change) correlations are
#' returned; differencing removes the shared long-term trend.
driver_correlations <- function(annual, responses, drivers, lags = 0:1) {
  annual <- dplyr::arrange(annual, hy)
  res <- list()
  for (r in responses) for (dv in drivers) for (lg in lags) {
    y <- annual[[r]]
    x <- dplyr::lag(annual[[dv]], lg)
    for (type in c("level", "difference")) {
      yy <- if (type == "level") y else c(NA, diff(y))
      xx <- if (type == "level") x else c(NA, diff(x))
      ok <- is.finite(yy) & is.finite(xx)
      if (sum(ok) < 8) next
      ct <- suppressWarnings(stats::cor.test(yy[ok], xx[ok], method = "spearman"))
      res[[length(res) + 1]] <- tibble::tibble(
        response = r, driver = dv, lag = lg, type = type, n = sum(ok),
        rho = unname(ct$estimate), p = ct$p.value)
    }
  }
  dplyr::bind_rows(res)
}
