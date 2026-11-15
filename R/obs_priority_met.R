# Measured-first meteorology for the extreme-event analysis.
#
# For each variable the measured record is used wherever it exists and the bias-corrected ERA5 hourly series
# (`era5_corrected_hourly`) only fills the gaps. Every value carries its source so the mix can be shown.
#   rain (daily, mm)         : Whakarewarewa rain gauge (pc10_climate_met, 1901-)  >  MetService airport hourly rain (1993-2023, daily sums)  >  bias-corrected ERA5
#   air temperature (daily max, degC) : lake buoy (2007-)  >  airport (MetService hourly, put on the lake scale with the pipeline's own
#                              airport -> buoy transfer function, 1993-2023)  >  bias-corrected ERA5
#   wind speed (hourly, m/s) : lake buoy (2007-; hourly means)  >  bias-corrected ERA5.  Airport wind is not used: it reads ~35% lower than the
#                              lake (3.6 vs 5.3 m/s) and the pipeline has no airport -> buoy wind transfer.
# The buoy rain record is not used (mean daily rain near zero, see climate-scenario-workflow.qmd).
# All times are in the pipeline's fixed NZST (Etc/GMT-12) so a "day" is the same in every source.

#' Hourly means of a Date + MET_* data frame (sub-hourly buoy records are averaged to the hour).
.hourly_mean <- function(df, var, tz = "Etc/GMT-12") {
  d <- df[!is.na(df[[var]]), c("Date", var)]
  names(d) <- c("Date", "x")
  d$hour <- as.POSIXct(format(d$Date, "%Y-%m-%d %H:00:00", tz = tz), tz = tz)
  out <- stats::aggregate(x ~ hour, data = d, FUN = mean)
  names(out) <- c("Date", var)
  out
}

#' Build the measured-first daily rain and tmax and the hourly wind series.
#' @param whaka `pc10_climate_met` (daily, MET_pprain in m/day)
#' @param airport `airport_met_aeme` (MetService hourly; MET_pprain in m/h)
#' @param airport_to_lake optional metscale bias-correction object (`niwa_to_buoy_bias_correction`) used to put airport temperature on the lake scale
#' @param buoy `all_buoy_met_data`
#' @param era5 `era5_corrected_hourly`
#' @param min_hours minimum number of hourly values for a day's maximum temperature to count as measured
#' @return list(daily = data.frame(date, rain_mm, rain_src, tmax, tmax_src), wind = data.frame(Date, wind, src))
build_obs_priority_met <- function(whaka, airport, airport_to_lake = NULL, buoy, era5, min_hours = 18, tz = "Etc/GMT-12") {
  # ---- ERA5 (bias-corrected) daily and hourly: the gap filler
  era5 <- as.data.frame(era5)
  e_day <- as.Date(format(era5$Date, "%Y-%m-%d", tz = tz))
  e_daily <- data.frame(date = sort(unique(e_day)))
  e_daily$rain_mm <- as.numeric(tapply(era5$MET_pprain * 1000, e_day, sum, na.rm = TRUE)[as.character(e_daily$date)])
  e_daily$tmax    <- as.numeric(tapply(era5$MET_tmpair, e_day, max, na.rm = TRUE)[as.character(e_daily$date)])

  # ---- rain: Whakarewarewa gauge > airport hourly sums > ERA5
  wh <- data.frame(date = as.Date(whaka$Date), rain_mm = whaka$MET_pprain * 1000)
  wh <- wh[!is.na(wh$rain_mm), ]
  ap_day <- as.Date(format(airport$Date, "%Y-%m-%d", tz = tz))
  ap_n   <- tapply(!is.na(airport$MET_pprain), ap_day, sum)
  ap <- data.frame(date = as.Date(names(ap_n)), n = as.integer(ap_n),
                   rain_mm = as.numeric(tapply(airport$MET_pprain * 1000, ap_day, sum, na.rm = TRUE)[names(ap_n)]))
  ap <- ap[ap$n >= min_hours, c("date", "rain_mm")]

  # ---- temperature: buoy hourly > airport (lake scale) hourly > ERA5
  tmax_from_hourly <- function(h) {                       # h: Date, MET_tmpair (hourly)
    dd <- as.Date(format(h$Date, "%Y-%m-%d", tz = tz))
    n <- tapply(!is.na(h$MET_tmpair), dd, sum); mx <- tapply(h$MET_tmpair, dd, max, na.rm = TRUE)
    out <- data.frame(date = as.Date(names(n)), tmax = as.numeric(mx[names(n)]), n = as.integer(n))
    out[out$n >= min_hours & is.finite(out$tmax), c("date", "tmax")]
  }
  bu_t <- tmax_from_hourly(.hourly_mean(as.data.frame(buoy), "MET_tmpair", tz))
  ap_h <- as.data.frame(airport)[, c("Date", "MET_tmpair")]
  if (!is.null(airport_to_lake)) ap_h <- as.data.frame(metscale::apply_met_bias_correction(airport[, c("Date", "MET_tmpair")], airport_to_lake, verbose = FALSE))[, c("Date", "MET_tmpair")]
  ap_t <- tmax_from_hourly(.hourly_mean(ap_h, "MET_tmpair", tz))

  dates <- e_daily$date
  pick <- function(primary, secondary, fallback, col) {
    v <- fallback[[col]][match(dates, fallback$date)]; src <- rep("ERA5 (bias-corrected)", length(dates))
    i2 <- match(dates, secondary$date); ok2 <- !is.na(i2); v[ok2] <- secondary[[col]][i2[ok2]]; src[ok2] <- attr(secondary, "src")
    i1 <- match(dates, primary$date);   ok1 <- !is.na(i1); v[ok1] <- primary[[col]][i1[ok1]];   src[ok1] <- attr(primary, "src")
    list(v = v, src = src)
  }
  attr(wh, "src") <- "Whakarewarewa gauge"; attr(ap, "src") <- "Airport (MetService)"
  attr(bu_t, "src") <- "Lake buoy"; attr(ap_t, "src") <- "Airport (lake scale)"
  r <- pick(wh, ap, e_daily, "rain_mm"); t <- pick(bu_t, ap_t, e_daily, "tmax")
  daily <- data.frame(date = dates, rain_mm = r$v, rain_src = r$src, tmax = t$v, tmax_src = t$src)

  # ---- wind: buoy hourly means, ERA5 for every hour without one
  bu_w <- .hourly_mean(as.data.frame(buoy), "MET_wndspd", tz)
  w <- data.frame(Date = era5$Date, wind = era5$MET_wndspd, src = "ERA5 (bias-corrected)", stringsAsFactors = FALSE)
  k <- match(as.numeric(w$Date), as.numeric(bu_w$Date)); ok <- !is.na(k)
  w$wind[ok] <- bu_w$MET_wndspd[k[ok]]; w$src[ok] <- "Lake buoy"
  list(daily = daily, wind = w)
}

#' Fixed thresholds from the measured-first record, same definitions as `compute_extreme_thresholds()`:
#' wet-day (>= 1 mm) rain p95/p99 (or `rain_thresholds`, e.g. the whole Whakarewarewa record), hourly wind p95/p99, daily-max temperature p95, over `ref_years`.
compute_extreme_thresholds_obs <- function(obs, ref_years, rain_thresholds = NULL) {
  d <- obs$daily; d <- d[as.integer(format(d$date, "%Y")) %in% ref_years, ]
  w <- obs$wind;  w <- w[as.integer(format(w$Date, "%Y")) %in% ref_years, ]
  if (is.null(rain_thresholds)) {
    wet <- d$rain_mm[!is.na(d$rain_mm) & d$rain_mm >= 1]
    rain_thresholds <- list(rain_p95 = stats::quantile(wet, 0.95, names = FALSE), rain_p99 = stats::quantile(wet, 0.99, names = FALSE))
  }
  list(rain_p95 = rain_thresholds$rain_p95, rain_p99 = rain_thresholds$rain_p99,
       wind_p95 = stats::quantile(w$wind, 0.95, na.rm = TRUE, names = FALSE), wind_p99 = stats::quantile(w$wind, 0.99, na.rm = TRUE, names = FALSE),
       tmax_p95 = stats::quantile(d$tmax, 0.95, na.rm = TRUE, names = FALSE))
}

#' Annual extreme indices (same definitions as `compute_annual_extreme_indices()`) from the measured-first record.
#' Needs R/summarise_climate_extremes.R sourced (for .days_in_spells and .count_runs).
compute_annual_extreme_indices_obs <- function(obs, thresholds, years) {
  d <- obs$daily; d$year <- as.integer(format(d$date, "%Y")); w <- obs$wind; w$year <- as.integer(format(w$Date, "%Y"))
  lapply(years, function(y) {
    dy <- d[d$year == y, ]; wy <- w[w$year == y, ]
    data.frame(year = y, rx1day = max(dy$rain_mm, na.rm = TRUE), heavy_rain_days = sum(dy$rain_mm > thresholds$rain_p95, na.rm = TRUE),
               extreme_rain_days = sum(dy$rain_mm > thresholds$rain_p99, na.rm = TRUE), hot_days = sum(dy$tmax > thresholds$tmax_p95, na.rm = TRUE),
               wsdi = .days_in_spells(dy$tmax > thresholds$tmax_p95, min_len = 3), max_wind = max(wy$wind, na.rm = TRUE),
               storm_wind_hours = sum(wy$wind > thresholds$wind_p95, na.rm = TRUE), severe_wind_hours = sum(wy$wind > thresholds$wind_p99, na.rm = TRUE),
               storm_events = .count_runs(wy$wind > thresholds$wind_p95), n_days = nrow(dy), n_hours = nrow(wy))
  }) |> do.call(what = rbind)
}
