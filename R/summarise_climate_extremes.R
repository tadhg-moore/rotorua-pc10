# Extreme/storm-event indices from hourly AEME MET_* data.
# Applied to `era5_corrected_hourly` (baseline) and each branch of
# `scenario_hourly_met` (GCM x scenario x window), using fixed thresholds
# derived once from a historical reference so that a "heavy rain day" or
# "storm hour" means the same absolute thing in every period -- frequency
# changes are then directly interpretable as intensification, not a
# shifting yardstick.
#
# Note: `MET_pprain` is metres/timestep (AEME convention, see
# met-source-comparison.qmd for the ERA5 unit bug this caught) -- every
# function here converts to mm internally so thresholds/indices are
# human-readable.

#' Extract the long daily rainfall record for threshold estimation
#' (Whakarewarewa, via PC10/BOPRC -- 1901-present, the longest rainfall
#' record available for this site by ~40 years over the next-best source)
#'
#' @param pc10_met `pc10_climate_met` (Date + MET_pprain in metres/day)
#' @return data frame `date`, `rain_mm`, non-NA rows only
extract_long_rain_record <- function(pc10_met) {
  d <- data.frame(date = as.Date(pc10_met$Date), rain_mm = pc10_met$MET_pprain * 1000)
  d[!is.na(d$rain_mm), ]
}

#' Wet-day rainfall percentile thresholds from a long daily record
#'
#' @param long_rain output of [extract_long_rain_record()]
#' @return named list: `rain_p95`/`rain_p99` (mm/day), `n_years`,
#'   `start`/`end` (record span)
compute_long_rain_thresholds <- function(long_rain) {
  wet <- long_rain$rain_mm[long_rain$rain_mm >= 1]
  list(
    rain_p95 = stats::quantile(wet, 0.95, na.rm = TRUE, names = FALSE),
    rain_p99 = stats::quantile(wet, 0.99, na.rm = TRUE, names = FALSE),
    n_years  = length(unique(format(long_rain$date, "%Y"))),
    start    = min(long_rain$date),
    end      = max(long_rain$date)
  )
}

#' Gumbel (extreme-value type I) fit by the method of moments -- a simple,
#' dependency-free way to turn a >100-year annual-maximum series into
#' return-level estimates well beyond the length of any GCM/reanalysis
#' window used elsewhere in this pipeline (Euler-Mascheroni constant used
#' below, 0.5772156649, is the standard closed-form method-of-moments
#' calibration for this distribution)
gumbel_fit <- function(annual_max) {
  s <- stats::sd(annual_max, na.rm = TRUE)
  m <- mean(annual_max, na.rm = TRUE)
  sigma <- sqrt(6) * s / pi
  mu <- m - 0.5772156649 * sigma
  list(mu = mu, sigma = sigma, n = sum(!is.na(annual_max)))
}

#' Gumbel return level for given return period(s), years
gumbel_return_level <- function(fit, return_period) {
  fit$mu - fit$sigma * log(-log(1 - 1 / return_period))
}

#' Annual-maximum daily rainfall return levels from the long record
#'
#' @param long_rain output of [extract_long_rain_record()]
#' @param return_periods years, e.g. `c(2, 5, 10, 20, 50, 100)`
#' @return data frame `return_period_years`, `rx1day_mm`
compute_rain_return_levels <- function(long_rain, return_periods = c(2, 5, 10, 20, 50, 100)) {
  annual_max <- long_rain |>
    dplyr::mutate(year = as.integer(format(date, "%Y"))) |>
    dplyr::group_by(year) |>
    dplyr::summarise(rx1day = max(rain_mm, na.rm = TRUE), .groups = "drop")

  fit <- gumbel_fit(annual_max$rx1day)
  data.frame(
    return_period_years = return_periods,
    rx1day_mm = gumbel_return_level(fit, return_periods)
  )
}

#' Fixed extreme-event thresholds from the bias-corrected hourly baseline
#'
#' @param hourly bias-corrected hourly met (`era5_corrected_hourly`), with
#'   `Date`, `MET_pprain` (m/timestep), `MET_wndspd`, `MET_tmpair`
#' @param ref_years integer vector, the historical reference window
#' @param rain_thresholds optional override for `rain_p95`/`rain_p99` (e.g.
#'   from [compute_long_rain_thresholds()] on the 125-year Whakarewarewa
#'   record) -- 20 years of ERA5 is thin for estimating a p95/p99 rainfall
#'   threshold; the long, independent land-station record gives a far more
#'   robust definition of "heavy"/"extreme" rain. When `NULL`, falls back
#'   to computing rain thresholds from `hourly` itself, as before.
#' @return named list: `rain_p95`/`rain_p99` (mm/day, wet days only),
#'   `wind_p95`/`wind_p99` (m/s, hourly), `tmax_p95` (degC, daily max)
compute_extreme_thresholds <- function(hourly, ref_years, rain_thresholds = NULL) {
  day <- as.Date(hourly$Date)
  yr  <- as.integer(format(day, "%Y"))
  ref <- hourly[yr %in% ref_years, ]
  ref_day <- as.Date(ref$Date)

  daily_tmax <- tapply(ref$MET_tmpair, ref_day, max, na.rm = TRUE)

  if (is.null(rain_thresholds)) {
    daily_rain_mm <- tapply(ref$MET_pprain * 1000, ref_day, sum, na.rm = TRUE)
    wet <- daily_rain_mm[daily_rain_mm >= 1]
    rain_thresholds <- list(
      rain_p95 = stats::quantile(wet, 0.95, na.rm = TRUE, names = FALSE),
      rain_p99 = stats::quantile(wet, 0.99, na.rm = TRUE, names = FALSE)
    )
  }

  list(
    rain_p95 = rain_thresholds$rain_p95,
    rain_p99 = rain_thresholds$rain_p99,
    wind_p95 = stats::quantile(ref$MET_wndspd, 0.95, na.rm = TRUE, names = FALSE),
    wind_p99 = stats::quantile(ref$MET_wndspd, 0.99, na.rm = TRUE, names = FALSE),
    tmax_p95 = stats::quantile(daily_tmax, 0.95, na.rm = TRUE, names = FALSE)
  )
}

#' Count contiguous runs of TRUE in a logical vector (storm "events")
.count_runs <- function(x) {
  r <- rle(x)
  sum(r$values, na.rm = TRUE)
}

#' Longest-run-aware count of days in spells >= `min_len` (e.g. warm-spell
#' duration index)
.days_in_spells <- function(x, min_len = 3) {
  r <- rle(x)
  keep <- r$values & r$lengths >= min_len
  sum(r$lengths[keep])
}

#' Annual extreme/storm indices for one hourly met series
#'
#' @param hourly hourly met data frame (`era5_corrected_hourly` or one
#'   branch of `scenario_hourly_met`)
#' @param thresholds output of [compute_extreme_thresholds()]
#' @return one row per calendar year: `year`, `rx1day` (mm, annual max
#'   daily rain), `heavy_rain_days`/`extreme_rain_days` (days > p95/p99),
#'   `max_wind` (m/s, annual max hourly), `storm_wind_hours`/
#'   `severe_wind_hours` (hours > p95/p99), `storm_events` (contiguous
#'   spells above p95), `hot_days` (days > tmax p95), `wsdi` (days in
#'   hot spells >= 3 days)
compute_annual_extreme_indices <- function(hourly, thresholds) {
  day <- as.Date(hourly$Date)
  yr  <- as.integer(format(day, "%Y"))

  daily <- data.frame(day = day, year = yr,
                      rain_mm = hourly$MET_pprain * 1000, tmax = hourly$MET_tmpair) |>
    dplyr::group_by(year, day) |>
    dplyr::summarise(rain_mm = sum(rain_mm, na.rm = TRUE),
                     tmax = max(tmax, na.rm = TRUE), .groups = "drop")

  hourly_df <- data.frame(year = yr, wind = hourly$MET_wndspd)

  daily |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      rx1day          = max(rain_mm, na.rm = TRUE),
      heavy_rain_days = sum(rain_mm > thresholds$rain_p95, na.rm = TRUE),
      extreme_rain_days = sum(rain_mm > thresholds$rain_p99, na.rm = TRUE),
      hot_days        = sum(tmax > thresholds$tmax_p95, na.rm = TRUE),
      wsdi            = .days_in_spells(tmax > thresholds$tmax_p95, min_len = 3),
      .groups = "drop"
    ) |>
    dplyr::left_join(
      hourly_df |>
        dplyr::group_by(year) |>
        dplyr::summarise(
          max_wind          = max(wind, na.rm = TRUE),
          storm_wind_hours  = sum(wind > thresholds$wind_p95, na.rm = TRUE),
          severe_wind_hours = sum(wind > thresholds$wind_p99, na.rm = TRUE),
          storm_events      = .count_runs(wind > thresholds$wind_p95),
          .groups = "drop"
        ),
      by = "year"
    )
}

#' Mean-annual extreme-index summary for one gcm/scenario/window branch
#'
#' @param hourly one branch of `scenario_hourly_met`
#' @param thresholds output of [compute_extreme_thresholds()]
#' @param gcm,scenario,window labels to attach (from `scenario_delta_grid`)
#' @return one-row data frame: labels + mean-annual value of every index
#'   in [compute_annual_extreme_indices()]
summarise_branch_extremes <- function(hourly, thresholds, gcm, scenario, window) {
  ann <- compute_annual_extreme_indices(hourly, thresholds)
  ann |>
    dplyr::summarise(dplyr::across(-year, \(x) mean(x, na.rm = TRUE))) |>
    dplyr::mutate(gcm = gcm, scenario = scenario, window = window, .before = 1)
}
