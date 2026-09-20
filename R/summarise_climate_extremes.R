# Extreme/storm-event indices from hourly AEME MET_* data.
# Applied to `era5_corrected_hourly` (baseline) and each branch of
# `scenario_hourly_met` (GCM x scenario x window), using fixed thresholds
# derived once from the historical baseline so that a "heavy rain day" or
# "storm hour" means the same absolute thing in every period -- frequency
# changes are then directly interpretable as intensification, not a
# shifting yardstick.

#' Fixed extreme-event thresholds from the bias-corrected hourly baseline
#'
#' @param hourly bias-corrected hourly met (`era5_corrected_hourly`), with
#'   `Date`, `MET_pprain`, `MET_wndspd`, `MET_tmpair`
#' @param ref_years integer vector, the historical reference window
#' @return named list: `rain_p95`/`rain_p99` (mm/day, wet days only),
#'   `wind_p95`/`wind_p99` (m/s, hourly), `tmax_p95` (degC, daily max)
compute_extreme_thresholds <- function(hourly, ref_years) {
  day <- as.Date(hourly$Date)
  yr  <- as.integer(format(day, "%Y"))
  ref <- hourly[yr %in% ref_years, ]
  ref_day <- as.Date(ref$Date)

  daily_rain <- tapply(ref$MET_pprain, ref_day, sum, na.rm = TRUE)
  daily_tmax <- tapply(ref$MET_tmpair, ref_day, max, na.rm = TRUE)
  wet <- daily_rain[daily_rain >= 1]

  list(
    rain_p95 = stats::quantile(wet, 0.95, na.rm = TRUE, names = FALSE),
    rain_p99 = stats::quantile(wet, 0.99, na.rm = TRUE, names = FALSE),
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
                      rain = hourly$MET_pprain, tmax = hourly$MET_tmpair) |>
    dplyr::group_by(year, day) |>
    dplyr::summarise(rain = sum(rain, na.rm = TRUE),
                     tmax = max(tmax, na.rm = TRUE), .groups = "drop")

  hourly_df <- data.frame(year = yr, wind = hourly$MET_wndspd)

  daily |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      rx1day          = max(rain, na.rm = TRUE),
      heavy_rain_days = sum(rain > thresholds$rain_p95, na.rm = TRUE),
      extreme_rain_days = sum(rain > thresholds$rain_p99, na.rm = TRUE),
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
    dplyr::summarise(dplyr::across(-year, mean, na.rm = TRUE)) |>
    dplyr::mutate(gcm = gcm, scenario = scenario, window = window, .before = 1)
}
