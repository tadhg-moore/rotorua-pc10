#' Daily mean discharge from a BOPRC Aquarius "points as recorded" bulk export.
#'
#' Downloads the instantaneous record, drops the export footer and computes
#' time-weighted daily means (NZST days): the record is linearly interpolated
#' to a 15-minute grid, but not across gaps longer than `max_gap_h` hours, so
#' irregularly sampled early records (1970s-80s) are not biased toward the times
#' that happen to have most points. Only the daily summary is returned so the
#' large instantaneous table is not stored by targets.
#'
#' @param url BOPRC BulkExport URL (see tar_target(*_flow_url) in _targets.R)
#' @param site_id,site_name labels attached to the result
#' @param max_gap_h longest gap (hours) that is interpolated across
#' @return tibble: site_id, site_name, date, q_mean (m3/s), q_max, n_obs
read_bop_daily_flow <- function(url, site_id, site_name, max_gap_h = 6) {
  dat <- read_bop_csv_url(url)
  names(dat)[1:2] <- c("time", "q")
  dat <- dat |>
    dplyr::filter(!is.na(.data$time), !is.na(suppressWarnings(as.numeric(.data$q)))) |>
    dplyr::mutate(q = as.numeric(.data$q)) |>
    dplyr::arrange(.data$time)
  # work in seconds on a UTC+12 clock so that as.Date() gives the NZST day
  t_num <- as.numeric(dat$time)
  step <- 900
  grid <- seq(ceiling(min(t_num) / step) * step, floor(max(t_num) / step) * step, by = step)
  q_grid <- stats::approx(t_num, dat$q, xout = grid, ties = mean)$y
  # blank out grid points in gaps longer than max_gap_h
  idx <- findInterval(grid, t_num)
  nxt <- pmin(idx + 1, length(t_num))
  gap <- t_num[nxt] - t_num[pmax(idx, 1)]
  q_grid[gap > max_gap_h * 3600] <- NA_real_
  day <- as.Date(as.POSIXct(grid, origin = "1970-01-01", tz = "Etc/GMT-12"))
  tibble::tibble(date = day, q = q_grid) |>
    dplyr::group_by(.data$date) |>
    dplyr::summarise(q_mean = mean(.data$q, na.rm = TRUE), q_max = suppressWarnings(max(.data$q, na.rm = TRUE)),
                     n_obs = sum(!is.na(.data$q)), .groups = "drop") |>
    dplyr::filter(is.finite(.data$q_mean), .data$n_obs >= 0.5 * 96) |>   # >= 12 h of data
    dplyr::mutate(site_id = site_id, site_name = site_name, .before = 1)
}
