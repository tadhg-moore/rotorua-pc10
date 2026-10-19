#' Daily met series computed from the corrected hourly series
#'
#' Aggregates `era5_corrected_hourly` (hourly, local-time zone `tz`) to local calendar days, so
#' the daily forcing differs from the hourly forcing only by resolution (same source, same bias
#' correction). Rain is summed (m/day, same unit as the hourly series), snow summed, wind speed is the
#' mean of hourly speeds (the mean wind speed that drives mixing), u/v are mean components, the
#' direction comes from the mean vector, everything else is the daily mean. Days with fewer than
#' `min_hours` hours (the partial first and last day) are dropped.
make_daily_from_hourly <- function(hourly, tz = attr(hourly, "tz"), min_hours = 22) {
  if (is.null(tz)) tz <- "Etc/GMT-12"
  d <- as.Date(format(hourly$Date, tz = tz, "%Y-%m-%d"))
  sumv <- c("MET_pprain", "MET_ppsnow")
  meanv <- setdiff(names(hourly), c("Date", sumv, "MET_wnddir"))
  num <- hourly[, c(sumv, meanv)]
  agg_mean <- rowsum(as.matrix(hourly[, meanv]), d, na.rm = TRUE) / as.vector(table(d))
  agg_sum  <- rowsum(as.matrix(hourly[, sumv]), d, na.rm = TRUE)
  n <- as.vector(table(d))
  out <- data.frame(Date = as.Date(rownames(agg_mean)), agg_mean, agg_sum, check.names = FALSE)
  out$MET_wnddir <- (atan2(out$MET_wnduvu, out$MET_wnduvv) * 180 / pi) %% 360
  out <- out[n >= min_hours, ]
  rownames(out) <- NULL
  out[, c("Date", setdiff(names(hourly), "Date"))]
}
