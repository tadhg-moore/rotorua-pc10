#' Extend the bias-corrected daily baseline back in time using the raw daily ERA5
#'
#' Raw daily ERA5 (`era5_daily`, 1980-2023, mm/day rain, wind as u/v) is mapped onto
#' the buoy-corrected daily series (`era5_corrected_daily_baseline`, rain in m/day) with a
#' per-calendar-month correction fitted on the overlap: additive for temperature,
#' multiplicative for rain, wind speed, shortwave and pressure. The correction assumes the
#' ERA5 bias is stationary. The corrected series is used as is where it exists.
#' @returns data frame in m/day rain with the AEME MET_* columns, for `from` to `to`.
make_daily_long_met <- function(era5_daily, corrected_daily, from = "1990-01-01",
                                to = "2022-06-30") {
  raw <- era5_daily
  raw$Date <- as.Date(raw$Date)
  raw$MET_wndspd <- sqrt(raw$MET_wnduvu^2 + raw$MET_wnduvv^2)
  cor <- corrected_daily
  cor$Date <- as.Date(cor$Date)
  ov <- merge(raw, cor, by = "Date", suffixes = c(".raw", ".cor"))
  ov$month <- as.integer(format(ov$Date, "%m"))
  raw$month <- as.integer(format(raw$Date, "%m"))
  mon <- function(v, fun) {
    x <- ov[[paste0(v, ".raw")]]; y <- ov[[paste0(v, ".cor")]]
    vapply(1:12, function(m) {
      i <- ov$month == m & is.finite(x) & is.finite(y)
      fun(x[i], y[i])
    }, numeric(1))
  }
  add <- function(x, y) mean(y) - mean(x)
  mult <- function(x, y) if (mean(x) > 0) mean(y) / mean(x) else 1
  raw$MET_pprain <- raw$MET_pprain / 1000   # mm/day -> m/day, same unit as the corrected series
  ov$MET_pprain.raw <- ov$MET_pprain.raw / 1000
  fac <- list(
    MET_tmpair = mon("MET_tmpair", add), MET_tmpdew = mon("MET_tmpdew", add),
    MET_wndspd = mon("MET_wndspd", mult), MET_pprain = mon("MET_pprain", mult),
    MET_radswd = mon("MET_radswd", mult), MET_prsttn = mon("MET_prsttn", mult))
  out <- raw[raw$Date >= as.Date(from) & raw$Date <= as.Date(to), ]
  m <- out$month
  out$MET_tmpair <- out$MET_tmpair + fac$MET_tmpair[m]
  out$MET_tmpdew <- out$MET_tmpdew + fac$MET_tmpdew[m]
  sp_old <- out$MET_wndspd
  out$MET_wndspd <- sp_old * fac$MET_wndspd[m]
  k <- ifelse(sp_old > 0, out$MET_wndspd / sp_old, 1)
  out$MET_wnduvu <- out$MET_wnduvu * k; out$MET_wnduvv <- out$MET_wnduvv * k
  out$MET_pprain <- out$MET_pprain * fac$MET_pprain[m]
  out$MET_radswd <- out$MET_radswd * fac$MET_radswd[m]
  out$MET_prsttn <- out$MET_prsttn * fac$MET_prsttn[m]
  # relative humidity from the corrected air and dew point (Magnus)
  es <- function(t) 6.112 * exp(17.62 * t / (243.12 + t))
  out$MET_humrel <- pmin(100, 100 * es(out$MET_tmpdew) / es(out$MET_tmpair))
  out$MET_prvapr <- es(out$MET_tmpdew)
  # use the buoy-corrected series itself wherever it exists
  keep <- intersect(names(out), names(cor))
  hit <- out$Date %in% cor$Date
  out[hit, keep] <- cor[match(out$Date[hit], cor$Date), keep]
  attr(out, "monthly_factors") <- fac
  out[, setdiff(names(out), "month")]
}
