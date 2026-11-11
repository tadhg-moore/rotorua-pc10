#' Stratification and anoxia day counts from a transient run's daily summary
#'
#' Definitions follow the existing calibration scripts:
#'  - stratified day: surface (0-2 m) minus bottom (>= 17 m) temperature >= `strat_dt`
#'    (calibration/scenarios30/analyse_scenarios30.R);
#'  - anoxic day: bottom (>= 17 m) mean oxygen < `anox_thr` mg/L
#'    (calibration/anoxia/01_bottom_anoxia.R).
#'
#' @param daily data frame with Date, temp_surf, temp_bot, oxy_bot
#' @param strat_dt temperature difference (degC) defining a stratified day
#' @param anox_thr oxygen threshold (mg/L) defining an anoxic day
#' @return daily with logical `strat` and `anoxic` columns and a water year `wy`
#'   (July-June, named by the year it ends in)
add_strat_anoxia <- function(daily, strat_dt = 1, anox_thr = 2) {
  daily$strat  <- (daily$temp_surf - daily$temp_bot) >= strat_dt
  daily$anoxic <- daily$oxy_bot < anox_thr
  yr <- as.integer(format(daily$Date, "%Y")); mo <- as.integer(format(daily$Date, "%m"))
  daily$wy <- ifelse(mo >= 7, yr + 1L, yr)
  daily
}

#' Per water year counts: days observed, stratified days, anoxic days, and the
#' longest unbroken stratified and anoxic spells. Years with fewer than
#' `min_days` days (partial first/last year) are dropped.
annual_strat_anoxia <- function(daily, min_days = 360, ...) {
  d <- add_strat_anoxia(daily, ...)
  longest <- function(x) { x <- x[!is.na(x)]; if (!any(x)) 0L else max(rle(x)$lengths[rle(x)$values]) }
  out <- do.call(rbind, lapply(split(d, d$wy), function(g) data.frame(
    wy = g$wy[1], n_days = nrow(g),
    strat_days = sum(g$strat, na.rm = TRUE), anoxic_days = sum(g$anoxic, na.rm = TRUE),
    longest_strat_spell = longest(g$strat), longest_anoxic_spell = longest(g$anoxic))))
  rownames(out) <- NULL
  out[out$n_days >= min_days, ]
}
