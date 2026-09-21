# Time series and distribution diagnostics for one bias-corrected variable,
# comparing raw ERA5, bias-corrected ERA5 and the buoy observation the
# correction was fit against -- a visual complement to
# `era5_bias_correction$skill`'s numeric cross-validated scores. Reuses
# `met_daily_long()` from compare_met_sources.R (already sourced) so this
# stays consistent with met-source-comparison.qmd's daily-aggregation
# convention (rain summed, everything else averaged).

.bias_var_labels <- c(
  MET_wndspd = "Wind speed (m/s)",
  MET_pprain = "Rainfall (mm/day)",
  MET_tmpair = "Air temperature (°C)",
  MET_radswd = "Shortwave radiation (W/m²)",
  MET_humrel = "Relative humidity (%)",
  MET_prsttn = "Station pressure (Pa)"
)

#' Long daily data frame of raw/bias-corrected/observed for one variable,
#' restricted to a window (default: the buoy's own record span, since that's
#' the only period all three sources can be compared over)
bias_correction_daily_long <- function(raw, corrected, obs, variable, window = NULL) {
  long <- met_daily_long(list(raw = raw, corrected = corrected, buoy = obs)) |>
    dplyr::filter(variable == !!variable)
  ## MET_pprain is metres/day (AEME convention, see met-source-comparison.qmd);
  ## convert to mm/day here so these plots are human-readable.
  if (identical(variable, "MET_pprain")) long$value <- long$value * 1000
  if (is.null(window)) window <- range(obs$Date, na.rm = TRUE)
  long |> dplyr::filter(Date >= window[1], Date <= window[2])
}

.bias_source_labels <- c(buoy = "Buoy (observed)", raw = "Raw ERA5",
                         corrected = "Bias-corrected ERA5")
.bias_source_pal <- c("Buoy (observed)" = "black", "Raw ERA5" = "#B0B0B0",
                      "Bias-corrected ERA5" = "#4C72B0")

#' Daily time series: raw vs. bias-corrected ERA5 vs. buoy, one variable
#'
#' @param raw,corrected,obs wide `Date` + `MET_*` data frames, e.g.
#'   `era5_hourly_met`, `era5_corrected_hourly`, `rotorua_buoy_met_aeme_hr`
#' @param variable a name in [.bias_var_labels]
#' @param window optional `c(min, max)` Date range; defaults to the full
#'   span of `obs`
plot_bias_correction_timeseries <- function(raw, corrected, obs, variable, window = NULL) {
  d <- bias_correction_daily_long(raw, corrected, obs, variable, window) |>
    dplyr::mutate(source = factor(.bias_source_labels[source], unname(.bias_source_labels)))

  ggplot2::ggplot(d, ggplot2::aes(Date, value, colour = source)) +
    ggplot2::geom_line(na.rm = TRUE, alpha = 0.85) +
    ggplot2::scale_colour_manual(values = .bias_source_pal) +
    ggplot2::labs(x = NULL, y = .bias_var_labels[[variable]], colour = NULL,
                 title = paste0("Daily ", tolower(.bias_var_labels[[variable]]),
                               ": raw vs. bias-corrected ERA5 vs. buoy")) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(legend.position = "top")
}

#' Distribution comparison: raw vs. bias-corrected ERA5 vs. buoy, one
#' variable, over the same window
#'
#' @inheritParams plot_bias_correction_timeseries
#' @param wet_only for rainfall: drop near-zero days (< 1 mm) before
#'   plotting, so the (usually large) dry-day spike doesn't dominate
#' @param log1p_transform for skewed variables like rainfall: plot
#'   `log(1 + value)` instead of the raw value
plot_bias_correction_distribution <- function(raw, corrected, obs, variable, window = NULL,
                                              wet_only = FALSE, log1p_transform = FALSE) {
  d <- bias_correction_daily_long(raw, corrected, obs, variable, window)
  if (wet_only) d <- d |> dplyr::filter(value >= 1)
  if (log1p_transform) d <- d |> dplyr::mutate(value = log1p(value))
  d <- d |> dplyr::mutate(source = factor(.bias_source_labels[source], unname(.bias_source_labels)))

  xlab <- if (log1p_transform) paste0("log(1 + ", .bias_var_labels[[variable]], ")") else .bias_var_labels[[variable]]
  subtitle <- if (wet_only) "Wet days only (>=1 mm)" else NULL

  ggplot2::ggplot(d, ggplot2::aes(value, colour = source, fill = source)) +
    ggplot2::geom_density(alpha = 0.15, linewidth = 0.8, na.rm = TRUE) +
    ggplot2::scale_colour_manual(values = .bias_source_pal) +
    ggplot2::scale_fill_manual(values = .bias_source_pal) +
    ggplot2::labs(x = xlab, y = "density", colour = NULL, fill = NULL, subtitle = subtitle,
                 title = paste0("Distribution of daily ", tolower(.bias_var_labels[[variable]]))) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(legend.position = "top")
}
