# Visualise how extreme/storm-event indices (from
# `summarise_climate_extremes.R`) change across GCMs, scenarios and windows,
# relative to the bias-corrected historical baseline.

.extreme_index_labels <- c(
  rx1day             = "Max daily rainfall (mm)",
  heavy_rain_days    = "Heavy rain days (>p95, days/yr)",
  extreme_rain_days  = "Extreme rain days (>p99, days/yr)",
  max_wind           = "Max hourly wind speed (m/s)",
  storm_wind_hours   = "Storm wind hours (>p95, hrs/yr)",
  severe_wind_hours  = "Severe wind hours (>p99, hrs/yr)",
  storm_events       = "Wind storm events (spells/yr)",
  hot_days           = "Hot days (>p95 Tmax, days/yr)",
  wsdi               = "Warm-spell days (WSDI, days/yr)"
)

#' Faceted summary of extreme-index change by scenario/window, across GCMs
#'
#' @param extremes_df output of `summarise_branch_extremes()`, row-bound
#'   across every gcm/scenario/window branch (columns: gcm, scenario,
#'   window, then one column per index)
#' @param baseline_indices named numeric vector/list, mean-annual index
#'   values for the historical reference period (same names as the index
#'   columns of `extremes_df`), shown as a dashed reference line
#' @param indices character vector of index column names to plot (must be
#'   named in [.extreme_index_labels])
#' @return a ggplot object: one facet per index, window on x, GCM-ensemble
#'   mean per scenario as a point, GCM min-max range as an error bar,
#'   dashed line = historical baseline
plot_extreme_index_summary <- function(extremes_df, baseline_indices, indices) {
  long <- extremes_df |>
    tidyr::pivot_longer(dplyr::all_of(indices), names_to = "index", values_to = "value") |>
    dplyr::mutate(
      index    = factor(.extreme_index_labels[index], unname(.extreme_index_labels[indices])),
      window   = factor(window, sort(unique(window))),
      scenario = factor(scenario, sort(unique(scenario)))
    )

  rng <- long |>
    dplyr::group_by(index, scenario, window) |>
    dplyr::summarise(mean = mean(value, na.rm = TRUE),
                     ymin = min(value, na.rm = TRUE),
                     ymax = max(value, na.rm = TRUE), .groups = "drop")

  base_long <- data.frame(index = names(baseline_indices),
                          value = as.numeric(baseline_indices)) |>
    dplyr::filter(index %in% indices) |>
    dplyr::mutate(index = factor(.extreme_index_labels[index], unname(.extreme_index_labels[indices])))

  pal <- c(ssp126 = "#4C72B0", ssp245 = "#55A868",
          ssp370 = "#DD8452", ssp585 = "#C44E52")

  ggplot2::ggplot(rng, ggplot2::aes(window, mean, colour = scenario)) +
    ggplot2::geom_hline(data = base_long, ggplot2::aes(yintercept = value),
                        linetype = "dashed", colour = "grey40") +
    ggplot2::geom_pointrange(
      ggplot2::aes(ymin = ymin, ymax = ymax),
      position = ggplot2::position_dodge(0.5), linewidth = 0.5, size = 0.35) +
    ggplot2::facet_wrap(~ index, scales = "free_y") +
    ggplot2::scale_colour_manual(values = pal) +
    ggplot2::labs(
      x = NULL, y = "mean annual value (GCM range shown)", colour = "scenario",
      title = "Change in extreme/storm-event frequency and intensity",
      subtitle = paste0("Points = GCM-ensemble mean per scenario/window; bars = GCM min-max range; ",
                        "dashed line = historical baseline (bias-corrected ERA5, 1995-2014)")
    ) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(legend.position = "top", strip.background = ggplot2::element_blank())
}
