# Visualise inter-GCM spread in monthly climate-scenario delta-change
# factors -- the CMIP6-model uncertainty that is the dominant source of
# spread once a single downscaling chain and delta-change method are fixed.

.gcm_delta_var_labels <- c(
  MET_tmpair = "Air temperature (°C)",
  MET_radswd = "Shortwave radiation (W/m²)",
  MET_pprain = "Rainfall (ratio, futr / hist)",
  MET_wndspd = "Wind speed (ratio, futr / hist)",
  MET_humrel = "Relative humidity (ratio, futr / hist)"
)

#' Plot the across-GCM range of monthly delta-change factors, one panel per
#' variable, for a single scenario/window (or faceted by scenario)
#'
#' @param deltas_df output of [compute_gcm_monthly_deltas()], row-bound
#'   across every GCM (and, optionally, every scenario/window)
#' @param window which future window (`deltas_df$window`) to plot
#' @param scenarios which scenarios to show as separate coloured
#'   ribbons/lines; `NULL` (default) uses every scenario present
#' @return a ggplot object: month on x, delta on y, one facet per variable,
#'   coloured ribbons spanning the GCM min-max range per scenario, with
#'   individual GCMs shown as thin lines so outlier models are visible
plot_gcm_delta_variability <- function(deltas_df, window, scenarios = NULL) {
  d <- deltas_df |>
    dplyr::filter(window == !!window)
  if (!is.null(scenarios)) d <- d |> dplyr::filter(scenario %in% scenarios)

  d <- d |>
    dplyr::mutate(
      var_label = factor(.gcm_delta_var_labels[variable],
                         unname(.gcm_delta_var_labels)),
      scenario  = factor(scenario, sort(unique(scenario)))
    )

  rng <- d |>
    dplyr::group_by(var_label, scenario, month) |>
    dplyr::summarise(ymin = min(delta, na.rm = TRUE),
                     ymax = max(delta, na.rm = TRUE),
                     ymean = mean(delta, na.rm = TRUE),
                     .groups = "drop")

  pal <- c(ssp126 = "#4C72B0", ssp245 = "#55A868",
          ssp370 = "#DD8452", ssp585 = "#C44E52")

  ggplot2::ggplot() +
    (if (any(grepl("ratio", unique(d$var_label)))) {
      ggplot2::geom_hline(
        data = data.frame(var_label = unique(d$var_label[grepl("ratio", d$var_label)])),
        ggplot2::aes(yintercept = 1), linetype = "dotted", colour = "grey50")
    }) +
    ggplot2::geom_hline(
      data = data.frame(var_label = unique(d$var_label[!grepl("ratio", d$var_label)])),
      ggplot2::aes(yintercept = 0), linetype = "dotted", colour = "grey50") +
    ggplot2::geom_ribbon(
      data = rng,
      ggplot2::aes(month, ymin = ymin, ymax = ymax, fill = scenario),
      alpha = 0.18) +
    ggplot2::geom_line(
      data = d, ggplot2::aes(month, delta, group = interaction(gcm, scenario),
                             colour = scenario),
      linewidth = 0.3, alpha = 0.55) +
    ggplot2::geom_line(
      data = rng, ggplot2::aes(month, ymean, colour = scenario),
      linewidth = 1.1) +
    ggplot2::facet_wrap(~ var_label, scales = "free_y") +
    ggplot2::scale_x_continuous(breaks = 1:12, labels = month.abb) +
    ggplot2::scale_colour_manual(values = pal) +
    ggplot2::scale_fill_manual(values = pal) +
    ggplot2::labs(
      x = NULL, y = "monthly delta-change factor",
      colour = "scenario", fill = "scenario",
      title = paste0("Inter-GCM spread in monthly delta-change factors, ", window),
      subtitle = paste0(dplyr::n_distinct(d$gcm), " GCMs × ",
                        dplyr::n_distinct(d$scenario), " SSPs; ",
                        "thin lines = individual GCMs, band = GCM min–max, ",
                        "thick line = GCM mean")
    ) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(legend.position = "top", strip.background = ggplot2::element_blank())
}
