# Visualise the year-by-year delta-change trajectories behind the transient
# scenarios (compute_gcm_transient_deltas()): how each GCM's annual-mean
# change evolves from the historical reference to 2100, per SSP.

#' Annual-mean delta trajectories, one panel per variable
#'
#' @param deltas_df output of [compute_gcm_transient_deltas()], row-bound
#'   across every GCM
#' @param variables `MET_*` names to show (labels from
#'   `.gcm_delta_var_labels`, defined in plot_gcm_delta_variability.R)
#' @return a ggplot object: year on x, annual-mean delta (mean of the 12
#'   monthly factors) on y; thin lines = individual GCMs, thick line =
#'   GCM mean, colour = SSP
plot_gcm_transient_deltas <- function(deltas_df,
                                      variables = c("MET_tmpair", "MET_pprain",
                                                    "MET_wndspd", "MET_radswd")) {
  ann <- deltas_df |>
    dplyr::filter(variable %in% variables) |>
    dplyr::group_by(gcm, scenario, variable, year) |>
    dplyr::summarise(delta = mean(delta, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(
      var_label = factor(.gcm_delta_var_labels[variable], unname(.gcm_delta_var_labels[variables])),
      scenario  = factor(scenario, sort(unique(scenario)))
    )

  ens <- ann |>
    dplyr::group_by(var_label, scenario, year) |>
    dplyr::summarise(delta = mean(delta, na.rm = TRUE), .groups = "drop")

  pal <- c(ssp126 = "#4C72B0", ssp245 = "#55A868",
          ssp370 = "#DD8452", ssp585 = "#C44E52")

  ref <- data.frame(
    var_label = factor(.gcm_delta_var_labels[variables], unname(.gcm_delta_var_labels[variables])),
    ref = ifelse(unname(GCM_DELTA_KIND[variables]) == "ratio", 1, 0)
  )

  ggplot2::ggplot() +
    ggplot2::geom_hline(data = ref, ggplot2::aes(yintercept = ref),
                        linetype = "dotted", colour = "grey50") +
    ggplot2::geom_line(
      data = ann, ggplot2::aes(year, delta, group = interaction(gcm, scenario),
                               colour = scenario),
      linewidth = 0.3, alpha = 0.45) +
    ggplot2::geom_line(data = ens, ggplot2::aes(year, delta, colour = scenario),
                       linewidth = 1.1) +
    ggplot2::facet_wrap(~ var_label, scales = "free_y") +
    ggplot2::scale_colour_manual(values = pal) +
    ggplot2::labs(
      x = NULL, y = "annual-mean delta-change factor", colour = "scenario",
      title = "Transient delta-change trajectories by GCM and SSP",
      subtitle = paste0("30-year sliding window along each GCM's historical → SSP run, ",
                        "relative to the 1995–2014 reference; thin = individual GCMs, ",
                        "thick = GCM mean")
    ) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(legend.position = "top", strip.background = ggplot2::element_blank())
}
