plot_rotorua_weir_profile <- function(
    channel_width_m = 26,
    lower_width_m = 4.6,
    lower_crest_m = 278.166,
    upper_crest_m = 279.316,
    stoplog_height_m = 0.270,
    stoplog_counts = 0:3
) {
  if (
    !is.numeric(channel_width_m) ||
    length(channel_width_m) != 1L ||
    !is.finite(channel_width_m) ||
    channel_width_m <= 0 ||
    !is.numeric(lower_width_m) ||
    length(lower_width_m) != 1L ||
    !is.finite(lower_width_m) ||
    lower_width_m <= 0 ||
    lower_width_m > channel_width_m
  ) {
    stop(
      "`channel_width_m` and `lower_width_m` must be positive, finite widths, ",
      "with `lower_width_m` no greater than `channel_width_m`.",
      call. = FALSE
    )
  }
  if (
    !is.numeric(lower_crest_m) ||
    length(lower_crest_m) != 1L ||
    !is.finite(lower_crest_m) ||
    !is.numeric(upper_crest_m) ||
    length(upper_crest_m) != 1L ||
    !is.finite(upper_crest_m) ||
    !is.numeric(stoplog_height_m) ||
    length(stoplog_height_m) != 1L ||
    !is.finite(stoplog_height_m) ||
    upper_crest_m <= lower_crest_m ||
    stoplog_height_m <= 0
  ) {
    stop(
      "Crest elevations must be finite and ordered, and ",
      "`stoplog_height_m` must be positive.",
      call. = FALSE
    )
  }
  if (
    !is.numeric(stoplog_counts) ||
    length(stoplog_counts) == 0L ||
    anyNA(stoplog_counts) ||
    any(!is.finite(stoplog_counts)) ||
    any(stoplog_counts < 0 | stoplog_counts > 3 |
        stoplog_counts != floor(stoplog_counts))
  ) {
    stop(
      "`stoplog_counts` must contain whole numbers from 0 to 3.",
      call. = FALSE
    )
  }
  
  channel_center <- channel_width_m / 2
  lower_start <- channel_center - lower_width_m / 2
  lower_end <- channel_center + lower_width_m / 2
  base_m <- lower_crest_m - 0.15
  profile <- do.call(
    rbind,
    lapply(stoplog_counts, function(n) {
      panel <- paste(n, if (n == 1) "stoplog" else "stoplogs")
      sections <- data.frame(
        n_stoplogs = panel,
        x_min = c(0, lower_start, lower_end),
        x_max = c(lower_start, lower_end, channel_width_m),
        ymin = base_m,
        ymax = c(upper_crest_m, lower_crest_m, upper_crest_m),
        structure = c("Upper crest", "Lower crest", "Upper crest")
      )
      logs <- if (n > 0) {
        data.frame(
          n_stoplogs = panel,
          x_min = lower_start,
          x_max = lower_end,
          ymin = lower_crest_m + (seq_len(n) - 1) * stoplog_height_m,
          ymax = lower_crest_m + seq_len(n) * stoplog_height_m,
          structure = "Stoplog"
        )
      } else {
        NULL
      }
      rbind(sections, logs)
    })
  )
  profile$n_stoplogs <- factor(
    profile$n_stoplogs,
    levels = paste(
      stoplog_counts,
      ifelse(stoplog_counts == 1, "stoplog", "stoplogs")
    )
  )
  
  references <- data.frame(
    level_m = c(279.776, 279.966, 280.076),
    reference = factor(
      c("Install trigger", "Remove trigger", "Consented maximum"),
      levels = c("Install trigger", "Remove trigger", "Consented maximum")
    )
  )
  
  ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = profile,
      ggplot2::aes(
        xmin = x_min,
        xmax = x_max,
        ymin = ymin,
        ymax = ymax,
        fill = structure
      ),
      colour = "grey20",
      linewidth = 0.35
    ) +
    ggplot2::geom_hline(
      data = references,
      ggplot2::aes(
        yintercept = level_m,
        colour = reference,
        linetype = reference
      ),
      linewidth = 0.6
    ) +
    ggplot2::facet_wrap(~n_stoplogs, ncol = 2) +
    ggplot2::scale_fill_manual(
      values = c(
        "Upper crest" = "grey65",
        "Lower crest" = "#56B4E9",
        "Stoplog" = "#E69F00"
      ),
      name = "Weir structure"
    ) +
    ggplot2::scale_colour_manual(
      values = c(
        "Install trigger" = "#0072B2",
        "Remove trigger" = "#D55E00",
        "Consented maximum" = "#009E73"
      ),
      name = "Lake-level reference"
    ) +
    ggplot2::scale_linetype_manual(
      values = c(
        "Install trigger" = "dotted",
        "Remove trigger" = "dashed",
        "Consented maximum" = "solid"
      ),
      name = "Lake-level reference"
    ) +
    ggplot2::coord_cartesian(
      ylim = c(base_m, max(280.15, upper_crest_m + 3 * stoplog_height_m)),
      expand = FALSE
    ) +
    ggplot2::labs(
      title = "Ōhau Channel weir crest profile",
      subtitle = paste0(
        "Across-channel profile; lower crest is ", lower_width_m,
        " m wide within a ", channel_width_m, " m channel"
      ),
      x = "Distance across channel (m)",
      y = "Elevation (m)",
      caption = paste(
        "Stoplogs are shown stacked on the lower crest.",
        "Lake-level references are horizontal; profile is schematic."
      )
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
}

# Examples:
# flow_curves <- generate_rotorua_weir_curves()
# plot_rotorua_weir_curves(flow_curves)
plot_rotorua_weir_profile()

plot_rotorua_glm_weir_comparison <- function(
    crest_height_m = 279.1469,
    crest_factor = 0.3622977,
    n_stoplogs = 0L,
    channel_width_m = 26,
    lower_width_m = 4.6,
    lower_crest_m = 278.166,
    upper_crest_m = 279.316,
    stoplog_height_m = 0.270
) {
  if (
    !is.numeric(crest_height_m) ||
    length(crest_height_m) != 1L ||
    !is.finite(crest_height_m) ||
    !is.numeric(crest_factor) ||
    length(crest_factor) != 1L ||
    !is.finite(crest_factor) ||
    crest_factor <= 0
  ) {
    stop(
      "`crest_height_m` must be finite and `crest_factor` must be positive.",
      call. = FALSE
    )
  }
  if (
    !is.numeric(n_stoplogs) ||
    length(n_stoplogs) != 1L ||
    !is.finite(n_stoplogs) ||
    n_stoplogs < 0 ||
    n_stoplogs > 3 ||
    n_stoplogs != floor(n_stoplogs)
  ) {
    stop("`n_stoplogs` must be a whole number from 0 to 3.", call. = FALSE)
  }
  if (
    !is.numeric(channel_width_m) ||
    length(channel_width_m) != 1L ||
    !is.finite(channel_width_m) ||
    channel_width_m <= 0 ||
    !is.numeric(lower_width_m) ||
    length(lower_width_m) != 1L ||
    !is.finite(lower_width_m) ||
    lower_width_m <= 0 ||
    lower_width_m > channel_width_m ||
    !is.numeric(lower_crest_m) ||
    length(lower_crest_m) != 1L ||
    !is.finite(lower_crest_m) ||
    !is.numeric(upper_crest_m) ||
    length(upper_crest_m) != 1L ||
    !is.finite(upper_crest_m) ||
    upper_crest_m <= lower_crest_m ||
    !is.numeric(stoplog_height_m) ||
    length(stoplog_height_m) != 1L ||
    !is.finite(stoplog_height_m) ||
    stoplog_height_m <= 0
  ) {
    stop("Weir dimensions and elevations must be finite and physically valid.",
         call. = FALSE)
  }
  
  lower_crest_with_logs_m <- lower_crest_m + n_stoplogs * stoplog_height_m
  channel_center <- channel_width_m / 2
  lower_start <- channel_center - lower_width_m / 2
  lower_end <- channel_center + lower_width_m / 2
  base_m <- min(lower_crest_m, crest_height_m) - 0.15
  actual <- data.frame(
    x_min = c(0, lower_start, lower_end),
    x_max = c(lower_start, lower_end, channel_width_m),
    ymin = base_m,
    ymax = c(
      upper_crest_m,
      lower_crest_with_logs_m,
      upper_crest_m
    ),
    structure = c("Actual upper crest", "Actual lower crest", "Actual upper crest")
  )
  logs <- if (n_stoplogs > 0) {
    data.frame(
      x_min = lower_start,
      x_max = lower_end,
      ymin = lower_crest_m +
        (seq_len(n_stoplogs) - 1) * stoplog_height_m,
      ymax = lower_crest_m + seq_len(n_stoplogs) * stoplog_height_m,
      structure = "Installed stoplogs"
    )
  } else {
    NULL
  }
  actual <- rbind(actual, logs)
  glm_crest <- data.frame(
    x = 0,
    xend = channel_width_m,
    y = crest_height_m,
    yend = crest_height_m
  )
  plot_top <- max(
    280.15,
    upper_crest_m,
    lower_crest_with_logs_m,
    crest_height_m
  ) + 0.04
  
  ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = actual,
      ggplot2::aes(
        xmin = x_min,
        xmax = x_max,
        ymin = ymin,
        ymax = ymax,
        fill = structure
      ),
      colour = "grey20",
      linewidth = 0.35
    ) +
    ggplot2::geom_segment(
      data = glm_crest,
      ggplot2::aes(x = x, xend = xend, y = y, yend = yend,
                   colour = "GLM single crest"),
      linewidth = 1.1,
      linetype = "longdash"
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "Actual upper crest" = "grey65",
        "Actual lower crest" = "#56B4E9",
        "Installed stoplogs" = "#E69F00"
      ),
      name = "Physical weir"
    ) +
    ggplot2::scale_colour_manual(
      values = c("GLM single crest" = "#D55E00"),
      name = "GLM approximation"
    ) +
    ggplot2::coord_cartesian(
      xlim = c(0, channel_width_m),
      ylim = c(base_m, plot_top),
      expand = FALSE
    ) +
    ggplot2::labs(
      title = "Actual Ōhau weir and GLM single-crest approximation",
      subtitle = paste0(
        "GLM crest height = ", format(crest_height_m, nsmall = 4),
        " m; crest factor = ", format(crest_factor, nsmall = 4),
        "; actual stoplogs shown = ", n_stoplogs
      ),
      x = "Distance across channel (m)",
      y = "Elevation (m)",
      caption = paste(
        "The dashed GLM line represents one level crest across the full width.",
        "Crest factor scales flow; it does not change the drawn geometry."
      )
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
}

# Examples:
# flow_curves <- generate_rotorua_weir_curves()
# plot_rotorua_weir_curves(flow_curves)
plot_rotorua_weir_profile()
plot_rotorua_glm_weir_comparison()
