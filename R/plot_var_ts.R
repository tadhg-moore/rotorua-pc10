# Function to plot GCM spatial data
plot_var_ts <- function(gcm_ts_df, variable = "tas", metadata, files) {
  # List all matching files
  # files <- list.files(
  #   dir, 
  #   pattern = paste0("^", variable, "_.*\\.nc$"), 
  #   full.names = TRUE
  # )
  
  var_meta <- metadata |> 
    dplyr::filter(variable == !!variable) |>
    dplyr::distinct(variable, .keep_all = TRUE)
  
  gcms <- c("ACCESS-CM2", "AWI-CM-1-1-MR", "CNRM-CM6-1", "EC-Earth3", 
            "GFDL-ESM4", "NZESM", "NorESM2-MM")
  # Separate historical and scenario files
  
  hist_df <- gcm_ts_df |> 
    dplyr::filter(variable == !!variable &
                    scenario %in% c("historical"))
  scen_all <- gcm_ts_df |> 
    dplyr::filter(variable == !!variable &
                    !(scenario %in% c("historical")))
  
  ref <- hist_df |> 
    dplyr::group_by(gcm) |>
    dplyr::summarise(
      ref_val = mean(value, na.rm = TRUE),
      .groups = "drop"
    )
  
  
  all <- dplyr::bind_rows(hist_df, scen_all) |> 
    dplyr::left_join(ref, by = "gcm") |>
    dplyr::mutate(
      anom = value - ref_val,
      scenario = factor(scenario, levels = c("historical", sort(unique(scen_all$scenario))))
    )
  
  p <- ggplot2::ggplot() +
    ggplot2::geom_vline(xintercept = 2015, linetype = "dotted") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::geom_line(data = all, ggplot2::aes(x = year, y = anom, 
                                                color = gcm), alpha = 0.4) +
    ggplot2::facet_grid(~scenario, scales = "free_x") +
    ggplot2::geom_smooth(data = all, 
                         ggplot2::aes(x = year, y = anom, group = scenario),
                         method = "loess", color = "black", se = TRUE) +
    ggplot2::labs(
      title = paste0("Annual Mean ", var_meta$variable_name_long,
                     " Anomalies"),
      x = "Year",
      y = expression(Delta~from~Historical~Mean),
      color = "Scenario"
    ) +
    ggplot2::theme_minimal()
  
  p
  
  
}
