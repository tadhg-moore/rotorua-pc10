plot_gcm_spatial <- function(df, variable = "tas", gcm = "ACCESS-CM2", metadata,
                             x = NULL) {
  var_meta <- metadata |> 
    dplyr::filter(variable == !!variable & gcm == !!gcm) |> 
    dplyr::distinct(variable, .keep_all = TRUE)
  if (variable == "tas") {
    var_meta$units <- "°C"
  } else if (variable == "pr") {
    var_meta$units <- "mm/year"
  } else if (variable == "hurs") {
    var_meta$units <- "%"
  } else if (variable == "rsds") {
    var_meta$units <- "W/m²"
  } else if (variable == "sfcWind") {
    var_meta$units <- "m/s"
  }
  
  hist_df <- df |> 
    dplyr::filter(variable == !!variable & gcm == !!gcm &
                    scenario == "historical")
  
  p1 <- ggplot2::ggplot() +
    ggplot2::geom_raster(data = hist_df, 
                         ggplot2::aes(x = x, y = y, fill = value)) +
    ggplot2::scale_fill_viridis_c() +
    ggplot2::labs(
      fill = "Value",
      x = "Longitude", y = "Latitude"
    ) +
    # Move legend to bottom
    ggplot2::coord_equal() +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
  
  ref_value <- mean(hist_df$value, na.rm = TRUE)
  
  scen_df <- df |> 
    dplyr::filter(variable == !!variable & gcm == !!gcm &
                    scenario != "historical") |> 
    dplyr::mutate(anom = value - ref_value)
  
  
  max_val <- abs(max(scen_df$anom))
  rng <- range(scen_df$anom, na.rm = TRUE)
  # Check if unidirectional or bidirectional
  if (rng[1] >= 0) {
    # Unidirectional positive
    palette <- "OrRd"
    limits <- c(0, max_val)
    direction <- 1
  } else if (rng[2] <= 0) {
    # Unidirectional negative
    palette <- "YlOrRd"
    limits <- c(-max_val, 0)
    direction <- -1
  } else  if (rng[1] < 0 & rng[2] > 0) {
    # Bidirectional
    palette <- "PRGn"
    limits <- c(-max_val, max_val)
    direction <- 1
  }
  
  
  p2 <- ggplot2::ggplot() +
    ggplot2::geom_raster(data = scen_df, ggplot2::aes(x = x, y = y, 
                                                      fill = anom)) +
    ggplot2::facet_grid(cols = ggplot2::vars(period),
                        rows = ggplot2::vars(scenario)) +
    ggplot2::scale_fill_fermenter(
      palette = palette,
      direction = direction,
      na.value = "transparent"
    ) +
    ggplot2::coord_equal() +
    ggplot2::labs(
      fill = expression(Delta~Historical~Mean),
      x = "Longitude", y = "Latitude"
    ) +
    ggplot2::theme_minimal() 
  p2
  
  if (!is.null(x)) {
    # Convert to WGS 84
    shp <- x |> 
      sf::st_transform(crs = 4326) |> 
      sf::st_geometry()
    
    # Convert to data frame for ggplot
    shp_df <- sf::st_coordinates(shp) |> 
      as.data.frame()
    
    p1 <- p1 +
      ggplot2::geom_path(data = shp_df, 
                         ggplot2::aes(x = X, y = Y), 
                         color = "black", size = 0.5)
    
    p2 <- p2 +
      ggplot2::geom_path(data = shp_df, 
                         ggplot2::aes(x = X, y = Y), 
                         color = "black", size = 0.5)
    
  }
  
  # Combine plots so p2 is twice the width of p1
  g <- patchwork::wrap_plots(p1, p2) +
    patchwork::plot_layout(widths = c(1, 2)) +
    patchwork::plot_annotation(
      title = paste0("Mean ", var_meta$variable_name_long, 
                     " and Changes from Historical for ", gcm),
      subtitle = paste0("Historical Mean (", var_meta$units, 
                        ") and Changes under Scenarios"),
      caption = "Data source: NZ Climate Data Portal"
    )
  return(g)
}
