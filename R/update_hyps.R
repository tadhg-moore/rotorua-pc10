fix_hyps <- function(aeme, lake_elev) {
  inp <- AEME::input(aeme)
  hyps <- inp$hypsograph 
  lke <- AEME::lake(aeme)
  # lake_elev <- lke$elevation - 5
  # Round lake_elev up to the nearest 0.2
  browser()
  lake_elev_rnd <- ceiling(lake_elev / 0.2) * 0.2
  
  
  ext_hyps <- hyps |> 
    dplyr::filter(depth > 0) |> 
    dplyr::mutate(
      elev = lake_elev + depth
    )
  
  hyps_adj <- hyps |>
    dplyr::filter(elev <= lake_elev) |> 
    dplyr::mutate(depth = -1 * (max(elev) - elev))
  
  hyps2 <- dplyr::bind_rows(
    hyps_adj,
    ext_hyps
  ) |> 
    dplyr::arrange(dplyr::desc(elev))
  # aeme <- AEME::add_hypsograph(aeme = aeme, hypsograph = hyps2)
  return(hyps2)
  
  hyps3 <- AEME::get_hypsograph(aeme)
  AEME::plot_hyps(aeme, add_surface = T)
  plot(hyps$area, hyps$depth, type = "l", xlab = "Area (m²)", ylab = "Depth (m)", main = "Hypsograph")
  points(hyps2$area, hyps2$depth, col = "red", pch = 19)
  abline(h = 0, v = 80000000, col = "blue", lty = 2)
}

update_hyps <- function(aeme, hyps) {
  inp <- AEME::input(aeme)
  lke <- AEME::lake(aeme)
  hyps_elev <- hyps |> 
    dplyr::filter(depth == 0) |> 
    dplyr::pull(elev)
  lke$elevation <- hyps_elev
  AEME::lake(aeme) <- lke
  
  browser()
  
  inp$init_depth <- abs(min(hyps$depth))
  inp$init_profile$depth <- c(0, abs(min(hyps$depth)))
  AEME::input(aeme) <- inp
  
  aeme <- AEME::add_hypsograph(aeme = aeme, hypsograph = hyps)
  return(aeme)

}
