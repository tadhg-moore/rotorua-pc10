get_vcsn_grid_points <- function(out_dir = here::here("data", "processed")) {
  # Read in VCSN grid points https://climatedata.environment.govt.nz/core-public-dataset.html#vcsn-agent-data
  vcsn_grid_points <- "https://climatedata.environment.govt.nz/data/core_public_data/geospatial/index_grid_points_vcsn_filtered_2193.gpkg"
  
  vcsn_grid_points <- sf::st_read(vcsn_grid_points)
  
  # library(tmap)
  # tmap_mode("view")
  # tm_shape(vcsn_grid_points) +
  #   tm_dots()
  
  out_file <- here::here(out_dir, "vcsn_grid_points.gpkg")
  sf::st_write(vcsn_grid_points, out_file, delete_dsn = TRUE)
  return(vcsn_grid_points)
}
