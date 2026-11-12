fill_bathy <- function(file, outfile) {
  r <- terra::rast(file)
  names(r) <- "depth"
  
  # 1. Find the small interior gaps (same as before)
  na_mask <- terra::classify(is.na(r), cbind(0, NA))
  p       <- terra::patches(na_mask, directions = 8)
  sz      <- terra::freq(p)
  small   <- sz$value[sz$count < 10000]
  holes   <- terra::classify(p, cbind(small, 1), others = NA)
  
  hole_cells <- terra::cells(holes)              # cell numbers of the gap cells only
  rm(na_mask, p, holes); gc()
  
  # 2. Fill the gaps in a plain matrix
  m  <- as.matrix(r, wide = TRUE)         # rows x columns, same layout as the raster
  nr <- nrow(m); nc <- ncol(m)
  rr <- (hole_cells - 1) %/% nc + 1       # cell numbers run row by row
  cc <- (hole_cells - 1) %%  nc + 1
  todo <- which(is.na(m[cbind(rr, cc)]))
  
  while (length(todo) > 0) {
    i <- rr[todo]; j <- cc[todo]
    s <- n <- numeric(length(todo))
    for (di in -1:1) for (dj in -1:1) {   # the 8 neighbours
      if (di == 0 && dj == 0) next
      v  <- m[cbind(pmin(pmax(i + di, 1), nr), pmin(pmax(j + dj, 1), nc))]
      ok <- !is.na(v)
      s[ok] <- s[ok] + v[ok]
      n[ok] <- n[ok] + 1
    }
    upd <- n > 0
    if (!any(upd)) break
    m[cbind(i[upd], j[upd])] <- s[upd] / n[upd]
    todo <- todo[!upd]
  }
  
  # 3. Back to a raster, and the depth values for the hypsograph
  filled <- terra::rast(m, crs = terra::crs(r), extent = terra::ext(r))
  names(filled) <- "depth"
  d <- m[!is.na(m)]
  rm(m); gc()
  
  xy   <- cbind(1886247, 5775609)
  cell <- terra::cellFromXY(filled, xy)
  old  <- filled[cell][1, 1]          # the filled value currently in that cell
  old                                 # should be a depth around 30–40 m, not NA
  
  filled[cell] <- 53.5
  d[match(old, d)] <- 53.5            # keeps the hypsograph values in step
  
  dir <- dirname(outfile)
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  
  # filled <- -filled
  filled <- 297.63 - filled # Elevation from pdf 
  terra::writeRaster(filled, outfile, overwrite = TRUE)
  return(outfile)
}

get_gns_hyps <- function(file) {
  
  x <- terra::rast(file)
  
  mm <- terra::minmax(x)
  surf_elev <- mm[2]
  x_adj <- surf_elev - x
  d         <- terra::values(x_adj, na.rm = TRUE)[, 1]
  cell_area <- prod(terra::res(x))                     # 4 m²
  
  max_depth <- max(d)
  
  zi     <- AEME::model_layer_structure |> 
    dplyr::filter(zi <= abs(ceiling(max_depth)))
  z <- zi |> 
    dplyr::pull(z)
  z <- c(0, z)
  n_ge  <- length(d) - findInterval(z, sort(d), left.open = TRUE)   # cells with depth >= z
  hyps  <- data.frame(depth = z, area = n_ge * cell_area) |> 
    dplyr::filter(area > 0)
  
  # The map's colour scale tops out at ~41.8 m; true max depth is 53.5 m
  hyps <- rbind(hyps, data.frame(depth = 53.5, area = 0))
  
  # Capacity
  V      <- sum(d) * cell_area                       # m³
  A0     <- length(d) * cell_area                    # surface area, m²
  zmean  <- V / A0
  V_trap <- sum(diff(hyps$depth) * (head(hyps$area, -1) + tail(hyps$area, -1)) / 2)
  
  # plot(hyps$area / 1e6, -hyps$depth, type = "l",
  #      xlab = "Area (km²)", ylab = "Depth (m)")
  hyps <- hyps |> 
    dplyr::mutate(
      elev = surf_elev - depth,
      depth= -depth
    )
  
  return(hyps)
  
}

#' Survey hypsograph in the model's elevation datum, with the deep basin removed
#'
#' Takes the GNS survey hypsograph, truncates it at `cut_depth` (area closed to
#' zero at that depth), rebases elevations to the surface elevation used in the
#' model hypsograph, and re-attaches the above-surface extension rows from
#' `corr_hyps` so it can be passed straight to `update_hyps()`.
get_gns_hyps_model <- function(gns_hyps, corr_hyps, cut_depth = 25) {
  surf_elev <- corr_hyps |>
    dplyr::filter(depth == 0) |>
    dplyr::pull(elev)

  below <- gns_hyps |>
    dplyr::filter(depth > -cut_depth)
  # Interpolate area at the cut depth, then close the basin to zero area there
  below <- dplyr::bind_rows(
    below,
    data.frame(depth = -cut_depth, area = 0)
  ) |>
    dplyr::mutate(elev = surf_elev + depth)

  above <- corr_hyps |>
    dplyr::filter(depth > 0)

  dplyr::bind_rows(above, below) |>
    dplyr::select(elev, area, depth) |>
    dplyr::arrange(dplyr::desc(elev))
}
