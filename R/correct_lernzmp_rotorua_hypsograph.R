#' Correct Rotorua hypsograph
#' The hypsograph for Lake Rotorua (AEME ID 11133) appears to be incorrect, with
#' a bug that creates a 5m shelf at the shoreline 

# Install packages
install.packages(c("remotes", "dplyr"))
remotes::install_github("limnotrack/AEME", ref = "dev")
remotes::install_github("limnotrack/aemetools", ref = "dev")

# Get the AEME object for Lake Rotorua from the api
aeme <- aemetools::get_aeme(11133, api_key = "INSERT_KEY")

AEME::plot_hyps(aeme)

hyps <- AEME::get_hypsograph(aeme)
# Ruh ro! Dodgy looking hypsograph. Let's correct this

# Median lake level above sea level
# from: https://envdata.boprc.govt.nz/Data/DataSet/Summary/Location/FL150407/DataSet/Lake%20Level/NZVD2016/Interval/Latest
lake_elev <- 279.859 # masl

hyps_adj <- hyps |>
  dplyr::filter(elev <= lake_elev) |> 
  # Recalculate depth using the maximum elevation as the lake level
  dplyr::mutate(depth = -1 * (max(elev) - elev))

# Quick plot
plot(hyps_adj$area, hyps_adj$depth, type = "l", ylab = "Depth (m)", 
     xlab = "Area (m^2)")


# Calculate the incremental volumes
hyps_adj <- hyps_adj |> 
  dplyr::arrange(-depth) |> 
  dplyr::mutate(
    depth_next = dplyr::lead(depth), # next depth (shallower)
    area_next  = dplyr::lead(area), # next area (shallower)
    h          = abs(depth_next - depth), # height of the layer between current and next depth
    
    # Trapezoid rule: average of two areas × height
    vol_trap = (area + area_next) / 2 * h,
    
    # Frustum rule: accounts for curved basin shape - more accurate for natural lakes
    vol_frus = (h / 3) * (area + area_next + sqrt(area * area_next)),
    
    # Replace NA in the last row (no next depth) with 0
    vol_trap = dplyr::coalesce(vol_trap, 0),
    vol_frus = dplyr::coalesce(vol_frus, 0)
  )

# Calculate total volume and volume below 22m depth
tot_vol <- sum(hyps_adj$vol_frus)

# Volume less than 22m depth
vol_lt_22m <- sum(hyps_adj$vol_frus[hyps_adj$depth <= -22])

# Calculate percentages
(vol_lt_22m / tot_vol) * 100 
# 0.2021972 # %

# Volume greater than 22m depth
vol_gt_22m <- sum(hyps_adj$vol_frus[hyps_adj$depth > -22])
(vol_gt_22m / tot_vol) * 100
# 99.7978 # %
