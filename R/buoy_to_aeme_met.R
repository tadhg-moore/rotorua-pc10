buoy_to_aeme_met <- function(met, meas_height = 2, unit = c("day", "hour")) {
  met_aeme <- met |> 
    dplyr::mutate(
      DateTime = lubridate::as_datetime(DateTime, tz = "Etc/GMT-12"),
      Date = lubridate::round_date(DateTime, unit = unit),
      WndSpd = metscale::wind_at_height(WndSpd, from = meas_height, to = 10),
      PpRain = PpRain / 1000,
      # Speed-weighted direction components (NA if either speed or direction is missing)
      wnd_sin = WndSpd * sin(WndDir * pi / 180),
      wnd_cos = WndSpd * cos(WndDir * pi / 180),
      wnd_spd_valid = dplyr::if_else(is.na(wnd_sin) | is.na(wnd_cos), NA_real_, WndSpd)
    ) |>
    dplyr::group_by(Date) |>
    dplyr::summarise(
      MET_tmpair = mean(TmpAir, na.rm = TRUE),
      
      # Vector-mean components for the hour
      .mean_sin = mean(wnd_sin, na.rm = TRUE),
      .mean_cos = mean(wnd_cos, na.rm = TRUE),
      .mean_spd = mean(wnd_spd_valid, na.rm = TRUE),
      
      MET_prsttn = mean(PrBaro, na.rm = TRUE) * 100,
      MET_wndspd = mean(WndSpd, na.rm = TRUE) * 0.5144444, # knots to m/s
      MET_humrel = mean(HumRel, na.rm = TRUE),
      MET_radswd = mean(RadSWD, na.rm = TRUE),
      MET_pprain = sum(PpRain, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      # Speed-weighted mean direction, wrapped to [0, 360); NA if there was no wind at all
      MET_wnddir = dplyr::if_else(
        .mean_sin == 0 & .mean_cos == 0,
        NA_real_,
        (atan2(.mean_sin, .mean_cos) * 180 / pi + 360) %% 360
      ),
      # Directional steadiness: vector-mean speed / scalar-mean speed (0 to 1)
      MET_wnddir_Rbar = sqrt(.mean_sin^2 + .mean_cos^2) / .mean_spd
    ) |>
    dplyr::select(-.mean_sin, -.mean_cos, -.mean_spd) |>
    dplyr::arrange(Date) |>
    dplyr::filter(!is.na(Date))
    
  # met_aeme |> 
  #   dplyr::filter(is.na(MET_tmpair)) 
  # summary(met_aeme)
  return(met_aeme)
}
