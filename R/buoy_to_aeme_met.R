buoy_to_aeme_met <- function(met, meas_height = 2, unit = c("day", "hour")) {
  met_aeme <- met |> 
    dplyr::mutate(
      DateTime = lubridate::as_datetime(met$DateTime, tz = "Pacific/Auckland"),
      Date = lubridate::round_date(DateTime, unit = unit),
      WndSpd = metscale::wind_at_height(WndSpd, from = meas_height, to = 10),
      PpRain = PpRain / 1000
    ) |>
    dplyr::group_by(Date) |>
    dplyr::summarise(
      MET_tmpair = mean(TmpAir, na.rm = TRUE),
      MET_wnddir = atan2(
        mean(sin(WndDir * pi / 180), na.rm = TRUE),
        mean(cos(WndDir * pi / 180), na.rm = TRUE)
      ) * 180 / pi %% 360,
      MET_wnddir_Rbar = sqrt(
        mean(sin(WndDir * pi / 180), na.rm = TRUE)^2 +
          mean(cos(WndDir * pi / 180), na.rm = TRUE)^2
      ),
      MET_prsttn = mean(PrBaro, na.rm = TRUE) * 100,
      MET_wndspd = mean(WndSpd, na.rm = TRUE),
      MET_humrel = mean(HumRel, na.rm = TRUE),
      MET_radswd = mean(RadSWD, na.rm = TRUE),
      MET_pprain = sum(PpRain, na.rm = TRUE), 
      .groups = "drop"
    ) |> 
    dplyr::arrange(Date) |> 
    dplyr::filter(!is.na(Date))
    
  # met_aeme |> 
  #   dplyr::filter(is.na(MET_tmpair)) 
  # summary(met_aeme)
  return(met_aeme)
}
