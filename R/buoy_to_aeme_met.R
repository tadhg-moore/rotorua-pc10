buoy_to_aeme_met <- function(met) {
  met_aeme <- met |> 
    dplyr::mutate(
      Date = as.Date(DateTime),
      PpRain = PpRain / 1000
    ) |>
    dplyr::group_by(Date) |>
    dplyr::summarise(
      MET_tmpair = mean(TmpAir, na.rm = TRUE),
      # MET_wnddir = mean(WndDir, na.rm = TRUE),
      MET_prsttn = mean(PrBaro, na.rm = TRUE),
      MET_wndspd = mean(WndSpd, na.rm = TRUE),
      MET_humrel = mean(HumRel, na.rm = TRUE),
      MET_radswd = mean(RadSWD, na.rm = TRUE),
      MET_pprain = sum(PpRain, na.rm = TRUE)
    )
  # met_aeme |> 
  #   dplyr::filter(is.na(MET_tmpair)) 
  # summary(met_aeme)
  return(met_aeme)
}
