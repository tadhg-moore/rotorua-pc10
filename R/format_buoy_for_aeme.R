format_buoy_for_aeme <- function(df) {
  
  # Bin depths to 0.5m
  df <- df |> 
    dplyr::mutate(
      Date = as.Date(DateTime),
      depth_bin = round(DptSns * 2) / 2
      )
  
  df_daily <- df |> 
    dplyr::group_by(Date, depth_bin) |> 
    # Summarise from TmpWtr:ORP columns
    dplyr::summarise(dplyr::across(TmpWtr:ORP, \(x) mean(x, na.rm = TRUE)),
                     .groups = "drop")
  
  df_daily_long <- df_daily |> 
    tidyr::pivot_longer(
      cols = TmpWtr:ORP,
      names_to = "variable",
      values_to = "value"
    ) |> 
    # Remove NAs
    dplyr::filter(!is.na(value))
  
  # AEME::generate_var_map_code(data = df_daily_long, var_col_name = "variable")
  var_map <- tibble::tribble(
    ~var_aeme, ~name, ~unit,
    "CHM_oxy", "DOconc", "mg/L",
    "CHM_oxysat", "DOpsat", "%",
    "CHM_ph", "pH", "1",
    "HYD_temp", "TmpWtr", "degC"
  )
  
  aeme_obs <- AEME::lake_obs_to_aeme(data = df_daily_long,
                                       datetime_col_name = "Date",
                                     depth_col_name = "depth_bin", 
                                     var_col_name = "variable",
                                     value_col_name = "value",
                                     var_map = var_map)
  return(aeme_obs)
  
}
