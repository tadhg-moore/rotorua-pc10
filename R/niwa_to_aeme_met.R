niwa_to_aeme_met <- function(data) {
  sel_vars <- c(#"Evaporation [mm]",
                "Radiation [MJ/m2]", "Rainfall [mm]",
                "Mean Temperature [Deg C]", #"Mean Relative Humidity [percent]", 
                "Speed [m/s]")
  sel_method <- c("Penman-Open-Water-Evaporation", "Global", "daily")
  
  df <- data |> 
    dplyr::filter(variable_raw %in% sel_vars,
                  file_subtype %in% sel_method,
                  date >= as.Date("1992-01-01")
                  # date
                  ) |> 
    dplyr::mutate(
      na_chk = is.na(value),
      aeme_var = dplyr::case_when(
        file_variable == "Evaporation" ~ "LKE_evpflx",
        file_variable == "Radiation" ~ "MET_radswd",
        file_variable == "Rain" ~ "MET_pprain",
        file_variable == "Temperature" ~ "MET_tmpair",
        file_variable == "Wind" ~ "MET_wndspd",
        TRUE ~ NA_character_
      )
    ) |> 
    dplyr::select(date, aeme_var, value) 
  
  df_wid <- df |>
    tidyr::pivot_wider(names_from = aeme_var, values_from = value)
    
  return(df_wid)
  
  data |> 
    dplyr::group_by(variable_raw, file_subtype) |>
    dplyr::summarise(
      min_date = min(date),
      max_date = max(date),
    )
  
  ggplot(df, aes(x = date, y = aeme_var, fill = na_chk)) +
    geom_tile()
}