read_ctd <- function(file) {
  
  sheets <- readxl::excel_sheets(file)
  
  coord_sheet <- sheets[grepl("Coordinates", sheets, ignore.case = TRUE)]
  ctd_sheets <- sheets[grepl("CTD", sheets, ignore.case = TRUE)]
  
  coords <- readxl::read_excel(file, sheet = coord_sheet) |> 
    sf::st_as_sf(coords = c("easting", "northing"), crs = 2193)
  
  # tm_shape(coords) +
  #   tm_dots()
  
  ctd_data <- lapply(ctd_sheets, function(sheet) {
    readxl::read_excel(file, sheet = sheet)
  }) |> 
    dplyr::bind_rows() |> 
    dplyr::mutate(Date = as.Date(Time))
  
  df_long <- ctd_data |> 
    dplyr::select(-Time, -LocationName) |> 
    tidyr::pivot_longer(
      cols = -c(Site, Date, `Depth (m)`),
      names_to = "variable",
      values_to = "value"
    ) |> 
    dplyr::filter(!is.na(value), value >= 0)
  
  # AEME::generate_var_map_code(data = df_long, var_col_name = "variable")
  var_map <- tibble::tribble(
    ~var_aeme, ~name, ~unit,
    "CHM_oxy", "Dissolved oxygen (g/m^3)", "mg/L",
    # "HYD_dens", "Conductivity (uS/cm)", "kg/m3",
    "HYD_temp", "Water Temperature (degC)", "degC",
    "PHY_tchla", "Chlorophyll (ug/l)", "mg/m^3",
    "CHM_oxysat", "Dissolved oxygen - field percentage saturation (%)", "%"
    # "NA", "Photosynthetically Active Radiation (umol/m^2/s)", "NA",
    # "NA", "Specific Conductance (uS/cm)", "NA",
    # "NA", "Turbidity, nephelometric turbidity units (NTU) (_NTU)", "NA",
  )
  
  aeme_obs <- AEME::lake_obs_to_aeme(data = df_long,
                                     datetime_col_name = "Date",
                                     lake_id_col = "Site",
                                     depth_col_name = "Depth (m)", 
                                     var_col_name = "variable",
                                     value_col_name = "value",
                                     var_map = var_map)
  
  # aeme_obs |> 
  #   dplyr::group_by(lake_id, var_aeme) |>
  #   dplyr::summarise(
  #     mean = mean(value, na.rm = TRUE),
  #     sd = sd(value, na.rm = TRUE),
  #     min = min(value, na.rm = TRUE),
  #     max = max(value, na.rm = TRUE),
  #     uniq_depths = length(unique(depth_from)),
  #     n_na = sum(is.na(value)),
  #     n_dates = length(unique(Date)),
  #     n_values = dplyr::n()
  #   ) 
  
  return(aeme_obs)
}
