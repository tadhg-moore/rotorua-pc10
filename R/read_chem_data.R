read_chem_data <- function(file, lake = "Rotorua") {
  
  sheets <- readxl::excel_sheets(file)
  
  site_sheet <- sheets[grepl("site", sheets, ignore.case = TRUE)]
  data_sheet <- sheets[grepl("data", sheets, ignore.case = TRUE)]
  
  sites <- readxl::read_excel(file, sheet = site_sheet) |> 
    sf::st_as_sf(coords = c("Easting", "Northing"), crs = 2193) |> 
    dplyr::filter(grepl(lake, LocationName, ignore.case = TRUE))
  
  site_id <- sites$Site
  
  df_wid <- readxl::read_excel(file, sheet = data_sheet)
  df_wid <- df_wid |> 
    dplyr::filter(AquariusID %in% site_id) |> 
    dplyr::mutate(
      # Extract after "_" character
      type = stringr::str_extract(AquariusID, "(?<=_).+"),
      type = dplyr::case_when(
        grepl("BOT", type, ignore.case = TRUE) ~ "bottom",
        grepl("HYP", type, ignore.case = TRUE) ~ "hypolimnion",
        grepl("INT", type, ignore.case = TRUE) ~ "integrated",
        TRUE ~ "Unknown"
      )
    )
}

format_chem_for_aeme <- function(df) {
  
  # Nutrients are reported in mg/m^3; AEME holds them in g/m^3 (chl-a stays mg/m^3)
  nutrients <- c("TP (mg/m^3)", "TN (mg/m^3)", "NNN (mg/m^3)",
                 "NH4-N (mg/m^3)", "DRP (mg/m^3)")
  
  df_long <- df |> 
    dplyr::mutate(Date = as.Date(Date),
                  DepthFrom = dplyr::coalesce(DepthFrom, 0)) |> 
    dplyr::select(AquariusID, Date, DepthFrom,
                  `TP (mg/m^3)`, `TN (mg/m^3)`, `CHLA (mg/m^3)`, `NNN (mg/m^3)`,
                  `NH4-N (mg/m^3)`, `DRP (mg/m^3)`,
                  `pH (pH Units)`, `Secchi (m)`) |> 
    tidyr::pivot_longer(cols = -c(AquariusID, Date, DepthFrom),
                        names_to = "variable", values_to = "value") |> 
    dplyr::filter(!is.na(value), value >= 0) |> 
    dplyr::mutate(value = dplyr::if_else(variable %in% nutrients, value / 1000, value))
  
  var_map <- tibble::tribble(
    ~var_aeme,      ~name,              ~unit,
    "PHS_tp",       "TP (mg/m^3)",      "g/m^3",
    "NIT_tn",       "TN (mg/m^3)",      "g/m^3",
    "PHY_tchla",    "CHLA (mg/m^3)",    "mg/m^3",
    "NIT_nit",      "NNN (mg/m^3)",     "g/m^3",
    "NIT_amm",      "NH4-N (mg/m^3)",   "g/m^3",
    "PHS_frp",      "DRP (mg/m^3)",     "g/m^3",
    "CHM_ph",       "pH (pH Units)",    "1",
    "RAD_secchi",   "Secchi (m)",       "m"
  )
  
  AEME::lake_obs_to_aeme(data = df_long,
                         datetime_col_name = "Date",
                         lake_id_col = "AquariusID",
                         depth_col_name = "DepthFrom",
                         var_col_name = "variable",
                         value_col_name = "value",
                         var_map = var_map)
}
