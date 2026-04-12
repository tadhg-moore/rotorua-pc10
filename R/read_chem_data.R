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
