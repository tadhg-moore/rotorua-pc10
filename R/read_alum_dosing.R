read_alum_dosing <- function(file) {
  # Read the Excel file
  readxl::excel_sheets(file)  # List all sheet names in the Excel file
  data <- readxl::read_excel(file, sheet = "Rotorua", 
                             col_types = c("text", "numeric", "date", rep("numeric", 9)))
  
  # Clean and process the data as needed
  # For example, you might want to rename columns, filter rows, etc.
  data |> 
    dplyr::select(Date, `Utuhina Alum (L/day)`, `Puarenga alum (L/day)`) |> 
    dplyr::rename(
      utuhina_alum_l_per_day = `Utuhina Alum (L/day)`,
      puarenga_alum_l_per_day = `Puarenga alum (L/day)`
    ) |> 
    dplyr::mutate(
      utuhina_alum_l_per_day = dplyr::if_else(is.na(utuhina_alum_l_per_day), 0, utuhina_alum_l_per_day),
      puarenga_alum_l_per_day = dplyr::if_else(is.na(puarenga_alum_l_per_day), 0, puarenga_alum_l_per_day),
      utuhina_alum_l_per_day = dplyr::if_else(utuhina_alum_l_per_day < 0, 0, utuhina_alum_l_per_day),
      puarenga_alum_l_per_day = dplyr::if_else(puarenga_alum_l_per_day < 0, 0, puarenga_alum_l_per_day)
    )
}
