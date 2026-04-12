read_niwa_climate_file <- function(filepath) {
  
  # ---- 1. Extract metadata from filename ----
  fname <- basename(filepath)
  fname_noext <- tools::file_path_sans_ext(fname)
  
  parts <- strsplit(fname_noext, "__")[[1]]
  
  station_id <- parts[1]
  variable   <- parts[2]
  subtype    <- if (length(parts) >= 3) parts[3] else NA
  frequency  <- if (length(parts) >= 4) parts[4] else NA
  
  
  # ---- 2. Read file ----
  df <- readr::read_csv(filepath, show_col_types = FALSE)
  
  
  # ---- 3. Standardise datetime ----
  if (!"Observation time UTC" %in% names(df)) {
    stop("Missing 'Observation time UTC' column.")
  }
  
  df <- df |>
    dplyr::mutate(
      datetime_utc = lubridate::ymd_hms(.data$`Observation time UTC`,
                                        tz = "UTC"),
      date = as.Date(datetime_utc)
    )
  
  
  # ---- 4. Pivot measurement columns ----
  # Identify numeric measurement columns
  numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  
  # Remove period columns from main value columns
  value_cols <- numeric_cols[!grepl("Period|PERIOD", numeric_cols,
                                    ignore.case = TRUE)]
  
  # Identify frequency columns where all values are either "D" or "H"
  freq_cols <- names(df)[vapply(df, function(col) {
    is.character(col) && all(col %in% c("D", "H", NA))
  }, logical(1))]
  
  df_long <- df |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(value_cols),
      names_to = "variable_raw",
      values_to = "value_raw"
    )
  
  
  # ---- 5. Unit conversions ----
  df_long <- df_long |>
    dplyr::mutate(
      
      value = dplyr::case_when(
        
        # Radiation conversion: MJ/m2 → W/m2
        grepl("Radiation", variable_raw, ignore.case = TRUE) ~
          (value_raw * 1e6) / (24 * 3600),
        
        # Rainfall conversion: mm/day → m/day
        grepl("Rainfall", variable_raw, ignore.case = TRUE) ~
          value_raw / 1000,

        # Evaporation conversion: mm/day → m/s
        grepl("Evaporation", variable_raw, ignore.case = TRUE) ~
          (value_raw / 1000) / (24 * 60 * 60),
        
        # Temperatures already °C
        grepl("Temperature", variable_raw, ignore.case = TRUE) ~
          value_raw,
        
        # Relative humidity already percent
        grepl("Humidity", variable_raw, ignore.case = TRUE) ~
          value_raw,
        
        TRUE ~ value_raw
      ),
      
      unit = dplyr::case_when(
        grepl("Radiation", variable_raw, ignore.case = TRUE) ~ "W/m2",
        grepl("Temperature", variable_raw, ignore.case = TRUE) ~ "degC",
        grepl("Humidity", variable_raw, ignore.case = TRUE) ~ "percent",
        grepl("Rainfall", variable_raw, ignore.case = TRUE) ~ "m/day",
        grepl("Evaporation", variable_raw, ignore.case = TRUE) ~ "m/s",
        TRUE ~ NA_character_
      ),
      # frequency = dplyr::case_when(
      #   grepl("D", `Frequency [D/H]`) ~ "daily",
      #   grepl("H", `Frequency [D/H]`) ~ "hourly",
      #   .default = NA_character_
      # )
    )
  
  
  # ---- 6. Attach metadata ----
  df_long |>
    dplyr::mutate(
      station_id = station_id,
      file_variable = variable,
      file_subtype = subtype,
      file_frequency = frequency
    ) |>
    dplyr::select(
      station_id,
      file_variable,
      file_subtype,
      file_frequency,
      datetime_utc,
      date,
      variable_raw,
      value,
      unit
    )
}
