read_met_service_xl <- function(file, tz) {
  # Read the Excel file
  # sheets <- readxl::excel_sheets(file)
  data <- readxl::read_excel(file, sheet = "Rotorua Aerodrome (NZROA)",
                             col_types = c("date", rep("numeric", 17))
                             )
  metadata <- readxl::read_excel(file, sheet = "About this data...", skip = 13)
  
  new_names <- c(
    "date",
    "wind_dir_10min_deg_true",
    "wind_speed_10min_m_s",
    "wind_dir_hourly_deg_true",
    "wind_speed_hourly_m_s",
    "cloud_1_amount_okta",
    "cloud_1_height_ft",
    "cloud_2_amount_okta",
    "cloud_2_height_ft",
    "cloud_3_amount_okta",
    "cloud_3_height_ft",
    "temperature_c",
    "dew_point_c",
    "relative_humidity_pct",
    "msl_pressure_hpa",
    "station_pressure_hpa",
    "rain_hourly_mm",
    "radiation_hourly_w_m2"
  )
  
  stopifnot(length(new_names) == ncol(data))
  names(data) <- new_names
  
  data$date <- lubridate::force_tz(data$date, tzone = tz)
  
  # Return the cleaned data
  return(data)
  
}
