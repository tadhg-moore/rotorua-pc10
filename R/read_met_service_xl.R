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

#' Convert the MetService Rotorua Aerodrome (NZROA) hourly table to the AEME
#' `Date` + `MET_*` layout
#'
#' Units follow the "About this data..." sheet of the source workbook: wind in
#' m/s, rain in mm, pressure in hPa, radiation in W/m2. Rain, wind and
#' radiation are *hour-ending* values (they describe the hour preceding the
#' timestamp); temperature, humidity and pressure are instantaneous.
#'
#' Wind speed/direction use the hourly mean where recorded and fall back to
#' the 10-minute mean otherwise (the hourly columns are missing for most of
#' the earlier record).
#'
#' @param met output of [read_met_service_xl()]
#' @return data frame with `Date`, `MET_tmpair` (degC), `MET_humrel` (%),
#'   `MET_prsttn` (Pa), `MET_pprain` (m/hour), `MET_radswd` (W/m2),
#'   `MET_wndspd` (m/s) and `MET_wnddir` (degrees)
airport_met_to_aeme_met <- function(met) {
  data.frame(
    Date       = met$date,
    MET_tmpair = met$temperature_c,
    MET_humrel = met$relative_humidity_pct,
    MET_prsttn = met$station_pressure_hpa * 100,
    MET_pprain = met$rain_hourly_mm / 1000,
    MET_radswd = met$radiation_hourly_w_m2,
    MET_wndspd = dplyr::coalesce(met$wind_speed_hourly_m_s, met$wind_speed_10min_m_s),
    MET_wnddir = dplyr::coalesce(met$wind_dir_hourly_deg_true, met$wind_dir_10min_deg_true)
  )
}
