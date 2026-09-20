#' Combine the raw NIWA hourly climate CSVs into one AEME-format hourly
#' data frame
#'
#' Each hourly file (temperature, wind, radiation, rain, pressure) is read
#' and reshaped separately -- unlike [read_niwa_climate_file()], which is
#' written for the *daily* files and hard-codes a 24 h accumulation window
#' when converting radiation (MJ/m2 -> W/m2) and evaporation (mm/day ->
#' m/s). That assumption does not hold for the hourly files: their
#' `PERIOD [hrs]` column varies row to row (e.g. rain was recorded as
#' 6-hourly synoptic totals before the station was automated, radiation is
#' usually but not always a 1 h window), so every rate/energy conversion
#' here divides by the row's own `PERIOD [hrs]` instead of assuming 1 or 24.
#'
#' The result uses the same `Date` + `MET_*` wide layout as
#' [buoy_to_aeme_met()] and [read_era5_hourly_met()], so it can be passed
#' directly as the reference (or an alternative reference) to
#' `metscale::fit_met_bias_correction()` alongside `era5_hourly_met` --
#' e.g. `metscale::fit_met_bias_correction(era5_hourly_met,
#' niwa_met_hourly_aeme, vars = c("MET_tmpair", "MET_wndspd", "MET_radswd",
#' "MET_humrel", "MET_prsttn"), method = "scale", by = "doy-loess")`.
#'
#' @param files character vector of paths to the `*__hourly.csv` NIWA files
#'   (`niwa_met_hourly_files`)
#' @param tz timezone label attached to `Date`, as a fixed offset to match
#'   `era5_hourly_met` (see [read_era5_hourly_met()]) rather than a DST-aware
#'   zone
#'
#' @return data frame with columns `Date` (POSIXct), `MET_tmpair`,
#'   `MET_humrel`, `MET_radswd`, `MET_pprain`, `MET_wndspd`, `MET_prsttn`
niwa_hourly_to_aeme_met <- function(files, tz = "Etc/GMT-12") {

  find_file <- function(pattern) {
    hit <- files[grepl(pattern, basename(files), ignore.case = TRUE)]
    if (length(hit) != 1) {
      stop("Expected exactly one hourly NIWA file matching '", pattern,
           "', found ", length(hit), call. = FALSE)
    }
    hit
  }

  read_hourly <- function(pattern) {
    readr::read_csv(find_file(pattern), show_col_types = FALSE)
  }

  # readr already parses "Observation time UTC" as POSIXct (it matches
  # readr's ISO-8601 guess), so only re-parse it when that guess didn't
  # happen -- running ymd_hms() on an already-POSIXct column silently
  # returns NA for every row.
  to_utc <- function(x) {
    if (inherits(x, "POSIXct")) {
      lubridate::with_tz(x, tzone = "UTC")
    } else {
      lubridate::ymd_hms(x, tz = "UTC")
    }
  }

  # ---- Radiation: MJ/m2 accumulated over PERIOD [hrs] -> mean W/m2 ----
  rad <- read_hourly("^1770__Radiation")
  rad_df <- data.frame(
    datetime_utc = to_utc(rad[["Observation time UTC"]]),
    MET_radswd = (rad[["Radiation [MJ/m2]"]] * 1e6) /
      (rad[["PERIOD [hrs]"]] * 3600)
  )

  # ---- Rain: mm accumulated over PERIOD [hrs] -> mean m/hr ----
  rain <- read_hourly("^1770__Rain")
  rain_df <- data.frame(
    datetime_utc = to_utc(rain[["Observation time UTC"]]),
    MET_pprain = (rain[["Rainfall [mm]"]] / 1000) / rain[["PERIOD [hrs]"]]
  )

  # ---- Wind speed, already an instantaneous/short-window m/s value ----
  wind <- read_hourly("^1770__Wind")
  wind_df <- data.frame(
    datetime_utc = to_utc(wind[["Observation time UTC"]]),
    MET_wndspd = wind[["Speed [m/s]"]]
  )

  # ---- Temperature (deg C) + relative humidity (percent) ----
  temp <- read_hourly("^1770__Temperature")
  mean_temp <- temp[["Mean Temperature [Deg C]"]]
  # Early records only report max/min, not the mean -- fall back to their
  # midpoint rather than dropping the hour entirely.
  midpoint <- (temp[["Maximum Temperature [Deg C]"]] +
                 temp[["Minimum Temperature [Deg C]"]]) / 2
  mean_temp <- ifelse(is.na(mean_temp), midpoint, mean_temp)
  temp_df <- data.frame(
    datetime_utc = to_utc(temp[["Observation time UTC"]]),
    MET_tmpair = mean_temp,
    MET_humrel = temp[["Mean Relative Humidity [percent]"]]
  )

  # ---- Pressure: mean sea level pressure, hPa -> Pa ----
  pres <- read_hourly("^1770__Pressure")
  pres_df <- data.frame(
    datetime_utc = to_utc(pres[["Observation time UTC"]]),
    MET_prsttn = pres[["Mean sea level pressure [Hpa]"]] * 100
  )

  # A handful of timestamps are duplicated within a single file (e.g. the
  # rain file); dedupe each variable on its own before joining so the joins
  # below stay one-to-one instead of blowing up combinatorially.
  dedupe <- function(df) {
    df |>
      dplyr::filter(!is.na(datetime_utc)) |>
      dplyr::distinct(datetime_utc, .keep_all = TRUE)
  }

  met <- list(temp_df, rad_df, rain_df, wind_df, pres_df) |>
    lapply(dedupe) |>
    Reduce(function(x, y) dplyr::full_join(x, y, by = "datetime_utc"), x = _) |>
    dplyr::arrange(datetime_utc) |>
    dplyr::mutate(Date = datetime_utc) |>
    dplyr::select(Date, MET_tmpair, MET_humrel, MET_radswd, MET_pprain,
                  MET_wndspd, MET_prsttn)

  attr(met$Date, "tzone") <- tz

  met
}
