#' Column metadata for the downloadable met data (one row per file x column)
#'
#' Units follow the AEME `MET_*` convention used throughout the pipeline.
#' @noRd
met_data_metadata <- function() {
  tz_note <- paste0("Fixed UTC+12 (NZST, tz 'Etc/GMT-12'); no daylight saving. ",
                    "Written as local clock time 'YYYY-MM-DD HH:MM:SS'.")

  common <- tibble::tribble(
    ~column, ~description, ~units,
    "MET_tmpair", "Air temperature", "degC",
    "MET_humrel", "Relative humidity", "%",
    "MET_prsttn", "Station (surface) air pressure", "Pa",
    "MET_wndspd", "Wind speed at 10 m", "m/s",
    "MET_wnddir", "Wind direction (direction the wind blows from, clockwise from north)", "degrees",
    "MET_radswd", "Downwelling shortwave radiation", "W/m2",
    "MET_pprain", "Rainfall depth per hour (or per logger interval for sub-hourly rows)", "m"
  )
  pick <- function(cols) common[match(cols, common$column), ]

  buoy <- dplyr::bind_rows(
    tibble::tibble(
      column = "Date",
      description = "Timestamp of the record",
      units = "local clock time",
      notes = paste0("Old buoy rows are every 15 min (mostly on the quarter hour); ",
                     "new-buoy rows are hourly.")),
    pick(c("MET_tmpair", "MET_humrel", "MET_prsttn", "MET_wndspd", "MET_wnddir",
           "MET_radswd", "MET_pprain")) |>
      dplyr::mutate(notes = c(
        "",
        "Raw sensor RH. Reads ~4-9% below ERA5/airport 2007-2021 and drifts high-bias from 2024 (2026 values unreliable) - treat with caution.",
        "Old buoy has a fixed offset correction applied after 2008-06-15 and is missing before that.",
        "Scaled to 10 m from the sensor height (old buoy 1.5 m, new buoy 2 m) with metscale::wind_at_height(); already in m/s.",
        "Hourly speed-weighted vector mean for the new buoy.",
        "New buoy only (from 2022-02-21); the old buoy has no shortwave sensor.",
        "Divide by 1000 to get mm. Quality-controlled and unit-corrected upstream (rotorua_buoy_met_data); sum rows over a period for totals.")),
    tibble::tibble(
      column = "MET_wnddir_Rbar",
      description = "Directional steadiness: vector-mean wind speed / scalar-mean wind speed",
      units = "0-1 (unitless)",
      notes = "1 = constant direction, 0 = no preferred direction. New buoy only."),
    tibble::tibble(
      column = "logger",
      description = "Which buoy logger produced the row",
      units = "old (2007-2022) / new (2022-)",
      notes = "")
  ) |>
    dplyr::mutate(file = "buoy_met.csv",
                  timezone = tz_note,
                  time_resolution = "15-minute (old buoy) / hourly (new buoy)")

  era5_orig <- dplyr::bind_rows(
    tibble::tibble(column = "Date", description = "Timestamp of the record",
                   units = "local clock time",
                   notes = "Raw ERA5-Land hourly series as read by the pipeline (read_era5_hourly_met())."),
    pick(c("MET_tmpair", "MET_humrel", "MET_prsttn", "MET_wndspd", "MET_wnddir",
           "MET_radswd", "MET_pprain")) |> dplyr::mutate(notes = c(
      "2 m air temperature", "Derived from 2 m temperature and dewpoint",
      "Surface pressure", "10 m wind speed",
      "Wind direction", "Shortwave radiation",
      "ERA5-Land total precipitation, converted from mm/hr to m/hr. Wet in ~41% of hours (drizzle bias).")),
    tibble::tribble(
      ~column, ~description, ~units, ~notes,
      "MET_tmpdew", "Dewpoint temperature at 2 m", "degC", "",
      "MET_wnduvu", "Eastward (u) wind component at 10 m", "m/s", "",
      "MET_wnduvv", "Northward (v) wind component at 10 m", "m/s", "",
      "MET_radlwd", "Downwelling longwave radiation", "W/m2", "")
  ) |>
    dplyr::mutate(file = "era5_hourly_original.csv",
                  timezone = tz_note, time_resolution = "hourly")

  era5_corr <- dplyr::bind_rows(
    tibble::tibble(column = "Date", description = "Timestamp of the record",
                   units = "local clock time", notes = "As in the original file."),
    pick(c("MET_tmpair", "MET_humrel", "MET_prsttn", "MET_wndspd", "MET_wnddir",
           "MET_radswd", "MET_pprain")) |> dplyr::mutate(notes = c(
      "Day-of-year additive scale correction against the combined buoy record",
      "Day-of-year additive scale correction against the MetService (Rotorua Aerodrome) hourly record, not the buoy",
      "Additive scale correction against the buoy record (removes the ~1.1 kPa elevation offset)",
      "Day-of-year ratio scale correction against the buoy (10 m, buoy wind scaled from sensor height)",
      "Not bias-corrected; unchanged from ERA5",
      "Day-of-year ratio scale correction against the new-buoy shortwave",
      "Two-stage: monthly quantile mapping to the buoy rain distribution (removes drizzle), then a monthly ratio scale to restore the buoy total")),
    tibble::tribble(
      ~column, ~description, ~units, ~notes,
      "MET_cldcvr", "Cloud cover fraction", "0-1 (unitless)", "Derived by metscale::expand_met() from the corrected variables",
      "MET_prvapr", "Vapour pressure", "hPa", "Derived by metscale::expand_met() from the corrected variables",
      "MET_prmslp", "Mean sea-level pressure", "Pa", "Derived by metscale::expand_met() from the corrected variables",
      "MET_tmpdew", "Dewpoint temperature at 2 m", "degC", "Original ERA5 value; NOT re-derived from the corrected temperature/humidity",
      "MET_radlwd", "Downwelling longwave radiation", "W/m2", "Original ERA5 value; not bias-corrected",
      "MET_wnduvu", "Eastward (u) wind component", "m/s", "Derived by metscale::expand_met() from the corrected wind",
      "MET_wnduvv", "Northward (v) wind component", "m/s", "Derived by metscale::expand_met() from the corrected wind",
      "MET_ppsnow", "Snowfall depth per hour", "m", "Always zero at this site")
  ) |>
    dplyr::mutate(file = "era5_hourly_bias_corrected.csv",
                  timezone = tz_note, time_resolution = "hourly")

  dplyr::bind_rows(buoy, era5_orig, era5_corr) |>
    dplyr::select(file, column, description, units, timezone, time_resolution, notes)
}

#' Write the downloadable met data zip (buoy, ERA5 original, ERA5 bias-corrected
#' and a column metadata CSV). The Rotorua Airport (NIWA / MetService) data are
#' intentionally not included.
#'
#' @param buoy_old,buoy_new old (2007-22) and new (2022-) buoy hourly AEME met
#' @param era5_original raw ERA5 hourly met (`era5_hourly_met`)
#' @param era5_corrected bias-corrected ERA5 hourly met (`era5_corrected_hourly`)
#' @param out_zip output zip path
#' @return `out_zip`
make_met_data_zip <- function(buoy_old, buoy_new, era5_original, era5_corrected,
                              out_zip) {
  tmp <- file.path(tempdir(), "rotorua_met_data")
  unlink(tmp, recursive = TRUE)
  dir.create(tmp, recursive = TRUE)

  fmt <- function(d) {
    d <- as.data.frame(d)
    d$Date <- format(d$Date, "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT-12")
    num <- vapply(d, is.numeric, logical(1))
    d[num] <- lapply(names(d)[num], function(n) {
      round(d[[n]], if (n == "MET_pprain") 7 else 3)
    })
    d
  }

  buoy <- dplyr::bind_rows(old = buoy_old, new = buoy_new, .id = "logger") |>
    dplyr::arrange(Date) |>
    dplyr::select(Date, dplyr::starts_with("MET_"), logger)

  readr::write_csv(fmt(buoy), file.path(tmp, "buoy_met.csv"), na = "")
  readr::write_csv(fmt(era5_original), file.path(tmp, "era5_hourly_original.csv"), na = "")
  readr::write_csv(fmt(era5_corrected), file.path(tmp, "era5_hourly_bias_corrected.csv"), na = "")
  readr::write_csv(met_data_metadata(), file.path(tmp, "metadata.csv"), na = "")

  dir.create(dirname(out_zip), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(out_zip)) file.remove(out_zip)
  zip::zip(out_zip, files = list.files(tmp), root = tmp, mode = "cherry-pick")
  out_zip
}
