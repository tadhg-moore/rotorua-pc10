unzip_pc10_data <- function(zip_file) {
  out_dir <- here::here("data", "processed", "pc10_data")
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  unzip(zip_file, exdir = out_dir, overwrite = TRUE)

  return(out_dir)
}

# Parse a single AQUARIUS Time-Series export CSV. These files start with a
# block of "#" comment lines (metadata) before the header row, and the number
# of comment lines varies slightly between exports, so it's detected rather
# than hardcoded.
read_aquarius_csv <- function(file) {
  header_lines <- readLines(file, n = 20, warn = FALSE)
  comment_lines <- header_lines[startsWith(header_lines, "#")]
  skip_n <- length(comment_lines)

  get_meta <- function(field) {
    line <- comment_lines[grepl(paste0("^#\\s*", field, ":"), comment_lines)]
    if (length(line) == 0) return(NA_character_)
    trimws(sub(paste0("^#\\s*", field, ":"), "", line[1]))
  }

  data <- readr::read_csv(file, skip = skip_n, show_col_types = FALSE)
  names(data)[1:3] <- c("datetime_utc", "datetime", "value")

  data |>
    dplyr::mutate(
      site = get_meta("Location"),
      variable = get_meta("Value parameter"),
      units = get_meta("Value units"),
      # The "Timestamp (UTC+12:00)" column is a fixed offset, not a
      # DST-aware NZ local time, so it must be parsed against a fixed-offset
      # tz (Etc/GMT-12 == UTC+12) rather than "Pacific/Auckland" -- the
      # latter would shift daylight-saving dates by an hour and corrupt the
      # daily date bucketing below.
      datetime = as.POSIXct(datetime, tz = "Etc/GMT-12"),
      date = as.Date(datetime)
    ) |>
    dplyr::select(site, variable, units, date, datetime, value,
                  dplyr::any_of(c("Approval Level", "Grade", "Qualifiers")))
}

extract_pc10_climate <- function(pc10_data_dir) {
  climate_dir <- file.path(pc10_data_dir, "Climate")
  files <- list.files(climate_dir, pattern = "\\.csv$", recursive = TRUE,
                      full.names = TRUE)

  climate_data <- dplyr::bind_rows(lapply(files, read_aquarius_csv))

  return(climate_data)
}

#' Summarise the PC10 climate stations, one row per variable/site
#'
#' Use this to decide which station to feed [pc10_climate_to_aeme_met()] --
#' record length and completeness matter as much as proximity to the lake,
#' since a short/gappy "nearest" record is often worse for forcing a model
#' than a longer one a few km further away.
pc10_climate_station_summary <- function(pc10_climate_data) {
  pc10_climate_data |>
    dplyr::group_by(variable, site, units) |>
    dplyr::summarise(
      n = dplyr::n(),
      start = min(date),
      end = max(date),
      pct_na = mean(is.na(value)),
      .groups = "drop"
    ) |>
    dplyr::arrange(variable, dplyr::desc(n))
}

#' Reshape PC10 climate data into the wide `Date` + `MET_*` layout used
#' elsewhere in this project (see [buoy_to_aeme_met()],
#' [niwa_hourly_to_aeme_met()]).
#'
#' PC10 only has air temperature, relative humidity and rainfall -- no
#' radiation or wind -- so the result is a partial met forcing, meant to be
#' combined with (or used to bias-correct/gap-fill) another source such as
#' the ERA5 or NIWA series already in this pipeline, not used standalone.
#'
#' One site is picked per variable via `stations`. Default picks the
#' longest, most complete record for each variable rather than assuming
#' geographic nearest-to-lake, since for Lake Rotorua all the realistic
#' candidate stations sit close to (or on) the lake margin anyway -- see
#' [pc10_climate_station_summary()] to compare alternatives or override.
#'
#' @param pc10_climate_data tidy data frame from [extract_pc10_climate()]
#' @param stations named character vector mapping variable -> site to use,
#'   e.g. `c("Air Temp" = "Rotorua at Edmund Rd")`
pc10_climate_to_aeme_met <- function(
    pc10_climate_data,
    stations = c(
      "Air Temp" = "Rotorua at Edmund Rd",
      "Rel Humidity" = "Rotorua at Edmund Rd",
      "Precip Total" = "Rotorua at Whakarewarewa"
    )
) {
  aeme_var <- c(
    "Air Temp" = "MET_tmpair",
    "Rel Humidity" = "MET_humrel",
    "Precip Total" = "MET_pprain"
  )

  missing_vars <- setdiff(names(stations), names(aeme_var))
  if (length(missing_vars) > 0) {
    stop("No AEME variable mapping for: ", paste(missing_vars, collapse = ", "),
         call. = FALSE)
  }

  df <- pc10_climate_data |>
    dplyr::filter(variable %in% names(stations),
                  site == stations[variable]) |>
    dplyr::mutate(
      value = dplyr::if_else(variable == "Precip Total", value / 1000, value),
      met_var = aeme_var[variable]
    ) |>
    # A handful of days have duplicate rows at a site (e.g. re-approved
    # readings re-exported); average rather than erroring the pivot.
    dplyr::group_by(date, met_var) |>
    dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop")

  met <- df |>
    tidyr::pivot_wider(names_from = met_var, values_from = value) |>
    dplyr::rename(Date = date) |>
    dplyr::arrange(Date)

  # Ensure every mapped variable has a column even if a station had zero
  # matching rows, so downstream code can rely on the column existing.
  for (col in unique(aeme_var[names(stations)])) {
    if (!col %in% names(met)) met[[col]] <- NA_real_
  }

  met |>
    dplyr::select(Date, dplyr::any_of(c("MET_tmpair", "MET_humrel",
                                        "MET_radswd", "MET_pprain",
                                        "MET_wndspd", "MET_prsttn")))
}
