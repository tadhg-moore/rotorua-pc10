# ENSO helpers: fetch climate indices, compute 3-month SOI, classify phases.
# Convention follows Stats NZ "El Nino Southern Oscillation: Data to 2025":
#   3-month rolling mean SOI; >= +1 La Nina, <= -1 El Nino, otherwise neutral.

#' Read a whitespace "year + 12 months" table (CPC SOI, PSL TPI style)
#' into long monthly format.
.read_year_month_table <- function(url, na_values = c(-99.9, -99.99, -999, -999.9, -9999,
                                                      99.9, 99.99, 999, 999.9)) {
  txt <- readLines(url, warn = FALSE)
  rows <- strsplit(trimws(txt), "\\s+")
  rows <- Filter(function(x) length(x) >= 13 && grepl("^[0-9]{4}$", x[1]), rows)
  mat <- do.call(rbind, lapply(rows, function(x) as.numeric(x[1:13])))
  out <- tibble::tibble(
    year  = rep(mat[, 1], each = 12),
    month = rep(1:12, times = nrow(mat)),
    value = as.vector(t(mat[, 2:13]))
  )
  out$value[out$value %in% na_values] <- NA_real_
  out |>
    dplyr::mutate(date = as.Date(sprintf("%d-%02d-01", year, month))) |>
    dplyr::select(date, value)
}

#' Download ENSO-related monthly indices (and cache them unless cache_dir = NULL).
#' @return tibble: date, index (SOI, ONI, IPO_TPI, SAM), value
get_enso_indices <- function(cache_dir = here::here("data", "raw", "enso"),
                             refresh = FALSE) {
  # cache_dir = NULL: always download, nothing written (used by the targets
  # pipeline, target `enso_indices`)
  if (!is.null(cache_dir)) {
    dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
    cache <- file.path(cache_dir, "enso_indices_monthly.csv")
    if (file.exists(cache) && !refresh) {
      return(readr::read_csv(cache, show_col_types = FALSE))
    }
  }

  # Southern Oscillation Index (standardised Tahiti - Darwin), NOAA CPC
  soi_txt <- readLines("https://www.cpc.ncep.noaa.gov/data/indices/soi", warn = FALSE)
  i0 <- grep("STANDARDIZED", soi_txt, ignore.case = TRUE)
  soi_txt <- soi_txt[(i0[1] + 1):length(soi_txt)]
  tf <- tempfile(fileext = ".txt"); writeLines(soi_txt, tf)
  soi <- .read_fwf_year_month(tf)

  # Oceanic Nino Index (ONI, Nino3.4 3-month running mean), NOAA CPC
  oni <- readr::read_table("https://www.cpc.ncep.noaa.gov/data/indices/oni.ascii.txt",
                           show_col_types = FALSE) |>
    dplyr::mutate(
      # SEAS is a 3-month label (e.g. DJF); date = centre month
      month = c(DJF = 1, JFM = 2, FMA = 3, MAM = 4, AMJ = 5, MJJ = 6, JJA = 7,
                JAS = 8, ASO = 9, SON = 10, OND = 11, NDJ = 12)[SEAS],
      date = as.Date(sprintf("%d-%02d-01", YR, month))
    ) |>
    dplyr::transmute(date, value = ANOM)

  # Interdecadal Pacific Oscillation tripole index (ERSSTv5), NOAA PSL
  ipo <- .read_year_month_table(
    "https://psl.noaa.gov/data/timeseries/IPOTPI/tpi.timeseries.ersstv5.data")

  # Southern Annular Mode (AAO index, NOAA CPC) - SAM proxy
  sam_txt <- readLines(
    "https://www.cpc.ncep.noaa.gov/products/precip/CWlink/daily_ao_index/aao/monthly.aao.index.b79.current.ascii",
    warn = FALSE)
  sam <- readr::read_table(I(sam_txt), col_names = c("year", "month", "value"),
                           show_col_types = FALSE) |>
    dplyr::mutate(date = as.Date(sprintf("%d-%02d-01", year, month))) |>
    dplyr::select(date, value)

  out <- dplyr::bind_rows(
    dplyr::mutate(soi, index = "SOI"),
    dplyr::mutate(oni, index = "ONI"),
    dplyr::mutate(ipo, index = "IPO_TPI"),
    dplyr::mutate(sam, index = "SAM")
  ) |>
    dplyr::filter(!is.na(value)) |>
    dplyr::arrange(index, date)
  if (!is.null(cache_dir)) readr::write_csv(out, cache)
  out
}

# CPC SOI file is fixed-width-ish: year then 12 values separated by spaces
.read_fwf_year_month <- function(path) {
  .read_year_month_table(path)
}

#' Classify ENSO phase from a monthly SOI series (Stats NZ convention).
#' @param soi tibble with columns date, value (monthly SOI)
#' @param threshold +/- threshold on the 3-month mean
classify_soi <- function(soi, threshold = 1) {
  soi |>
    dplyr::arrange(date) |>
    dplyr::mutate(
      # trailing 3-month mean (month t-2..t), as in a rolling average
      soi3 = as.numeric(stats::filter(value, rep(1 / 3, 3), sides = 1)),
      phase = dplyr::case_when(
        is.na(soi3)          ~ NA_character_,
        soi3 >= threshold    ~ "La Nina",
        soi3 <= -threshold   ~ "El Nino",
        TRUE                 ~ "Neutral"
      ),
      phase = factor(phase, levels = c("El Nino", "Neutral", "La Nina"))
    )
}

#' Classify ENSO phase from ONI (+/- 0.5 C, no persistence requirement here).
classify_oni <- function(oni, threshold = 0.5) {
  oni |>
    dplyr::arrange(date) |>
    dplyr::mutate(phase_oni = dplyr::case_when(
      value >= threshold  ~ "El Nino",
      value <= -threshold ~ "La Nina",
      TRUE                ~ "Neutral"))
}

#' Summarise contiguous runs of a phase (start, end, length in months).
phase_events <- function(x, phase_col = "phase") {
  x <- x[!is.na(x[[phase_col]]), ]
  r <- rle(as.character(x[[phase_col]]))
  ends <- cumsum(r$lengths)
  starts <- ends - r$lengths + 1
  tibble::tibble(phase = r$values, start = x$date[starts], end = x$date[ends],
                 months = r$lengths) |>
    dplyr::filter(phase != "Neutral")
}

#' Stats NZ / Earth Sciences NZ SOI (3-month mean, official phases), 1990-2025.
#' Primary index for 1990+; NOAA CPC SOI (get_enso_indices) is used before 1990.
read_statsnz_soi <- function(path = here::here("data", "raw", "enso",
                                               "soi_statsnz_1990_2025.csv")) {
  # `path` can be a local file or the Stats NZ download URL (target `statsnz_soi_url`)
  readr::read_csv(path, show_col_types = FALSE) |>
    dplyr::transmute(
      date = as.Date(sprintf("%d-%02d-01", year, month)),
      soi3 = soi_3mth_mean,
      temp_anom = de_trended_temp_anomaly,
      phase = factor(dplyr::recode(soi_phase, "El Ni\u00f1o" = "El Nino",
                                   "La Ni\u00f1a" = "La Nina"),
                     levels = c("El Nino", "Neutral", "La Nina")))
}

#' Long SOI series with phases: Stats NZ 3-month SOI from 1990, and the NOAA
#' CPC SOI (3-month trailing mean) rescaled to it before 1990 using a linear fit
#' over the overlap.
#' @param indices output of get_enso_indices() (needs index == "SOI")
#' @param statsnz output of read_statsnz_soi()
#' @return tibble: date, soi3, phase (El Nino / Neutral / La Nina), source
splice_soi <- function(indices, statsnz) {
  noaa <- classify_soi(indices |> dplyr::filter(index == "SOI") |>
                         dplyr::select(date, value)) |>
    dplyr::select(date, soi3_noaa = soi3)
  snz <- dplyr::select(statsnz, date, soi3_snz = soi3, phase_snz = phase)
  cf <- stats::coef(stats::lm(soi3_snz ~ soi3_noaa, dplyr::inner_join(snz, noaa, by = "date")))
  noaa |>
    dplyr::left_join(snz, by = "date") |>
    dplyr::mutate(
      source = ifelse(is.na(soi3_snz), "NOAA (rescaled)", "Stats NZ"),
      soi3 = ifelse(is.na(soi3_snz), cf[1] + cf[2] * soi3_noaa, soi3_snz),
      phase = dplyr::case_when(!is.na(phase_snz) ~ as.character(phase_snz),
                               soi3 >= 1 ~ "La Nina", soi3 <= -1 ~ "El Nino",
                               TRUE ~ "Neutral"),
      phase = factor(phase, levels = c("El Nino", "Neutral", "La Nina"))) |>
    dplyr::filter(!is.na(soi3)) |>
    dplyr::select(date, soi3, phase, source)
}
