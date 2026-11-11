# Readers for the water quality data used by the lake water quality synthesis
# (website/lake-water-quality-synthesis.qmd):
#   - BOPRC river WQ workbook (data/raw/flows/BOPRC - River WQ Data Rotorua.xlsx)
#   - PC10 Jun-26 lake chemistry and CTD workbooks (extend the bop_wq files to 2026)
#   - PC10 Jun-26 alum dose rates and Puarenga compliance monitoring
#   - Long-record alum dose in kg Al/day
# All readers are pure: they read and return a tibble, and write nothing.

# River WQ variables kept (column name prefix before the unit in brackets).
river_wq_keep <- c(
  "TN", "TKN", "NO3-N", "NNN", "NO2-N", "NH4-N", "DIN", "TP", "DRP",
  "TSS", "Turbidity", "Turbidity NTU", "DOC", "DO", "DO sat", "pH",
  "Water Temp", "Al tot", "Silica diss", "SiO2 diss reactive", "SpC",
  "Discharge"
)

#' Site metadata of the BOPRC river WQ workbook as an sf object (NZTM, EPSG 2193)
read_river_wq_sites <- function(file) {
  readxl::read_excel(file, sheet = "River WQ Location Metadata") |>
    dplyr::transmute(site_id = identifier, location = locationname,
                     latitude, longitude, easting, northing,
                     elevation_m = `elevation (mRL)`, lawa_id, recnzreachid) |>
    sf::st_as_sf(coords = c("easting", "northing"), crs = 2193, remove = FALSE)
}

#' BOPRC river WQ samples in long format
#'
#' The data sheet is wide (one row per sample, one column per determinand with
#' its unit in brackets, e.g. `TN (g/m^3)`). Columns are parsed into
#' `variable` and `unit`, restricted to `river_wq_keep`, and returned long.
#'
#' @return tibble: site_id, location, datetime, date, variable, unit, value
read_river_wq <- function(file, keep = river_wq_keep) {
  d <- readxl::read_excel(file, sheet = "River WQ Data", guess_max = 10000)
  id_cols <- c("Site", "Time", "LocationName")
  val_cols <- setdiff(names(d)[vapply(d, is.numeric, logical(1))], id_cols)
  hdr <- tibble::tibble(
    col = val_cols,
    variable = sub("^(.*) \\(([^()]*)\\)$", "\\1", val_cols),
    unit = ifelse(grepl("\\)$", val_cols),
                  sub("^(.*) \\(([^()]*)\\)$", "\\2", val_cols), NA_character_)
  ) |>
    dplyr::filter(variable %in% keep)

  d |>
    dplyr::select(dplyr::all_of(c(id_cols, hdr$col))) |>
    tidyr::pivot_longer(-dplyr::all_of(id_cols), names_to = "col",
                        values_to = "value", values_drop_na = TRUE) |>
    dplyr::left_join(hdr, by = "col") |>
    dplyr::transmute(
      site_id = Site, location = LocationName,
      datetime = lubridate::force_tz(Time, "Etc/GMT-12"),
      date = as.Date(Time), variable, unit, value
    ) |>
    dplyr::arrange(site_id, datetime, variable)
}

#' Map BOPRC river WQ locations to the PC10 inflow names
river_wq_inflow_key <- function() {
  tibble::tribble(
    ~location,                         ~inflow,       ~alum_dosed, ~note,
    "Utuhina at Lake Rd",              "Utuhina",     TRUE,  "below dosing plant",
    "Puarenga at FRI",                 "Puarenga",    TRUE,  "above dosing plant",
    "Ngongotaha at SH36",              "Ngongotaha",  FALSE, "",
    "Ngongotaha at SH5",               "Ngongotaha",  FALSE, "upstream, short record",
    "Kaikaitahuna at Hamurana Rd",     "Hamurana",    FALSE, "",
    "Awahou at SH36",                  "Awahou",      FALSE, "",
    "Waiteti at SH36",                 "Waiteti",     FALSE, "",
    "Waiteti at Oturoa Rd",            "Waiteti",     FALSE, "upstream",
    "Waiowhiro at Bonningtons Farm",   "Waiowhiro",   FALSE, "",
    "Waingaehe at SH30",               "Waingaehe",   FALSE, "",
    "Waiohewa at Rangiteaorere Rd",    "Waiohewa",    FALSE, "",
    "Ohau Channel at SH33",            "Ohau outflow", FALSE, "lake outflow"
  )
}

#' Lake chemistry from the PC10 Jun-26 "Rotorua S2 & S5 data 1989_2026.xlsx"
#'
#' Reads both site sheets, trims the padded headers, and returns one row per
#' sample with snake-case names (units mg/m^3 except pH, NTU, Secchi m).
#' `layer` is INT (integrated epilimnion), HYP (discrete hypolimnion) or BOT.
read_lake_chem_2026 <- function(file) {
  sheets <- readxl::excel_sheets(file)
  sheets <- sheets[grepl("data", sheets, ignore.case = TRUE) &
                     grepl("S[25]", sheets)]
  dplyr::bind_rows(lapply(sheets, function(s) {
    d <- readxl::read_excel(file, sheet = s, guess_max = 5000)
    names(d) <- trimws(names(d))
    d
  })) |>
    dplyr::transmute(
      site_id = AquariusID,
      site = StationName,
      layer = sub(".*_", "", AquariusID),
      date = as.Date(Date),
      depth_from = DepthFrom, depth_to = DepthTo,
      tp = `TP (mg/m^3)`, tn = `TN (mg/m^3)`, chla = `CHLA (mg/m^3)`,
      nnn = `NNN (mg/m^3)`, nh4 = `NH4-N (mg/m^3)`, drp = `DRP (mg/m^3)`,
      tkn = `TKN (mg/m3)`, turb = `Turbidity (NTU)`, ph = `pH (pH Units)`,
      secchi = `Secchi (m)`, al = `Al (mg/m3)`
    ) |>
    dplyr::arrange(site, layer, date)
}

#' CTD profiles from the PC10 Jun-26 "Lake Rotorua S2 & S5 Profile data 2001_2026.xlsx"
#'
#' Same long layout as `read_ctd()` (Site, Date, Depth (m), variable, value),
#' with short variable codes added.
read_lake_ctd_2026 <- function(file) {
  d <- readxl::read_excel(file, sheet = 1, guess_max = 20000)
  codes <- c(
    "Chlorophyll (ug/l)" = "chl",
    "Dissolved oxygen (g/m^3)" = "do_mgl",
    "Dissolved oxygen - field percentage saturation (%)" = "do_sat",
    "Photosynthetically Active Radiation (umol/m^2/s)" = "par",
    "Specific Conductance (uS/cm)" = "spcond",
    "Conductivity (uS/cm)" = "cond",
    "Turbidity, nephelometric turbidity units (NTU) (_NTU)" = "turb",
    "Water Temperature (degC)" = "temp_c"
  )
  d |>
    dplyr::mutate(Date = as.Date(Date)) |>
    dplyr::select(-LocationName) |>
    tidyr::pivot_longer(-c(Site, Date, `Depth (m)`), names_to = "variable",
                        values_to = "value", values_drop_na = TRUE) |>
    dplyr::filter(value >= 0) |>
    dplyr::mutate(var = unname(codes[variable]),
                  site = ifelse(grepl("^EK783777", Site), "Site 2", "Site 5")) |>
    dplyr::rename(depth_m = `Depth (m)`, date = Date, site_id = Site)
}

#' Daily alum dose rates Jan 2022 - Jul 2026 (PC10 Jun-26 alum workbook)
#' @return tibble: date, puarenga_l_d, utuhina_l_d, total_l_d
read_alum_dose_2026 <- function(file) {
  d <- readxl::read_excel(file, sheet = 1, col_names = FALSE, skip = 2,
                          .name_repair = "minimal")
  d <- d[, 1:7]
  names(d) <- c("date", "puarenga_l_hr", "puarenga_l_d", "utuhina_l_hr",
                "utuhina_l_d", "total_l_hr", "total_l_d")
  dt <- d$date
  d$date <- if (inherits(dt, c("POSIXct", "Date"))) as.Date(dt) else
    as.Date(suppressWarnings(as.numeric(dt)), origin = "1899-12-30")
  d |>
    dplyr::mutate(dplyr::across(-date, \(x) suppressWarnings(as.numeric(x)))) |>
    dplyr::filter(!is.na(date), !is.na(total_l_d)) |>
    dplyr::select(date, puarenga_l_d, utuhina_l_d, total_l_d)
}

#' Puarenga compliance monitoring: paired samples above (EK598179, Puarenga @ FRI)
#' and below (EK637325) the alum plant
#' @return tibble: datetime, position (upstream/downstream), site_id, ph, tp, drp, al (mg/L)
read_puarenga_compliance <- function(file) {
  d <- readxl::read_excel(file, sheet = "PUARENGA COMPLIANCE MONITORING",
                          col_names = FALSE, skip = 3, .name_repair = "minimal")
  num <- function(x) suppressWarnings(as.numeric(x))
  blk <- function(cols, position, site_id) {
    x <- d[, cols]
    tibble::tibble(
      datetime = as.POSIXct(num(x[[1]]) * 86400, origin = "1899-12-30", tz = "UTC"),
      position = position, site_id = site_id,
      ph = num(x[[2]]), tp = num(x[[3]]), drp = num(x[[4]]), al = num(x[[5]])
    )
  }
  dplyr::bind_rows(
    blk(1:5, "downstream", "EK637325"),
    blk(7:11, "upstream", "EK598179")
  ) |>
    dplyr::filter(!is.na(datetime), !is.na(tp) | !is.na(drp)) |>
    dplyr::mutate(date = as.Date(datetime))
}

#' Long-record daily alum dose in kg Al/day (Utuhina from 2006, Puarenga from 2010)
read_alum_al_kg <- function(file) {
  d <- readxl::read_excel(file, sheet = "Rotorua",
                          col_types = c("text", "numeric", "date", rep("numeric", 9)))
  pick <- function(pat) {
    nm <- grep(pat, names(d), ignore.case = TRUE, value = TRUE)[1]
    x <- d[[nm]]
    pmax(dplyr::coalesce(x, 0), 0)
  }
  tibble::tibble(
    date = as.Date(d$Date),
    utuhina_kg_al = pick("^Utuhina alumin"),
    puarenga_kg_al = pick("^Puarenga alumin")
  ) |>
    dplyr::filter(!is.na(date)) |>
    dplyr::mutate(total_kg_al = utuhina_kg_al + puarenga_kg_al)
}
