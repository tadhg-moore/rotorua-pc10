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

# River WQ variables that bear on the carbonate system / dissolved inorganic carbon (DIC):
# pH, alkalinity and bicarbonate/carbonate, hardness and the major ions (ionic strength and the
# alkalinity balance), conductivity, temperature (for the carbonate equilibria), DOC, oxygen and flow.
river_dic_vars <- c(
  "pH", "pH - field", "Alkt", "HCO3 diss", "CO3 diss", "Hard", "Ca diss", "Mg diss",
  "Na diss", "K diss", "Cl diss", "SO4 diss", "F diss", "Anions", "Cations", "Ion Balance",
  "SpC", "SpC - field", "Water Temp", "DOC", "DNPOC", "DO", "DO sat", "Discharge"
)

#' Simple range screening for the river WQ values (nothing is removed, only flagged)
#'
#' Returns a data frame with `qc_in_range` (TRUE = passes, FALSE = suspect) and `qc_reason`. The limits are
#' plausibility checks, not statistical outlier tests: pH < 5.5 or > 9 (potentially suspect, though acidic
#' swamp-fed streams such as the Waiohewa can be genuinely low), water temperature < 0 or > 30 degC,
#' DO < 0 or > 20 g/m^3, DO saturation < 0 or > 150 %, specific conductance <= 0 or > 1000 uS/cm,
#' alkalinity/bicarbonate <= 0, discharge < 0.
river_wq_screen <- function(variable, value) {
  lim <- function(v, lo, hi, lo_incl = TRUE) {
    low <- if (is.na(lo)) FALSE else if (lo_incl) value < lo else value <= lo
    high <- if (is.na(hi)) FALSE else value > hi
    list(bad = variable %in% v & (low | high),
         why = ifelse(low, paste0(if (lo_incl) "< " else "<= ", lo), paste0("> ", hi)))
  }
  rules <- list(
    lim(c("pH", "pH - field"), 5.5, 9),
    lim("Water Temp", 0, 30),
    lim("DO", 0, 20),
    lim("DO sat", 0, 150),
    lim("SpC", 0, 1000, lo_incl = FALSE), lim("SpC - field", 0, 1000, lo_incl = FALSE),
    lim(c("Alkt", "HCO3 diss"), 0, NA, lo_incl = FALSE),
    lim("Discharge", 0, NA)
  )
  bad <- Reduce(`|`, lapply(rules, `[[`, "bad"))
  reason <- rep(NA_character_, length(value))
  for (r in rules) reason[r$bad & is.na(reason)] <- paste(variable, r$why)[r$bad & is.na(reason)]
  data.frame(qc_in_range = !bad, qc_reason = reason)
}

#' River pH, alkalinity and other DIC-related variables in long form, named as the PC10 loads
#'
#' Reads the BOPRC river WQ workbook (via [read_river_wq()]) for `river_dic_vars` and adds `river`, the
#' stream name used in `data/raw/pc10_loads_2026` (see `pc10_inflow_key()`; Awahou, Hamurana, Ngongotaha,
#' Puarenga, Utuhina, Waingaehe, Waiohewa, Waiowhiro, Waiteti). The lake outlet (Ohau Channel at SH33) is
#' "Ohau outflow", matching the `outflow_ohau` file. Some rivers have two sampling sites (`site_id`).
#' `qc_in_range` and `qc_reason` come from [river_wq_screen()]; all values are kept.
#'
#' @return tibble: river, site_id, location, datetime (NZST), variable, unit, value, qc_in_range, qc_reason
read_river_dic_wq <- function(file, keep = river_dic_vars) {
  key <- river_wq_inflow_key()
  d <- read_river_wq(file, keep = keep) |>
    dplyr::left_join(dplyr::select(key, location, river = inflow), by = "location")
  stopifnot(!anyNA(d$river))
  d |>
    dplyr::bind_cols(river_wq_screen(d$variable, d$value)) |>
    dplyr::select(river, site_id, location, datetime, variable, unit, value, qc_in_range, qc_reason) |>
    dplyr::arrange(datetime, river, site_id, variable)
}

#' Correct lake TP for silica/arsenic interference in the 2010-2019 BOPRC method
#'
#' Formula and period from the "Metat data" sheet of "Rotorua S2 & S5 data 1989_2026.xlsx"
#' (University of Waikato correction from duplicate P and ion analyses; see also Hamill 2022):
#'   TP_corr = -3.29420378427001 + C + 0.976716336192513 * TP_old  (mg/m^3), C = 1.82648605256361 for Lake Rotorua,
#' applied from 20 August 2010 (start of analysis at the BOPRC lab) to September 2018 inclusive, plus July to
#' September 2019 (gap between the end of the trial and adoption of the new method). Trial (Oct 2018 - Jun 2019) and
#' new-method (from Oct 2019) results are not adjusted.
#' BOPRC supplies values already corrected for July 2015 onwards (Aquarius era) but not for Aug 2010 - Jun 2015
#' (transposed from Lakewatch). Already-corrected values are recognised because they back-transform exactly to whole
#' mg/m^3 (supplied TP is otherwise almost always whole mg/m^3), so BOPRC-corrected values are not corrected again.
#' Apply it only to TP as supplied: a few supplied values are not whole numbers, so a second pass would change them.
#'
#' @param date sample dates; @param tp TP as supplied (mg/m^3)
#' @return list(tp = corrected TP, status = "corrected here" / "corrected by BOPRC" / "not in correction period")
correct_lake_tp <- function(date, tp) {
  a <- 0.976716336192513; b <- -3.29420378427001 + 1.82648605256361
  in_window <- (date >= as.Date("2010-08-20") & date <= as.Date("2018-09-30")) |
    (date >= as.Date("2019-07-01") & date <= as.Date("2019-09-30"))
  back <- (tp - b) / a
  already <- is.finite(tp) & abs(back - round(back)) < 1e-4 & abs(tp - round(tp)) > 1e-6
  fix <- in_window & is.finite(tp) & !already
  status <- ifelse(!is.finite(tp), NA_character_,
                   ifelse(fix, "corrected here", ifelse(in_window, "corrected by BOPRC", "not in correction period")))
  list(tp = ifelse(fix, b + a * tp, tp), status = status)
}

#' Lake chemistry from the PC10 Jun-26 "Rotorua S2 & S5 data 1989_2026.xlsx"
#'
#' Reads both site sheets, trims the padded headers, and returns one row per
#' sample with snake-case names (units mg/m^3 except pH, NTU, Secchi m).
#' `layer` is INT (integrated epilimnion), HYP (discrete hypolimnion) or BOT.
#' TP is corrected consistently for the 2010-2019 method interference (`correct_lake_tp()`);
#' `tp_supplied` keeps the value as supplied and `tp_correction` says what was done.
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
    dplyr::mutate(tp_supplied = tp,
                  tp_correction = correct_lake_tp(date, tp)$status,
                  tp = correct_lake_tp(date, tp)$tp) |>
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
