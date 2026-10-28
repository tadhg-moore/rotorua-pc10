# Climate-scenario meteorology workflow (bias-correction -> delta-change ->
# disaggregation), following metscale::vignette("scenario-workflow").
# Order matters throughout: the ERA5-to-buoy correction is an absolute-level
# adjustment and must be applied to the baseline *before* the delta-change
# step, never folded into the delta itself.

#' Read the processed hourly ERA5-Land CSV into metscale's expected format
#'
#' The file's `Date` column is ISO 8601 UTC (`...Z`). This reads it as UTC
#' and relabels the timezone attribute to `tz` (a fixed offset, e.g.
#' `"Etc/GMT-12"` for NZST) without shifting the underlying instant, so
#' downstream local-time features (day-of-year fits, diurnal disaggregation)
#' use the right wall-clock hour.
#'
#' @param file path to the main hourly file, e.g.
#'   `data/processed/rotorua_era5_hourly_1988-2024.csv`
#' @param extend_file optional second file (e.g. `rotorua_era5_hourly.csv`,
#'   2011-2025) used only for rows later than the last row of `file`; the two
#'   files are identical where they overlap. Only columns present in both are
#'   kept (the older file also has `MET_ppsnow`, which nothing uses).
#' @param lat,lon,tz attached as attributes, as `metscale` functions expect
read_era5_hourly_met <- function(file, lat, lon, tz = "Etc/GMT-12",
                                 extend_file = NULL) {
  era5 <- utils::read.csv(file, check.names = FALSE, stringsAsFactors = FALSE)
  if (!is.null(extend_file)) {
    ext <- utils::read.csv(extend_file, check.names = FALSE, stringsAsFactors = FALSE)
    ext <- ext[ext$Date > max(era5$Date), ] # ISO 8601 strings sort chronologically
    cols <- intersect(names(era5), names(ext))
    era5 <- rbind(era5[, cols], ext[, cols])
  }
  era5$Date <- as.POSIXct(era5$Date, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
  attr(era5$Date, "tzone") <- tz
  era5 <- era5[!is.na(era5$Date), ]
  # Source file stores MET_pprain in mm/hour; every other AEME MET_pprain
  # series in this pipeline (buoy, airport, PC10) is metres accumulated
  # over the row's own timestep, so convert here once rather than carrying
  # the mismatch downstream (era5_bias_correction doesn't correct pprain,
  # so an unconverted value would flow uncorrected into every scenario).
  if ("MET_pprain" %in% names(era5)) {
    era5$MET_pprain <- era5$MET_pprain / 1000
  }
  attr(era5, "tz")  <- tz
  attr(era5, "lat") <- lat
  attr(era5, "lon") <- lon
  era5
}

#' CMIP short name -> AEME MET_* name, and additive/ratio delta kind
#' Matches metscale's `.cmip6_var_table` and `vignette("scenario-workflow")`.
GCM_DELTA_KIND <- c(MET_tmpair = "add",   MET_radswd = "add",
                    MET_pprain = "ratio", MET_wndspd = "ratio",
                    MET_humrel = "ratio")

#' Monthly delta-change factors for one GCM, every scenario vs. its own
#' historical baseline, for one or more future windows
#'
#' Wraps [metscale::climate_point_monthly_climatology()] -- called once per
#' GCM (rather than pointing it at the whole multi-GCM directory) because it
#' assumes one file per variable/experiment; a shared directory would merge
#' same-named columns from different models. Change factors are additive
#' for temperature/shortwave and multiplicative (ratio) for the
#' bounded/skewed fields, matching `vignette("scenario-workflow")`.
#'
#' @param gcm single GCM name, as it appears in the CMIP6 file names
#' @param cmip6_files character vector of all cropped CMIP6 netCDF paths
#'   (across all GCMs) -- filtered internally to this `gcm`
#' @param lon,lat lake point coordinates
#' @param scenarios SSP scenario codes to compute deltas for
#' @param ref_years integer vector, historical reference window
#' @param future_windows named list of integer year-vectors, e.g.
#'   `list("2041-2070" = 2041:2070)`
#' @param vars AEME `MET_*` names to compute deltas for (must be named in
#'   [GCM_DELTA_KIND])
#'
#' @return data frame: gcm, scenario, window, variable, month, kind,
#'   hist_value, fut_value, delta -- one row per GCM/scenario/window/
#'   variable/month (empty if this GCM has no files for a scenario)
compute_gcm_monthly_deltas <- function(gcm, cmip6_files, lon, lat,
                                       scenarios, ref_years, future_windows,
                                       vars = names(GCM_DELTA_KIND)) {
  gcm_files <- cmip6_files[grepl(paste0("_", gcm, "_"), cmip6_files, fixed = FALSE)]
  if (!length(gcm_files)) {
    warning("no CMIP6 files found for GCM: ", gcm, call. = FALSE)
    return(data.frame())
  }

  hist_clim <- metscale::climate_point_monthly_climatology(
    gcm_files, lon = lon, lat = lat, vars = vars,
    experiments = "historical", years = ref_years, verbose = FALSE)

  out <- lapply(scenarios, function(scn) {
    scn_files <- gcm_files[grepl(paste0("_", scn, "_"), gcm_files)]
    if (!length(scn_files)) return(NULL)  # e.g. NZESM has no ssp585

    lapply(names(future_windows), function(win) {
      fut_clim <- metscale::climate_point_monthly_climatology(
        scn_files, lon = lon, lat = lat, vars = vars,
        experiments = scn, years = future_windows[[win]], verbose = FALSE)

      dplyr::inner_join(
        hist_clim |> dplyr::select(variable, month, hist_value = value),
        fut_clim  |> dplyr::select(variable, month, fut_value = value),
        by = c("variable", "month")
      ) |>
        dplyr::mutate(
          gcm      = gcm,
          scenario = scn,
          window   = win,
          kind     = GCM_DELTA_KIND[variable],
          delta    = dplyr::if_else(kind == "ratio", fut_value / hist_value,
                                    fut_value - hist_value)
        )
    }) |> dplyr::bind_rows()
  }) |> dplyr::bind_rows()

  out |>
    dplyr::select(gcm, scenario, window, variable, month, kind,
                  hist_value, fut_value, delta)
}

#' Apply one GCM/scenario's monthly delta-change factors to the
#' bias-corrected daily ERA5 baseline
#'
#' @param baseline the bias-corrected daily baseline (from
#'   `metscale::bias_correct_daily_baseline()`), with `MET_*` columns
#' @param deltas data frame for a single gcm/scenario/window, as returned by
#'   one row-group of [compute_gcm_monthly_deltas()] (columns `variable`,
#'   `month`, `delta`, `kind`)
#' @param lat,lon,elev,tz passed to `metscale::expand_met()` to regenerate
#'   the dependent MET_* variables after shifting the primary ones
apply_gcm_delta_to_baseline <- function(baseline, deltas, lat, lon, elev, tz) {
  mo  <- as.integer(format(baseline$Date, "%m"))
  out <- baseline
  for (v in unique(deltas$variable)) {
    dv <- deltas[deltas$variable == v, ]
    f  <- dv$delta[match(mo, dv$month)]
    # unname(): `kind` can carry names, which made identical(kind, "ratio") FALSE so every ratio-type delta
    # (rainfall, wind speed, humidity) was added to the baseline instead of multiplied
    kind <- unname(as.character(dv$kind[1]))
    out[[v]] <- if (identical(kind, "ratio")) out[[v]] * f else out[[v]] + f
  }
  .finalise_delta_met(out, lat, lon, elev, tz)
}

#' Clip to physical bounds and regenerate the dependent MET_* variables
#' after primary variables have been shifted by delta factors
.finalise_delta_met <- function(out, lat, lon, elev, tz) {
  if ("MET_humrel" %in% names(out))
    out$MET_humrel <- pmin(pmax(out$MET_humrel, 0), 100)
  for (v in intersect(c("MET_radswd", "MET_wndspd", "MET_pprain"), names(out)))
    out[[v]][is.finite(out[[v]]) & out[[v]] < 0] <- 0

  keep <- intersect(c("Date", "MET_radswd", "MET_tmpair", "MET_pprain",
                      "MET_humrel", "MET_wndspd", "MET_prsttn"), names(out))
  em <- metscale::expand_met(out[, keep], lat = lat, lon = lon, elev = elev, tz = tz)
  ## expand_met() always derives MET_wnddir/MET_wnduvu/MET_wnduvv, but with no
  ## direction in `keep` these are a fictitious constant-direction decomposition
  ## of MET_wndspd, not a real wind vector. Drop them so downstream disaggregation
  ## shapes MET_wndspd directly (as a plain scalar mean, like temperature) instead
  ## of taking metscale's wind-vector code path -- direction is not needed here
  ## and reconstructing it this way was distorting disaggregated wind speed.
  em[c("MET_wnddir", "MET_wnduvu", "MET_wnduvv")] <- NULL
  attr(em, "tz") <- tz; attr(em, "lat") <- lat; attr(em, "lon") <- lon
  em
}

# ---- Transient (trajectory) scenarios -------------------------------------
# The time-slice workflow above gives three fixed-climate snapshots per
# GCM/SSP, each on the same ERA5 weather. For long-term nutrient-loading
# questions (e.g. N reduction from 2032) the lake response is path-dependent,
# so the climate has to evolve through time alongside the loads. The
# transient workflow below builds ONE continuous series per GCM/SSP:
#   1. a year-by-year sequence of real ERA5 weather (shuffled, same for every
#      scenario so differences between runs are the climate signal only),
#   2. monthly delta factors that change every year, from a sliding window
#      along the GCM's own historical -> SSP trajectory,
#   3. the same delta -> expand_met -> disaggregate chain as the slices.

#' Year-by-year monthly delta-change factors along a GCM trajectory
#'
#' For each target year, the "future" climatology is the monthly mean over a
#' sliding window of `window_length` years centred on that year, taken from
#' the GCM's `historical` experiment up to `hist_end` and from the scenario
#' experiment after it; the delta is that against the same fixed historical
#' reference climatology used by [compute_gcm_monthly_deltas()]. Windows that
#' would run past `end_year` are truncated there rather than shifted back
#' (shifting held the last ~15 years of the trajectory flat and under-warmed
#' the end of the run by ~0.6 degC under ssp585). The final years are
#' therefore averaged over fewer years (down to ~16 at 2099; see `n_years`)
#' and still lag the underlying trend by up to ~6 years of change.
#'
#' The point series is read once per GCM with
#' [metscale::extract_climate_point()] and averaged to year x month locally,
#' rather than calling [metscale::climate_point_monthly_climatology()] once
#' per year (~100 netCDF passes per GCM/SSP).
#'
#' @param gcm single GCM name, as in the CMIP6 file names
#' @param cmip6_files all cropped CMIP6 netCDF paths (filtered to `gcm`)
#' @param lon,lat lake point
#' @param scenarios SSP codes
#' @param ref_years historical reference window (as for the slice deltas)
#' @param target_years years to produce deltas for
#' @param window_length sliding-window length, years
#' @param vars AEME `MET_*` names (must be in [GCM_DELTA_KIND])
#' @param hist_end last year of the `historical` experiment
#' @param end_year last year of the SSP experiments
#' @return data frame: gcm, scenario, year, variable, month, kind, hist_value,
#'   fut_value, delta, n_years (years actually found in the window)
compute_gcm_transient_deltas <- function(gcm, cmip6_files, lon, lat, scenarios,
                                         ref_years, target_years,
                                         window_length = 30,
                                         vars = names(GCM_DELTA_KIND),
                                         hist_end = 2014, end_year = 2100) {
  gcm_files <- cmip6_files[grepl(paste0("_", gcm, "_"), cmip6_files, fixed = FALSE)]
  if (!length(gcm_files)) {
    warning("no CMIP6 files found for GCM: ", gcm, call. = FALSE)
    return(data.frame())
  }

  pt <- metscale::extract_climate_point(
    gcm_files, lon = lon, lat = lat, vars = vars,
    experiments = c("historical", scenarios), verbose = FALSE)

  ym <- pt |>
    tidyr::pivot_longer(dplyr::all_of(vars), names_to = "variable", values_to = "value") |>
    dplyr::mutate(year  = as.integer(format(Date, "%Y")),
                  month = as.integer(format(Date, "%m"))) |>
    dplyr::group_by(experiment, variable, year, month) |>
    dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop")

  ref <- ym |>
    dplyr::filter(experiment == "historical", year %in% ref_years) |>
    dplyr::group_by(variable, month) |>
    dplyr::summarise(hist_value = mean(value, na.rm = TRUE), .groups = "drop")

  win_tbl <- lapply(target_years, function(y) {
    start <- y - window_length %/% 2 + 1
    data.frame(year_target = y, year = start:min(start + window_length - 1, end_year))
  }) |> dplyr::bind_rows()

  lapply(intersect(scenarios, unique(ym$experiment)), function(scn) {
    combo <- ym |>
      dplyr::filter((experiment == "historical" & year <= hist_end) |
                      (experiment == scn & year > hist_end))

    win_tbl |>
      dplyr::inner_join(combo, by = "year", relationship = "many-to-many") |>
      dplyr::group_by(year_target, variable, month) |>
      dplyr::summarise(fut_value = mean(value, na.rm = TRUE),
                       n_years = dplyr::n_distinct(year[!is.na(value)]), .groups = "drop") |>
      dplyr::inner_join(ref, by = c("variable", "month")) |>
      dplyr::mutate(
        gcm      = gcm,
        scenario = scn,
        year     = year_target,
        # unname(): see apply_gcm_delta_to_baseline()
        kind     = unname(GCM_DELTA_KIND[variable]),
        delta    = dplyr::if_else(kind == "ratio", fut_value / hist_value,
                                  fut_value - hist_value)
      ) |>
      dplyr::select(gcm, scenario, year, variable, month, kind,
                    hist_value, fut_value, delta, n_years)
  }) |> dplyr::bind_rows()
}

#' Calendar years that are fully present in a daily series
complete_years <- function(daily) {
  yr <- as.integer(format(daily$Date, "%Y"))
  n  <- table(yr)
  yrs <- as.integer(names(n))
  full <- ifelse((yrs %% 4 == 0 & yrs %% 100 != 0) | yrs %% 400 == 0, 366L, 365L)
  yrs[n == full]
}

#' Map each simulated year to a real ERA5 weather year
#'
#' Source years are drawn in shuffled cycles (every source year is used about
#' equally often, and the same year is not repeated at a fixed lag the way a
#' simple loop would), with leap-year targets drawn only from leap-year
#' sources so no 29 February has to be invented or dropped. The mapping
#' depends only on `seed`, so every GCM/SSP (and the no-delta control) sees
#' identical weather.
#'
#' @param target_years simulated years
#' @param source_years complete ERA5 baseline years to draw from
#' @param seed RNG seed (restored on exit; the global RNG state is untouched)
#' @return data frame: target_year, source_year
build_transient_year_map <- function(target_years, source_years, seed = 42) {
  is_leap <- function(y) (y %% 4 == 0 & y %% 100 != 0) | y %% 400 == 0
  draw <- function(pool, n) {
    if (n == 0) return(integer(0))
    if (!length(pool)) stop("no source years available for this leap-year class",
                            call. = FALSE)
    out <- integer(0)
    while (length(out) < n) out <- c(out, pool[sample.int(length(pool))])
    out[seq_len(n)]
  }
  withr::with_seed(seed, {
    leap <- is_leap(target_years)
    src  <- integer(length(target_years))
    src[leap]  <- draw(source_years[is_leap(source_years)],  sum(leap))
    src[!leap] <- draw(source_years[!is_leap(source_years)], sum(!leap))
  })
  data.frame(target_year = target_years, source_year = src)
}

#' Stitch the ERA5 daily baseline into a continuous series on target dates
#'
#' @param baseline bias-corrected daily baseline (one row per day)
#' @param year_map output of [build_transient_year_map()]
build_transient_weather <- function(baseline, year_map) {
  yr <- as.integer(format(baseline$Date, "%Y"))
  md <- format(baseline$Date, "%m-%d")
  tz <- attr(baseline$Date, "tzone")
  lapply(seq_len(nrow(year_map)), function(i) {
    sel <- yr == year_map$source_year[i]
    src <- baseline[sel, , drop = FALSE]
    new_date <- paste0(year_map$target_year[i], "-", md[sel])
    src$Date <- if (inherits(baseline$Date, "POSIXct")) {
      as.POSIXct(new_date, tz = if (is.null(tz) || !nzchar(tz)) "UTC" else tz)
    } else {
      as.Date(new_date)
    }
    src
  }) |> dplyr::bind_rows()
}

#' Apply year-varying monthly deltas to a stitched weather series
#'
#' @param weather output of [build_transient_weather()]
#' @param deltas one gcm/scenario of [compute_gcm_transient_deltas()]
#'   (columns `variable`, `year`, `month`, `delta`, `kind`); `NULL` gives the
#'   no-delta control (stationary baseline climate)
#' @inheritParams apply_gcm_delta_to_baseline
apply_transient_deltas_to_baseline <- function(weather, deltas, lat, lon, elev, tz) {
  out <- weather
  if (!is.null(deltas) && nrow(deltas)) {
    key <- paste(as.integer(format(out$Date, "%Y")), as.integer(format(out$Date, "%m")))
    for (v in unique(deltas$variable)) {
      dv <- deltas[deltas$variable == v, ]
      f  <- dv$delta[match(key, paste(dv$year, dv$month))]
      if (anyNA(f)) stop("missing transient deltas for ", v, call. = FALSE)
      kind <- unname(as.character(dv$kind[1]))
      out[[v]] <- if (identical(kind, "ratio")) out[[v]] * f else out[[v]] + f
    }
  }
  .finalise_delta_met(out, lat, lon, elev, tz)
}

#' One transient scenario's daily met: stitched ERA5 weather + trajectory deltas
#'
#' @param baseline `era5_corrected_daily_baseline`
#' @param year_map output of [build_transient_year_map()]
#' @param deltas_df all GCMs/scenarios of [compute_gcm_transient_deltas()]
#' @param gcm,scenario which trajectory; `scenario = "control"` applies no deltas
build_transient_daily_met <- function(baseline, year_map, deltas_df, gcm, scenario,
                                      lat, lon, elev, tz) {
  weather <- build_transient_weather(baseline, year_map)
  d <- if (identical(scenario, "control")) {
    NULL
  } else {
    deltas_df[deltas_df$gcm == gcm & deltas_df$scenario == scenario, ]
  }
  apply_transient_deltas_to_baseline(weather, d, lat, lon, elev, tz)
}

#' Disaggregate a scenario's daily met to hourly, wind as a plain scalar
#'
#' Strips the wind direction/vector columns from the donor so
#' `disaggregate_met_to_hourly()` shapes `MET_wndspd` directly (see
#' [.finalise_delta_met()]).
disaggregate_scenario_met <- function(daily, donor, lat, lon, elev, tz, seed = 42) {
  donor[intersect(c("MET_wnddir", "MET_wnduvu", "MET_wnduvv"), names(donor))] <- NULL
  metscale::disaggregate_met_to_hourly(
    daily, donor = donor, method = "fragments", swr = "clearsky",
    lat = lat, lon = lon, elev = elev, tz = tz, seed = seed,
    expand = TRUE, verbose = FALSE)
}
