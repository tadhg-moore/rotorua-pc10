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
#' @param file path to `data/processed/rotorua_era5_hourly.csv`
#' @param lat,lon,tz attached as attributes, as `metscale` functions expect
read_era5_hourly_met <- function(file, lat, lon, tz = "Etc/GMT-12") {
  era5 <- utils::read.csv(file, check.names = FALSE, stringsAsFactors = FALSE)
  era5$Date <- as.POSIXct(era5$Date, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
  attr(era5$Date, "tzone") <- tz
  era5 <- era5[!is.na(era5$Date), ]
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
    kind <- dv$kind[1]
    out[[v]] <- if (identical(kind, "ratio")) out[[v]] * f else out[[v]] + f
  }
  if ("MET_humrel" %in% names(out))
    out$MET_humrel <- pmin(pmax(out$MET_humrel, 0), 100)
  for (v in intersect(c("MET_radswd", "MET_wndspd", "MET_pprain"), names(out)))
    out[[v]][is.finite(out[[v]]) & out[[v]] < 0] <- 0

  keep <- intersect(c("Date", "MET_radswd", "MET_tmpair", "MET_pprain",
                      "MET_humrel", "MET_wndspd", "MET_prsttn"), names(out))
  em <- metscale::expand_met(out[, keep], lat = lat, lon = lon, elev = elev, tz = tz)
  attr(em, "tz") <- tz; attr(em, "lat") <- lat; attr(em, "lon") <- lon
  em
}
