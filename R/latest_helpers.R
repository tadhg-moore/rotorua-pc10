# Helpers for the "latest baseline run" workflow (see R/run_latest.R, calibration/all_stage/export_latest.R, website/rotorua-glm-latest-run.qmd).
#
# OneDrive layout (rotorua-pc10/latest/):
#   setup/            GLM-AED namelist directory of the calibrated baseline (glm4.nml, plots.nml, aed/, bcs/)
#   aeme.rds          Aeme object with the inputs and all observations (baseline, no scenarios)
#   parameters.csv    calibrated parameters in AEME format (applied on top of setup/ by run_latest.R)
#   exe/              GLM-AED executable and DLLs (+ BUILD_INFO.txt)
#   metadata.json     what the setup is (parameter source, exporter, git commit, period, executable)
#   output/           parquet output of the last run (written by run_latest.R)

#' Write the full output of a run to parquet.
#'
#' `aeme` must already hold the run's output (see `AEME::load_output()`), `nc` is the GLM output.nc and `path` the folder holding
#' `<lake dir>/glm_aed/`. Writes into `out_dir`:
#'   output_raw/        every core variable at every depth, daily (AEME::write_aeme_parquet; values rounded to 4 decimals)
#'   sediment.parquet   date, zone, var_sim, value: sediment-zone variables (sediment-water fluxes SDF_Fsed_*, alum ALU_*, ...); zone NA = lake-wide
#'   alum_water.parquet date, depth, var_sim, value: water-column alum variables (too large for AEME to load)
write_run_parquet <- function(aeme, nc, path, out_dir, model = "glm_aed", from = NULL) {
  stopifnot(requireNamespace("arrow", quietly = TRUE))
  unlink(out_dir, recursive = TRUE); dir.create(out_dir, recursive = TRUE)
  aeme <- AEME::load_output(aeme, model = model, path = path, extra_vars = "sediment", zone_summary = TRUE)
  AEME::write_aeme_parquet(aeme, out_dir, model = model)

  sg <- AEME::output(aeme)$ens_001[[model]]$sediment
  zdate <- sg$SDF_Fsed_frp_Z$dim_values$time
  tg <- lapply(names(sg), function(v) {
    x <- sg[[v]]
    if (inherits(x, "aeme_grouped_var")) {
      dt <- x$dim_values$time
      data.frame(date = rep(dt, each = nrow(x$value)), zone = rep(x$dim_values$nzones, times = length(dt)), var_sim = v, value = signif(as.vector(x$value), 6))
    } else data.frame(date = zdate, zone = NA_integer_, var_sim = v, value = signif(as.numeric(x), 6))
  })
  arrow::write_parquet(dplyr::bind_rows(tg), file.path(out_dir, "sediment.parquet"), compression = "gzip")

  ncf <- ncdf4::nc_open(nc); on.exit(ncdf4::nc_close(ncf), add = TRUE)
  z <- ncdf4::ncvar_get(ncf, "z"); ns <- ncdf4::ncvar_get(ncf, "NS"); tm <- ncdf4::ncvar_get(ncf, "time"); t0n <- ncdf4::ncatt_get(ncf, "time", "units")$value
  orig <- as.Date(strsplit(t0n, " ")[[1]][3]); d <- if (grepl("hour", t0n)) orig + floor(tm / 24) else orig + tm
  keep <- if (is.null(from)) seq_along(d) else which(d >= as.Date(from))
  al <- lapply(intersect(c("ALU_ala", "ALU_alp", "ALU_uptake_w", "ALU_fpH", "ALU_age_w"), names(ncf$var)), function(v) {
    a <- ncdf4::ncvar_get(ncf, v)
    lapply(keep, function(i) { k <- seq_len(ns[i]); h <- z[k, i]; data.frame(date = d[i], depth = round(max(h) - h, 3), var_sim = v, value = signif(a[k, i], 6)) }) |> dplyr::bind_rows()
  }) |> dplyr::bind_rows()
  arrow::write_parquet(al, file.path(out_dir, "alum_water.parquet"), compression = "gzip")
  invisible(out_dir)
}

#' Observations of an Aeme object as one long table (date, var_aeme, depth, value) for parquet / plotting.
observations_table <- function(aeme) {
  o <- AEME::observations(aeme)
  lk <- o$lake[, c("Date", "var_aeme", "depth", "value")]
  lv <- o$level; lv$depth <- NA_real_
  out <- rbind(lk, lv[, c("Date", "var_aeme", "depth", "value")]); out$date <- as.Date(out$Date); out$Date <- NULL
  out[, c("date", "var_aeme", "depth", "value")]
}

#' Zip the GLM-AED set-up (namelists, AED files, all boundary files in bcs/) with parameters.csv and a README, for download.
#' Written to `zip_file`; the executable is not included (it is in latest/exe on OneDrive).
make_setup_zip <- function(latest, zip_file) {
  stopifnot(requireNamespace("zip", quietly = TRUE))
  meta <- jsonlite::fromJSON(file.path(latest, "metadata.json")); stage <- file.path(tempdir(), "glm_aed_setup"); unlink(stage, recursive = TRUE); dir.create(stage)
  file.copy(list.files(file.path(latest, "setup"), full.names = TRUE), stage, recursive = TRUE); file.copy(file.path(latest, "parameters.csv"), stage)
  writeLines(c("Lake Rotorua GLM-AED baseline set-up (calibrated, alum on)", "",
    paste0("Exported: ", meta$exported, " by ", meta$exported_by, " (git ", meta$git_commit, ")"),
    paste0("Simulation: ", meta$simulation$start, " to ", meta$simulation$stop, " (", meta$simulation$spin_up_days, "-day spin-up; output used from ", meta$simulation$output_from, ")"),
    paste0("Forcing: ", meta$forcing), paste0("Executable: ", paste(meta$executable[1:2], collapse = "; ")), "",
    "Contents: glm4.nml, plots.nml, aed/ (AED namelist and parameter csv files), bcs/ (meteo_glm.csv, inflow_*.csv, outflow_wbal.csv),",
    "parameters.csv (calibrated parameters in AEME format, already applied in the namelists/csv files here).",
    "Run: put the GLM-AED executable (alum build, see above) next to glm4.nml and run `glm.exe glm4.nml`; output goes to output/.",
    "Inflow concentrations are in GLM-AED units (mmol/m3), flows in m3/s."), file.path(stage, "README.txt"))
  zip_file <- file.path(normalizePath(dirname(zip_file), mustWork = TRUE), basename(zip_file)); unlink(zip_file); zip::zip(zip_file, files = list.files(stage, recursive = FALSE), root = stage, mode = "cherry-pick"); invisible(zip_file)
}

#' Get the downloadable set-up zip of the latest baseline into `dest`: from OneDrive (rotorua-pc10/latest/output) when a token is set, else from the local mirror.
#' Needs R/sync_aeme_onedrive.R sourced when the OneDrive branch is used. Returns `dest`.
sync_latest_setup_zip <- function(dest, mirror = here::here("onedrive_mirror", "rotorua-pc10", "latest", "output", "glm_aed_setup.zip")) {
  if (nzchar(Sys.getenv("ONEDRIVE_TOKEN_ENCRYPTED"))) return(sync_onedrive_file("rotorua-pc10/latest/output/glm_aed_setup.zip", dest))
  stopifnot(file.exists(mirror)); dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE); file.copy(mirror, dest, overwrite = TRUE); dest
}
