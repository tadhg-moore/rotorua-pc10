# Push website/images and website/data to OneDrive (rotorua-pc10/website_assets) so the
# {targets} pipeline can sync them for the site build. Run after regenerating report
# outputs (e.g. calibration/07_export_report_data.R), from the repository root:
#   Rscript website/scripts/push_assets.R
# Uses the locally synced OneDrive folder; override with ROTORUA_ONEDRIVE.
od <- Sys.getenv("ROTORUA_ONEDRIVE",
                 "C:/Users/mooret/OneDrive - The University of Waikato/rotorua-pc10")
dest <- file.path(od, "website_assets")
stopifnot(dir.exists(od))
for (d in c("images", "data")) {
  from <- file.path("website", d)
  to <- file.path(dest, d)
  dir.create(to, recursive = TRUE, showWarnings = FALSE)
  # data/processed is produced by the pipeline itself, not an asset
  files <- list.files(from, recursive = TRUE, all.files = FALSE)
  if (d == "data") files <- files[!startsWith(files, "processed/")]
  for (f in files) {
    dir.create(dirname(file.path(to, f)), recursive = TRUE, showWarnings = FALSE)
    if (!file.exists(file.path(to, f)) ||
        file.mtime(file.path(from, f)) > file.mtime(file.path(to, f)))
      file.copy(file.path(from, f), file.path(to, f), overwrite = TRUE)
  }
  message(d, ": ", length(files), " files checked")
}
