# Run manually, locally, after tar_make() has finished computing the CMIP6
# targets (cmip6_files, tutira_cmip6_files) — this pushes the results to
# OneDrive so GitHub Actions can download them instead of recomputing
# (see _targets.R: cmip6_files / tutira_cmip6_files are conditional on
# Sys.getenv("CI") and download from OneDrive rather than running
# process_cmip6_shp() when running in CI).
#
# Re-run this any time the CMIP6 outputs change locally.

source(here::here("R", "sync_aeme_onedrive.R"))

upload_onedrive_folder(
  here::here("data", "processed", "rotorua_lakes_cmip6"),
  "rotorua-pc10/data/processed/rotorua_lakes_cmip6"
)

upload_onedrive_folder(
  here::here("data", "processed", "tutira_cmip6"),
  "rotorua-pc10/data/processed/tutira_cmip6"
)
