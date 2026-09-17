# Run manually, locally, after tar_make() has finished computing the CMIP6
# targets (cmip6_files, tutira_cmip6_files, gcm_ts_summary/gcm_ts_df) — this
# pushes the results to OneDrive so GitHub Actions can download them instead
# of recomputing (see _targets.R: these targets are conditional on
# Sys.getenv("CI") and download from OneDrive rather than running the real
# computation when running in CI).
#
# Re-run this any time the CMIP6 outputs or gcm_ts_df change locally.

source(here::here("R", "sync_aeme_onedrive.R"))

upload_onedrive_folder(
  here::here("data", "processed", "rotorua_lakes_cmip6"),
  "rotorua-pc10/data/processed/rotorua_lakes_cmip6"
)

upload_onedrive_folder(
  here::here("data", "processed", "tutira_cmip6"),
  "rotorua-pc10/data/processed/tutira_cmip6"
)

# gcm_ts_summary (>1 day to compute — summarise_gcm_ts() over every
# variable x GCM combination) has no on-disk output of its own; export its
# downstream gcm_ts_df to a file so it can be synced the same way.
gcm_ts_df_file <- here::here("data", "processed", "gcm_ts_df.rds")
saveRDS(targets::tar_read(gcm_ts_df), gcm_ts_df_file)
upload_onedrive_file(
  gcm_ts_df_file,
  "rotorua-pc10/data/processed/gcm_ts_df.rds"
)
