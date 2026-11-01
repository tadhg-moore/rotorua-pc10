# Runner: sources scripts/pest_calib_v6.R with timestamped logging and a
# traceback on failure (same pattern as run_pest_calib_v4.R). Run from the
# PROJECT ROOT. Per-lake selection comes from env vars read by the hooks in
# pest_calib_v6.R:
#   PEST_LAKE_IDX, PEST_DOWNSTREAM_LIDS, PEST_PIN_SEDFLUX_LAKES
#
# Each PEST agent starts its own R session; skip the renv sandbox activation
# stall (see run_pest_calib_v4.R).
Sys.setenv(RENV_CONFIG_SANDBOX_ENABLED = "FALSE")

options(warn = 1, error = function() {
  cat("\n##### ERROR at ", format(Sys.time()), " #####\n", sep = "")
  cat(geterrmessage(), "\n")
  cat("--- traceback ---\n")
  tb <- sys.calls()
  for (k in seq_along(tb)) {
    ln <- paste(deparse(tb[[k]]), collapse = " ")
    cat(sprintf("%3d: %s\n", k, substr(ln, 1, 300)))
  }
  cat("##### END ERROR #####\n")
  q(status = 1L)
})
cat("### start", format(Sys.time()), "\n")
source("scripts/pest_calib_v6.R", echo = FALSE)
cat("### done", format(Sys.time()), "\n")
