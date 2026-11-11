# Run the latest baseline locally from a "latest" folder (the OneDrive layout in R/latest_helpers.R) and write the parquet output back into it.
#   Rscript R/run_latest.R [latest_dir]      default: onedrive_mirror/rotorua-pc10/latest   (a folder synced from OneDrive works the same way)
# Steps: copy setup/ into a run folder, apply parameters.csv, run GLM with latest/exe/glm.exe, write output/ (parquet) and drop output.nc (about 7 GB).
# Env: RUN_KEEP_NC=1 keeps output.nc in the run folder.
.libPaths(c(.libPaths(), "C:/Users/mooret/AppData/Local/R/win-library/4.5"))
suppressPackageStartupMessages(library(AEME)); source("R/latest_helpers.R")
args <- commandArgs(TRUE); latest <- normalizePath(if (length(args)) args[1] else "onedrive_mirror/rotorua-pc10/latest", mustWork = TRUE)
work <- normalizePath(Sys.getenv("RUN_WORK", "calibration/all_stage/work_latest"), mustWork = FALSE); lake_dir <- file.path(work, "LID11133_rotorua", "glm_aed")
exe <- file.path(latest, "exe", "glm.exe"); stopifnot(file.exists(exe), dir.exists(file.path(latest, "setup")))
options(AEME.glm_exec = exe)

unlink(work, recursive = TRUE); dir.create(file.path(lake_dir, "output"), recursive = TRUE)
file.copy(file.path(latest, "setup", c("aed", "bcs", "glm4.nml", "plots.nml")), lake_dir, recursive = TRUE)
aeme <- readRDS(file.path(latest, "aeme.rds")); param <- read.csv(file.path(latest, "parameters.csv"), check.names = FALSE, stringsAsFactors = FALSE)
cat("parameters:", nrow(param), "rows;", format(AEME::time(aeme)$start), "to", format(AEME::time(aeme)$stop), "\n")

# Parameters are applied to the copied set-up and GLM is run directly: AEME::run_aeme() would rewrite bcs/meteo_glm.csv from the Aeme object,
# and the calibrated meteorology (rain conversion) lives in setup/bcs.
t0 <- Sys.time()
AEME::input_model_parameters(aeme = aeme, model = "glm_aed", param = AEME::expand_zone_ratios(param), path = work, derive_totals = TRUE)
# input_model_parameters() regenerates the bcs files from the Aeme object (meteo lacks the calibration build's rain conversion): restore the set-up's bcs
file.copy(list.files(file.path(latest, "setup", "bcs"), full.names = TRUE), file.path(lake_dir, "bcs"), overwrite = TRUE)
r <- processx::run(exe, "glm4.nml", wd = lake_dir, error_on_status = FALSE); stopifnot(r$status == 0)
nc <- file.path(lake_dir, "output", "output.nc"); stopifnot(file.exists(nc)); cat("model run", format(round(difftime(Sys.time(), t0, units = "mins"), 1)), "
")
res <- aeme

write_run_parquet(res, nc, path = work, out_dir = file.path(latest, "output", "model"), from = AEME::time(aeme)$start)
arrow::write_parquet(observations_table(aeme), file.path(latest, "output", "observations.parquet"), compression = "gzip")
writeLines(jsonlite::toJSON(list(run = format(Sys.time(), "%Y-%m-%d %H:%M %Z"), run_by = Sys.info()[["user"]], minutes = round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1)), auto_unbox = TRUE, pretty = TRUE), file.path(latest, "output", "run_info.json"))
make_setup_zip(latest, file.path(latest, "output", "glm_aed_setup.zip"))   # downloadable set-up shown at the top of the page
if (!nzchar(Sys.getenv("RUN_KEEP_NC"))) unlink(nc)
f <- list.files(file.path(latest, "output"), recursive = TRUE, full.names = TRUE); cat(length(f), "output files,", round(sum(file.size(f)) / 1e6), "MB\n")
