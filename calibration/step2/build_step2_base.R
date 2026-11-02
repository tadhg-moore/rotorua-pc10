# Step 2 base model: alum-off Lake Rotorua GLM-AED over the pre-alum record, driven by the corrected hourly
# ERA5 series (era5_corrected_hourly, 1988-2025). Alum dosing starts 2006-06-15, so the window stops 2006-05-31.
#   Rscript calibration/step2/build_step2_base.R
suppressPackageStartupMessages({library(targets); library(dplyr)})
suppressPackageStartupMessages({library(AEME)}); source("R/build_calib_base_aeme.R")
tar_load(c(hyps_no_hole, era5_corrected_hourly, rotorua_inflow_list_matrix, zone_heights, weir_params,
           aed_models_alum, model, ext_elev, old_buoy_wq))
S2_START <- Sys.getenv("S2_START", "1990-01-01"); S2_STOP <- Sys.getenv("S2_STOP", "2006-05-31")
infl <- rotorua_inflow_list_matrix[[1]]
# no alum anywhere in this window
for (n in names(infl)) { infl[[n]]$ALU_ala <- 0; infl[[n]]$ALU_alp <- 0 }
out <- build_calib_base_aeme(
  aeme_download_file = "LID11133_rotorua/aeme_download.rds", hyps_no_hole = hyps_no_hole,
  era5_corrected_hourly = era5_corrected_hourly, inflows = infl, zone_heights = zone_heights,
  weir_params = weir_params, aed_models_alum = aed_models_alum, model = model, ext_elev = ext_elev,
  alum_nml_file = "website/LakeRotorua/aed/aed_original.nml",
  out_file = file.path("calibration", "step2", paste0("aeme_base_1990", Sys.getenv("S2_TAG"), ".rds")),
  build_path = file.path("calibration", "step2", paste0("build", Sys.getenv("S2_TAG"))), old_buoy_wq = old_buoy_wq,
  sim_start = S2_START, sim_stop = S2_STOP, spin_up_days = 540)
cat("built", out, "\n")
