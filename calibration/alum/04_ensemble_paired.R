# Re-run the alum-parameter ensemble members (alum ON, 2005-2022) keeping the observation-paired series (get_var use_obs = TRUE), so that the bottom DO is
# compared at the observed depths exactly as in calibration/anoxia/01_bottom_anoxia.R (the first ensemble script averaged all model layers >= 17 m,
# a different definition from the observed sensor depths).
Sys.setenv(S2_STAGE = "o", S2_DIR = file.path("calibration", "alum", "ens_runs"), S2_ALUM = "1", S2_BASE = "calibration/step3/aeme_base_2005_alum.rds",
           S2_ALUM_FROM = "calibration/step2alum/runs/step2_best.rds", S2_START = "2005-01-01", S2_STOP = "2022-06-29")
source("calibration/step2/step2_common.R")
members <- readRDS("calibration/posterior/alum_members.rds"); b <- step2_build("pair")
P <- lapply(seq_along(members), function(m) { cat("member", m, "\n"); flush.console()
  res <- aemetools::run_aeme_param(b$aeme, param = members[[m]][, PCOLS], model = MODEL, path = b$path, return_aeme = TRUE); if (!inherits(res, "Aeme")) return(NULL)
  csps_pair(res, vars = c("CHM_oxy", "PHS_tp", "PHS_frp", "PHY_tchla")) })
saveRDS(P, "calibration/posterior/alum_ensemble_pairs.rds"); cat("done\n")
