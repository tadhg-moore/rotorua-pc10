# Alum efficiency sensitivity: scale k_w and k_s (the rates multiplied by the pH modifier fpH in aed_alum.F90) by a common factor and compare
# surface TP / FRP / chlorophyll and bottom DO against observations, 2005-2022, alum ON, O member. Factors follow Sanila (2026): fpH ~0.4-0.6 in the
# dosing zone (pH 5-6, pH_opt 7, sigma 1), ~0.6-1 at ambient lake pH 7-8; field Al:P ~7:1 vs jar test 0.7:1.
Sys.setenv(S2_STAGE = "o", S2_DIR = file.path("calibration", "alum", "eff_runs"), S2_ALUM = "1", S2_BASE = "calibration/step3/aeme_base_2005_alum.rds",
           S2_ALUM_FROM = "calibration/step2alum/runs/step2_best.rds", S2_START = "2005-01-01", S2_STOP = "2022-06-29")
source("calibration/step2/step2_common.R")
best <- readRDS("calibration/step2o/runs/step2_best.rds")$best; b <- step2_build("eff")
scales <- c(1, 0.7, 0.5, 0.3, 0.1)
P <- lapply(scales, function(s) { cat("scale", s, "\n"); flush.console()
  p <- best; i <- p$name %in% c("aed_alum/k_w", "aed_alum/k_s"); p$value[i] <- p$value[i] * s
  res <- aemetools::run_aeme_param(b$aeme, param = p[, PCOLS], model = MODEL, path = b$path, return_aeme = TRUE); if (!inherits(res, "Aeme")) return(NULL)
  csps_pair(res, vars = c("CHM_oxy", "PHS_tp", "PHS_frp", "PHY_tchla")) })
names(P) <- scales; saveRDS(P, "calibration/alum/efficiency_sensitivity_pairs.rds"); unlink(file.path("calibration", "alum", "eff_runs"), recursive = TRUE); cat("done\n")
