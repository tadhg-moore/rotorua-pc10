# Run the step 2 base (1990-2006, alum off) with the step 1 parameter set and score the nutrient / chlorophyll record.
#   Rscript calibration/step2/run_step2_baseline.R
Sys.setenv(SA_VARIANT = "noalum", SA_BASE = "calibration/step2/aeme_base_1990_v2.rds", SA_START = "1990-01-01",
           SA_STOP = "2006-05-31", SA_SPINUP = "540", SA_DIR_OVERRIDE = file.path("calibration", "step2", "runs"))
source("calibration/sa/sa_common.R")
out_dir <- SA_DIR
b <- sa_build("sa_base"); saveRDS(b$aeme, file.path(out_dir, "sa_base_aeme.rds"))
best <- readRDS("calibration/checkpoints/step1_final_best.rds")$best
res <- aemetools::run_aeme_param(b$aeme, param = best[, PCOLS], model = MODEL, path = b$path, return_aeme = TRUE)
vars <- c("PHY_tchla", "PHS_tp", "PHS_frp", "NIT_tn", "NIT_nit", "NIT_amm", "RAD_secchi")
P <- csps_pair(res, vars = vars); saveRDS(P, file.path(out_dir, "step2_baseline_pairs.rds"))
tab <- do.call(rbind, lapply(names(P), function(v) { d <- P[[v]]; d <- d[d$depth <= 5, ]
  data.frame(var = v, n = nrow(d), obs_mean = mean(d$obs), sim_mean = mean(d$sim), bias = mean(d$sim - d$obs),
             rmse = sqrt(mean((d$sim - d$obs)^2)), r = suppressWarnings(cor(d$obs, d$sim))) }))
print(tab, digits = 3)
for (v in c("PHY_tchla", "PHS_tp")) { d <- P[[v]]; d <- d[d$depth <= 5, ]
  cat("\n", v, "annual mean obs / sim\n"); print(round(aggregate(cbind(obs, sim) ~ yr, d, mean), 1), row.names = FALSE) }
