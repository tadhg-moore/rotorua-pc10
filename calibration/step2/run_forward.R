# One forward simulation with the best step 2 / 2b parameter set over a 20-year window, with the GLM mass balance
# written, so the N / P budgets can be inspected.
#   Rscript calibration/step2/run_forward.R
# Environment (all optional):
#   FWD_STAGE  "b" (default, step 2b best member), "c" (step 2c) or "a" (step 2 best member)
#   FWD_START / FWD_STOP  default 1990-01-01 / 2009-12-31 (20 years; the ERA5 forcing runs to 2025)
#   FWD_ALUM   "0" (default, no alum: the pre-alum set-up the nutrients were calibrated under) or "1" (alum dosing on)
#   FWD_SPINUP spin-up days, default 540
# Output: calibration/step2/forward/<stage>/  (aeme object, paired obs/sim, mass balance tables, budget plots)
stage <- Sys.getenv("FWD_STAGE", "b")
start <- Sys.getenv("FWD_START", "1990-01-01"); stop <- Sys.getenv("FWD_STOP", "2009-12-31")
alum  <- identical(Sys.getenv("FWD_ALUM", "0"), "1")
src   <- Sys.getenv("FWD_SRC", unset = NA); if (is.na(src)) src <- switch(stage, b = file.path("calibration", "step2b", "runs"), c = file.path("calibration", "step2c", "runs"), d = file.path("calibration", "step2d", "runs"), e = file.path("calibration", "step2e", "runs"), l = file.path("calibration", "step2l", "runs"), f = file.path("calibration", "step2f", "runs"), g = file.path("calibration", "step2g", "runs"), o = file.path("calibration", "step2o", "runs"), alum = file.path("calibration", "step2alum", "runs"), p = file.path("calibration", "step2p", "runs"), file.path("calibration", "step2", "runs"))
out   <- file.path("calibration", "step2", "forward", paste0(stage, Sys.getenv("FWD_TAG", ""))); dir.create(out, recursive = TRUE, showWarnings = FALSE)   # FWD_TAG keeps a long run apart from the 20-year one

# step2_common.R sets SA_* from the environment before sourcing sa_common.R (and sets the stage-b parameters/scales)
Sys.setenv(S2_STAGE = if (nzchar(Sys.getenv("FWD_S2STAGE"))) Sys.getenv("FWD_S2STAGE") else if (stage %in% c("b", "c", "d", "e", "l", "f", "g", "o", "alum", "p")) stage else "", S2_DIR = out)
source("calibration/step2/step2_common.R")
SA_START <- start; SA_STOP <- stop                      # sa_build() reads these globals when it calls set_time()
if (alum) SA_VARIANT <- "alum"

b <- step2_build("fwd")
best <- readRDS(file.path(src, "step2_best.rds"))$best
cat("parameters:", nrow(best), "| window", start, "to", stop, "| alum", alum, "| stage", stage, "\n")

# switch the GLM mass balance on for TN, TP and TSS (GLM v4 &mass_balance block); skipped if the build lacks it
a <- tryCatch(AEME::set_glm_balance_groups(b$aeme, groups = c("TN", "TP", "TSS"), path = b$path),
              error = function(e) { message("mass balance not set: ", conditionMessage(e)); b$aeme })

res <- aemetools::run_aeme_param(a, param = best[, PCOLS], model = MODEL, path = b$path, return_aeme = TRUE)
stopifnot(inherits(res, "Aeme"))
saveRDS(res, file.path(out, "forward_aeme.rds"))

# observed vs simulated pairs (surface samples are scored in step 2; all depths kept here), with the refractory pools
# added to the simulated totals exactly as in the calibration
P <- csps_pair(res, vars = c("PHY_tchla", "PHS_tp", "PHS_frp", "NIT_tn", "NIT_nit", "NIT_amm", "HYD_temp", "CHM_oxy"))
if (STEP2_FIX_TOTALS) {
  if (!is.null(P$NIT_tn)) P$NIT_tn$sim <- P$NIT_tn$sim + DONR_GM3
  if (!is.null(P$PHS_tp)) P$PHS_tp$sim <- P$PHS_tp$sim + DOPR_GM3
}
saveRDS(P, file.path(out, "forward_pairs.rds"))
tab <- do.call(rbind, lapply(names(P), function(v) { d <- P[[v]]; d <- d[d$depth <= 5, ]
  data.frame(var = v, n = nrow(d), obs_mean = mean(d$obs), sim_mean = mean(d$sim), bias = mean(d$sim - d$obs),
             rmse = sqrt(mean((d$sim - d$obs)^2)), r = suppressWarnings(cor(d$obs, d$sim))) }))
print(tab, digits = 3); write.csv(tab, file.path(out, "forward_surface_fit.csv"), row.names = FALSE)

# mass balance: totals (TN / TP / TSS) and per state variable
for (by in c("total", "var")) {
  d <- tryCatch(AEME::diag_glm_mass_balance(aeme = res, path = b$path, by = by, units = "kg"), error = function(e) { message("mass balance (", by, "): ", conditionMessage(e)); NULL })
  if (!is.null(d)) { saveRDS(d, file.path(out, paste0("mass_balance_", by, ".rds"))); cat("\nmass balance by", by, ":\n"); print(utils::head(if (!is.null(d$summary)) d$summary else d, 20)) }
}
nb <- tryCatch(AEME::diag_nutrient_budget(res, model = MODEL), error = function(e) NULL)
if (!is.null(nb)) { cat("\nnutrient bias summary:\n"); print(nb); write.csv(nb, file.path(out, "nutrient_bias.csv"), row.names = FALSE) }
cat("\nDone. Outputs in", out, "\n")
