# One forward run of a lake with its posterior BEST parameters (as the ensemble runner would), reporting bias vs
# observations for key variables - separates "ensemble/posterior problem" from "calibration problem".
#   Rscript R/pest_workflow/test_forward_best.R <LAKE_TAG>      (env PEST_WF_ROOT etc.)
suppressMessages({ library(targets); library(AEME); library(aemetools); library(dplyr) })
source("R/pest_workflow/manifest.R"); source("R/pest_workflow/setup.R"); source("R/pest_workflow/pest_extract.R")
source("R/set_lake_inflows.R"); source("R/lspc_to_aeme.R"); source("R/add_block_spinup.R")
source("R/pest_workflow/run_ensemble.R"); source("R/pest_workflow/sanity_checks.R")
tag <- commandArgs(trailingOnly = TRUE)[1]
ROOT <<- file.path(LOCAL_ROOT, "_builds", paste0("fwd_", tag)); dir.create(ROOT, recursive = TRUE, showWarnings = FALSE)
aeme0 <<- build_ensemble_aeme0(tag, "baseline")
pp <- read_posterior_ptabs(tag)
ptab <- pp$best[, intersect(PARAM_COLS, names(pp$best))]; for (cc in setdiff(PARAM_COLS, names(ptab))) ptab[[cc]] <- NA
ptab <- ptab[, PARAM_COLS]
b <- fresh_build("fwd", VARS_SIM, use_bgc = TRUE, switches = SWITCHES, n_zones = n_glm_zones)
a <- run_aeme_param(aeme = b$aeme, param = ptab, model = MODEL, path = b$path, return_aeme = TRUE)
stopifnot(inherits(a, "Aeme"))
ms <- member_series(a, "best", c("NCS_ss1", "HYD_temp", "LKE_lvlwtr", "PHY_tchla", "CHM_oxy"))
st <- targets::tar_read(simulation_time); tr <- function(d) d[d$Date >= as.Date(st$start) & d$Date <= as.Date(st$stop), ]
out <- tr(ms$pairs) |> summarise(n = n(), obs_mean = mean(obs), sim_mean_at_obs = mean(sim), bias = mean(sim - obs), .by = c(variable, depth_class))
print(as.data.frame(out), digits = 4)
unlink(ROOT, recursive = TRUE, force = TRUE)
