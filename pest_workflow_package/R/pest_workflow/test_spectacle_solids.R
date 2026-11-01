# Why are Spectacle's modelled solids ~4,900 mg/L? One forward run per hypothesis (posterior best parameters):
#   A control   B epsilon = 0 (resuspension off)   C w_ss[1] = 2.0 (table max)   D = B + C
#   E seepage_rate = 0 (no water loss without solids)   F inflow solids = 0   G inflow solids / 100
# Reports bias vs observations, the yearly mean surface solids (accumulation shows as a steady rise) and the
# solids mass balance (inflow load vs what the lake holds).
#   Rototoa-style env: PEST_WF_ROOT etc.; Rscript R/pest_workflow/test_spectacle_solids.R <A|B|C|D|E>
suppressMessages({ library(targets); library(AEME); library(aemetools); library(dplyr) })
source("R/pest_workflow/manifest.R"); source("R/pest_workflow/setup.R"); source("R/pest_workflow/pest_extract.R")
source("R/set_lake_inflows.R"); source("R/lspc_to_aeme.R"); source("R/add_block_spinup.R")
source("R/pest_workflow/run_ensemble.R"); source("R/pest_workflow/sanity_checks.R")
exp <- commandArgs(trailingOnly = TRUE)[1]; tag <- "LID22999_spectacle"
ROOT <<- file.path(LOCAL_ROOT, "_builds", paste0("spsol_", exp)); dir.create(ROOT, recursive = TRUE, showWarnings = FALSE)
aeme0 <<- build_ensemble_aeme0(tag, "baseline")
if (exp %in% c("F", "G")) {   # F: inflow solids = 0; G: inflow solids / 100
  inf <- AEME::get_inflows(aeme0); f <- if (exp == "F") 0 else 0.01
  for (nm in names(inf)) for (v in intersect(c("NCS_tss", "NCS_ss1"), names(inf[[nm]]))) inf[[nm]][[v]] <- inf[[nm]][[v]] * f
  aeme0 <<- AEME::set_inflows(aeme0, inflows = inf)
}
pp <- read_posterior_ptabs(tag)
ptab <- pp$best[, intersect(PARAM_COLS, names(pp$best))]; for (cc in setdiff(PARAM_COLS, names(ptab))) ptab[[cc]] <- NA
ptab <- ptab[, PARAM_COLS]
setv <- function(nm, idx, v) { i <- ptab$name == nm & (is.na(ptab$index) | ptab$index == idx); stopifnot(any(i)); ptab$value[i] <<- v }
if (exp %in% c("B", "D")) setv("aed_noncohesive/epsilon", NA, 0)
if (exp %in% c("C", "D")) setv("aed_noncohesive/w_ss", 1, 2.0)
if (exp == "E") setv("outflow/seepage_rate", NA, 0)
if (exp == "H") { setv("aed_noncohesive/w_ss", 2, 1.0); setv("aed_noncohesive/tau_0", 2, 20) }   # H: second particle class settles fast, never resuspends
if (exp == "I") { setv("aed_noncohesive/w_ss", 1, 1.0); setv("aed_noncohesive/tau_0", 1, 20); setv("aed_noncohesive/w_ss", 2, 1.0); setv("aed_noncohesive/tau_0", 2, 20); setv("aed_noncohesive/epsilon", NA, 0) }   # I: both classes settle at 1 m/d, no resuspension
b <- fresh_build(paste0("spsol_", exp), VARS_SIM, use_bgc = TRUE, switches = SWITCHES, n_zones = n_glm_zones)
a <- run_aeme_param(aeme = b$aeme, param = ptab, model = MODEL, path = b$path, return_aeme = TRUE)
stopifnot(inherits(a, "Aeme"))
ms <- member_series(a, exp, c("NCS_ss1", "HYD_temp", "LKE_lvlwtr"))
st <- targets::tar_read(simulation_time); tr <- function(d) d[d$Date >= as.Date(st$start) & d$Date <= as.Date(st$stop), ]
cat("\n== experiment", exp, "==\n")
print(as.data.frame(tr(ms$pairs) |> summarise(n = n(), obs_mean = mean(obs), sim_at_obs = mean(sim), bias = mean(sim - obs), .by = c(variable, depth_class))), digits = 4)
yr <- tr(ms$sim) |> filter(variable == "NCS_ss1", depth_class == "surface") |> mutate(year = format(Date, "%Y")) |> summarise(mean_surface_solids = mean(sim), .by = year)
cat("\nyearly mean surface solids (mg/L):\n"); print(as.data.frame(yr), digits = 4)
# mass balance (inflow load in tonnes over the simulation window)
inf <- AEME::get_inflows(a); load_t <- 0
for (nm in names(inf)) { x <- inf[[nm]]; x$Date <- as.Date(x$Date); x <- x[x$Date >= as.Date(st$start) & x$Date <= as.Date(st$stop), ]
  if ("NCS_ss1" %in% names(x)) load_t <- load_t + sum(x$HYD_flow * x$NCS_ss1, na.rm = TRUE) / 1e6 }
vol <- tryCatch({ v <- AEME::get_var(a, model = MODEL, var_sim = "LKE_vol", use_obs = FALSE, remove_spin_up = TRUE); mean(v$value, na.rm = TRUE) }, error = function(e) NA)
cat(sprintf("\ninflow solids load over the window: %.0f t; mean lake volume %s m3\n", load_t, format(vol, big.mark = ",")))
unlink(ROOT, recursive = TRUE, force = TRUE)
