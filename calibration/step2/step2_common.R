# Step 2 of the staged plan: calibrate phytoplankton, nitrogen and phosphorus on the pre-alum lake record,
# with physics, oxygen and organic matter held at the step 1 result and alum off.
#
# Setup: hourly-forced base (calibration/step2/aeme_base_1990_v2.rds), 1990-01 to 2006-05-31 (alum dosing
# starts 2006-06-15), 540 days spin-up. Calibration years 1990-2003; 2004-01 to 2006-05 held out (reported
# only, never used to update parameters).
Sys.setenv(SA_VARIANT = if (identical(Sys.getenv("S2_ALUM"), "1")) "alum" else "noalum", SA_BASE = Sys.getenv("S2_BASE", "calibration/step2/aeme_base_1990_v2.rds"),
           SA_START = Sys.getenv("S2_START", "1990-01-01"), SA_STOP = Sys.getenv("S2_STOP", "2006-05-31"), SA_SPINUP = "540",
           SA_DIR_OVERRIDE = Sys.getenv("S2_DIR", file.path("calibration", "step2", "runs")))
source("calibration/sa/sa_common.R")

CAL_END2 <- as.Date(Sys.getenv("S2_CALEND", "2003-12-31"))
STEP2_FIX_TOTALS <- !identical(Sys.getenv("PEST_FIX_TOTALS"), "0")   # add refractory DON/DOP to TN/TP

# --- parameters ----------------------------------------------------------------------------------
# Free: phytoplankton traits (cyanobacteria + greens; diatoms are a negligible pool), P adsorption and sediment
# P release, nitrogen loss and sediment ammonium release, PON hydrolysis. Everything else, including every
# step 1 parameter, is held at the step 1 best member.
STEP2_PARAMS <- data.frame(
  name = c(rep(c("R_growth", "R_resp", "I_K", "KePHY", "K_N", "K_P", "w_p", "theta_growth"), each = 2),
           "aed_phosphorus/w_po4ads", rep("aed_sed_const2d/fsed_frp", 4),
           "aed_nitrogen/rdenit", "aed_nitrogen/kdenit", "aed_nitrogen/rnitrif", "aed_organic_matter/rpon_hydrol",
           rep("aed_sed_const2d/fsed_amm", 4)),
  group = c(rep(c("cyano", "green"), 8), rep(NA_character_, 13)),
  index = c(rep(c(1, 2), 8), NA, 1:4, rep(NA, 4), 1:4), stringsAsFactors = FALSE)
# Step 2b (S2_STAGE=b): starts from the step 2 best member and addresses the nutrient losses found in its budgets --
# POM settling (w_pom) and P sorption (kpo4p) are freed, the sorbed-P settling velocity (w_po4ads) is confined to a
# physical range (it was -25 m/d, started at -10000), the sediment P release can reach the measured zone fluxes
# (0.07-0.08 mmol/m2/d in the deep zones), and total P / total N count more in the score.
# Step 2c (S2_STAGE=c): starts from the step 2b best member. The 2b misfits were low FRP (bias -1.4), low ammonium (-1.2),
# high nitrate (+0.65) and a worse held-out chlorophyll fit. 2b got there by driving the sediment release to 1-3 orders of
# magnitude below the measured zone fluxes and the P sorption (kpo4p) towards its ceiling, so 2c anchors the sediment FRP and
# NH4 fluxes to the measured zone values (x0.2 to x3), caps kpo4p at 5, and gives FRP / NH4 more weight in the score.
STEP2C <- identical(Sys.getenv("S2_STAGE"), "c")
# Step 2d (S2_STAGE=d): the 20-year budgets of the 2b and 2c best members showed that (i) the sediment is a prescribed source -- 2c's
# 10 mmol/m2/d NH4 and 0.13 mmol/m2/d FRP releases (not measured: they were derived from zone-median concentrations) were recycled by the
# phytoplankton and settled out again, raising chlorophyll without raising FRP / NH4; (ii) P sorption is inert (adsorbed P ~2% of FRP, zero
# sorption rate), so kpo4p and w_po4ads are removed from the free set and held at the 2b values. 2d starts from the 2b best member, keeps
# the sediment NH4 / FRP releases within a factor of 4 of the 2b values, and uses the 2b score weights.
STEP2D <- identical(Sys.getenv("S2_STAGE"), "d")
# Step 2e (S2_STAGE=e): the AED source (libaed-water) shows the sediment has no nitrate sink (fsed_nit = 0 in every zone, and the linked
# SDF_Fsed_nit overrides aed_nitrogen's own value), so the calibration used water-column denitrification (rdenit) as a stand-in; the realised
# sediment ammonium / nitrate fluxes are the zone value x Ksed/(Ksed + O2) x a temperature factor; and particulate organic matter is
# effectively inert (w_pom -0.006 m/d, PON / POP hydrolysis timescales of years). 2e adds a per-zone sediment nitrate flux with the
# oxygen-dependent form (Fsed_nit_model = 2, uptake favoured when O2 is low), frees Ksed_amm / Ksed_nit, caps the water-column denitrification
# rate, and widens the POM settling and PON / POP hydrolysis ranges. S2_SORB=1 also switches P sorption onto the simulated solids
# (ads_use_external_tss = .false.) and frees kpo4p / w_po4ads. Starts from the 2b best member.
# Step 2L (S2_STAGE=l): light stage. The 2e diagnostics showed the model's suspended solids (mean 34 g/m3) give ~2.3 /m of attenuation against
# ~0.8 observed, so growth is light-limited and the biology parameters were tuned around a dark lake. 2L keeps all 2e parameters (biology,
# nutrients) fixed and frees the resuspension / settling / extinction parameters (aed_noncohesive epsilon, tau_0, w_ss, Ke_ss; Kw; KePHY) against
# Secchi depth (via the model's RAD_secchi), turbidity (as TSS = 1.5 x NTU) and a temperature guard. 2f then recalibrates phytoplankton and nutrients.
STEP2L <- identical(Sys.getenv("S2_STAGE"), "l")
# Step 2F (S2_STAGE=f): recalibrate phytoplankton, nutrients and sediment fluxes under the corrected light. Starts from the 2L best member (2e biology
# + calibrated light), holds every light parameter (Kw, KePHY, noncohesive epsilon / tau_0 / w_ss / Ke_ss) fixed, and frees the 2e parameter set with
# wider growth / respiration ranges (the 2e values were tuned to a dark lake and give ~4x too much chlorophyll under realistic light).
STEP2F <- identical(Sys.getenv("S2_STAGE"), "f")
# Step 2G (S2_STAGE=g): the 2f forward run still has the wrong seasonal cycle -- model chlorophyll peaks in Jan-Mar (24-31 ug/L vs 10-13 observed) and is
# half the observed in Jul-Oct, so nitrate accumulates in winter (0.07-0.11 vs 0.02-0.035 g/m3 x 1e-0) and ammonium stays ~5x low. The spring / winter
# diatom bloom is missing because silica is 1 mmol/m3 and the diatom parameters were never calibrated. 2G sets the silica supply (S2_SI, default 200 mmol/m3,
# written into the built inflow files because AEME multiplies large SIL_rsi inputs by 16.64), frees the diatom parameters, and adds monthly-climatology
# (seasonal) terms for chlorophyll, nitrate, ammonium, FRP and TP to the score. Starts from the 2f best member; light stays fixed at the 2L best.
STEP2G <- identical(Sys.getenv("S2_STAGE"), "g")
# Step 2O (S2_STAGE=o): bottom oxygen / anoxia. The 2g member underestimates anoxia over 2007-2022 (anoxic days 96 vs 240 observed on the observed days;
# bottom-DO depletion rate Oct-Feb about half the observed in several years). With alum ON (S2_ALUM=1, the step 3 alum parameters) so the post-2006
# productivity is realistic, frees the sediment oxygen demand (fsed_oxy by zone, Ksed_oxy, theta_sed_oxy) and the organic-matter oxygen consumption
# (rdom_minerl, rpoc_hydrol, theta_minerl); score = bottom DO (>= 17 m) bias / RMSE, anoxic days and annual minimum per water year, Oct-Feb depletion rate,
# plus the standard nutrient / chlorophyll / temperature residuals as guards. Starts from the 2g best member; light fixed.
STEP2O <- identical(Sys.getenv("S2_STAGE"), "o")
# Step 2alum (S2_STAGE=alum, S2_ALUM=1): alum module ON, calibrated to FRP (and TP, chlorophyll, aluminium) over 2005-2022 with the alum pools accumulating,
# on the 2g biology. Frees the 8 aed_alum parameters; objective = calibration/step2/alum_resid.R (step 3's, with tighter FRP scales). Cal 2006-06..2018-06,
# held out 2018-07..2022-06. Window set with S2_START=2005-01-01 S2_STOP=2022-06-29.
# Step 2P (S2_STAGE=p, S2_ALUM=1, step3 base): joint FRP stage -- the 8 alum parameters plus the sediment P release (fsed_frp by zone, Ksed_frp, theta_sed_frp) are freed
# together against the alum objective (FRP-weighted); starts from the 2O best member. FRP was still ~46% low at the surface after the oxygen stage.
# Step 2Q (S2_STAGE=q, S2_ALUM=1, S2_ALP=1, S2_BASE=calibration/alum/stream/base_V1.rds): particulate P stage. The lake TP error follows the inflow TP load (wet years +66 to +113%) because
# particulate organic P settles very slowly (w_pom = -0.019 m/d). Frees POM settling (w_pom), POP hydrolysis (rpop_hydrol) and the adsorbed-P settling velocity (w_po4ads) with the alum
# objective plus annual-TP terms (wet-year bias, year-to-year correlation); alum parameters fixed (k_w and k_s x S2_KSCALE, default 0.85, for the mean pH efficiency). Starts from the 2O member.
# S2_ALP=1 adds the in-stream-bound alum P (ALU_alp, present in the inflows of the base) to the built inflow files and the GLM namelist (AEME names it ALU_alap and leaves it out).
STEP2A <- Sys.getenv("S2_STAGE") %in% c("alum", "p", "q"); STEP2P <- identical(Sys.getenv("S2_STAGE"), "p"); STEP2Q <- identical(Sys.getenv("S2_STAGE"), "q")
STEP2E <- Sys.getenv("S2_STAGE") %in% c("e", "l", "f", "g", "o", "alum", "p", "q"); S2_SORB <- identical(Sys.getenv("S2_SORB"), "1")
STEP2B <- Sys.getenv("S2_STAGE") %in% c("b", "c", "d", "e", "l", "f", "g", "o", "alum", "p", "q")
if (STEP2B) STEP2_PARAMS <- rbind(STEP2_PARAMS, data.frame(name = c("aed_organic_matter/w_pom", "aed_phosphorus/kpo4p"),
                                                          group = NA_character_, index = NA_real_, stringsAsFactors = FALSE))
if (STEP2D || (STEP2E && !S2_SORB)) STEP2_PARAMS <- STEP2_PARAMS[!STEP2_PARAMS$name %in% c("aed_phosphorus/kpo4p", "aed_phosphorus/w_po4ads"), ]
if (STEP2E) STEP2_PARAMS <- rbind(STEP2_PARAMS, data.frame(
  name = c(rep("aed_sed_const2d/fsed_nit", 4), "aed_nitrogen/ksed_amm", "aed_nitrogen/ksed_nit", "aed_organic_matter/rpop_hydrol"),
  group = NA_character_, index = c(1:4, NA, NA, NA), stringsAsFactors = FALSE))
if (STEP2F || STEP2G || STEP2O) STEP2_PARAMS <- STEP2_PARAMS[STEP2_PARAMS$name != "KePHY", ]
if (STEP2A) STEP2_PARAMS <- data.frame(name = paste0("aed_alum/", c("w_ala", "k_w", "K_P", "k_s", "k_age_w", "k_age_s", "k_bur", "k_res")), group = NA_character_, index = NA_real_, stringsAsFactors = FALSE)
if (STEP2O) STEP2_PARAMS <- data.frame(name = c(rep("aed_sed_const2d/fsed_oxy", 4), "aed_oxygen/ksed_oxy", "aed_oxygen/theta_sed_oxy", "aed_organic_matter/rdom_minerl", "aed_organic_matter/rpoc_hydrol", "aed_organic_matter/theta_minerl"),
                                       group = NA_character_, index = c(1:4, rep(NA, 5)), stringsAsFactors = FALSE)
if (STEP2P) STEP2_PARAMS <- data.frame(name = c(paste0("aed_alum/", c("w_ala", "k_w", "K_P", "k_s", "k_age_w", "k_age_s", "k_bur", "k_res")), rep("aed_sed_const2d/fsed_frp", 4), "aed_phosphorus/ksed_frp", "aed_phosphorus/theta_sed_frp"),
                                       group = NA_character_, index = c(rep(NA, 8), 1:4, NA, NA), stringsAsFactors = FALSE)
S2_PHYTO <- identical(Sys.getenv("S2_PHYTO"), "1")      # stage p variant: frees K_P, X_pmin, X_pmax, R_resp of cyano / green / diatom instead of the alum and sediment P parameters
if (STEP2P && S2_PHYTO) STEP2_PARAMS <- data.frame(name = rep(c("K_P", "X_pmin", "X_pmax", "R_resp"), each = 3), group = rep(c("cyano", "green", "diatom"), 4), index = rep(1:3, 4), stringsAsFactors = FALSE)
S2_JOINT <- identical(Sys.getenv("S2_JOINT"), "1")      # stage p variant: joint stage -- phytoplankton P kinetics (12), oxygen (9), sediment P release (6) and nitrogen (13) freed together; objective = alum_resid.R with the JOINT terms
if (STEP2P && S2_JOINT) { .g3 <- c("cyano", "green", "diatom")
  STEP2_PARAMS <- data.frame(
    name = c(rep(c("K_P", "X_pmin", "X_pmax", "R_resp"), each = 3), rep("aed_sed_const2d/fsed_oxy", 4), "aed_oxygen/ksed_oxy", "aed_oxygen/theta_sed_oxy", "aed_organic_matter/rdom_minerl", "aed_organic_matter/rpoc_hydrol", "aed_organic_matter/theta_minerl",
             rep("aed_sed_const2d/fsed_frp", 4), "aed_phosphorus/ksed_frp", "aed_phosphorus/theta_sed_frp",
             "aed_nitrogen/rdenit", rep("aed_sed_const2d/fsed_nit", 4), rep("aed_sed_const2d/fsed_amm", 4), rep("K_N", 3), "aed_organic_matter/rpon_hydrol"),
    group = c(rep(.g3, 4), rep(NA, 9 + 4 + 2 + 1 + 4 + 4), .g3, NA), stringsAsFactors = FALSE)
  STEP2_PARAMS$index <- c(rep(1:3, 4), 1:4, rep(NA, 5), 1:4, NA, NA, NA, 1:4, 1:4, 1:3, NA) }
if (STEP2P && S2_JOINT && identical(Sys.getenv("S2_JOINT_ALUMFIT"), "1")) STEP2_PARAMS <- data.frame(name = c(paste0("aed_alum/", c("w_ala", "k_w", "K_P", "k_s", "k_age_w", "k_age_s", "k_bur", "k_res")), rep("aed_sed_const2d/fsed_frp", 4), "aed_phosphorus/ksed_frp", "aed_phosphorus/theta_sed_frp"),
                                       group = NA_character_, index = c(rep(NA, 8), 1:4, NA, NA), stringsAsFactors = FALSE)   # joint objective with only the alum and sediment P parameters free (alum refit under a new stream rule; needs S2_ALUM_WIDE=1)
S2_Q2 <- identical(Sys.getenv("S2_Q2"), "1")    # narrower stage q: w_pom fixed at the stage-o value (the full q fit worsened total N); DOM mineralisation (rdom_minerl, shared by DOC/DON/DOP) freed within 0.5-2x
if (STEP2Q) STEP2_PARAMS <- data.frame(name = if (S2_Q2) c("aed_organic_matter/rpop_hydrol", "aed_phosphorus/w_po4ads", "aed_organic_matter/rdom_minerl") else c("aed_organic_matter/w_pom", "aed_organic_matter/rpop_hydrol", "aed_phosphorus/w_po4ads"), group = NA_character_, index = NA_real_, stringsAsFactors = FALSE)
if (STEP2G) STEP2_PARAMS <- rbind(STEP2_PARAMS, data.frame(name = c("R_growth", "R_resp", "I_K", "K_N", "K_P", "w_p", "theta_growth"), group = "diatom", index = 3, stringsAsFactors = FALSE))
if (STEP2L) STEP2_PARAMS <- data.frame(
  name = c("aed_noncohesive/epsilon", rep("aed_noncohesive/tau_0", 2), rep("aed_noncohesive/w_ss", 2), rep("aed_noncohesive/Ke_ss", 2), "light/Kw", "KePHY", "KePHY"),
  group = c(rep(NA_character_, 8), "cyano", "green"), index = c(NA, 1, 2, 1, 2, 1, 2, NA, 1, 2), stringsAsFactors = FALSE)
.key2 <- function(d) paste(d$name, ifelse(is.na(d$group), "", d$group), ifelse(is.na(d$index), "", d$index))

step2_table <- function(b) {
  tab <- sa_table(b)
  s1 <- readRDS("calibration/checkpoints/step1_final_best.rds")$best          # step 1 best member
  hit <- match(.key2(tab), .key2(s1)); use <- !is.na(hit)
  tab$value[use] <- s1$value[hit[use]]
  if (STEP2B) {                                                 # start from the step 2 best member
    s2 <- readRDS("calibration/step2/runs/step2_best.rds")$best
    h2 <- match(.key2(tab), .key2(s2)); u2 <- !is.na(h2); tab$value[u2] <- s2$value[h2[u2]]
  }
  if (STEP2E && !any(tab$name == "aed_sed_const2d/fsed_nit")) {     # the table has no sediment nitrate rows: clone the FRP zone rows
    nz <- tab[tab$name == "aed_sed_const2d/fsed_frp", ]; nz$name <- "aed_sed_const2d/fsed_nit"; nz$value <- -0.1; nz$min <- -3; nz$max <- -0.005
    tab <- rbind(tab, nz)
  }
  if (STEP2C || STEP2D || STEP2E) {                             # ... and from the step 2b best member
    s2b <- readRDS("calibration/step2b/runs/step2_best.rds")$best
    h2 <- match(.key2(tab), .key2(s2b)); u2 <- !is.na(h2); tab$value[u2] <- s2b$value[h2[u2]]
  }
  # on the first pass the step 1 values stay inside their (now fixed) ranges
  sel <- .key2(tab) %in% .key2(STEP2_PARAMS)
  tab$min[!sel] <- tab$value[!sel]; tab$max[!sel] <- tab$value[!sel]; tab$theme[!sel] <- "fixed"
  # ranges for this step: chlorophyll is 6x too low at the baseline, so allow growth well above the base value
  rg <- tab$name == "R_growth" & sel; v <- tab$value[rg]; tab$min[rg] <- 0.5 * v; tab$max[rg] <- 5 * v
  rr <- tab$name == "R_resp" & sel;   v <- tab$value[rr]; tab$min[rr] <- 0.3 * v; tab$max[rr] <- 3 * v
  if (STEP2B) {
    setr <- function(nm, lo, hi, v = NULL, idx = NULL) { i <- which(tab$name == nm & (is.null(idx) | tab$index %in% idx))
      tab$min[i] <<- lo; tab$max[i] <<- hi; tab$scale[i] <<- "log"; if (!is.null(v)) tab$value[i] <<- v }
    setr("aed_phosphorus/w_po4ads", -2, -0.001, v = -0.2)
    setr("aed_phosphorus/kpo4p", 0.1, 20)
    setr("aed_organic_matter/w_pom", -0.5, -0.001)
    setr("aed_sed_const2d/fsed_frp", 5e-4, 0.2)
  }
  if (STEP2D) {                                                 # sediment releases within x/ 4 of the 2b values; sorption held at the 2b values
    s2b <- readRDS("calibration/step2b/runs/step2_best.rds")$best
    for (nm in c("aed_sed_const2d/fsed_amm", "aed_sed_const2d/fsed_frp")) for (z in 1:4) {
      v <- s2b$value[s2b$name == nm & s2b$index %in% z]; setr(nm, v / 4, v * 4, v = v, idx = z) }
    for (nm in c("aed_phosphorus/kpo4p", "aed_phosphorus/w_po4ads")) {
      i <- which(tab$name == nm); v <- s2b$value[s2b$name == nm]; tab$value[i] <- v; tab$min[i] <- v; tab$max[i] <- v; tab$theme[i] <- "fixed" }
  }
  if (STEP2E) {
    s2b <- readRDS("calibration/step2b/runs/step2_best.rds")$best
    for (nm in c("aed_sed_const2d/fsed_amm", "aed_sed_const2d/fsed_frp")) for (z in 1:4) {
      v <- s2b$value[s2b$name == nm & s2b$index %in% z]; setr(nm, v / 4, v * 4, v = v, idx = z) }
    for (z in 1:4) setr("aed_sed_const2d/fsed_nit", -3, -0.005, v = -0.1, idx = z)
    setr("aed_nitrogen/ksed_amm", 5, 200, v = 41.25); setr("aed_nitrogen/ksed_nit", 5, 200, v = 73.26)
    setr("aed_nitrogen/rdenit", 0.05, 5, v = min(max(s2b$value[s2b$name == "aed_nitrogen/rdenit"], 0.05), 5))
    setr("aed_organic_matter/rpon_hydrol", 1e-4, 0.05); setr("aed_organic_matter/rpop_hydrol", 1e-4, 0.05, v = 3e-4)
    if (S2_SORB) { setr("aed_phosphorus/kpo4p", 0.05, 10, v = 0.5); setr("aed_phosphorus/w_po4ads", -2, -0.001, v = -0.2) }
    else for (nm in c("aed_phosphorus/kpo4p", "aed_phosphorus/w_po4ads")) {
      i <- which(tab$name == nm); v <- s2b$value[s2b$name == nm]; tab$value[i] <- v; tab$min[i] <- v; tab$max[i] <- v; tab$theme[i] <- "fixed" }
  }
  if (STEP2C) {
    setr("aed_phosphorus/kpo4p", 0.1, 5)
    meas_frp <- c(0.0783, 0.0717, 0.0103, 0.0051); meas_amm <- c(3.82, 1.94, 0.317, 0.071)   # measured zone fluxes, mmol/m2/d
    for (z in 1:4) {
      setr("aed_sed_const2d/fsed_frp", 0.2 * meas_frp[z], 3 * meas_frp[z], v = meas_frp[z], idx = z)
      setr("aed_sed_const2d/fsed_amm", 0.2 * meas_amm[z], 3 * meas_amm[z], v = meas_amm[z], idx = z) }
  }
  if (STEP2L) {                                                 # light stage: 2e best member everywhere, then free the light parameters
    s2e <- readRDS("calibration/step2e/runs/step2_best.rds")$best
    he <- match(.key2(tab), .key2(s2e)); ue <- !is.na(he); tab$value[ue] <- s2e$value[he[ue]]
    tab$min[!sel] <- tab$value[!sel]; tab$max[!sel] <- tab$value[!sel]; tab$theme[!sel] <- "fixed"
    nc <- tab[rep(which(tab$name == "aed_sed_const2d/fsed_frp")[1], 7), ]; nc$group <- NA_character_
    nc$name <- c("aed_noncohesive/epsilon", "aed_noncohesive/tau_0", "aed_noncohesive/tau_0", "aed_noncohesive/w_ss", "aed_noncohesive/w_ss", "aed_noncohesive/Ke_ss", "aed_noncohesive/Ke_ss")
    nc$index <- c(NA, 1, 2, 1, 2, 1, 2); nc$value <- c(3e-5, 0.08, 0.05, -0.5, -0.03, 0.06, 0.06)
    nc$min <- c(1e-6, 0.02, 0.02, -3, -0.3, 0.02, 0.02); nc$max <- c(3e-4, 0.5, 0.5, -0.05, -0.003, 0.15, 0.15); nc$scale <- "log"; nc$theme <- "light"
    tab <- rbind(tab, nc); sel <- .key2(tab) %in% .key2(STEP2_PARAMS)
    i <- which(tab$name == "light/Kw"); tab$value[i] <- 0.38; tab$min[i] <- 0.30; tab$max[i] <- 0.45; tab$scale[i] <- "log"; tab$theme[i] <- "light"
    i <- which(tab$name == "KePHY" & tab$group %in% c("cyano", "green")); tab$min[i] <- 5e-4; tab$max[i] <- 0.03; tab$theme[i] <- "light"
    if (nzchar(Sys.getenv("S2_L_FROM"))) {                      # second light round: start every parameter from a later best member (biology + calibrated light)
      sx <- readRDS(Sys.getenv("S2_L_FROM"))$best; hx <- match(.key2(tab), .key2(sx)); ux <- !is.na(hx); tab$value[ux] <- sx$value[hx[ux]]
      tab$min[!sel] <- tab$value[!sel]; tab$max[!sel] <- tab$value[!sel] }
  }
  if (STEP2F || STEP2G || STEP2O || STEP2A) {                   # biology free (F, G) or fixed (O, alum), light fixed at the 2L best member
    nc <- tab[rep(which(tab$name == "aed_sed_const2d/fsed_frp")[1], 7), ]; nc$group <- NA_character_
    nc$name <- c("aed_noncohesive/epsilon", "aed_noncohesive/tau_0", "aed_noncohesive/tau_0", "aed_noncohesive/w_ss", "aed_noncohesive/w_ss", "aed_noncohesive/Ke_ss", "aed_noncohesive/Ke_ss")
    nc$index <- c(NA, 1, 2, 1, 2, 1, 2); nc$scale <- "log"; nc$theme <- "light"; tab <- rbind(tab, nc)
    sl <- readRDS("calibration/step2l/runs/step2_best.rds")$best
    hl <- match(.key2(tab), .key2(sl)); ul <- !is.na(hl); tab$value[ul] <- sl$value[hl[ul]]
    if (STEP2G || STEP2O || STEP2A) { sf <- readRDS("calibration/step2f/runs/step2_best.rds")$best; hf <- match(.key2(tab), .key2(sf)); uf <- !is.na(hf); tab$value[uf] <- sf$value[hf[uf]] }
    if (STEP2O || STEP2A) { sg <- readRDS("calibration/step2g/runs/step2_best.rds")$best; hg <- match(.key2(tab), .key2(sg)); ug <- !is.na(hg); tab$value[ug] <- sg$value[hg[ug]]
      if (identical(Sys.getenv("S2_ALUM"), "1")) { s3 <- readRDS(Sys.getenv("S2_ALUM_FROM", "calibration/step3/runs/step3_best.rds"))$best; h3 <- match(.key2(tab), .key2(s3)); u3 <- !is.na(h3) & grepl("^aed_alum/", tab$name); tab$value[u3] <- s3$value[h3[u3]] } }
    for (of in strsplit(Sys.getenv("S2_OVERLAY"), ",")[[1]]) if (nzchar(of)) { so <- readRDS(of)$best; ho <- match(.key2(tab), .key2(so)); uo <- !is.na(ho) & !grepl("^aed_alum/", tab$name); tab$value[uo] <- so$value[ho[uo]] }   # refit: overlay later-stage best members (oxygen, particulate) on everything except the alum parameters
    sel <- .key2(tab) %in% .key2(STEP2_PARAMS); tab$min[!sel] <- tab$value[!sel]; tab$max[!sel] <- tab$value[!sel]; tab$theme[!sel] <- "fixed"
    setf <-function(nm, grp, lo, hi) { i <- which(tab$name == nm & tab$group %in% grp); tab$min[i] <<- lo; tab$max[i] <<- hi; tab$scale[i] <<- "log" }
    if (STEP2O && nzchar(Sys.getenv("S2_O_FROM"))) {            # oxygen round 2: start everything from a later best member (e.g. the light-round-2 member)
      sx <- readRDS(Sys.getenv("S2_O_FROM"))$best; hx <- match(.key2(tab), .key2(sx)); ux <- !is.na(hx); tab$value[ux] <- sx$value[hx[ux]]
      sel <- .key2(tab) %in% .key2(STEP2_PARAMS); tab$min[!sel] <- tab$value[!sel]; tab$max[!sel] <- tab$value[!sel]; tab$theme[!sel] <- "fixed" }
    if (STEP2P) { if (!nzchar(Sys.getenv("S2_OVERLAY"))) { so <- readRDS("calibration/step2o/runs/step2_best.rds")$best; ho <- match(.key2(tab), .key2(so)); uo <- !is.na(ho); tab$value[uo] <- so$value[ho[uo]] }       # with S2_OVERLAY the later-stage members were already overlaid above
      sel <- .key2(tab) %in% .key2(STEP2_PARAMS); tab$min[!sel] <- tab$value[!sel]; tab$max[!sel] <- tab$value[!sel]; tab$theme[!sel] <- "fixed"
      for (z in 1:4) { i <- which(tab$name == "aed_sed_const2d/fsed_frp" & tab$index %in% z); v <- tab$value[i]; tab$min[i] <- 0.3 * v; tab$max[i] <- 3 * v; tab$scale[i] <- "log"; tab$theme[i] <- "phosphorus" }
      i <- which(tab$name == "aed_phosphorus/ksed_frp"); tab$min[i] <- 1; tab$max[i] <- 50; tab$scale[i] <- "log"; tab$theme[i] <- "phosphorus"
      i <- which(tab$name == "aed_phosphorus/theta_sed_frp"); tab$min[i] <- 1.02; tab$max[i] <- 1.12; tab$scale[i] <- "lin"; tab$theme[i] <- "phosphorus"
      if (S2_PHYTO) { tab$min[] <- tab$value; tab$max[] <- tab$value; tab$theme[] <- "fixed"                # phytoplankton P stage: only the P kinetics of the three groups are free, everything else (incl. sediment P, alum) fixed at the overlaid members
        for (j in which(.key2(tab) %in% .key2(STEP2_PARAMS))) { v <- tab$value[j]; nm <- tab$name[j]
          r <- if (nm == "K_P") c(0.3, 3) * v else if (nm %in% c("X_pmin", "X_pmax")) c(0.5, 2) * v else c(0.01, 0.4)
          tab$min[j] <- r[1]; tab$max[j] <- r[2]; tab$scale[j] <- "log"; tab$theme[j] <- "phyto" } }
      if (S2_JOINT) { tab$min[] <- tab$value; tab$max[] <- tab$value; tab$theme[] <- "fixed"                # joint stage: ranges for every free parameter (log scale unless stated)
        rg <- function(nm, lo, hi, sc = "log", th = "joint") { for (j in which(tab$name == nm)) { tab$min[j] <<- lo; tab$max[j] <<- hi; tab$scale[j] <<- sc; tab$theme[j] <<- th } }                 # absolute range
        mult <- function(nm, lo, hi, sc = "log", th = "joint") { for (j in which(tab$name == nm)) { r <- sort(c(lo, hi) * tab$value[j]); tab$min[j] <<- r[1]; tab$max[j] <<- r[2]; tab$scale[j] <<- sc; tab$theme[j] <<- th } }   # range as multiples of the current value
        mult("K_P", 0.3, 3, th = "phyto"); mult("X_pmin", 0.5, 2, th = "phyto"); mult("X_pmax", 0.5, 2, th = "phyto"); rg("R_resp", 0.01, 0.4, th = "phyto"); mult("K_N", 0.5, 2, th = "nitrogen")
        mult("aed_sed_const2d/fsed_oxy", 0.3, 4, th = "oxygen"); rg("aed_oxygen/ksed_oxy", 10, 300, th = "oxygen"); rg("aed_oxygen/theta_sed_oxy", 1.04, 1.15, "lin", "oxygen"); rg("aed_organic_matter/rdom_minerl", 0.005, 0.3, th = "oxygen")
        rg("aed_organic_matter/rpoc_hydrol", 1e-4, 0.05, th = "oxygen"); rg("aed_organic_matter/theta_minerl", 1.04, 1.15, "lin", "oxygen")
        mult("aed_sed_const2d/fsed_frp", 0.3, 3, th = "phosphorus"); rg("aed_phosphorus/ksed_frp", 1, 50, th = "phosphorus"); rg("aed_phosphorus/theta_sed_frp", 1.02, 1.12, "lin", "phosphorus")
        rg("aed_nitrogen/rdenit", 0.02, 10, th = "nitrogen"); mult("aed_sed_const2d/fsed_nit", 0.2, 5, th = "nitrogen"); mult("aed_sed_const2d/fsed_amm", 0.3, 3, th = "nitrogen"); mult("aed_organic_matter/rpon_hydrol", 0.2, 5, th = "nitrogen")
        if (identical(Sys.getenv("S2_JOINT_ALUMFIT"), "1")) { sel <- .key2(tab) %in% .key2(STEP2_PARAMS); tab$min[!sel] <- tab$value[!sel]; tab$max[!sel] <- tab$value[!sel]; tab$theme[!sel] <- "fixed" } } }   # alum refit: the joint ranges above are by name, so fix everything outside the alum and sediment P set again
    if (STEP2Q) { so <- readRDS("calibration/step2o/runs/step2_best.rds")$best; ho <- match(.key2(tab), .key2(so)); uo <- !is.na(ho); tab$value[uo] <- so$value[ho[uo]]
      i <- tab$name %in% c("aed_alum/k_w", "aed_alum/k_s"); tab$value[i] <- tab$value[i] * as.numeric(Sys.getenv("S2_KSCALE", "0.85"))       # mean pH efficiency
      sel <- .key2(tab) %in% .key2(STEP2_PARAMS); tab$min[!sel] <- tab$value[!sel]; tab$max[!sel] <- tab$value[!sel]; tab$theme[!sel] <- "fixed"
      lim2 <- function(nm, lo, hi) { i <- which(tab$name == nm); tab$min[i] <<- lo; tab$max[i] <<- hi; tab$scale[i] <<- "log"; tab$theme[i] <<- "particulate" }
      lim2("aed_organic_matter/w_pom", -1, -0.005); lim2("aed_organic_matter/rpop_hydrol", 0.002, 0.3); lim2("aed_phosphorus/w_po4ads", -3, -0.02)
      if (S2_Q2) { i <- which(tab$name == "aed_organic_matter/rdom_minerl"); v <- tab$value[i]; tab$min[i] <- 0.5 * v; tab$max[i] <- 2 * v; tab$scale[i] <- "log"; tab$theme[i] <- "particulate" } }
    if (STEP2A) { i <- which(grepl("^aed_alum/", tab$name)); tab$scale[i] <- "log"; tab$theme[i] <- "alum"
      if (identical(Sys.getenv("S2_ALUM_WIDE"), "1")) {          # second round: wider ranges (w_ala, K_P and k_age_w sat at the first-round limits)
        wr <- list(w_ala = c(0.2, 10), k_w = c(0.1, 20), K_P = c(0.02, 3), k_s = c(0.02, 1), k_age_w = c(0.02, 1), k_age_s = c(0.01, 1), k_bur = c(0.002, 0.2), k_res = c(5e-4, 0.5))
        for (nm in names(wr)) { j <- which(tab$name == paste0("aed_alum/", nm)); tab$min[j] <- wr[[nm]][1]; tab$max[j] <- wr[[nm]][2] } } }
    if (!STEP2O && !STEP2A) { setf("R_growth", c("cyano", "green"), 0.2, 2.5); setf("R_resp", c("cyano", "green"), 0.01, 0.25); setf("I_K", c("cyano", "green"), 5, 150) }
    if (STEP2O) {
      for (z in 1:4) { i <- which(tab$name == "aed_sed_const2d/fsed_oxy" & tab$index %in% z); v <- tab$value[i]; r <- sort(c(0.3 * v, 4 * v)); tab$min[i] <- r[1]; tab$max[i] <- r[2]; tab$scale[i] <- "log"; tab$theme[i] <- "oxygen" }
      lim <- function(nm, lo, hi, sc) { i <- which(tab$name == nm); tab$min[i] <<- lo; tab$max[i] <<- hi; tab$scale[i] <<- sc; tab$theme[i] <<- "oxygen" }
      lim("aed_oxygen/ksed_oxy", 10, 300, "log"); lim("aed_oxygen/theta_sed_oxy", 1.04, 1.15, "lin"); lim("aed_organic_matter/rdom_minerl", 0.005, 0.3, "log")
      lim("aed_organic_matter/rpoc_hydrol", 1e-4, 0.05, "log"); lim("aed_organic_matter/theta_minerl", 1.04, 1.15, "lin") }
    if (STEP2G) { setf("R_growth", "diatom", 0.2, 2.5); setf("R_resp", "diatom", 0.01, 0.25); setf("I_K", "diatom", 5, 150); setf("K_N", "diatom", 0.5, 15); setf("K_P", "diatom", 0.02, 0.5)
      i <- which(tab$name == "w_p" & tab$group == "diatom"); tab$min[i] <- -0.3; tab$max[i] <- 0.1; tab$scale[i] <- "lin"; i <- which(tab$name == "theta_growth" & tab$group == "diatom"); tab$min[i] <- 1.04; tab$max[i] <- 1.12; tab$scale[i] <- "lin" }
  }
  tab$value[sel] <- pmin(pmax(tab$value[sel], tab$min[sel]), tab$max[sel])
  tab
}

# The AED totals behind NIT_tn / PHS_tp leave out the refractory DON / DOP pools (OGM_donr, OGM_dopr), which are
# near-constant (donr_initial = 9 mmol N/m3, dopr_initial = 0.15 mmol P/m3 in aed.nml). run_aeme_param() rewrites
# aed.nml and drops any edit of the totals block, so the pools are added to the simulated TN / TP here instead.
DONR_GM3 <- 9 * 14.0067 / 1000     # 0.126 g N/m3
DOPR_GM3 <- 0.15 * 30.974 / 1000   # 0.0046 g P/m3
step2_build <- function(tag = "sa_base") {
  b <- sa_build(tag)
  if (STEP2E) {                      # structural switches go through the configuration and are written to the build, as apply_switches() does
    cfg <- AEME::configuration(b$aeme)
    cfg[[MODEL]][["bgc"]][["aed"]][["aed_nitrogen"]][["fsed_nit_model"]] <- 2
    if (S2_SORB) cfg[[MODEL]][["bgc"]][["aed"]][["aed_phosphorus"]][["ads_use_external_tss"]] <- FALSE
    AEME::configuration(b$aeme) <- cfg
    AEME::write_configuration(aeme = b$aeme, model = MODEL, path = b$path, include_boundary = FALSE)
  }
  if (STEP2G || STEP2O || STEP2A || (STEP2L && nzchar(Sys.getenv("S2_L_FROM")))) {  # silica supply: lake initial value through the configuration, inflows by overwriting the built files
    si <- as.numeric(Sys.getenv("S2_SI", "200")); cfg <- AEME::configuration(b$aeme)
    cfg[[MODEL]][["bgc"]][["aed"]][["aed_silica"]][["rsi_initial"]] <- si
    ip <- cfg[[MODEL]][["hydrodynamic"]][["init_profiles"]]; v <- matrix(ip$wq_init_vals, nrow = ip$num_depths); colnames(v) <- ip$wq_names; v[, "SIL_rsi"] <- si
    cfg[[MODEL]][["hydrodynamic"]][["init_profiles"]][["wq_init_vals"]] <- as.vector(v); AEME::configuration(b$aeme) <- cfg
    AEME::write_configuration(aeme = b$aeme, model = MODEL, path = b$path, include_boundary = FALSE)
    for (f in list.files(b$path, "^inflow_.*[.]csv$", recursive = TRUE, full.names = TRUE)) { d <- read.csv(f, check.names = FALSE)
      if ("SIL_rsi" %in% names(d)) { d$SIL_rsi <- si; write.csv(d, f, row.names = FALSE, quote = FALSE) } }
  }
  if (nzchar(Sys.getenv("S2_ALP"))) {   # stream-bound alum P: ALU_alp column in every built inflow file and in inflow_vars of the GLM namelist
    inf <- AEME::inflows(b$aeme)$data
    nml <- list.files(b$path, "glm4?[.]nml$", recursive = TRUE, full.names = TRUE)[1]; x <- readLines(nml); i <- grep("^\\s*inflow_vars", x)
    if (!any(grepl("ALU_alp", x[i]))) { x[i] <- paste0(x[i], ",'ALU_alp'"); writeLines(x, nml) }
    for (f in list.files(b$path, "^inflow_.*[.]csv$", recursive = TRUE, full.names = TRUE)) { d <- read.csv(f, check.names = FALSE); nm <- sub("^inflow_(.*)[.]csv$", "\\1", basename(f)); a <- inf[[nm]]
      d$ALU_alp <- if (!is.null(a) && "ALU_alp" %in% names(a)) { v <- a$ALU_alp[match(as.Date(d$time), as.Date(a$Date))]; v[is.na(v)] <- 0; v } else 0
      write.csv(d, f, row.names = FALSE, quote = FALSE) }
  }
  b
}

# --- objective -----------------------------------------------------------------------------------
# Surface (0-5 m) concentrations: bias and RMSE of the log ratio sim/obs (a floor keeps near-zero samples
# finite). A residual of 1 = the stated scale.
STEP2_VARS <- data.frame(
  variable = c("PHY_tchla", "PHS_tp", "PHS_frp", "NIT_tn", "NIT_nit", "NIT_amm"),
  floor = c(1, 0.002, 0.002, 0.02, 0.003, 0.003),
  scale = c(0.5, 0.25, 0.7, 0.3, 0.7, 0.7), stringsAsFactors = FALSE)
if (STEP2B) STEP2_VARS$scale[STEP2_VARS$variable == "PHS_tp"] <- 0.15
if (STEP2B) STEP2_VARS$scale[STEP2_VARS$variable == "NIT_tn"] <- 0.2
if (STEP2C) STEP2_VARS$scale[STEP2_VARS$variable %in% c("PHS_frp", "NIT_amm")] <- 0.5
STEP2_CHL_SIG <- c("chl_summer_bias", "chl_p90", "chl_f_gt10", "chl_f_gt20", "chl_annual_rmse")
STEP2_GUARD <- c("temp_bias", "temp_rmse", "oxy_bias", "oxy_rmse", "oxy_bottom_bias")
STEP2_NAMES <- c(paste(rep(STEP2_VARS$variable, each = 2), c("bias", "rmse"), sep = "_"), STEP2_CHL_SIG, STEP2_GUARD)
if (STEP2A) { source("calibration/step2/alum_resid.R"); STEP2_NAMES <- STEP3_NAMES }
if (STEP2O) STEP2_NAMES <- c(STEP2_NAMES, "do_bias", "do_rmse", "anox_bias", "anox_rmse", "domin_bias", "doslope_bias")
if (STEP2G) STEP2_NAMES <- c(STEP2_NAMES, "chl_seas", "nit_seas", "amm_seas", "frp_seas", "tp_seas")
if (STEP2L) STEP2_NAMES <- c("kd_bias", "kd_rmse", "kd_r", "tss_bias", "tss_rmse", "tss_r", "temp_bias", "temp_rmse")
PENALTY2 <- 5
MIN_PAIRS <- 8

lr <- function(d, fl) log(d$sim + fl) - log(d$obs + fl)
step2_resid <- function(P) {
  y <- setNames(rep(NA_real_, length(STEP2_NAMES)), STEP2_NAMES)
  for (i in seq_len(nrow(STEP2_VARS))) {
    r <- STEP2_VARS[i, ]; d <- P[[r$variable]]
    if (is.null(d)) next; d <- d[d$depth <= 5, ]
    if (nrow(d) < MIN_PAIRS) { y[paste0(r$variable, c("_bias", "_rmse"))] <- 0; next }   # no data in this window
    e <- lr(d, r$floor)
    y[paste0(r$variable, "_bias")] <- mean(e) / r$scale
    y[paste0(r$variable, "_rmse")] <- sqrt(mean(e^2)) / (2 * r$scale)
  }
  d <- P$PHY_tchla
  if (!is.null(d)) { d <- d[d$depth <= 5, ]
    if (nrow(d) >= MIN_PAIRS) {
      s <- d[d$mo %in% c(12, 1:4), ]
      y["chl_summer_bias"] <- if (nrow(s) >= 4) mean(lr(s, 1)) / 0.5 else 0
      y["chl_p90"] <- (log(quantile(d$sim, 0.9) + 1) - log(quantile(d$obs, 0.9) + 1)) / 0.5
      y["chl_f_gt10"] <- (mean(d$sim > 10) - mean(d$obs > 10)) / 0.15
      y["chl_f_gt20"] <- (mean(d$sim > 20) - mean(d$obs > 20)) / 0.1
      a <- aggregate(cbind(obs, sim) ~ yr, d, mean); cnt <- table(d$yr); a <- a[cnt[as.character(a$yr)] >= 3, ]
      y["chl_annual_rmse"] <- if (nrow(a) >= 2) sqrt(mean((log(a$sim + 1) - log(a$obs + 1))^2)) / 0.5 else 0
    } else y[STEP2_CHL_SIG] <- 0 }
  # guards: the phytoplankton changes must not wreck the step 1 temperature and oxygen fit (obs from 2003)
  g <- function(v, bias_sc, rmse_sc, nm, bottom = FALSE) {
    d <- P[[v]]; if (is.null(d) || nrow(d) < 50) { y[paste0(nm, c("_bias", "_rmse"))] <<- 0; return(invisible()) }
    y[paste0(nm, "_bias")] <<- mean(d$sim - d$obs) / bias_sc; y[paste0(nm, "_rmse")] <<- sqrt(mean((d$sim - d$obs)^2)) / rmse_sc
    if (bottom) { b <- d[d$depth >= 17, ]; y["oxy_bottom_bias"] <<- if (nrow(b) >= 20) mean(b$sim - b$obs) / 1 else 0 } }
  g("HYD_temp", 0.5, 1.0, "temp"); g("CHM_oxy", 1.0, 2.0, "oxy", bottom = TRUE)
  if (STEP2O && !is.null(P$CHM_oxy)) {              # bottom oxygen / anoxia (daily mean of >= 17 m, observed days only)
    ox <- P$CHM_oxy[P$CHM_oxy$depth >= 17, ]
    if (nrow(ox) >= 20) {
      B <- aggregate(cbind(obs, sim) ~ Date, ox, mean); B$wy <- as.integer(format(B$Date, "%Y")) + as.integer(as.integer(format(B$Date, "%m")) >= 7)
      y["do_bias"] <- mean(B$sim - B$obs) / 0.5; y["do_rmse"] <- sqrt(mean((B$sim - B$obs)^2)) / 1.0
      W <- do.call(rbind, lapply(split(B, B$wy), function(x) { if (nrow(x) < 60) return(NULL); w <- x$wy[1]
        sl <- function(col) { z <- x[x$Date >= as.Date(paste0(w - 1, "-10-15")) & x$Date <= as.Date(paste0(w, "-02-15")), ]; if (nrow(z) < 30) NA_real_ else unname(coef(lm(z[[col]] ~ as.numeric(z$Date)))[2]) }
        data.frame(oa = sum(x$obs < 2), sa = sum(x$sim < 2), om = min(x$obs), sm = min(x$sim), os = sl("obs"), ss = sl("sim")) }))
      if (!is.null(W) && nrow(W) >= 2) { e <- log1p(W$sa) - log1p(W$oa); y["anox_bias"] <- mean(e) / 0.7; y["anox_rmse"] <- sqrt(mean(e^2)) / 1.0
        y["domin_bias"] <- mean(W$sm - W$om) / 1.0; y["doslope_bias"] <- if (sum(!is.na(W$os) & !is.na(W$ss)) >= 2) mean(W$ss - W$os, na.rm = TRUE) / 0.02 else 0 } } }
  if (STEP2G) {                                     # monthly-climatology error of the surface series (needs >= 6 months with >= 2 samples)
    seas <- function(d, fl, sc) { if (is.null(d)) return(0); d <- d[d$depth <= 5, ]; if (nrow(d) < MIN_PAIRS) return(0)
      a <- aggregate(cbind(obs, sim) ~ mo, d, mean); n <- table(d$mo)[as.character(a$mo)]; a <- a[n >= 2, ]; if (nrow(a) < 6) return(0)
      sqrt(mean((log(a$sim + fl) - log(a$obs + fl))^2)) / sc }
    y["chl_seas"] <- seas(P$PHY_tchla, 1, 0.5); y["nit_seas"] <- seas(P$NIT_nit, 0.003, 0.7); y["amm_seas"] <- seas(P$NIT_amm, 0.003, 0.7)
    y["frp_seas"] <- seas(P$PHS_frp, 0.002, 0.7); y["tp_seas"] <- seas(P$PHS_tp, 0.002, 0.25) }
  y[is.na(y)] <- 0                                  # nothing observed in this window
  y
}

# one model run -> list(cal, val) residual vectors; NULL on failure
# Light-stage evaluation (the model does not output a Secchi series, so the upper-5 m extinction coefficient is compared with the observed Kd =
# VLEC where measured, else 1.85 / Secchi; TSS against turbidity at 1.5 g/m3 per NTU; temperature guard)
light_obs_all <- function() { D <- readRDS(file.path("website", "images", "light", "light_obs.rds")); D$kd <- ifelse(!is.na(D$vlec), D$vlec, 1.85 / D$secchi); D[, c("Date", "kd", "turb")] }
light_eval <- function(aeme, path, param) {
  res <- tryCatch(aemetools::run_aeme_param(aeme, param = param[, PCOLS], model = MODEL, path = path, return_aeme = TRUE), error = function(e) NULL)
  if (is.null(res) || identical(res, 999) || !inherits(res, "Aeme")) return(NULL)
  P <- csps_pair(res, vars = "HYD_temp")
  nc <- ncdf4::nc_open(file.path(AEME::get_lake_dir(res, path), "glm_aed", "output", "output.nc")); on.exit(ncdf4::nc_close(nc))
  t0 <- ncdf4::ncatt_get(nc, "time", "units")$value; orig <- as.Date(strsplit(t0, " ")[[1]][3]); tm <- ncdf4::ncvar_get(nc, "time")
  dts <- if (grepl("hour", t0)) orig + floor(tm / 24) else orig + tm; NS <- ncdf4::ncvar_get(nc, "NS"); Z <- ncdf4::ncvar_get(nc, "z"); m <- ncdf4::ncvar_get(nc, "TOT_tss"); mk <- ncdf4::ncvar_get(nc, "extc")
  up5 <- function(a) vapply(seq_along(NS), function(i) { n <- NS[i]; h <- Z[seq_len(n), i]; th <- diff(c(0, h)); k <- (h[n] - (h - th / 2)) <= 5; sum(a[seq_len(n)[k], i] * th[k]) / sum(th[k]) }, 1)
  T <- merge(light_obs_all(), data.frame(Date = dts, tss = up5(m), extc = up5(mk)), by = "Date"); T <- T[T$Date >= as.Date(SA_START), ]
  ev <- function(win) {
    tw <- T[win(T$Date), ]; sk <- tw[!is.na(tw$kd), ]; tb <- tw[!is.na(tw$turb), ]; y <- setNames(rep(0, length(STEP2_NAMES)), STEP2_NAMES)
    if (nrow(sk) >= MIN_PAIRS) { e <- log(sk$extc / sk$kd); y["kd_bias"] <- mean(e) / 0.15; y["kd_rmse"] <- sqrt(mean(e^2)) / 0.3; y["kd_r"] <- 1 - suppressWarnings(cor(sk$extc, sk$kd)) }
    if (nrow(tb) >= MIN_PAIRS) { e <- log((tb$tss + 0.5) / (1.5 * tb$turb + 0.5)); y["tss_bias"] <- mean(e) / 0.5; y["tss_rmse"] <- sqrt(mean(e^2)) / 1.0; y["tss_r"] <- 1 - suppressWarnings(cor(tb$tss, tb$turb)) }
    d <- P$HYD_temp[win(P$HYD_temp$Date), ]; if (!is.null(d) && nrow(d) >= 50) { y["temp_bias"] <- mean(d$sim - d$obs) / 0.5; y["temp_rmse"] <- sqrt(mean((d$sim - d$obs)^2)) / 1.0 }
    y[!is.finite(y)] <- PENALTY2; y }
  list(cal = ev(function(d) d <= CAL_END2), val = ev(function(d) d > CAL_END2))
}
step2_eval <- function(aeme, path, param) {
  if (STEP2L) return(light_eval(aeme, path, param))
  if (STEP2A) return(step3_eval(aeme, path, param))
  res <- tryCatch(aemetools::run_aeme_param(aeme, param = param[, PCOLS], model = MODEL, path = path,
                                            return_aeme = TRUE), error = function(e) NULL)
  if (is.null(res) || identical(res, 999) || !inherits(res, "Aeme")) return(NULL)
  P <- csps_pair(res, vars = c(STEP2_VARS$variable, "HYD_temp", "CHM_oxy"))
  if (is.null(P$PHY_tchla) && is.null(P$PHS_tp)) return(NULL)
  if (STEP2_FIX_TOTALS) {
    if (!is.null(P$NIT_tn)) P$NIT_tn$sim <- P$NIT_tn$sim + DONR_GM3
    if (!is.null(P$PHS_tp)) P$PHS_tp$sim <- P$PHS_tp$sim + DOPR_GM3
  }
  cal <- lapply(P, \(d) d[d$Date <= CAL_END2, ]); val <- lapply(P, \(d) d[d$Date > CAL_END2, ])
  fin <- function(y) { y[!is.finite(y)] <- PENALTY2; y }
  list(cal = fin(step2_resid(cal)), val = fin(step2_resid(val)))
}
