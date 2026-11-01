# Shared model configuration for the PEST workflow runners (ensembles,
# scenarios). Must stay in step with pest_calib_v6.R: anything that changes how
# a lake model is BUILT has to be identical at calibration and at forward-run
# time, otherwise the calibrated parameters are applied to a different model.
#
# NOTE: OUTLET_OVERRIDE / MIXING_OVERRIDE used to be defined only inside
# pest_calib_v6.R, so scripts that sourced just pest_funs.R (e.g.
# generate_pest_ensemble_output.R) silently skipped them - fix_outlet_depth()
# and fix_mixing() test exists("..._OVERRIDE"). Te Kanae, Pehiakura Small and
# Pupuke ensembles were therefore built with a different outlet/mixing setup
# than they were calibrated with. TODO: make pest_calib_v6.R source this file
# (or the future recipes/*.yml) so there is one definition.

MODEL <- "glm_aed"
LOCAL_ROOT <- Sys.getenv("PEST_LOCAL_ROOT", "C:/Users/data/Desktop/ac_lake_calib/pest_workflow")   # bulky Aeme rds + scratch builds
ROOT <- file.path(LOCAL_ROOT, "_builds")                            # used by fresh_build()
CHECKPOINT_ROOT <- Sys.getenv("PEST_CHECKPOINT_ROOT", "staged-calibration-v4/checkpoints")
dir.create(ROOT, recursive = TRUE, showWarnings = FALSE)

# pest_funs.R moved from the project root into scripts/ (2026-10-02)
source(Find(file.exists, c("scripts/pest_funs.R", "pest_funs.R")))

OUTLET_OVERRIDE <- list(
  LID49895 = list(elev = 49.3 + 13.41),  # Te Kanae: base_elev + stage-2 median level
  LID49307 = list(elev = 72.61)          # Pehiakura Small: base_elev + median non-zero level
)
MIXING_OVERRIDE <- list(
  LID50151 = list(name = "deep_mixing", value = 1)   # Pupuke
)

SWITCH_CSV <- file.path("data", "processed", "aed_sa_calib_switches.csv")
SWITCHES <- read_switches(SWITCH_CSV)
n_glm_zones <- 3L
PHYTO_GROUPS <- c("cyano", "green", "diatom")
SWITCH_CHOICE <- list("aed_phosphorus/PO4AdsorptionModel" = 1, "aed_phytoplankton/settling" = 1)
SWITCH_SKIP <- c("aed_nitrogen/oxy_lim")
PHYTO_SWITCH_GROUPS <- list(simSiUptake = "diatom", simNFixation = "cyano")
PARAM_ALIAS <- c("aed_organic_matter/rdoc_minerl" = "aed_organic_matter/rdom_minerl")
STRICT_TABLES <- TRUE

vars_bgc_all <- c("LKE_lvlwtr", "HYD_temp", "HYD_thmcln", "CHM_oxy", "CHM_oxynal", "CHM_oxycln",
                  "NIT_amm", "NIT_nit", "NIT_tn", "PHS_frp", "PHS_tp",
                  "PHY_tchla", "PHY_cyano", "PHY_green", "PHY_diatom", "NCS_ss1")
ANCHOR_VARS <- c("HYD_temp", "HYD_schstb")
VARS_SIM <- unique(c(vars_bgc_all, ANCHOR_VARS))
PARAM_COLS <- c("model", "file", "name", "value", "min", "max", "group", "index")
