# Shared setup for the Morris sensitivity analysis (SA) with CSPS outputs.
# Run from the project root. Uses the daily-forced base model (aeme_base_daily.rds).
# SA_VARIANT: "alum" (alum module on, dosed inflows, alum parameters varied; default) or
# "noalum" (alum dose set to zero; same design, alum parameters still sampled but inert).
SA_VARIANT <- Sys.getenv("SA_VARIANT", "alum")
# SA_TAG: suffix for other forcing/window setups (e.g. "_hourly"); SA_BASE/SA_START/SA_STOP/SA_SPINUP set them.
SA_TAG <- Sys.getenv("SA_TAG", "")
SA_DIR <- Sys.getenv("SA_DIR_OVERRIDE", paste0(if (identical(SA_VARIANT, "alum")) file.path("calibration", "sa_runs") else
  file.path("calibration", paste0("sa_runs_", SA_VARIANT)), SA_TAG))
# PEST_PATCHED_AEMETOOLS defaults to off: the patched aemetools copy (calibration/lib_aemetools_patched) predates the
# current aemetools and would shadow it. Only the PEST stage scripts need its log1p region transform.
Sys.setenv(PEST_PATCHED_AEMETOOLS = Sys.getenv("PEST_PATCHED_AEMETOOLS", "0"),
           PEST_CALIB_ROOT = file.path(getwd(), SA_DIR, "build"),
           PEST_CHECKPOINT_ROOT = file.path(SA_DIR, "ckpt"))
source("calibration/00_config.R")
source("calibration/csps_metrics.R")
source("calibration/sa/sa_params.R")

SA_BASE   <- Sys.getenv("SA_BASE", "aeme_base_daily.rds")
# Scored window: 3 years with a dry-ish and a wet-ish part, daily temperature/oxygen obs, regular
# nutrient sampling. Spin-up is 1.5 years: the same for every run, so it does not bias the effects.
SA_START  <- Sys.getenv("SA_START", "2010-07-01")
SA_STOP   <- Sys.getenv("SA_STOP",  "2013-06-30")
SA_SPINUP <- as.numeric(Sys.getenv("SA_SPINUP", "548"))
SA_VARS   <- c("HYD_temp", "CHM_oxy", "PHY_tchla", "PHS_frp", "NIT_amm", "NIT_nit", "PHS_tp", "NIT_tn")
dir.create(SA_DIR, recursive = TRUE, showWarnings = FALSE)

# Build the model once; returns list(aeme, path). The built directory is copied per worker.
sa_build <- function(tag = "sa_base") {
  a <- readRDS(SA_BASE)
  a <- thin_lake_obs(a)
  if (identical(SA_VARIANT, "noalum")) {      # no alum dosed: scale the dosed inflow ALU_ala column to zero
    aeme0 <<- a
    a <- scale_alum_dose(0)
  }
  meas <- as.data.frame(targets::tar_read(meas_param))
  meas <- meas[grepl("^aed_sed_const2d/fsed_(oxy|amm|nit|frp)$", meas$name) & meas$index %in% 2:5, ]
  meas$index <- meas$index - 1L
  meas$group <- NA_character_
  a <- AEME::add_param(a, meas[, PCOLS])
  a <- AEME::set_time(a, start = SA_START, stop = SA_STOP, spin_up = SA_SPINUP)
  utils::data("aeme_parameters", package = "AEME", envir = environment())
  utils::data("aeme_parameters_bgc", package = "AEME", envir = environment())
  POOL <<- unique(rbind(aeme_parameters[aeme_parameters$model == MODEL, PCOLS],
                        aeme_parameters_bgc[aeme_parameters_bgc$model == MODEL, PCOLS]))
  aeme0 <<- a
  period <<- list(start = as.Date(SA_START), stop = as.Date(SA_STOP))
  SWITCHES <<- read_switches(SWITCH_CSV)
  fresh_build_alum(tag, SA_VARS)
}

# Calibrated baseline (alum stage result) -> the SA parameter table
sa_table <- function(b) {
  full <- utils::read.csv(file.path("calibration", "checkpoints", "final", "alum_params.csv"),
                          stringsAsFactors = FALSE)
  cfg_glm <- AEME::configuration(b$aeme)$glm_aed
  ph <- utils::read.csv(list.files(b$path, "aed_phyto_pars.csv", recursive = TRUE, full.names = TRUE)[1], header = FALSE,
                        stringsAsFactors = FALSE)
  ph[] <- lapply(ph, function(z) gsub("'", "", z))
  names(ph) <- ph[1, ]; ph <- ph[-1, ]
  # The no-alum variant keeps the alum parameters in the design (they have no effect without a
  # dose), so both variants share the same sampling design and runs can be compared pairwise.
  tab <- sa_param_table(full, cfg_glm, ph)
  tab
}

# One model evaluation -> named numeric vector of CSPS outputs (NA vector on a failed run)
sa_eval <- function(aeme, path, param) {
  res <- tryCatch(aemetools::run_aeme_param(aeme, param = param[, PCOLS], model = MODEL,
                                            path = path, return_aeme = TRUE),
                  error = function(e) { message("run_aeme_param error: ", conditionMessage(e)); NULL })
  if (is.null(res) || identical(res, 999) || !inherits(res, "Aeme")) {
    message("run failed; class = ", paste(class(res), collapse = "/"))
    return(NULL)
  }
  P <- csps_pair(res)
  if (!length(P)) return(NULL)
  sig <- csps_signatures(P, "sim")
  st <- csps_state(P)
  flat <- c()
  if (nrow(st)) {
    for (i in seq_len(nrow(st))) {
      k <- paste0("st_", st$variable[i], "_", gsub("[^A-Za-z]+", "", st$region[i]))
      flat[paste0(k, "_bias")] <- st$bias[i]
      flat[paste0(k, "_rmse")] <- st$RMSE[i]
    }
  }
  c(sig, flat)
}
