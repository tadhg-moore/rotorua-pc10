# Helpers to run alum scenarios on the calibrated Rotorua GLM-AED model.
# Requires calibration/00_config.R to have been sourced (provides pest_funs.R:
# fix_mixing(), fix_noncohesive(), apply_switches(), SWITCHES, MODEL, PCOLS).

ALUM_VARS <- c("ALU_ala", "ALU_alap")
# Same module set as build_calib_base_aeme() (aed_alum last, no aed_oasim) and
# the &aed_alum defaults it was built with.
ALUM_AED_MODELS <- c("aed_sedflux", "aed_noncohesive", "aed_oxygen", "aed_silica",
                     "aed_nitrogen", "aed_phosphorus", "aed_organic_matter",
                     "aed_phytoplankton", "aed_totals", "aed_alum")
ALUM_NML_FILE <- file.path("calibration", "base_build", "LID11133_rotorua",
                           "glm_aed", "aed", "aed.nml")
ALUM_DEFAULTS <- c(k_w = 0.5, k_s = 0.1, K_P = 0.3, w_ala = 0.5,
                   k_age_w = 0.0998, k_age_s = 0.0998, k_bur = 0.01,
                   k_res = 0, sigma_pH = 1, pH_opt = 6, S_ala = 0)

#' Calibrated (stage 1-3) parameter set, optionally with alum overrides.
#' @param alum named numeric of aed_alum parameters to override.
alum_param_table <- function(alum = NULL,
                             bgc_csv = "calibration/checkpoints/final/bgc_params.csv") {
  p <- utils::read.csv(bgc_csv, stringsAsFactors = FALSE)[, PCOLS]
  if (length(alum)) {
    a <- data.frame(model = MODEL, file = "aed.nml",
                    name = paste0("aed_alum/", names(alum)), value = unname(alum),
                    min = unname(alum), max = unname(alum),
                    group = NA_character_, index = NA_integer_)
    p <- rbind(p, a)
  }
  p
}

#' Scale / switch off dosing in the alum-dosed inflows.
#' @param inflows named list of inflow data frames (ALU_ala in mmol/m3).
#' @param scale multiplier on ALU_ala (0 = no alum).
#' @param streams which streams are dosed (others set to 0).
#' @param stop_date dosing set to 0 after this date (NULL = never).
#' @param start_date dosing set to 0 before this date (NULL = never).
scale_alum_inflows <- function(inflows, scale = 1,
                               streams = c("Puarenga", "Utuhina"),
                               stop_date = NULL, start_date = NULL) {
  inflows$Rainfall <- NULL
  for (nm in names(inflows)) {
    k <- if (nm %in% streams) scale else 0
    d <- inflows[[nm]]
    d$ALU_ala <- d$ALU_ala * k
    if (!is.null(stop_date))  d$ALU_ala[d$Date > as.Date(stop_date)]  <- 0
    if (!is.null(start_date)) d$ALU_ala[d$Date < as.Date(start_date)] <- 0
    inflows[[nm]] <- d
  }
  inflows
}

#' AEME's model_controls call the alum-bound P pool "ALU_alap", but libaed's
#' aed_alum names it "ALU_alp" (GLM aborts: Cannot find "ALU_alap"). Rename it
#' in the GLM namelist and inflow files of a built model.
fix_alum_var_names <- function(p) {
  fs <- list.files(p, "(glm4?\\.nml|inflow_.*\\.csv)$", recursive = TRUE, full.names = TRUE)
  for (f in fs) {
    x <- readLines(f, warn = FALSE)
    if (any(grepl("ALU_alap", x, fixed = TRUE)))
      writeLines(gsub("ALU_alap", "ALU_alp", x, fixed = TRUE), f)
  }
  invisible(fs)
}

#' Stop unless aed_alum is enabled in the namelists under `p`.
check_alum_active <- function(p, stage = "build") {
  glm <- list.files(p, "glm4?\\.nml$", recursive = TRUE, full.names = TRUE)[1]
  aed <- list.files(p, "^aed\\.nml$", recursive = TRUE, full.names = TRUE)[1]
  ok <- any(grepl("ALU_ala", readLines(glm))) && any(grepl("^&aed_alum", readLines(aed)))
  if (!ok) stop("aed_alum is not active in the ", stage, " namelists under ", p)
  invisible(TRUE)
}

#' Build + run one scenario in <root>/<tag> and return the Aeme object.
#' @param aeme_base Aeme read from aeme_base.rds (already holds alum variables).
#' @param inflows inflow list (see scale_alum_inflows()).
#' @param alum named numeric of aed_alum overrides.
#' @param alum_nml named list of non-numeric &aed_alum settings, e.g. list(simResuspension = TRUE).
#' @param start,stop,spin_up simulation window.
#' @param vars_extra additional AED variables to simulate/output.
run_alum_scenario <- function(tag, aeme_base, inflows, alum = NULL,
                              start = "2016-07-01", stop = "2021-06-30",
                              spin_up = 730, root = file.path("scripts", "alum", "runs"),
                              aed_model_sel = ALUM_AED_MODELS,
                              alum_nml_file = ALUM_NML_FILE,
                              alum_nml = NULL,
                              vars_extra = c("PHS_frp", "PHS_tp", "PHY_tchla",
                                             "CHM_oxy", "HYD_temp")) {
  p <- file.path(root, tag)
  unlink(p, recursive = TRUE); dir.create(p, recursive = TRUE, showWarnings = FALSE)

  a <- AEME::remove_inflow(aeme_base, all = TRUE)
  a <- AEME::add_inflows(a, inflows)
  a <- AEME::set_time(a, start = start, stop = stop, spin_up = spin_up)

  mc <- AEME::get_model_controls(a, use_bgc = TRUE) |>
    AEME::set_vars_sim(vars_sim = unique(c(vars_extra, ALUM_VARS)))
  idx <- mc$var_aeme %in% ALUM_VARS
  mc$inf_default[idx] <- 0; mc$initial_wc[idx] <- 0

  a <- AEME::build_aeme(path = p, aeme = a, model = MODEL, model_controls = mc,
                        ext_elev = 5, use_bgc = TRUE, mass_balance = F)
  a <- fix_mixing(a, p)
  a <- fix_noncohesive(a, p)
  n_zones <- AEME::configuration(a)[[MODEL]]$hydrodynamic$sediment$n_zones
  a <- apply_switches(aeme = a, path = p, switches = SWITCHES,
                      groups = PHYTO_GROUPS, n_zones = n_zones)$aeme

  # build_aeme(use_bgc = TRUE) writes an aed.nml without aed_alum, and GLM-AED
  # then drops the ALU_* inflows/initial conditions. apply_switches() rewrites
  # aed.nml, so re-enable the module afterwards (as build_calib_base_aeme()
  # does) and append its namelist block (must stay last).
  a <- AEME::set_glm_aed_models(a, path = p, aed_models = aed_model_sel)
  aed_file <- AEME::get_model_config_files(a, path = p)$glm_aed["aed"]
  nml <- AEME::read_nml(aed_file)
  nml[["aed_alum"]] <- AEME::read_nml(alum_nml_file)[["aed_alum"]]
  # logical/character &aed_alum settings that cannot go through the numeric
  # parameter table (e.g. simResuspension = TRUE)
  for (k in names(alum_nml)) nml[["aed_alum"]][[k]] <- alum_nml[[k]]
  AEME::write_nml(nml, aed_file)
  check_alum_active(p)
  fix_alum_var_names(p)
  
  # remove mass_balance from glm nml file
  glm_file <- AEME::get_model_config_files(a, path = p)$glm_aed["glm4"]
  nml <- AEME::read_nml(glm_file)
  nml[["mass_balance"]] <- NULL
  AEME::write_nml(nml, glm_file)

  # AEME::run_aeme(a)
  res <- aemetools::run_aeme_param(a, param = alum_param_table(alum), model = MODEL,
                                   path = p, return_aeme = TRUE)
  out <- load_output(aeme = a, path = p, keep_diag = TRUE, extra_vars = "all", 
                     zone_summary = TRUE)
  outp <- output(res)
  if (identical(res, 999)) stop("run failed: ", tag)
  # run_aeme_param() returns the Aeme object even when GLM aborted.
  if (!file.exists(file.path(p, "LID11133_rotorua", "glm_aed", "output", "output.nc")) ||
      is.null(AEME::output(res)[["ens_001"]][[MODEL]]))
    stop("GLM-AED produced no output for ", tag, " (see ", p, ")")
  out
}

#' Lake-mean (depth-averaged) daily series of simulated variables.
lake_mean_series <- function(aeme, vars, tag) {
  dplyr::bind_rows(lapply(vars, function(v) {
    d <- tryCatch(AEME::get_var(aeme, model = MODEL, var_sim = v, remove_spin_up = TRUE),
                  error = function(e) NULL)
    if (is.null(d)) return(NULL)
    d |> dplyr::group_by(Date) |>
      dplyr::summarise(value = mean(.data[[intersect(c("sim", "value"), names(d))[1]]],
                                    na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(var = v, scenario = tag)
  }))
}
