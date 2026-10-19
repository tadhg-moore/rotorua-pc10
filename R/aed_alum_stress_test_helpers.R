# Helper functions for glm_aed_alum_stress_tests.qmd
#
# Running a scenario (write_inflow_glm() / set_outflow_glm() / run_test() /
# safe_run()), pulling variables back out of the GLM output (get_var(),
# surface(), bottom(), col_mean(), col_sum(), wc_mass(), benthic_mass()),
# and recording pass/fail/error results without stopping the render
# (check()).
#
# run_test()/safe_run() expect the following objects to already exist in the
# calling (global) environment: `aed` (case AED defaults, as read by
# `AEME::read_nml()`), `aed_models` (character vector of AED models to
# include), `def_alum_nml`/`def_carbon_nml` (stress-test defaults for the
# aed_alum/aed_carbon blocks) and `glm_def` (default GLM namelist, used when
# `run_test()`'s `glm_nml` argument is not supplied). check() expects a
# `.test_results` data frame to already exist in the calling environment.

# ── Inflow / outflow helpers ───────────────────────────────────────────────

#' Write a constant-concentration GLM inflow
#'
#' Builds a GLM inflow CSV with a constant FRP concentration (and, optionally,
#' a constant ALU_ala concentration) and points `glm_nml` at it. Used to hold
#' a maintained FRP (and/or alum) load ahead of a stress-test run. Pass
#' `frp_conc = 0` and `flow = 0` to effectively remove the inflow.
#'
#' @param glm_nml GLM namelist list (as read by `AEME::read_nml()`) to modify.
#' @param frp_conc Constant FRP concentration to write into the inflow (mmol P/m3).
#' @param flow Constant inflow flow rate (m3/d).
#' @param ala_conc Constant ALU_ala concentration to write into the inflow
#'   (mmol P/m3); omitted from the CSV when `<= 0`.
#' @param enabled If `FALSE`, disables the inflow (`num_inflows <- 0`)
#'   without writing the CSV — an alternative to `frp_conc = 0, flow = 0`.
#' @param inflow_dir Directory holding the inflow template/target CSVs.
#'
#' @return The modified GLM namelist list, invisibly. Also writes the inflow
#'   CSV and "LakeRotorua/glm4.nml" as a side effect. Pass the returned
#'   object as `run_test()`'s/`safe_run()`'s `glm_nml` argument so the
#'   inflow isn't lost when the AED namelist is rewritten.
#'
write_inflow_glm <- function(glm_nml, frp_conc, flow,
                              ala_conc = 0,
                             enabled = TRUE,
                              inflow_dir = "LakeRotorua/bcs") {
  
  if (enabled) {
    # Read the base inflow template (Date + flow columns expected)
    base_file <- file.path(inflow_dir, "inflow_Hamurana.csv")
    example_file <- file.path(inflow_dir, "inflow_example.csv")
    if (!file.exists(base_file)) {
      # If no template, build a minimal daily inflow over the simulation period
      sim_start <- as.Date(glm_nml$glm_setup$start)
      sim_stop  <- as.Date(glm_nml$glm_setup$stop)
      dates <- seq(sim_start, sim_stop, by = "day")
      base  <- data.frame(Date = format(dates, "%Y-%m-%d"),
                          FLOW = flow,
                          TEMP = 15.0, SALT = 0.0,
                          stringsAsFactors = FALSE)
    } else {
      base <- read.csv(base_file, stringsAsFactors = FALSE)
      base$flow <- flow
    }
    base$PHS_frp <- frp_conc
    if (ala_conc > 0) base$ALU_ala <- ala_conc
    write.csv(base, example_file, row.names = FALSE, quote = FALSE)
    num_inflows <- 1
  } else {
    num_inflows <- 0
  }
  # Update GLM nml to use this inflow file
  glm_mod <- glm_nml
  glm_mod$inflow$num_inflows <- num_inflows
  glm_mod$inflow$names_of_strms[1] <- "ExampleInflow"
  glm_mod$inflow$inflow_fl <- "bcs/inflow_example.csv"
  AEME::write_nml(glm_mod, file = "LakeRotorua/glm4.nml")
  invisible(glm_mod)
}

#' Enable or disable the GLM outlet
#'
#' @param glm_nml GLM namelist list to modify.
#' @param enabled If `FALSE`, sets `num_outlet` to 0 (disables all outflows).
#' @param outdir Directory to write the resulting "glm4.nml" into.
#'
#' @return The modified GLM namelist list, invisibly. Also writes
#'   "<outdir>/glm4.nml" as a side effect.
#'
set_outflow_glm <- function(glm_nml, enabled = TRUE, outdir = "LakeRotorua") {
  glm_mod <- glm_nml
  if (!enabled) {
    glm_mod$outflow$num_outlet <- 0L
  }
  AEME::write_nml(glm_mod, file = file.path(outdir, "glm4.nml"))
  invisible(glm_mod)
}

#' Update GLM initial-condition profiles (temperature and/or water-quality vars)
#'
#' Overwrites entries in the `&init_profiles` block of a GLM namelist:
#' `the_temps` and/or the matching slice of `wq_init_vals` for one or more
#' variables named in `wq_names`. `wq_init_vals` is stored as a single flat
#' vector with depths nested inside each variable (`var1_depth1,
#' var1_depth2, ..., var2_depth1, ...`), so updates are done via a
#' `num_depths` x `num_wq_vars` reshape rather than direct indexing.
#'
#' @param glm_nml GLM namelist list to modify.
#' @param temp Optional replacement for `the_temps` (deg C): a single value
#'   (broadcast to every depth) or a vector the same length as `the_depths`.
#' @param wq_init Optional named list of replacement initial values, named by
#'   the variable's entry in `wq_names` (e.g. `list(PHS_frp = 0.5)`). Each
#'   value is a single number (broadcast across depths) or a vector the same
#'   length as `the_depths`. Names not found in `wq_names` raise an error.
#'
#' @return The modified GLM namelist list, invisibly. Does not write to disk
#'   — pass the result as `run_test()`'s/`safe_run()`'s `glm_nml` argument,
#'   or use their `init_temp`/`init_wq` arguments to have this applied
#'   automatically.
#'
set_glm_init <- function(glm_nml, temp = NULL, wq_init = NULL) {
  ip <- glm_nml$init_profiles
  n_depths <- ip$num_depths

  if (!is.null(temp)) {
    ip$the_temps <- rep_len(temp, n_depths)
  }

  if (!is.null(wq_init) && length(wq_init) > 0) {
    wq_names <- ip$wq_names
    vals <- matrix(ip$wq_init_vals, nrow = n_depths, ncol = length(wq_names))
    for (v in names(wq_init)) {
      idx <- match(v, wq_names)
      if (is.na(idx)) {
        stop("set_glm_init: '", v, "' not found in wq_names (",
             paste(wq_names, collapse = ", "), ")")
      }
      vals[, idx] <- rep_len(wq_init[[v]], n_depths)
    }
    ip$wq_init_vals <- as.vector(vals)
  }

  glm_nml$init_profiles <- ip
  invisible(glm_nml)
}

# ── Run helpers ─────────────────────────────────────────────────────────────

#' Run one GLM-AED stress-test scenario
#'
#' Rebuilds `aed/aed.nml` from the shared `aed` case defaults (with the
#' `aed_alum`/`aed_carbon` blocks reset to the stress-test defaults
#' `def_alum_nml`/`def_carbon_nml`), applies `overrides` on top, writes
#' `glm_nml` to "glm4.nml", runs GLM-AED, and reads back the target output
#' variables.
#'
#' @param overrides Named list of named lists, one per AED namelist block to
#'   modify, e.g. `list(aed_alum = list(k_w = 0.5), aed_phosphorus = list(ksed_frp = 0))`.
#'   Parameters not named keep their default value; blocks not named are
#'   left untouched.
#' @param verbose Passed through to `run_glm_aed()`.
#' @param glm_nml GLM namelist list to write as "glm4.nml" before running.
#'   Defaults to `glm_def` (no inflow). Pass the object returned by
#'   `write_inflow_glm()`/`set_outflow_glm()` to run with a maintained
#'   inflow or a disabled outlet — passing the case default here instead
#'   would silently discard that configuration.
#' @param init_temp Optional initial temperature profile override, applied
#'   to `glm_nml` via `set_glm_init()` before running: a single value
#'   (broadcast to every depth) or a vector the same length as
#'   `glm_nml$init_profiles$the_depths`.
#' @param init_wq Optional named list of initial water-quality value
#'   overrides, applied to `glm_nml` via `set_glm_init()` before running —
#'   see that function's `wq_init` argument.
#'
#' @return A list of the target output variables (as returned by
#'   `read_glm_output()`), with a `zarea` element (lake surface area per
#'   layer) and a `mass_balance` element (see `read_mass_balance()`; `NULL`
#'   if `&mass_balance` wasn't configured on `glm_nml`) added. See
#'   `safe_run()` for a version that traps run errors.
#'
run_test <- function(overrides = list(), verbose = FALSE, glm_nml = glm_def,
                      init_temp = NULL, init_wq = NULL) {
  # setwd() first so every relative path below (incl. the on.exit cleanup)
  # is consistent regardless of where run_test() is called from.
  setwd("LakeRotorua/")
  on.exit(setwd("../"), add = TRUE)

  if (!is.null(init_temp) || !is.null(init_wq)) {
    glm_nml <- set_glm_init(glm_nml, temp = init_temp, wq_init = init_wq)
  }

  outfile <- "output/output.nc"
  unlink(outfile, force = TRUE)

  full_aed <- aed
  full_aed[["aed_alum"]]   <- def_alum_nml[["aed_alum"]]
  full_aed[["aed_carbon"]] <- def_carbon_nml[["aed_carbon"]]

  # overrides is nested by block: list(aed_alum = list(...), aed_phosphorus = list(...))
  for (blck in names(overrides)) {
    for (nm in names(overrides[[blck]])) {
      full_aed[[blck]][[nm]] <- overrides[[blck]][[nm]]
    }
  }

  # Arrange full_aed to match aed_models
  upd_aed <- list()
  upd_aed[["aed_models"]] <- full_aed[["aed_models"]]
  for (m in aed_models) {
    if (!is.null(full_aed[[m]])) upd_aed[[m]] <- full_aed[[m]]
    if (m == "aed_sedflux") upd_aed[["aed_sed_const2d"]] <- full_aed[["aed_sed_const2d"]]
  }
  class(upd_aed) <- "nml"

  AEME::write_nml(upd_aed, file = "aed/aed.nml")
  AEME::write_nml(glm_nml, "glm4.nml")
  run_glm_aed(sim_folder = ".", verbose = verbose)

  tgt_vars <- c("CHM_ph",
                "OXY_oxy",
                "ALU_ala", "ALU_alp", "ALU_ben_ala", "ALU_ben_alp",
                "ALU_uptake_w", "ALU_uptake_s", "ALU_fpH", "PHS_frp", "z", "NS",
                "V",
                "SDF_Fsed_frp_Z",
                "ALU_ben_ala_Z", "ALU_ben_alp_Z", "ALU_uptake_s_Z")
  out <- read_glm_output(file = outfile, vars_sim = tgt_vars, load_all = FALSE)
  nc <- ncdf4::nc_open(outfile)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  out[["zarea"]] <- ncdf4::ncvar_get(nc, "zarea")
  out[["mass_balance"]] <- read_mass_balance(glm_nml)
  return(out)
}

#' Run a stress-test scenario, catching errors
#'
#' Wraps `run_test()` in `tryCatch()` so a failed GLM-AED run returns `NULL`
#' (with a message) instead of stopping the whole Quarto render — individual
#' test chunks check `is.null(out)` and record a failed check instead of
#' aborting the document.
#'
#' @param overrides See `run_test()`.
#' @param verbose See `run_test()`.
#' @param glm_nml See `run_test()`.
#' @param init_temp See `run_test()`.
#' @param init_wq See `run_test()`.
#'
#' @return The result of `run_test()`, or `NULL` on error.
#'
safe_run <- function(overrides = list(), verbose = FALSE, glm_nml = glm_def,
                      init_temp = NULL, init_wq = NULL) {
  tryCatch(
    run_test(overrides, verbose = verbose, glm_nml = glm_nml,
             init_temp = init_temp, init_wq = init_wq),
    error = function(e) {
      message("run_test error: ", conditionMessage(e)); NULL
    })
}

# ── Extraction helpers ────────────────────────────────────────────────────

#' Extract one output variable
#'
#' @param out Output list returned by `run_test()`/`safe_run()`.
#' @param v Variable name.
#'
#' @return The variable's matrix/vector as stored in `out`.
#'
get_var  <- function(out, v) out[[v]]

#' Surface-layer time series from a layer x time matrix
#'
#' @param mat A layer x time matrix (top/surface layer is the last row,
#'   per GLM output convention).
#'
#' @return Numeric vector, one value per timestep.
#'
surface  <- function(out, v) out[[v]][nrow(out[[v]]), ]

#' Bottom-layer time series from a layer x time matrix
#'
#' @param mat A layer x time matrix (bottom layer is the first row,
#'   per GLM output convention).
#'
#' @return Numeric vector, one value per timestep.
#'
bottom   <- function(out, v) out[[v]][1, ]

#' Depth-mean time series for a variable
#'
#' @param out Output list returned by `run_test()`/`safe_run()`.
#' @param v Variable name.
#'
#' @return Numeric vector of per-timestep column means (NA layers dropped).
#'
col_mean <- function(out, v) {
  dat <- get_var(out, v)
  if (is(dat, "aeme_grouped_var")) {
    dat <- dat$value
  }
  colMeans(dat, na.rm = TRUE)
}

#' Depth-summed time series for a variable
#'
#' @param out Output list returned by `run_test()`/`safe_run()`.
#' @param v Variable name.
#'
#' @return Numeric vector of per-timestep column sums (NA layers dropped).
#'
col_sum <- function(out, v) {
  dat <- get_var(out, v)
  if (is(dat, "aeme_grouped_var")) {
    dat <- dat$value
  }
  colSums(dat, na.rm = TRUE)
}

#' Water-column total mass of a pelagic variable
#'
#' @param out Output list returned by `run_test()`/`safe_run()`; must contain
#'   `out[["V"]]`, the per-layer volume (m3).
#' @param v Variable name, in mmol/m3.
#' @param conv_fact Optional unit-conversion multiplier applied to the result.
#'
#' @return Numeric vector (mmol), one value per timestep: sum of
#'   concentration x layer volume across all layers.
#'
wc_mass <- function(out, v, conv_fact = 1) {
  colSums(out[[v]] * out[["V"]], na.rm = TRUE) * conv_fact
}

#' Benthic total mass of an areal (sheet) variable
#'
#' @param out Output list returned by `run_test()`/`safe_run()`; must contain
#'   `out[["zarea"]]`, the lake surface area (m2).
#' @param v Variable name, in mmol/m2.
#' @param conv_fact Optional unit-conversion multiplier applied to the result.
#'
#' @return Numeric vector (mmol), one value per timestep: areal concentration
#'   x lake surface area.
#'
benthic_mass <- function(out, v, conv_fact = 1) {
  mass <- get_var(out, v)
  colSums(mass$value * out[["zarea"]], na.rm = TRUE) * conv_fact
}

# ── Non-stopping check infrastructure ────────────────────────────────────

#' Record a non-stopping pass/fail/error check
#'
#' Evaluates `condition` and appends a row to the global `.test_results`
#' data frame (created in the calling script/document) instead of stopping
#' execution — lets the Quarto document render to completion and collect
#' every check's outcome in one summary table.
#'
#' @param id Short test identifier, e.g. "2.2".
#' @param label Human-readable description of the check.
#' @param condition Logical value (or expression evaluating to one) to check.
#' @param detail Optional extra detail string; auto-filled on FAIL if blank.
#'
#' @return The status string ("PASS"/"FAIL"/"ERROR"), invisibly. Also updates
#'   `.test_results` in the calling environment as a side effect.
#'
check <- function(id, label, condition, detail = "") {
  status <- tryCatch(
    { if (isTRUE(condition)) "PASS" else "FAIL" },
    error = function(e) "ERROR"
  )
  if (detail == "" && status == "FAIL") detail <- "condition evaluated FALSE"
  .test_results <<- rbind(.test_results, data.frame(
    id = id, label = label, status = status,
    detail = as.character(detail), stringsAsFactors = FALSE
  ))
  invisible(status)
}

#' Read the GLM water/mass-balance diagnostic CSV
#'
#' Reads the `&mass_balance` output file (`glm_balance.c`'s `write_balance()`,
#' called once per output step). Each row is a timestamp plus three columns
#' per tracked variable, in this fixed order: inflow total (load accumulated
#' since the previous write), lake total (instantaneous in-lake stock, Sum
#' over layers of value x LayerVol) and outflow total (load accumulated
#' since the previous write) — i.e. `Delta(lake) ~= inflow - outflow` over
#' each interval. GLM's own CSV header mislabels every variable after the
#' first (`glm_balance.c` reuses one buffer when formatting the header) and
#' the columns it prints for each variable don't line up with that header
#' order either (a known GLM output bug) — so the header is discarded and
#' columns are renamed positionally, in `inf`/`out`/`lake` order per
#' variable, from `glm_nml$mass_balance$balance_vars`.
#'
#' @param glm_nml GLM namelist list (as read by `AEME::read_nml()`, or the
#'   object actually written to "glm4.nml" for the run) — supplies
#'   `output$out_dir` and the `mass_balance` block (`balance_file`,
#'   `balance_varnum`, `balance_vars`).
#'
#' @return A data frame with columns `Date` and, for each tracked variable
#'   `v`, `inf_v`/`out_v`/`lake_v`; or `NULL` if the file doesn't exist
#'   (e.g. `&mass_balance` wasn't configured, or the run failed before
#'   writing it).
#'
read_mass_balance <- function(glm_nml) {
  mb_file <- file.path(glm_nml$output$out_dir, paste0(glm_nml$mass_balance$balance_file, ".csv"))
  mb_vars <- glm_nml$mass_balance$balance_vars
  mb_varnum <- glm_nml$mass_balance$balance_varnum
  mb_vars <- mb_vars[1:mb_varnum]
  # Column order is inf/out/lake per variable — corrects for a GLM output
  # bug where the written columns don't match the (also mislabeled) header.
  cnams <- paste0(c("inf", "out", "lake"), "_", rep(mb_vars, each = 3))
  if (file.exists(mb_file)) {
    mb <- read.csv(mb_file, stringsAsFactors = FALSE)
    names(mb) <- c("Date", cnams)
    mb$Date <- as.Date(mb$Date)
  } else {
    mb <- NULL
  }
  return(mb)
}
