# Extract the PEST posterior ensemble parameters, best-fit parameters, phi
# trajectory and Morris sensitivity for one lake from its stage-4 checkpoint.
# Generalised from generate_pest_ensemble_output.R (which also runs the
# members - that now lives in run_ensemble.R).
#
# Output (pest_workflow/<lake_tag>/posterior/):
#   posterior_params.csv  one row per (run, parameter): run, name_full, value
#   best_params.csv       s4$best parameter table (lowest-phi realisation)
#   phi_trajectory.csv    per-iteration phi summary where available
#   sensitivity_morris.csv  mu / mu_star per stage (s3, s4) where screens exist
#   manifest.json

extract_posterior <- function(lake_tag,
                              checkpoint_root = "staged-calibration-v4/checkpoints",
                              min_distinct = 2L) {
  s4_file <- file.path(checkpoint_root, sprintf("s4_%s.rds", lake_tag))
  if (!file.exists(s4_file)) stop("no stage-4 checkpoint: ", s4_file, call. = FALSE)
  s4 <- readRDS(s4_file)
  calib <- aemetools::read_calib(ctrl = s4$cal_ctrl, sim_id = s4$cal_id)

  final_iter <- max(calib$pest_posterior$iteration, na.rm = TRUE)
  surviving <- calib$pest_posterior$run[calib$pest_posterior$iteration == final_iter]

  # pest_posterior$iteration and simulation_data$gen are offset by one
  # (gen 1 = prior draw), so survivors of iteration N live in gen N + 1.
  sim_final <- calib$simulation_data[calib$simulation_data$gen == (final_iter + 1), ]
  sim_final <- sim_final[sim_final$run %in% surviving,
                         c("run", "parameter_name", "parameter_value")]
  sim_final <- sim_final[!duplicated(sim_final[c("run", "parameter_name")]), ]
  names(sim_final) <- c("run", "name_full", "value")

  if (!nrow(sim_final)) {
    stop(sprintf("[%s] no per-run parameters found for survivors at gen %d",
                 lake_tag, final_iter + 1), call. = FALSE)
  }
  # Guard: members must actually differ (the pre-2026-10-01 gen bug gave
  # identical members).
  spread <- tapply(sim_final$value, sim_final$name_full,
                   function(v) length(unique(signif(v, 10))))
  if (sum(spread >= min_distinct) == 0) {
    stop(sprintf("[%s] all posterior members identical - generation lookup failed",
                 lake_tag), call. = FALSE)
  }

  out_dir <- wf_dir(lake_tag, "posterior")
  utils::write.csv(sim_final, file.path(out_dir, "posterior_params.csv"), row.names = FALSE)
  utils::write.csv(s4$best, file.path(out_dir, "best_params.csv"), row.names = FALSE)
  if (!is.null(s4$phi)) {
    utils::write.csv(as.data.frame(s4$phi), file.path(out_dir, "phi_trajectory.csv"),
                     row.names = FALSE)
  }

  sens <- lapply(c("s3", "s4"), function(st) {
    f <- file.path(checkpoint_root, sprintf("screen_%s_%s.rds", st, lake_tag))
    if (!file.exists(f)) return(NULL)
    scr <- readRDS(f)
    if (is.null(scr$mu)) return(NULL)
    cbind(stage = st, as.data.frame(scr$mu))
  })
  sens <- dplyr::bind_rows(Filter(Negate(is.null), sens))
  if (nrow(sens)) {
    utils::write.csv(sens, file.path(out_dir, "sensitivity_morris.csv"), row.names = FALSE)
  }

  screen_files <- file.path(checkpoint_root,
                            sprintf("screen_%s_%s.rds", c("s3", "s4"), lake_tag))
  inputs <- describe_inputs(c(s4_file, screen_files[file.exists(screen_files)]))
  write_manifest(out_dir, "posterior", inputs,
                 extra = list(lake = lake_tag, cal_id = s4$cal_id,
                              final_iteration = final_iter,
                              n_members = length(unique(sim_final$run)),
                              n_params_varying = sum(spread >= min_distinct)))
  invisible(out_dir)
}

read_posterior_ptabs <- function(lake_tag) {
  d <- wf_dir(lake_tag, "posterior", create = FALSE)
  list(best = utils::read.csv(file.path(d, "best_params.csv"), stringsAsFactors = FALSE),
       post = utils::read.csv(file.path(d, "posterior_params.csv"), stringsAsFactors = FALSE))
}
