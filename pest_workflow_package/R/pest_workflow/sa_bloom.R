# Bloom-oriented Morris sensitivity analysis (diagnostic - NOT part of calibration).
# Question: which parameters control surface chl-a / cyano / green / diatom (mean, peaks, bloom
# frequency), and can ANY parameter in the candidate set lift modelled chl-a towards the
# observations (Rototoa is ~3 ug/L too low)?
#
#   Rscript R/pest_workflow/sa_bloom.R [LAKE_TAG]
#   env: PEST_WF_ROOT / PEST_LOCAL_ROOT / PEST_CHECKPOINT_ROOT (as for the other workflow scripts;
#        selects whose posterior best_params is the reference point), RENV_PATHS_LIBRARY
#        SA_R (trajectories, default 6), SA_LEVELS (default 6), SA_WORKERS (default 24),
#        SA_EXTRA (1 = also vary candidate params from aed_sa_calib_params.csv that the stage-4
#        calibration does not adjust, default 1)
#
# Design: Morris (1991) trajectories over the unit cube; parameters are scaled linearly between
# their min/max, or log-scaled when positive and max/min >= 20. Every run is the full 2013-2023
# model; surface (<= 2 m) daily series of PHY_tchla / PHY_cyano / PHY_green / PHY_diatom are
# trimmed to the targets simulation_time and summarised. Per-run results are cached
# (resumable); a crashed worker is relaunched.
# Output: <PEST_WF_ROOT>/<tag>/diagnostics/sa_bloom/  runs.csv  morris.csv  reach.csv  design.csv
suppressMessages({ library(targets); library(AEME); library(aemetools); library(dplyr) })
source("R/pest_workflow/manifest.R"); source("R/pest_workflow/setup.R"); source("R/pest_workflow/pest_extract.R")
source("R/set_lake_inflows.R"); source("R/lspc_to_aeme.R"); source("R/add_block_spinup.R")
source("R/pest_workflow/run_ensemble.R")          # build_ensemble_aeme0(), free_gb()

# Worker (fresh R process via callr; must be self-contained - only its own body is shipped).
# Builds the lake once, runs the assigned design rows (id 0 = reference), caches per-run metrics.
sab_worker <- function(k, tag, aeme0_file, ptab, vary, vals, ids, out_dir, win, thr) {
  suppressMessages({ library(AEME); library(aemetools); library(dplyr) })
  source("R/pest_workflow/setup.R")
  BLOOM_VARS <- c("PHY_tchla", "PHY_cyano", "PHY_green", "PHY_diatom")
  bloom_metrics <- function(a) {
    out <- list(); thr_out <- list()
    for (v in BLOOM_VARS) {
      s <- tryCatch(AEME::get_var(a, model = MODEL, var_sim = v, use_obs = FALSE, remove_spin_up = TRUE), error = function(e) NULL)
      if (is.null(s) || !nrow(s)) next
      s$Date <- as.Date(s$Date)
      keep <- s$Date >= win[1] & s$Date <= win[2]
      if ("depth" %in% names(s)) keep <- keep & s$depth <= 2
      x <- tapply(as.numeric(s$value[keep]), s$Date[keep], mean, na.rm = TRUE)
      if (!length(x)) next
      mo <- as.integer(format(as.Date(names(x)), "%m"))
      t0 <- if (!is.null(thr[[v]])) thr[[v]] else unname(quantile(x, .9, na.rm = TRUE))
      thr_out[[v]] <- t0
      out[[v]] <- c(mean = mean(x), p90 = unname(quantile(x, .9)), p99 = unname(quantile(x, .99)), max = max(x),
                    summer_mean = mean(x[mo %in% c(12, 1, 2)]), frac_gt_thr = mean(x > t0))
    }
    m <- unlist(out); names(m) <- sub("\\.", "__", names(m))
    list(metrics = m, thr = thr_out)
  }
  ROOT <<- file.path(LOCAL_ROOT, "_builds", sprintf("sab_%s_w%02d", tag, k))
  dir.create(ROOT, recursive = TRUE, showWarnings = FALSE)
  aeme0 <<- readRDS(aeme0_file)
  f_of <- function(i) file.path(out_dir, "runs", sprintf("run_%05d.rds", i))
  todo <- ids[!file.exists(vapply(ids, f_of, ""))]
  if (!length(todo)) return(invisible(0L))
  b <- fresh_build(paste0("sab_w", k), VARS_SIM, use_bgc = TRUE, switches = SWITCHES, n_zones = n_glm_zones)
  on.exit(unlink(ROOT, recursive = TRUE, force = TRUE), add = TRUE)
  for (i in todo) {
    p <- ptab
    if (i > 0L) p$value[vary] <- vals[[as.character(i)]]
    a <- tryCatch(run_aeme_param(aeme = b$aeme, param = p, model = MODEL, path = b$path, return_aeme = TRUE),
                  error = function(e) NULL)
    res <- if (inherits(a, "Aeme")) tryCatch(bloom_metrics(a), error = function(e) NULL) else NULL
    saveRDS(if (is.null(res)) list(failed = TRUE) else res, paste0(f_of(i), ".tmp")); file.rename(paste0(f_of(i), ".tmp"), f_of(i))
    message(sprintf("[w%02d] run %d %s [%s]", k, i, if (is.null(res)) "FAILED" else "ok", Sys.time()))
  }
  invisible(length(todo))
}

if (sys.nframe() == 0) {            # CLI only
  tag <- { a <- commandArgs(trailingOnly = TRUE); if (length(a)) a[1] else "LID50270_rototoa" }
  R_TRAJ <- as.integer(Sys.getenv("SA_R", "6")); P <- as.integer(Sys.getenv("SA_LEVELS", "6"))
  n_workers <- as.integer(Sys.getenv("SA_WORKERS", "24")); use_extra <- Sys.getenv("SA_EXTRA", "1") == "1"
  out_dir <- file.path(LOCAL_ROOT, tag, "diagnostics", "sa_bloom"); dir.create(file.path(out_dir, "runs"), recursive = TRUE, showWarnings = FALSE)
  pub_dir <- file.path(PEST_WF_ROOT, tag, "diagnostics", "sa_bloom"); dir.create(pub_dir, recursive = TRUE, showWarnings = FALSE)
  st <- targets::tar_read(simulation_time); win <- as.Date(c(st$start, st$stop))

  # ---- parameter set: reference = posterior best ---------------------------------------------
  pp <- read_posterior_ptabs(tag)
  ptab <- pp$best[, intersect(PARAM_COLS, names(pp$best))]; for (cc in setdiff(PARAM_COLS, names(ptab))) ptab[[cc]] <- NA
  ptab <- ptab[, PARAM_COLS]; ptab$in_calibration <- TRUE
  if (use_extra) {                         # candidate parameters the calibration does not adjust
    cand <- read.csv("data/processed/aed_sa_calib_params.csv", stringsAsFactors = FALSE)[, PARAM_COLS]
    cand <- cand[!cand$name %in% ptab$name & !cand$name %in% c("aed_phosphorus/kadsratio", "aed_phosphorus/qmax"), ]
    cand$in_calibration <- FALSE
    ptab <- rbind(ptab, cand)
  }
  key <- paste(ptab$name, ifelse(is.na(ptab$group), "", ptab$group), ifelse(is.na(ptab$index), "", ptab$index), sep = "|")
  vary <- which(is.finite(ptab$min) & is.finite(ptab$max) & ptab$max > ptab$min)
  k <- length(vary)
  message(sprintf("%d parameters varied (%d in calibration, %d extra); %d trajectories -> %d runs",
                  k, sum(ptab$in_calibration[vary]), sum(!ptab$in_calibration[vary]), R_TRAJ, R_TRAJ * (k + 1)))
  lg <- ptab$min[vary] > 0 & ptab$max[vary] / ptab$min[vary] >= 20
  to_val <- function(u) ifelse(lg, exp(log(ptab$min[vary]) + u * (log(ptab$max[vary]) - log(ptab$min[vary]))),
                               ptab$min[vary] + u * (ptab$max[vary] - ptab$min[vary]))

  # ---- Morris design (reproducible) ------------------------------------------------------------
  set.seed(7); delta <- P / (2 * (P - 1)); grid <- (0:(P / 2 - 1)) / (P - 1)
  des <- list(); step_par <- integer(); step_dir <- numeric(); traj <- integer(); rid <- 0L
  for (r in seq_len(R_TRAJ)) {
    x <- sample(grid, k, replace = TRUE); ord <- sample(k)
    rid <- rid + 1L; des[[rid]] <- x; step_par[rid] <- 0L; step_dir[rid] <- 0; traj[rid] <- r
    for (j in ord) {
      dir <- if (x[j] + delta <= 1 + 1e-9) 1 else -1
      x[j] <- x[j] + dir * delta
      rid <- rid + 1L; des[[rid]] <- x; step_par[rid] <- j; step_dir[rid] <- dir; traj[rid] <- r
    }
  }
  n_runs <- rid
  vals <- setNames(lapply(des, to_val), as.character(seq_len(n_runs)))
  # parameter values of every run (run 0 = reference), keyed by aemetools name_full: the input to
  # build_bloom_prior.R (bloom-seeded prior ensemble). The design is deterministic (seed 7).
  nf <- encode_param(ifelse(is.na(ptab$group), "NA", ptab$group), ptab$name, ifelse(is.na(ptab$index), "NA", ptab$index))
  pmat <- rbind(ptab$value[vary], do.call(rbind, unname(vals)))
  colnames(pmat) <- nf[vary]
  write.csv(data.frame(run = 0:n_runs, pmat, check.names = FALSE), file.path(pub_dir, "params.csv"), row.names = FALSE)
  write.csv(data.frame(run = seq_len(n_runs), trajectory = traj, step_param = ifelse(step_par > 0, key[vary][pmax(step_par, 1)], NA),
                       step_dir = step_dir), file.path(pub_dir, "design.csv"), row.names = FALSE)

  # ---- forward-model input once; reference run (id 0) first to fix the bloom thresholds --------------
  ROOT <<- file.path(LOCAL_ROOT, "_builds", paste0("sab_main_", tag)); dir.create(ROOT, recursive = TRUE, showWarnings = FALSE)
  aeme0 <<- build_ensemble_aeme0(tag, "baseline")
  aeme0_file <- file.path(out_dir, "aeme0.rds"); saveRDS(aeme0, aeme0_file)
  common <- list(tag = tag, aeme0_file = aeme0_file, ptab = ptab[, PARAM_COLS], vary = vary, vals = vals, out_dir = out_dir, win = win)
  ref_f <- file.path(out_dir, "runs", "run_00000.rds")
  if (!file.exists(ref_f)) {
    message("reference run (posterior best)")
    callr::r(sab_worker, args = c(list(k = 0L, ids = 0L, thr = NULL), common), show = TRUE)
  }
  ref <- readRDS(ref_f); if (isTRUE(ref$failed)) stop("the reference run (posterior best) failed", call. = FALSE)
  thr <- ref$thr

  # ---- parallel Morris runs -----------------------------------------------------------------------
  ids <- seq_len(n_runs)
  fg <- free_gb(); cap <- floor((fg - MIN_FREE_GB) / WORKER_GB); nw <- max(1L, min(n_workers, cap, n_runs))
  if (nw < n_workers) message(sprintf("disk guard: %.1f GB free -> %d workers", fg, nw))
  groups <- split(ids, (seq_along(ids) - 1L) %% nw + 1L)
  run_file <- function(i) file.path(out_dir, "runs", sprintf("run_%05d.rds", i))
  launch <- function(w) callr::r_bg(sab_worker, args = c(list(k = w, ids = groups[[w]], thr = thr), common),
                                    stdout = file.path(out_dir, sprintf("worker_%02d.log", w)), stderr = "2>&1")
  procs <- lapply(seq_len(nw), launch); relaunch <- rep(0L, nw)
  done_n <- function() sum(file.exists(vapply(ids, run_file, "")))
  repeat {
    Sys.sleep(60)
    for (w in seq_len(nw)) if (!procs[[w]]$is_alive()) {
      left <- groups[[w]][!file.exists(vapply(groups[[w]], run_file, ""))]
      if (length(left) && relaunch[w] < 3L) { relaunch[w] <- relaunch[w] + 1L; message("relaunching worker ", w); procs[[w]] <- launch(w) }
    }
    message(sprintf("[%s] %d / %d runs done", format(Sys.time(), "%H:%M"), done_n(), n_runs))
    if (done_n() >= n_runs || !any(vapply(procs, function(p) p$is_alive(), NA))) break
  }

  # ---- collect + Morris statistics ------------------------------------------------------------------
  all_ids <- c(0L, ids)
  rr <- lapply(all_ids, function(i) if (file.exists(run_file(i))) readRDS(run_file(i)))
  ok <- vapply(rr, function(x) !is.null(x) && !isTRUE(x$failed), NA)
  message(sum(ok), " of ", length(rr), " runs usable")
  mets <- names(rr[[which(ok)[1]]]$metrics)
  M <- t(vapply(rr[ok], function(x) x$metrics[mets], numeric(length(mets)))); colnames(M) <- mets
  run_id <- all_ids[ok]
  write.csv(data.frame(run = run_id, M), file.path(pub_dir, "runs.csv"), row.names = FALSE)
  # elementary effects: consecutive runs within a trajectory differing in exactly one parameter
  ee <- list()
  for (i in ids) if (step_par[i] > 0 && (i - 1L) %in% run_id && i %in% run_id) {
    j <- step_par[i]; dlt <- step_dir[i] * delta
    d <- (M[match(i, run_id), ] - M[match(i - 1L, run_id), ]) / dlt
    ee[[length(ee) + 1L]] <- data.frame(param = key[vary][j], in_calibration = ptab$in_calibration[vary][j], metric = mets, ee = unname(d))
  }
  ee <- bind_rows(ee)
  morris <- ee |> summarise(mu = mean(ee, na.rm = TRUE), mu_star = mean(abs(ee), na.rm = TRUE), sigma = sd(ee, na.rm = TRUE),
                            n = sum(is.finite(ee)), .by = c(param, in_calibration, metric)) |> arrange(metric, desc(mu_star))
  write.csv(morris, file.path(pub_dir, "morris.csv"), row.names = FALSE)
  reach <- data.frame(metric = mets, reference = M[match(0L, run_id), ], min = apply(M, 2, min, na.rm = TRUE),
                      max = apply(M, 2, max, na.rm = TRUE), run_of_max = run_id[apply(M, 2, which.max)])
  write.csv(reach, file.path(pub_dir, "reach.csv"), row.names = FALSE)

  cat("\n===== reference (posterior best) vs range reached over the Morris runs =====\n")
  print(reach |> mutate(across(where(is.numeric), \(v) signif(v, 3))), row.names = FALSE)
  for (m in c("PHY_tchla__mean", "PHY_tchla__p90", "PHY_tchla__frac_gt_thr", "PHY_cyano__p90", "PHY_green__p90", "PHY_diatom__p90")) {
    cat(sprintf("\n===== top 10 parameters for %s (mu* = mean |elementary effect| per unit cube) =====\n", m))
    print(morris |> filter(metric == m) |> head(10) |> mutate(across(where(is.numeric), \(v) signif(v, 3))), row.names = FALSE)
  }
  unlink(ROOT, recursive = TRUE, force = TRUE)
  message("sa_bloom DONE -> ", pub_dir)
}
