# Run the PEST posterior ensemble for one lake over the full baseline period
# and save ONE Aeme-list object holding every member.
#
# Usage (R 4.6.1):  Rscript R/pest_workflow/run_ensemble.R <LAKE_TAG> [N_MEMBERS=50] [N_WORKERS=24] [SCENARIO|all]
#
# - Members come from pest_workflow/<lake>/posterior/ (extract_posterior()).
#   Selection is a seeded permutation of the surviving runs, first N taken, so
#   the set is NESTED: N=50 is a strict subset of N=100. Scenario runs reuse
#   the same function, so baseline<->scenario realisations pair exactly.
# - Resumable: each finished member is cached to disk keyed by a hash of its
#   parameter table; a crash or restart skips completed members.
# - Output: <LOCAL_ROOT>/<lake>/baseline/aeme_ensemble.rds (named list run_<id>)
#   plus pest_workflow/<lake>/baseline/manifest.json.

select_members <- function(post, n_members, seed = 42L) {
  runs <- sort(unique(post$run))
  set.seed(seed)
  ord <- sample(runs)                       # fixed permutation for this seed
  ord[seq_len(min(n_members, length(ord)))]
}

member_ptab <- function(best, post, run_id) {
  base_full <- encode_param(ifelse(is.na(best$group), "NA", best$group), best$name,
                            ifelse(is.na(best$index), "NA", best$index))
  ev <- post[post$run == run_id, ]
  hit <- match(base_full, ev$name_full)
  ptab <- best
  ok <- !is.na(hit)
  ptab$value[ok] <- ev$value[hit[ok]]
  for (col in setdiff(PARAM_COLS, names(ptab))) ptab[[col]] <- NA
  ptab[, PARAM_COLS]
}

ptab_hash <- function(ptab) {
  paste(as.character(unclass(openssl::sha256(paste(
    paste(ptab$name, ptab$group, ptab$index, signif(ptab$value, 12)), collapse = ";")))),
    collapse = "")
}


# Free space on the drive holding LOCAL_ROOT, in GB. Each worker's build
# directory grows to ~1 GB of GLM output, so 24 workers need ~25 GB. A full
# disk mid-run corrupts cache writes and kills the supervisor (seen 2026-10-02).
free_gb <- function(path = LOCAL_ROOT) {
  drv <- substr(normalizePath(path, winslash = "/", mustWork = FALSE), 1, 1)
  v <- suppressWarnings(as.numeric(system2("powershell",
         c("-NoProfile", "-Command", shQuote(sprintf("(Get-PSDrive %s).Free/1GB", drv))),
         stdout = TRUE)))
  if (length(v) != 1 || is.na(v)) Inf else v
}
WORKER_GB <- 1.5       # budget per worker (build dir + member rds + headroom)
MIN_FREE_GB <- 8       # stop launching / kill workers below this

# Worker body (runs in a fresh R process). Builds the lake ONCE in its own
# directory (fresh_build() includes a full GLM run), then loops over its
# assigned members, caching each finished Aeme object to disk. Already-cached
# members are skipped, so a relaunch after a crash resumes where it stopped.
ens_worker <- function(k, lake_tag, aeme0_file, ptabs, names, files) {
  suppressMessages({ library(AEME); library(aemetools); library(dplyr) })
  source("R/pest_workflow/setup.R")
  ROOT <<- file.path(LOCAL_ROOT, "_builds", sprintf("%s_w%02d", lake_tag, k))
  dir.create(ROOT, recursive = TRUE, showWarnings = FALSE)
  aeme0 <<- readRDS(aeme0_file)
  resolved <- function(f) file.exists(f) | file.exists(paste0(f, ".failed"))
  todo <- which(!resolved(files))
  if (!length(todo)) return(invisible(0L))
  b <- fresh_build(paste0("ens_w", k), VARS_SIM, use_bgc = TRUE,
                   switches = SWITCHES, n_zones = n_glm_zones)
  on.exit(unlink(ROOT, recursive = TRUE, force = TRUE), add = TRUE)
  done <- 0L
  for (i in todo) {
    message(sprintf("[w%02d] %s [%s]", k, names[i], Sys.time()))
    a <- tryCatch(run_aeme_param(aeme = b$aeme, param = ptabs[[i]], model = MODEL,
                                 path = b$path, return_aeme = TRUE),
                  error = function(e) { message("  FAILED: ", conditionMessage(e)); NULL })
    # run_aeme_param() returns a numeric sentinel (999) when GLM fails, not an error
    if (!inherits(a, "Aeme")) {
      writeLines("GLM run failed (no Aeme returned)", paste0(files[i], ".failed"))
      next
    }
    tmp <- paste0(files[i], ".tmp")
    saveRDS(a, tmp); file.rename(tmp, files[i])      # atomic: no half-written cache files
    done <- done + 1L
  }
  invisible(done)
}

# Launch up to n_workers background R processes (callr), each owning a strided
# share of the outstanding members. A worker that dies (native GLM crash,
# OOM) is relaunched up to max_relaunch times; it resumes via the cache.
# A member that keeps failing is reported, not silently dropped.
resolved <- function(f) file.exists(f) | file.exists(paste0(f, ".failed"))

run_members_parallel <- function(lake_tag, aeme0_file, ptabs, status, todo,
                                 n_workers, log_dir, max_relaunch = 3L) {
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  n_workers <- min(n_workers, length(todo))
  fg <- free_gb()
  cap <- floor((fg - MIN_FREE_GB) / WORKER_GB)
  if (cap < 1) stop(sprintf("only %.1f GB free on drive - need >= %.1f GB to run even one worker",
                            fg, MIN_FREE_GB + WORKER_GB), call. = FALSE)
  if (cap < n_workers) {
    message(sprintf("  disk guard: %.1f GB free -> limiting workers %d -> %d", fg, n_workers, cap))
    n_workers <- cap
  }
  groups <- split(todo, (seq_along(todo) - 1L) %% n_workers + 1L)
  launch <- function(k) {
    idx <- groups[[k]]
    callr::r_bg(ens_worker,
                args = list(k = k, lake_tag = lake_tag, aeme0_file = aeme0_file,
                            ptabs = ptabs[idx], names = status$name[idx], files = status$file[idx]),
                stdout = file.path(log_dir, sprintf("w%02d.log", k)), stderr = "2>&1",
                supervise = TRUE)
  }
  procs <- lapply(seq_along(groups), launch)
  attempts <- rep(1L, length(groups)); finished <- rep(FALSE, length(groups))
  on.exit(for (p in procs) if (p$is_alive()) p$kill_tree(), add = TRUE)
  repeat {
    for (k in seq_along(groups)) {
      if (finished[k] || procs[[k]]$is_alive()) next
      if (all(resolved(status$file[groups[[k]]]))) { finished[k] <- TRUE; next }
      if (attempts[k] > max_relaunch) {
        message(sprintf("  worker %d gave up after %d launches; %d member(s) missing",
                        k, attempts[k], sum(!resolved(status$file[groups[[k]]]))))
        finished[k] <- TRUE; next
      }
      attempts[k] <- attempts[k] + 1L
      message(sprintf("  worker %d exited early - relaunch %d/%d", k, attempts[k] - 1L, max_relaunch))
      procs[[k]] <- tryCatch(launch(k), error = function(e) {
        message("  relaunch failed (", conditionMessage(e), ") - will retry next poll"); procs[[k]] })
    }
    fg <- free_gb()
    if (fg < MIN_FREE_GB / 2) {
      message(sprintf("  DISK LOW (%.1f GB free) - stopping workers; rerun to resume from cache", fg))
      for (p in procs) if (p$is_alive()) p$kill_tree()
      stop("stopped: disk space low", call. = FALSE)
    }
    n_done <- sum(resolved(status$file[todo]))
    message(sprintf("  [%s] %d/%d members done", format(Sys.time(), "%H:%M:%S"), n_done, length(todo)))
    if (all(finished)) break
    Sys.sleep(60)
  }
  invisible(NULL)
}

# Model object for a lake + scenario. Upstream lakes come from
# aeme_baseline_upstream; DOWNSTREAM lakes (lake_metadata$lake_upstream set)
# receive their parent's outflow for the SAME scenario as an inflow, taken from
# the parent's current ensemble (pest_workflow/<parent>/outflow/), and are
# refused if the parent's ensemble for that scenario is missing or stale.
build_ensemble_aeme0 <- function(lake_tag, scenario) {
  source("R/pest_workflow/inflow_fix.R")
  a <- fix_inflow_ss_units(.build_ensemble_aeme0(lake_tag, scenario))
  # build_lake_aeme0() sizes the model window to the lake's observations, so a lake with a short
  # observation record (Kawaupaka: 2021-2023) gets a short window. Forward ensembles must cover the
  # pipeline's simulation_time (the met and inflow series do), so widen the window to it; the 5-year
  # spin-up is kept. The calibrated parameters do not depend on the window length.
  st <- targets::tar_read(simulation_time); tm <- AEME::time(a)
  if (as.Date(tm$start) > as.Date(st$start) || as.Date(tm$stop) < as.Date(st$stop)) {
    message(sprintf("  widening model window %s -> %s to simulation_time %s -> %s",
                    as.Date(tm$start), as.Date(tm$stop), st$start, st$stop))
    tm$start <- as.Date(min(as.Date(tm$start), as.Date(st$start)))
    tm$stop  <- as.Date(max(as.Date(tm$stop), as.Date(st$stop)))
    AEME::time(a) <- tm
  }
  a
}
.build_ensemble_aeme0 <- function(lake_tag, scenario) {
  lid <- sub("_.*$", "", lake_tag)
  meta <- as.data.frame(targets::tar_read(lake_metadata))
  row <- meta[meta$lernzmp_id == lid, ]
  stopifnot(nrow(row) == 1)
  scen_args <- function() list(lake_routing = targets::tar_read(lake_routing),
                               lake_metadata = targets::tar_read(lake_metadata),
                               scenario = scenario,
                               spinup_mapping = targets::tar_read(spinup_mapping))
  if (is.na(row$lake_upstream)) {
    a <- build_lake_aeme0(targets::tar_read(aeme_baseline_upstream), lake_tag, VARS_SIM)
    if (scenario != "baseline") a <- do.call(set_lake_inflows, c(list(aeme = a), scen_args()))
    return(a)
  }
  source("R/pest_workflow/downstream.R")
  ptag <- parent_tag_for(lid)
  require_current_manifest(ptag, if (scenario == "baseline") "baseline" else file.path("scenarios", scenario),
                           step = paste("parent", ptag, scenario, "ensemble"))
  of_csv <- file.path(PEST_WF_ROOT, ptag, "outflow", sprintf("%s_outflow.csv", scenario))
  message("  downstream of ", ptag, ": inflow from ", of_csv)
  a <- build_downstream_aeme(lid, of_csv)
  a <- build_lake_aeme0(list(a), lake_tag, VARS_SIM)
  if (scenario != "baseline") {
    of <- utils::read.csv(of_csv, stringsAsFactors = FALSE)
    of$Date <- as.Date(of$Date)
    of <- of[, setdiff(names(of), c("model", "scenario"))]
    a <- do.call(set_lake_inflows, c(list(aeme = a, outflow_inflow = of), scen_args()))
  }
  a
}

run_lake_ensemble <- function(lake_tag, scenario = "baseline", n_members = 50L, seed = 42L,
                              n_workers = 24L, max_relaunch = 3L, .depth = 0L) {
  post_m <- require_current_manifest(lake_tag, "posterior", step = "posterior")
  pp <- read_posterior_ptabs(lake_tag)
  members <- select_members(pp$post, n_members, seed)
  message(sprintf("=== %s [%s]: ensemble, %d of %d posterior members (seed %d) ===",
                  lake_tag, scenario, length(members), length(unique(pp$post$run)), seed))

  # Period: must equal the targets pipeline's simulation_time
  st <- targets::tar_read(simulation_time)

  aeme0 <<- build_ensemble_aeme0(lake_tag, scenario)
  per <- AEME::time(aeme0)
  # AEME::time() start/stop is the window AFTER spin-up is removed (the spin-up
  # runs before `start`), so this is what get_var(remove_spin_up = TRUE) returns.
  # It is wider than the pipeline's simulation_time (build_lake_aeme0() widens
  # it); downstream summaries are trimmed to simulation_time. Require that the
  # model window CONTAINS simulation_time.
  per <- list(start = as.Date(per$start), stop = as.Date(per$stop))
  message("  targets simulation_time: ", st$start, " -> ", st$stop,
          " | model output window: ", per$start, " -> ", per$stop)
  # The window is set per lake from its observation coverage (build_lake_aeme0 ->
  # suggest_sim_period), so it may end a little before simulation_time (e.g.
  # Kuwakatai ends 2023-06-11). A late START is a real problem; an early END is
  # reported and the lake simply has fewer days at the end.
  if (per$start > as.Date(st$start))
    stop("model output window starts after targets simulation_time", call. = FALSE)
  if (per$stop < as.Date(st$stop))
    message("  NOTE: model window ends ", per$stop, ", before simulation_time end ", st$stop)

  out_dir <- file.path(LOCAL_ROOT, lake_tag, if (scenario == "baseline") "baseline" else file.path("scenarios", scenario))
  cache_dir <- file.path(out_dir, "members")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  status <- data.frame(run = members, name = paste0("run_", members),
                       hash = NA_character_, stringsAsFactors = FALSE)
  ptabs <- vector("list", length(members))
  for (i in seq_along(members)) {
    ptabs[[i]] <- member_ptab(pp$best, pp$post, members[i])
    status$hash[i] <- ptab_hash(ptabs[[i]])
  }
  status$file <- file.path(cache_dir, sprintf("%s__%s.rds", status$name, substr(status$hash, 1, 12)))

  # cached members for these runs whose parameters have since changed are stale
  # (members of other runs are left alone so a larger N later reuses them)
  existing <- list.files(cache_dir, full.names = TRUE)
  own <- existing[sub("__.*$", "", basename(existing)) %in% status$name]
  stale <- setdiff(own, c(status$file, paste0(status$file, ".failed")))
  if (length(stale)) { message("  removing ", length(stale), " stale cached members"); unlink(stale) }

  todo <- which(!resolved(status$file))
  message(sprintf("  %d cached, %d to run on up to %d workers", length(members) - length(todo),
                  length(todo), n_workers))
  if (length(todo)) {
    aeme0_file <- file.path(out_dir, "aeme0.rds")
    saveRDS(aeme0, aeme0_file)
    run_members_parallel(lake_tag, aeme0_file, ptabs, status, todo, n_workers,
                         log_dir = file.path(out_dir, "worker_logs"),
                         max_relaunch = max_relaunch)
  }
  status$ok <- file.exists(status$file)
  status$failed <- file.exists(paste0(status$file, ".failed"))


  # Validate every cached member while assembling: a corrupt file (e.g. a write
  # cut off by a full disk) is deleted and recomputed; a numeric sentinel (GLM
  # failure) becomes a .failed marker. Neither is ever counted as a good member.
  ens <- vector("list", nrow(status))
  recompute <- 0L
  for (i in which(status$ok)) {
    x <- tryCatch(readRDS(status$file[i]), error = function(e) NULL)
    if (inherits(x, "Aeme")) { ens[[i]] <- x; next }
    if (is.null(x)) { message("  corrupt cache file removed, will recompute: ", basename(status$file[i]))
                      recompute <- recompute + 1L }
    else writeLines("GLM run failed (numeric sentinel)", paste0(status$file[i], ".failed"))
    unlink(status$file[i]); status$ok[i] <- FALSE
    status$failed[i] <- !is.null(x)
  }
  if (recompute > 0L && .depth < 1L) {
    return(run_lake_ensemble(lake_tag, scenario, n_members, seed, n_workers, max_relaunch, .depth = 1L))
  }
  names(ens) <- status$name
  ok <- status[status$ok, ]
  ens <- ens[ok$name]
  if (any(status$failed)) message("  GLM-failed members (excluded): ",
                                  paste(status$name[status$failed], collapse = ", "))
  ens_file <- file.path(out_dir, "aeme_ensemble.rds")
  saveRDS(ens, ens_file)

  # lake outflow of the first good member: the inflow for a downstream lake
  # (one representative trace, as in the earlier scenario scripts)
  if (length(ens)) {
    of <- tryCatch(AEME::aeme_to_inflow(ens[[1]]), error = function(e) NULL)
    if (!is.null(of)) {
      of_dir <- wf_dir(lake_tag, "outflow")
      of_file <- file.path(of_dir, sprintf("%s_outflow.csv", scenario))
      utils::write.csv(of, of_file, row.names = FALSE)
      message("  saved outflow (member ", names(ens)[1], "): ", of_file)
    }
  }

  man_dir <- if (scenario == "baseline") wf_dir(lake_tag, "baseline") else wf_dir(lake_tag, "scenarios", scenario)
  utils::write.csv(status, file.path(man_dir, "members.csv"), row.names = FALSE)
  write_manifest(man_dir, "ens_baseline",
                 inputs = c(post_m$inputs[0],
                            describe_inputs(file.path(wf_dir(lake_tag, "posterior", create = FALSE),
                                                      c("posterior_params.csv", "best_params.csv")))),
                 extra = list(lake = lake_tag, scenario = scenario, n_requested = n_members, n_ok = nrow(ok),
                              n_failed = sum(!status$ok), n_glm_failed = sum(status$failed),
                              failed_members = status$name[status$failed], seed = seed,
                              model_period = list(start = as.character(per$start), stop = as.character(per$stop)),
                              simulation_time = list(start = as.character(st$start), stop = as.character(st$stop)),
                              posterior_cal_id = post_m$cal_id,
                              ensemble_file = normalizePath(ens_file, winslash = "/"),
                              ensemble_sha256 = file_sha256(ens_file)))
  message(sprintf("=== %s done: %d/%d members -> %s ===", lake_tag, nrow(ok), length(members), ens_file))
  invisible(ens_file)
}

if (sys.nframe() == 0) {
  suppressMessages({ library(targets); library(AEME); library(aemetools); library(dplyr) })
  source("R/pest_workflow/manifest.R")
  source("R/pest_workflow/setup.R")
  source("R/pest_workflow/pest_extract.R")
  source("R/set_lake_inflows.R"); source("R/lspc_to_aeme.R"); source("R/add_block_spinup.R")
  args <- commandArgs(trailingOnly = TRUE)
  # Rscript run_ensemble.R <LAKE_TAG> [N=50] [WORKERS=24] [SCENARIO|all=baseline]
  n_mem <- if (length(args) >= 2) as.integer(args[2]) else 50L
  n_wrk <- if (length(args) >= 3) as.integer(args[3]) else 24L
  scen  <- if (length(args) >= 4) args[4] else "baseline"
  if (scen == "all") scen <- c("baseline", "m1_dairy", "m1_non_dairy", "m2_dairy",
                               "m2_non_dairy", "m3_dairy", "m3_non_dairy")
  # A flag file <PEST_WF_ROOT>/.skip_scenarios_<LAKE_TAG> limits an "all" request to the
  # baseline (used while comparing calibration recipes: scenarios are only worth
  # running for the recipe that is kept). Delete the flag to run them.
  flag <- file.path(PEST_WF_ROOT, paste0(".skip_scenarios_", args[1]))
  if (file.exists(flag) && length(scen) > 1) {
    message("NOTE: ", flag, " exists - running the baseline only (delete it to run the scenarios)")
    scen <- "baseline"
  }
  for (sc in scen) run_lake_ensemble(args[1], sc, n_mem, n_workers = n_wrk)
}
