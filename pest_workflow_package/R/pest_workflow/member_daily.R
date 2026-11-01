# Per-member daily series cache: one parquet per (lake, scenario) holding every
# ensemble member's daily depth-class means, TRIMMED to the targets pipeline's
# simulation_time (the model itself outputs a wider window - see
# sanity_checks / run_ensemble notes). Everything downstream that compares or
# summarises members (scenario_comparison.R, sanity checks) reads this instead
# of re-opening the multi-hundred-MB Aeme ensembles.
#
# File: <LOCAL_ROOT>/<lake>/summaries/<scenario>_member_daily.parquet
# Columns: member, scenario, variable, depth_class, Date, sim
# Provenance: pest_workflow/<lake>/summaries/<scenario>_member_daily.json
#   (ensemble sha256, trimmed window, n_members, built time)
#
# Usage: Rscript R/pest_workflow/member_daily.R <LAKE_TAG> [SCENARIO|all|missing]
#   env MD_WORKERS (default 4) - keep small while ensembles are running

suppressMessages({ library(AEME); library(dplyr); library(arrow); library(parallel) })
source("R/pest_workflow/manifest.R")

MODEL <- "glm_aed"
SUMMARY_VARS <- c("LKE_lvlwtr", "HYD_temp", "HYD_thmcln", "CHM_oxy", "NIT_amm", "NIT_nit",
                  "NIT_tn", "PHS_frp", "PHS_tp", "PHY_tchla", "NCS_ss1")
ONE_D <- c("LKE_lvlwtr", "HYD_thmcln", "HYD_strat")
ALL_SCENARIOS <- c("baseline", "m1_dairy", "m1_non_dairy", "m2_dairy", "m2_non_dairy",
                   "m3_dairy", "m3_non_dairy")

# surface <= 2 m; bottom = deeper than the shallower of (3 m above bed, 85% of
# depth) - the dashboard's zmax - 3 rule leaves Rototoa's deepest samples in 'mid'
classify_depth <- function(variable, depth_m, zmax) {
  if (variable %in% ONE_D || all(is.na(depth_m))) return(rep("whole_lake", length(depth_m)))
  out <- rep("mid", length(depth_m))
  out[!is.na(depth_m) & depth_m <= 2] <- "surface"
  out[!is.na(depth_m) & depth_m >= min(zmax - 3, 0.85 * zmax)] <- "bottom"
  out
}

member_daily_one <- function(aeme, nm, vars) {
  zmax <- tryCatch(AEME::lake(aeme)$depth, error = function(e) NA_real_)
  bind_rows(lapply(vars, function(v) {
    s <- tryCatch(AEME::get_var(aeme, model = MODEL, var_sim = v, use_obs = FALSE,
                                remove_spin_up = TRUE), error = function(e) NULL)
    if (is.null(s) || !nrow(s)) return(NULL)
    dep <- if ("depth" %in% names(s)) s$depth else NA_real_
    zm <- if (is.finite(zmax)) zmax else suppressWarnings(max(dep, na.rm = TRUE))
    data.frame(Date = as.Date(s$Date), depth_class = classify_depth(v, dep, zm),
               sim = as.numeric(s$value)) |>
      summarise(sim = mean(sim, na.rm = TRUE), .by = c(Date, depth_class)) |>
      mutate(variable = v, member = nm)
  }))
}
# top level on purpose: a closure made inside build_member_daily() would ship the
# whole ensemble (its environment) to every worker
member_daily_task <- function(it, vars) member_daily_one(it$aeme, it$name, vars)

member_daily_path <- function(lake_tag, scenario, ext = "parquet") {
  d <- if (ext == "parquet") file.path(LOCAL_ROOT_MD, lake_tag, "summaries") else
    file.path("pest_workflow", lake_tag, "summaries")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  file.path(d, sprintf("%s_member_daily.%s", scenario, ext))
}
LOCAL_ROOT_MD <- Sys.getenv("PEST_LOCAL_ROOT", "C:/Users/data/Desktop/ac_lake_calib/pest_workflow")   # EDIT default or set PEST_LOCAL_ROOT

#' Build (or reuse) the cache. Reuses it only when the ensemble sha256 matches.
build_member_daily <- function(lake_tag, scenario, n_workers = as.integer(Sys.getenv("MD_WORKERS", "4"))) {
  mdir <- if (scenario == "baseline") "baseline" else file.path("scenarios", scenario)
  man <- require_current_manifest(lake_tag, mdir, step = paste("ensemble", scenario))
  pq <- member_daily_path(lake_tag, scenario); js <- member_daily_path(lake_tag, scenario, "json")
  if (file.exists(pq) && file.exists(js)) {
    prov <- jsonlite::read_json(js)
    if (identical(prov$ensemble_sha256, man$ensemble_sha256)) {
      message(sprintf("[%s/%s] member_daily up to date", lake_tag, scenario)); return(invisible(pq))
    }
  }
  if (!identical(file_sha256(man$ensemble_file), man$ensemble_sha256))
    stop("ensemble file differs from its manifest: ", man$ensemble_file, call. = FALSE)
  st <- targets::tar_read(simulation_time)
  ens <- readRDS(man$ensemble_file)
  ens <- ens[vapply(ens, function(x) isS4(x) && is(x, "Aeme"), logical(1))]
  message(sprintf("[%s/%s] extracting %d members (trim %s to %s)", lake_tag, scenario,
                  length(ens), st$start, st$stop))
  items <- lapply(names(ens), function(n) list(name = n, aeme = ens[[n]])); rm(ens)
  cl <- makeCluster(min(n_workers, length(items))); on.exit(stopCluster(cl), add = TRUE)
  clusterEvalQ(cl, suppressMessages({ library(AEME); library(dplyr) }))
  clusterExport(cl, c("MODEL", "ONE_D", "classify_depth", "member_daily_one", "member_daily_task"),
                envir = globalenv())
  res <- parLapply(cl, items, member_daily_task, vars = SUMMARY_VARS)
  d <- bind_rows(res) |> filter(Date >= as.Date(st$start), Date <= as.Date(st$stop)) |>
    mutate(scenario = scenario)
  write_parquet(d, pq)
  jsonlite::write_json(list(lake = lake_tag, scenario = scenario, ensemble_sha256 = man$ensemble_sha256,
                            n_members = length(items), window = c(as.character(st$start), as.character(st$stop)),
                            built = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"), rows = nrow(d)),
                       js, auto_unbox = TRUE, pretty = TRUE)
  invisible(pq)
}

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  lake <- args[1]; which <- if (length(args) >= 2) args[2] else "all"
  have <- ALL_SCENARIOS[file.exists(file.path("pest_workflow", lake,
            ifelse(ALL_SCENARIOS == "baseline", "baseline", file.path("scenarios", ALL_SCENARIOS)), "manifest.json"))]
  todo <- if (which %in% c("all", "missing")) have else which
  for (sc in todo) build_member_daily(lake, sc)
}
