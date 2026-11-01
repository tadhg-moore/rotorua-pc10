# Sanity checks for a lake's ensemble (baseline or a scenario):
#   1. ensemble confidence bands vs observed time series, per variable
#      (rows = depth class: surface / mid / bottom / whole_lake)
#   2. observed vs modelled (ensemble median, 5-95% whiskers), per variable
#   3. metrics table + integrity checks, written next to the plots
#
# Usage (R 4.6.x, project root):
#   Rscript R/pest_workflow/sanity_checks.R <LAKE_TAG> [SCENARIO=baseline]
#   env CHECK_WORKERS=8 sets parallel workers
#
# Output: pest_workflow/<lake>/checks/<scenario>/
#   ts_<var>.png            bands + obs time series
#   obs_vs_model.png        one panel per variable
#   metrics.csv             n, bias, RMSE, NSE, r, % obs inside 5-95 band
#   integrity.csv           PASS/WARN/FAIL checks (members, period, spread, NAs)
#
# Reads the ensemble through its manifest (sha256-checked), so it cannot plot a
# stale or hand-modified file.

suppressMessages({ library(AEME); library(dplyr); library(ggplot2); library(parallel) })
source("R/pest_workflow/manifest.R")

MODEL <- "glm_aed"
CHECK_VARS <- c("LKE_lvlwtr", "HYD_temp", "HYD_thmcln", "CHM_oxy", "NIT_amm", "NIT_nit",
                "NIT_tn", "PHS_frp", "PHS_tp", "PHY_tchla", "NCS_ss1")
ONE_D <- c("LKE_lvlwtr", "HYD_thmcln", "HYD_strat")
DEPTH_LEVELS <- c("surface", "mid", "bottom", "whole_lake")

classify_depth <- function(variable, depth_m, zmax) {
  if (variable %in% ONE_D || all(is.na(depth_m))) return(rep("whole_lake", length(depth_m)))
  out <- rep("mid", length(depth_m))
  out[!is.na(depth_m) & depth_m <= 2] <- "surface"
  # shallower of (3 m above bed, 85% of depth): the dashboard's zmax - 3 rule leaves
  # Rototoa's deepest observations (23 m of 26.1 m) in 'mid'
  out[!is.na(depth_m) & depth_m >= min(zmax - 3, 0.85 * zmax)] <- "bottom"
  out
}

# One member -> list(sim = daily bins, pairs = obs/sim pairs at observation points)
member_series <- function(aeme, nm, vars) {
  zmax <- tryCatch(AEME::lake(aeme)$depth, error = function(e) NA_real_)
  one <- lapply(vars, function(v) {
    s <- tryCatch(AEME::get_var(aeme, model = MODEL, var_sim = v, use_obs = FALSE,
                                remove_spin_up = TRUE), error = function(e) NULL)
    p <- tryCatch(AEME::get_var(aeme, model = MODEL, var_sim = v, use_obs = TRUE,
                                remove_spin_up = TRUE), error = function(e) NULL)
    zm <- if (is.finite(zmax)) zmax else suppressWarnings(max(c(s$depth, p$depth), na.rm = TRUE))
    sim <- if (!is.null(s) && nrow(s)) {
      dep <- if ("depth" %in% names(s)) s$depth else NA_real_
      data.frame(Date = as.Date(s$Date), depth_class = classify_depth(v, dep, zm),
                 sim = as.numeric(s$value)) |>
        summarise(sim = mean(sim, na.rm = TRUE), .by = c(Date, depth_class)) |>
        mutate(variable = v, member = nm)
    }
    prs <- if (!is.null(p) && nrow(p) && "obs" %in% names(p)) {
      dep <- if ("depth" %in% names(p)) p$depth else NA_real_
      data.frame(Date = as.Date(p$Date), depth = dep, depth_class = classify_depth(v, dep, zm),
                 obs = as.numeric(p$obs), sim = as.numeric(p$sim)) |>
        filter(!is.na(obs), !is.na(sim)) |> mutate(variable = v, member = nm)
    }
    list(sim = sim, pairs = prs)
  })
  list(sim = bind_rows(lapply(one, `[[`, "sim")), pairs = bind_rows(lapply(one, `[[`, "pairs")))
}

# Top-level on purpose: a function created inside run_sanity_checks() would drag
# its whole enclosing environment (the full ensemble) to every worker.
member_task <- function(it, vars) member_series(it$aeme, it$name, vars)

nse  <- function(o, s) 1 - sum((o - s)^2) / sum((o - mean(o))^2)
kge  <- function(o, s) { r <- suppressWarnings(cor(o, s)); a <- sd(s) / sd(o); b <- mean(s) / mean(o)
                         1 - sqrt((r - 1)^2 + (a - 1)^2 + (b - 1)^2) }

run_sanity_checks <- function(lake_tag, scenario = "baseline",
                              n_workers = as.integer(Sys.getenv("CHECK_WORKERS", "8"))) {
  man_dir <- if (scenario == "baseline") wf_dir(lake_tag, "baseline", create = FALSE) else
    wf_dir(lake_tag, "scenarios", scenario, create = FALSE)
  man <- require_current_manifest(lake_tag, if (scenario == "baseline") "baseline" else file.path("scenarios", scenario),
                                  step = paste("ensemble", scenario))
  if (!identical(file_sha256(man$ensemble_file), man$ensemble_sha256))
    stop("ensemble file differs from its manifest", call. = FALSE)
  out_dir <- file.path(PEST_WF_ROOT, lake_tag, "checks", scenario)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  ens <- readRDS(man$ensemble_file)
  is_aeme <- vapply(ens, function(x) isS4(x) && is(x, "Aeme"), logical(1))
  message(sprintf("[%s/%s] %d members (%d Aeme)", lake_tag, scenario, length(ens), sum(is_aeme)))
  ens <- ens[is_aeme]

  cl <- makeCluster(min(n_workers, length(ens)))
  on.exit(stopCluster(cl), add = TRUE)
  clusterEvalQ(cl, suppressMessages({ library(AEME); library(dplyr) }))
  clusterExport(cl, c("MODEL", "ONE_D", "classify_depth", "member_series", "member_task"), envir = globalenv())
  # each worker receives only its own share of members (parLapply splits X)
  items <- lapply(names(ens), function(n) list(name = n, aeme = ens[[n]]))
  res <- parLapply(cl, items, member_task, vars = CHECK_VARS)
  rm(items)
  sim   <- bind_rows(lapply(res, `[[`, "sim"))
  pairs <- bind_rows(lapply(res, `[[`, "pairs"))
  zmax <- AEME::lake(ens[[1]])$depth
  obs_one <- {
    # observations are identical across members; take them from member 1's pairs
    pairs |> filter(member == names(ens)[1])
  }

  # ---- 1. bands vs obs time series --------------------------------------------
  bands <- sim |>
    summarise(q05 = quantile(sim, .05, na.rm = TRUE), q25 = quantile(sim, .25, na.rm = TRUE),
              q50 = median(sim, na.rm = TRUE), q75 = quantile(sim, .75, na.rm = TRUE),
              q95 = quantile(sim, .95, na.rm = TRUE), .by = c(Date, variable, depth_class)) |>
    mutate(depth_class = factor(depth_class, DEPTH_LEVELS))
  obs_ts <- obs_one |> summarise(obs = mean(obs), .by = c(Date, variable, depth_class)) |>
    mutate(depth_class = factor(depth_class, DEPTH_LEVELS))
  for (v in unique(bands$variable)) {
    b <- filter(bands, variable == v); o <- filter(obs_ts, variable == v)
    p <- ggplot() +
      geom_ribbon(data = b, aes(Date, ymin = q05, ymax = q95), fill = "steelblue", alpha = .25) +
      geom_ribbon(data = b, aes(Date, ymin = q25, ymax = q75), fill = "steelblue", alpha = .35) +
      geom_line(data = b, aes(Date, q50), colour = "steelblue4", linewidth = .3) +
      geom_point(data = o, aes(Date, obs), colour = "black", size = .8, alpha = .7) +
      facet_grid(depth_class ~ ., scales = "free_y") +
      labs(title = sprintf("%s | %s | %s", lake_tag, scenario, v),
           subtitle = sprintf("%d members; band = 5-95%% (light), 25-75%% (dark); points = observations",
                              n_distinct(sim$member)), x = NULL, y = v) +
      theme_bw(base_size = 9)
    ggsave(file.path(out_dir, sprintf("ts_%s.png", v)), p, width = 10,
           height = 1.6 + 2 * n_distinct(b$depth_class), dpi = 130)
  }

  # ---- 2. obs vs modelled ------------------------------------------------------
  # ensemble median / band at each observation (Date, depth)
  ovm <- pairs |>
    summarise(obs = first(obs), q05 = quantile(sim, .05, na.rm = TRUE), q50 = median(sim, na.rm = TRUE),
              q95 = quantile(sim, .95, na.rm = TRUE), depth_class = first(depth_class),
              .by = c(variable, Date, depth))
  ovm$depth_class <- factor(ovm$depth_class, DEPTH_LEVELS)
  p2 <- ggplot(ovm, aes(obs, q50, colour = depth_class)) +
    geom_abline(colour = "grey40") +
    geom_linerange(aes(ymin = q05, ymax = q95), alpha = .25, linewidth = .3) +
    geom_point(size = .8, alpha = .6) +
    facet_wrap(~ variable, scales = "free") +
    labs(title = sprintf("%s | %s | observed vs modelled (ensemble median, 5-95%% whiskers)", lake_tag, scenario),
         x = "observed", y = "modelled") + theme_bw(base_size = 9)
  ggsave(file.path(out_dir, "obs_vs_model.png"), p2, width = 12, height = 9, dpi = 130)

  # ---- 3. metrics + integrity -------------------------------------------------
  metrics <- ovm |>
    summarise(n = n(), bias = mean(q50 - obs), rmse = sqrt(mean((q50 - obs)^2)),
              nse = if (n() > 2) nse(obs, q50) else NA_real_,
              kge = if (n() > 2) kge(obs, q50) else NA_real_,
              r = if (n() > 2) suppressWarnings(cor(obs, q50)) else NA_real_,
              pct_obs_in_band = 100 * mean(obs >= q05 & obs <= q95),
              .by = c(variable, depth_class)) |> arrange(variable, depth_class)
  write.csv(metrics, file.path(out_dir, "metrics.csv"), row.names = FALSE)

  per <- range(sim$Date)
  spread <- sim |> summarise(sd = sd(sim, na.rm = TRUE), .by = c(Date, variable, depth_class)) |>
    summarise(med_sd = median(sd, na.rm = TRUE), .by = variable)
  chk <- function(name, ok, detail, warn_only = FALSE)
    data.frame(check = name, status = if (ok) "PASS" else if (warn_only) "WARN" else "FAIL", detail = detail)
  integrity <- bind_rows(
    chk("members", length(ens) >= 0.9 * (man$n_requested %||% length(ens)),
        sprintf("%d good of %s requested (%s GLM-failed)", length(ens), man$n_requested %||% "?", man$n_glm_failed %||% "?"),
        warn_only = TRUE),
    chk("period", per[1] <= as.Date("2014-07-01") && per[2] >= as.Date("2023-06-01"),
        sprintf("model output %s to %s; targets simulation_time 2014-07-01 to 2023-06-30", per[1], per[2])),
    chk("no_all_NA_variables", all(!is.na(sapply(split(sim$sim, sim$variable), function(x) mean(x, na.rm = TRUE)))),
        "every checked variable has finite simulated values"),
    chk("ensemble_has_spread", all(spread$med_sd > 0, na.rm = TRUE),
        paste(sprintf("%s sd=%.3g", spread$variable, spread$med_sd), collapse = "; ")),
    chk("bottom_temp_plausible", {
        t <- sim |> filter(variable == "HYD_temp", depth_class == "bottom"); !nrow(t) || (min(t$sim, na.rm = TRUE) > 2 && max(t$sim, na.rm = TRUE) < 32) },
        "bottom temperature within 2-32 C"),
    chk("obs_coverage", all(is.na(metrics$pct_obs_in_band) | metrics$pct_obs_in_band >= 20),
        sprintf("%d variable/depth groups have <20%% of observations inside the 5-95%% band",
                sum(metrics$pct_obs_in_band < 20, na.rm = TRUE)), warn_only = TRUE)
  )
  write.csv(integrity, file.path(out_dir, "integrity.csv"), row.names = FALSE)
  print(integrity, row.names = FALSE); print(as.data.frame(metrics), digits = 3, row.names = FALSE)
  message("[sanity] plots + tables in ", out_dir)
  invisible(list(metrics = metrics, integrity = integrity))
}

`%||%` <- function(a, b) if (is.null(a)) b else a

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  run_sanity_checks(args[1], if (length(args) >= 2) args[2] else "baseline")
}
