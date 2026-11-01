# Compare two calibration recipes for the same lake from their baseline ensembles
# (A = raw chlorophyll targets, B = log1p chlorophyll targets), scored against the
# RAW observations so the comparison is not tied to either objective.
#
#   Rscript R/pest_workflow/compare_ab_calibrations.R
#   env: AB_TAG (default LID50270_rototoa), AB_A_ROOT / AB_B_ROOT (workflow roots holding each
#        recipe's baseline manifest), AB_OUT (output dir), AB_A_FILE / AB_B_FILE (ensemble rds overrides),
#        AB_WORKERS (default 8), AB_MEMBERS (default all), AB_WINDOW ("2021-01-01,2023-01-01")
#
# Output (pest_workflow_logphy/ab_comparison/):
#   ab_metrics.csv       per variable x depth class: bias, RMSE, NSE, KGE, % obs inside the
#                        5-95% ensemble band, in/out of the calibration window, for A and B
#   ab_logchl.csv        chlorophyll-a / cyanobacteria on the log1p scale + bloom metrics
#   ab_summary.csv       RMSE ratio B/A per variable (>1.1 flagged: B worse)

suppressMessages({ library(AEME); library(dplyr); library(tidyr); library(parallel) })
source("R/pest_workflow/manifest.R")
source("R/pest_workflow/sanity_checks.R")     # member_series(), member_task, nse(), kge()

tag <- Sys.getenv("AB_TAG", "LID50270_rototoa")
n_workers <- as.integer(Sys.getenv("AB_WORKERS", "8"))
win <- as.Date(strsplit(Sys.getenv("AB_WINDOW", "2021-01-01,2023-01-01"), ",")[[1]])
n_mem <- Sys.getenv("AB_MEMBERS", "")
VARS <- c("HYD_temp", "CHM_oxy", "PHY_tchla", "PHY_cyano", "NIT_amm", "PHS_frp", "PHS_tp", "NIT_tn", "NCS_ss1")

ens_file <- function(env_name, wf_root) {
  f <- Sys.getenv(env_name, "")
  if (nzchar(f)) return(f)
  read_manifest(file.path(wf_root, tag, "baseline"))$ensemble_file
}
A_ROOT <- Sys.getenv("AB_A_ROOT", "pest_workflow"); B_ROOT <- Sys.getenv("AB_B_ROOT", "pest_workflow_logphy")
files <- c(A = ens_file("AB_A_FILE", A_ROOT), B = ens_file("AB_B_FILE", B_ROOT))
st <- targets::tar_read(simulation_time)
trim <- function(d) d[d$Date >= as.Date(st$start) & d$Date <= as.Date(st$stop), ]

pairs_for <- function(f) {
  message("reading ", f)
  ens <- readRDS(f); ens <- ens[vapply(ens, function(x) isS4(x) && is(x, "Aeme"), logical(1))]
  if (nzchar(n_mem)) ens <- ens[seq_len(min(as.integer(n_mem), length(ens)))]
  items <- lapply(names(ens), function(n) list(name = n, aeme = ens[[n]])); rm(ens); invisible(gc())
  cl <- makeCluster(min(n_workers, length(items))); on.exit(stopCluster(cl), add = TRUE)
  clusterEvalQ(cl, suppressMessages({ library(AEME); library(dplyr) }))
  clusterExport(cl, c("MODEL", "ONE_D", "classify_depth", "member_series", "member_task"), envir = globalenv())
  res <- parLapply(cl, items, member_task, vars = VARS)
  list(pairs = trim(bind_rows(lapply(res, `[[`, "pairs"))), n = length(items))
}
res <- lapply(files, pairs_for)

ovm_of <- function(p) p |>
  summarise(obs = first(obs), q05 = quantile(sim, .05, na.rm = TRUE), sim = median(sim, na.rm = TRUE),
            q95 = quantile(sim, .95, na.rm = TRUE), depth_class = first(depth_class),
            .by = c(variable, Date, depth))
metrics_of <- function(o, by) o |> summarise(
  n = n(), bias = mean(sim - obs), rmse = sqrt(mean((sim - obs)^2)),
  nse = if (n() > 2) nse(obs, sim) else NA_real_, kge = if (n() > 2) kge(obs, sim) else NA_real_,
  pct_in_band = 100 * mean(obs >= q05 & obs <= q95), .by = all_of(by))

ovm <- lapply(res, function(r) ovm_of(r$pairs))
ovm <- Map(function(o) mutate(o, window = ifelse(Date >= win[1] & Date <= win[2], "in_window", "outside")), ovm)
m_all <- bind_rows(lapply(names(ovm), function(k) metrics_of(ovm[[k]], c("variable", "depth_class")) |> mutate(recipe = k, window = "all")))
m_win <- bind_rows(lapply(names(ovm), function(k) metrics_of(ovm[[k]], c("variable", "depth_class", "window")) |> mutate(recipe = k)))
metrics <- bind_rows(m_all, m_win) |> arrange(variable, depth_class, window, recipe)

# ---- log scale + bloom metrics for the phytoplankton variables --------------------------------
logm <- bind_rows(lapply(names(ovm), function(k) {
  o <- ovm[[k]] |> filter(variable %in% c("PHY_tchla", "PHY_cyano"))
  thr <- o |> summarise(thr = quantile(obs, .9, na.rm = TRUE), .by = c(variable, depth_class))
  o <- left_join(o, thr, by = c("variable", "depth_class"))
  o |> summarise(n = n(), log1p_bias = mean(log1p(sim) - log1p(obs)), log1p_rmse = sqrt(mean((log1p(sim) - log1p(obs))^2)),
                 spearman = suppressWarnings(cor(obs, sim, method = "spearman")),
                 obs_bloom_n = sum(obs >= thr), bloom_hit_rate = mean(sim[obs >= thr] >= thr[obs >= thr]),
                 false_alarm_rate = mean(sim[obs < thr] >= thr[obs < thr]),
                 .by = c(variable, depth_class)) |> mutate(recipe = k)
})) |> arrange(variable, depth_class, recipe)

# ---- RMSE ratio B/A -------------------------------------------------------------------------------
ratio <- m_all |> select(variable, depth_class, recipe, rmse, bias) |>
  pivot_wider(names_from = recipe, values_from = c(rmse, bias)) |>
  mutate(rmse_ratio_B_over_A = rmse_B / rmse_A,
         verdict = ifelse(rmse_ratio_B_over_A > 1.1, "B WORSE", ifelse(rmse_ratio_B_over_A < 0.9, "B better", "similar"))) |>
  arrange(variable, depth_class)

out <- Sys.getenv("AB_OUT", file.path(B_ROOT, "ab_comparison"))
dir.create(out, recursive = TRUE, showWarnings = FALSE)
write.csv(metrics, file.path(out, "ab_metrics.csv"), row.names = FALSE)
write.csv(logm, file.path(out, "ab_logchl.csv"), row.names = FALSE)
write.csv(ratio, file.path(out, "ab_summary.csv"), row.names = FALSE)

cat(sprintf("\nmembers: A=%d, B=%d | calibration window %s to %s\n", res$A$n, res$B$n, win[1], win[2]))
cat("\n===== RMSE (raw units) and ratio B/A, all dates =====\n")
print(as.data.frame(ratio |> mutate(across(where(is.numeric), \(v) signif(v, 3)))), row.names = FALSE)
cat("\n===== phytoplankton on the log1p scale, and bloom skill (obs >= own 90th percentile) =====\n")
print(as.data.frame(logm |> mutate(across(where(is.numeric), \(v) signif(v, 3)))), row.names = FALSE)
cat("\n===== chlorophyll-a skill inside vs outside the calibration window =====\n")
print(as.data.frame(m_win |> filter(variable == "PHY_tchla") |> select(depth_class, window, recipe, n, bias, rmse, kge, pct_in_band) |>
                      mutate(across(where(is.numeric), \(v) signif(v, 3))) |> arrange(depth_class, window, recipe)), row.names = FALSE)
