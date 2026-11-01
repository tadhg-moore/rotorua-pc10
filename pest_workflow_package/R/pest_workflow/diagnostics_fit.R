# Fit diagnostics (read-only, light CPU):
#  D1 skill inside vs outside the calibration window
#  D2 calibrated parameters sitting at their bounds
#  D3 inflow suspended solids vs lake suspended solids
#  D4 observation censoring / basic obs stats
# Output: pest_workflow/diagnostics/*.csv and printed summary.
#   Rscript R/pest_workflow/diagnostics_fit.R

suppressMessages({ library(targets); library(AEME); library(dplyr); library(tidyr) })
source("R/pest_workflow/manifest.R")
source("R/pest_workflow/sanity_checks.R")        # member_series(), classify_depth()
MD <- "pest_workflow/diagnostics"; dir.create(MD, recursive = TRUE, showWarnings = FALSE)

LAKES <- list(
  LID50270_rototoa   = list(win = c("2021-01-01", "2023-01-01")),   # from the v6 calibration log
  LID50269_kuwakatai = list(win = c("2021-08-04", "2023-08-04"))
)
N_MEM <- 8
VARS <- c("HYD_temp", "CHM_oxy", "PHY_tchla", "NCS_ss1", "NIT_amm", "PHS_frp", "PHS_tp", "NIT_tn")

# ---- D1: inside vs outside the calibration window ------------------------------------
d1 <- list(); simmean <- list()
for (tag in names(LAKES)) {
  man <- require_current_manifest(tag, "baseline", step = "baseline")
  ens <- readRDS(man$ensemble_file)
  ens <- ens[vapply(ens, function(x) isS4(x) && is(x, "Aeme"), logical(1))]
  ens <- ens[seq_len(min(N_MEM, length(ens)))]
  res <- lapply(names(ens), function(n) member_series(ens[[n]], n, VARS))
  rm(ens); invisible(gc())
  pairs <- bind_rows(lapply(res, `[[`, "pairs")); sim <- bind_rows(lapply(res, `[[`, "sim"))
  w <- as.Date(LAKES[[tag]]$win)
  ovm <- pairs |> summarise(obs = first(obs), sim = median(sim, na.rm = TRUE),
                            .by = c(variable, depth_class, Date, depth)) |>
    mutate(window = ifelse(Date >= w[1] & Date <= w[2], "in_calibration_window", "outside"))
  d1[[tag]] <- ovm |> summarise(n = n(), obs_mean = mean(obs), sim_mean = mean(sim),
                                bias = mean(sim - obs), rmse = sqrt(mean((sim - obs)^2)),
                                .by = c(variable, depth_class, window)) |> mutate(lake = tag)
  simmean[[tag]] <- sim |> summarise(sim_mean = mean(sim, na.rm = TRUE), .by = c(variable, depth_class)) |> mutate(lake = tag)
}
d1 <- bind_rows(d1) |> arrange(lake, variable, depth_class, window)
write.csv(d1, file.path(MD, "d1_window_skill.csv"), row.names = FALSE)

# ---- D2: parameters at their bounds ----------------------------------------------------
read_best <- function(f, lake) { x <- read.csv(f, stringsAsFactors = FALSE); x$lake <- lake; x }
files <- c(
  setNames(file.path("pest_workflow", names(LAKES), "posterior", "best_params.csv"), names(LAKES)),
  Tomarata = "dashboard_inputs/calibration/LID21871_tomarata_CURRENT_best_params.csv",
  Kawaupaka = "dashboard_inputs/calibration/LID45818_kawaupaka_CURRENT_best_params.csv",
  TeKanae = "dashboard_inputs/calibration/LID49895_tekanae_CURRENT_best_params.csv",
  Spectacle_old = "dashboard_inputs/calibration/LID22999_spectacle_CURRENT_best_params.csv")
files <- files[file.exists(files)]
bp <- bind_rows(Map(read_best, files, names(files)))
bp <- bp |> filter(is.finite(min), is.finite(max), max > min) |>
  mutate(pos = (value - min) / (max - min),
         at_bound = ifelse(pos <= 0.03, "lower", ifelse(pos >= 0.97, "upper", "")))
d2 <- bp |> filter(at_bound != "") |> select(lake, name, group, index, value, min, max, pos, at_bound) |> arrange(name, lake)
write.csv(d2, file.path(MD, "d2_params_at_bounds.csv"), row.names = FALSE)
d2_count <- d2 |> summarise(lakes = paste(lake, collapse = ","), n_lakes = n(), .by = c(name, at_bound)) |> arrange(desc(n_lakes))
write.csv(d2_count, file.path(MD, "d2_params_at_bounds_by_param.csv"), row.names = FALSE)

# ---- D3 + D4: inflow suspended solids and observation stats --------------------------------
up <- tar_read(aeme_baseline_upstream); dn <- tar_read(aeme_baseline_downstream)
get_aeme <- function(lid) { for (l in c(up, dn)) if (identical(AEME::lake(l)$id, lid)) return(l); NULL }
d3 <- list(); d4 <- list()
for (tag in names(LAKES)) {
  a <- get_aeme(sub("_.*$", "", tag)); w <- as.Date(LAKES[[tag]]$win)
  inf <- AEME::get_inflows(a)
  for (nm in names(inf)) {
    x <- inf[[nm]]; x$Date <- as.Date(x$Date)
    for (v in intersect(c("NCS_ss1", "NCS_tss"), names(x))) {
      d3[[length(d3) + 1]] <- data.frame(lake = tag, inflow = nm, variable = v,
        flow_weighted_mean = sum(x[[v]] * x$HYD_flow, na.rm = TRUE) / sum(x$HYD_flow[!is.na(x[[v]])], na.rm = TRUE),
        median = median(x[[v]], na.rm = TRUE), max = max(x[[v]], na.rm = TRUE), mean_flow = mean(x$HYD_flow, na.rm = TRUE))
    }
  }
  ob <- AEME::observations(a)$lake
  ob <- ob[ob$var_aeme %in% c("NCS_ss1", "NCS_tss", "PHY_tchla", "CHM_oxy", "NIT_amm", "NIT_nit", "PHS_frp", "PHS_tp", "NIT_tn", "RAD_secchi"), ]
  d4[[tag]] <- ob |> summarise(n = n(), min = min(value, na.rm = TRUE), median = median(value, na.rm = TRUE),
                               mean = mean(value, na.rm = TRUE), max = max(value, na.rm = TRUE),
                               pct_at_min = 100 * mean(value <= min(value, na.rm = TRUE)),
                               .by = var_aeme) |> mutate(lake = tag)
}
d3 <- bind_rows(d3)
lake_ss <- bind_rows(d4) |> filter(var_aeme %in% c("NCS_ss1", "NCS_tss")) |> select(lake, var_aeme, obs_n = n, obs_mean = mean, obs_median = median)
write.csv(d3, file.path(MD, "d3_inflow_ss.csv"), row.names = FALSE)
d4 <- bind_rows(d4) |> select(lake, var_aeme, everything()) |> arrange(lake, var_aeme)
write.csv(d4, file.path(MD, "d4_obs_stats.csv"), row.names = FALSE)

cat("\n===== D1: window skill (median of", N_MEM, "members) =====\n")
print(as.data.frame(d1 |> mutate(across(where(is.numeric), \(x) signif(x, 3)))), row.names = FALSE)
cat("\n===== D2: parameters at bounds, by parameter (lakes:", paste(unique(bp$lake), collapse = ","), ") =====\n")
print(as.data.frame(d2_count |> head(25)), row.names = FALSE)
cat("\n===== D3: inflow suspended solids (mg/L?) vs lake obs =====\n")
print(as.data.frame(d3 |> mutate(across(where(is.numeric), \(x) signif(x, 3)))), row.names = FALSE)
print(as.data.frame(lake_ss), row.names = FALSE)
cat("\nmodelled lake NCS_ss1 mean:\n"); print(as.data.frame(bind_rows(simmean) |> filter(variable == "NCS_ss1")), row.names = FALSE)
cat("\n===== D4: observation stats =====\n")
print(as.data.frame(d4 |> mutate(across(where(is.numeric), \(x) signif(x, 3)))), row.names = FALSE)
