suppressMessages({library(dplyr); library(tidyr)})
DOPR <- 0.15 * 30.974 / 1000
sets <- c("V1", "F50", "F0"); R <- lapply(sets, function(s) readRDS(file.path("calibration/alum/stream", paste0("result_", s, ".rds")))); names(R) <- sets
tp <- bind_rows(lapply(sets, function(s) R[[s]]$P$PHS_tp |> filter(depth <= 5, is.finite(obs), is.finite(sim)) |> mutate(sim = sim + DOPR, set = s)))
A <- tp |> group_by(set, yr) |> summarise(obs = mean(obs), sim = mean(sim), .groups = "drop") |> pivot_wider(names_from = set, values_from = sim)
cat("Annual surface TP (g/m3): obs, V1 (flood x1), F50 (x0.5), F0 (x0)\n"); print(as.data.frame(A |> mutate(across(where(is.numeric) & !yr, \(x) round(x, 4)))), row.names = FALSE)
wet <- c(2011, 2012, 2017, 2018); dry <- setdiff(2006:2021, wet)
S <- tp |> mutate(yr = as.integer(yr)) |> filter(yr >= 2006, yr <= 2021) |> group_by(set, yr) |> summarise(obs = mean(obs), sim = mean(sim), .groups = "drop") |> mutate(err = sim - obs, wet = yr %in% wet)
cat("\nMean error (sim - obs, g/m3) in wet years (2011, 2012, 2017, 2018) and other years; RMSE over all years; mean bias % overall:\n")
print(as.data.frame(S |> group_by(set) |> summarise(wet_err = round(mean(err[wet]), 4), other_err = round(mean(err[!wet]), 4), rmse_annual = round(sqrt(mean(err^2)), 4), bias_pct = round(100 * mean(err) / mean(obs), 1), r_year = round(cor(sim, obs), 2), .groups = "drop")))
ov <- bind_rows(lapply(sets, function(s) { x <- R[[s]]$P; tibble(set = s,
  chl_bias = round(100 * (mean(x$PHY_tchla$sim[x$PHY_tchla$depth <= 5], na.rm = TRUE) / mean(x$PHY_tchla$obs[x$PHY_tchla$depth <= 5], na.rm = TRUE) - 1), 1),
  FRP_bias = round(100 * (mean(x$PHS_frp$sim[x$PHS_frp$depth <= 5], na.rm = TRUE) / mean(x$PHS_frp$obs[x$PHS_frp$depth <= 5], na.rm = TRUE) - 1), 1)) })); cat("\nOther variables, surface (0-5 m), 2005-2022:\n"); print(as.data.frame(ov))
ox <- bind_rows(lapply(sets, function(s) R[[s]]$P$CHM_oxy |> filter(depth >= 17, is.finite(obs), is.finite(sim)) |> group_by(Date) |> summarise(obs = mean(obs), sim = mean(sim), .groups = "drop") |> mutate(set = s)))
cat("\nBottom DO anoxic days (observed 239):\n"); print(as.data.frame(ox |> group_by(set) |> summarise(sim_anoxic = sum(sim < 2), obs_anoxic = sum(obs < 2), bias = round(mean(sim - obs), 3), .groups = "drop")))
