# Stage q best member: annual TP vs observations for flood-day particulate scaling 1 / 0.75 / 0.5 (Q100, Q75, Q50), compared with V1 (stage-o parameters), and the lake P pools in wet vs other years.
suppressMessages({library(dplyr); library(tidyr)})
DOPR <- 0.15 * 30.974 / 1000; MMOL_P <- 30.974 / 1000
sets <- c("P4","S1","O3"); R <- lapply(sets, function(s) readRDS(file.path("calibration/alum/stream", paste0("result_", s, ".rds")))); names(R) <- sets
wet <- c(2011, 2012, 2017, 2018)
tp <- bind_rows(lapply(sets, function(s) R[[s]]$P$PHS_tp |> filter(depth <= 5, is.finite(obs), is.finite(sim)) |> mutate(sim = sim + DOPR, set = s)))
S <- tp |> filter(yr >= 2006, yr <= 2021) |> group_by(set, yr) |> summarise(obs = mean(obs), sim = mean(sim), .groups = "drop") |> mutate(err = sim - obs, wet = yr %in% wet)
cat("Annual surface TP (g/m3):\n"); print(as.data.frame(S |> select(set, yr, sim) |> pivot_wider(names_from = set, values_from = sim) |> left_join(S |> filter(set == "V1") |> select(yr, obs), by = "yr") |> mutate(across(where(is.numeric) & !yr, \(x) round(x, 4)))), row.names = FALSE)
cat("\nSummary (2006-2021):\n"); print(as.data.frame(S |> group_by(set) |> summarise(wet_err = round(mean(err[wet]), 4), other_err = round(mean(err[!wet]), 4), rmse = round(sqrt(mean(err^2)), 4), bias_pct = round(100 * mean(err) / mean(obs), 1), r_year = round(cor(sim, obs), 2), .groups = "drop")))
ov <- bind_rows(lapply(sets, function(s) { x <- R[[s]]$P; f <- function(v) { d <- x[[v]]; d <- d[d$depth <= 5 & is.finite(d$obs) & is.finite(d$sim), ]; 100 * (mean(d$sim) / mean(d$obs) - 1) }
  ox <- x$CHM_oxy |> filter(depth >= 17, is.finite(obs), is.finite(sim)) |> group_by(Date) |> summarise(obs = mean(obs), sim = mean(sim), .groups = "drop")
  tn <- x$NIT_tn; tn <- tn[tn$depth <= 5 & is.finite(tn$obs) & is.finite(tn$sim), ]
  tibble(set = s, FRP_bias = round(f("PHS_frp"), 1), chl_bias = round(f("PHY_tchla"), 1), TN_bias_nooffset = round(100 * (mean(tn$sim) / mean(tn$obs) - 1), 1), anoxic = sum(ox$sim < 2), obs_anoxic = sum(ox$obs < 2)) }))
cat("\nOther variables:\n"); print(as.data.frame(ov))
# lake P pools (top 5 m, mmol P/m3 -> g P/m3), annual means, wet vs other years
if (!is.null(R$Q100$pool) && ncol(R$Q100$pool) > 4) {
  pl <- R$Q100$pool |> mutate(yr = as.integer(format(Date, "%Y"))) |> filter(yr >= 2006, yr <= 2021) |> select(-any_of(c("ala", "alp", "ALU_ala")))
  nm <- setdiff(names(pl), c("Date", "yr")); cat("\nPool variables found:", paste(nm, collapse = ", "), "\n")
  A <- pl |> group_by(yr) |> summarise(across(all_of(nm), \(x) mean(x, na.rm = TRUE)), .groups = "drop") |> mutate(wet = yr %in% wet)
  mmol <- intersect(nm, c("PHS_frp", "OGM_dop", "OGM_pop", "PHS_frp_ads", "ALU_alp", grep("_IP$", nm, value = TRUE)))
  cat("\nTop 5 m annual mean, g P/m3 (mmol x 0.031): wet years vs other years (Q100)\n")
  W <- A |> group_by(wet) |> summarise(across(all_of(mmol), \(x) round(mean(x) * MMOL_P, 4)), .groups = "drop"); print(as.data.frame(W))
  print(as.data.frame(A |> mutate(across(all_of(mmol), \(x) round(x * MMOL_P, 4))) |> select(yr, all_of(mmol))), row.names = FALSE)
}
