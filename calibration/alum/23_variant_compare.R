# Step 2: compare the in-stream reaction variants (V0 none, V1 full, V2 half) and the old base (efficiency_sensitivity_pairs.rds, scale 1.0 and 0.85-equivalent run 0.7) on surface TP, FRP, chlorophyll and bottom DO.
suppressMessages({library(dplyr); library(tidyr)})
V <- lapply(c("V0", "V1", "V2"), function(v) { f <- file.path("calibration", "alum", "stream", paste0("result_", v, ".rds")); if (file.exists(f)) readRDS(f) else NULL }); V <- Filter(Negate(is.null), V)
old <- readRDS("calibration/alum/efficiency_sensitivity_pairs.rds")[["0.7"]]            # old base (hybrid inflows), k_w and k_s x 0.7 (closest to 0.85 that was run is 0.7; 1.0 also shown)
old1 <- readRDS("calibration/alum/efficiency_sensitivity_pairs.rds")[["1"]]
oldall <- readRDS("calibration/alum/efficiency_sensitivity_pairs.rds"); sets <- c(setNames(lapply(oldall, function(p) list(P = p, pool = NULL)), paste0("old_k", names(oldall))), setNames(lapply(V, function(r) list(P = r$P, pool = r$pool)), vapply(V, function(r) r$name, "")))
per <- function(y) cut(y, c(2004, 2008, 2012, 2016, 2019, 2022), labels = c("2005-08", "2009-12", "2013-16", "2017-19", "2020-22"))
MMOL_P <- 30.974 / 1000
DOPR_GM3 <- 0.15 * 30.974 / 1000                                                          # refractory DOP missing from the model TP (as in calibration/step2/alum_resid.R)                                                                  # mmol P/m3 -> g P/m3
prep <- function(s, nm, with_alp = FALSE) { d <- s$P[[nm]]; d <- d[d$depth <= 5, ]; if (nm == "PHS_tp") d$sim <- d$sim + DOPR_GM3
  if (with_alp && !is.null(s$pool)) { a <- s$pool[, c("Date", "alp")]; d <- left_join(d, a, by = "Date"); d$sim <- d$sim + ifelse(is.na(d$alp), 0, d$alp * MMOL_P) }; d }
met <- bind_rows(lapply(names(sets), function(k) { s <- sets[[k]]
  rows <- list(TP = prep(s, "PHS_tp"), `TP+alp` = prep(s, "PHS_tp", TRUE), FRP = prep(s, "PHS_frp"), chl = prep(s, "PHY_tchla"))
  bind_rows(lapply(names(rows), function(v) { d <- rows[[v]]; d <- d[is.finite(d$obs) & is.finite(d$sim), ]; if (!nrow(d)) return(NULL)
    tibble(set = k, var = v, bias_pct = round(100 * (mean(d$sim) - mean(d$obs)) / mean(d$obs), 1), rmse = round(sqrt(mean((d$sim - d$obs)^2)), 4), r = round(cor(d$sim, d$obs), 3)) })) }))
cat("Overall surface fit (0-5 m), 2005-2022:\n"); print(as.data.frame(met |> pivot_wider(names_from = var, values_from = c(bias_pct, rmse, r))), width = 200)
cat("\nSurface TP (incl. alum-bound P for the new variants) by period, sim; observed in the last row (g/m3):\n")
tp <- bind_rows(lapply(names(sets), function(k) { d <- prep(sets[[k]], "PHS_tp", TRUE); d$set <- k; d })); tp$per <- per(tp$yr)
print(as.data.frame(tp |> group_by(set, per) |> summarise(sim = round(mean(sim, na.rm = TRUE), 4), .groups = "drop") |> pivot_wider(names_from = per, values_from = sim)))
cat("obs:", paste(round((tp |> filter(set == names(sets)[1]) |> group_by(per) |> summarise(o = mean(obs, na.rm = TRUE)))$o, 4), collapse = " "), "\n")
frp <- bind_rows(lapply(names(sets), function(k) { d <- prep(sets[[k]], "PHS_frp"); d$set <- k; d })); frp$per <- per(frp$yr)
cat("\nSurface FRP by period (sim); obs last row:\n"); print(as.data.frame(frp |> group_by(set, per) |> summarise(sim = round(mean(sim, na.rm = TRUE), 4), .groups = "drop") |> pivot_wider(names_from = per, values_from = sim)))
cat("obs:", paste(round((frp |> filter(set == names(sets)[1]) |> group_by(per) |> summarise(o = mean(obs, na.rm = TRUE)))$o, 4), collapse = " "), "\n")
ox <- bind_rows(lapply(names(sets), function(k) sets[[k]]$P$CHM_oxy |> filter(depth >= 17, is.finite(obs), is.finite(sim)) |> group_by(Date) |> summarise(obs = mean(obs), sim = mean(sim), .groups = "drop") |> mutate(set = k)))
cat("\nBottom DO (>= 17 m, observed days):\n"); print(as.data.frame(ox |> group_by(set) |> summarise(obs_anoxic = sum(obs < 2), sim_anoxic = sum(sim < 2), bias = round(mean(sim - obs), 3), r = round(cor(sim, obs), 3), .groups = "drop")))
cat("\nStep-3 style residuals (calibration 2006-2018; lower |value| is better):\n")
for (r in V) { cat(r$name, ": ", paste(names(r$cal), round(r$cal, 2), sep = "=", collapse = " "), "\n") }
