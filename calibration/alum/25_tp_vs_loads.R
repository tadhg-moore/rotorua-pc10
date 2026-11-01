# Does the lake TP error (V1 run, 2005-2022) track the inflow P loads?  Annual and monthly comparison of observed and simulated surface TP with the inflow loads by form.
suppressMessages({library(dplyr); library(tidyr); library(readr)})
DOPR <- 0.15 * 30.974 / 1000
r <- readRDS("calibration/alum/stream/result_V1.rds")
tp <- r$P$PHS_tp |> filter(depth <= 5, is.finite(obs), is.finite(sim)) |> mutate(sim = sim + DOPR, ym = format(Date, "%Y-%m"))
n <- read_csv("data/raw/flows/rotorua_inflows_populated_noalum.csv", show_col_types = FALSE); n$Date <- as.Date(n$Date); n$yr <- as.integer(format(n$Date, "%Y")); n$ym <- format(n$Date, "%Y-%m")
n <- n |> filter(Inflow != "Rainfall" | TRUE) |> mutate(across(c(DRP, DOPL, POPL, PIP), \(x) x * VOL / 1e6, .names = "L_{.col}"), L_TP = TP * VOL / 1e6)     # t P/day
ld <- n |> group_by(yr) |> summarise(vol_hm3 = sum(VOL[Inflow != "Rainfall"]) / 1e6, TP_t = sum(L_TP), DRP_t = sum(L_DRP), DOP_t = sum(L_DOPL), POP_t = sum(L_POPL), PIP_t = sum(L_PIP), .groups = "drop") |> mutate(part_t = POP_t + PIP_t)
A <- tp |> group_by(yr) |> summarise(n = n(), obs = mean(obs), sim = mean(sim), .groups = "drop") |> left_join(ld, by = "yr") |> mutate(err = sim - obs, rel = 100 * err / obs)
cat("Annual surface TP (g/m3, sim includes the refractory DOP offset) and inflow loads (t P/yr):\n"); print(as.data.frame(A |> filter(yr <= 2021) |> mutate(across(c(obs, sim, err), \(x) round(x, 4)), rel = round(rel), across(c(vol_hm3, TP_t, DRP_t, DOP_t, part_t, PIP_t, POP_t), \(x) round(x, 1))) |> select(yr, n, obs, sim, err, rel, vol_hm3, TP_t, DRP_t, part_t)), row.names = FALSE)
B <- A |> filter(yr >= 2006, yr <= 2021)
cat("\nCorrelations over 2006-2021 (n =", nrow(B), "):\n")
cc <- function(a, b) round(cor(B[[a]], B[[b]]), 2)
cat("  obs TP vs inflow TP load:", cc("obs", "TP_t"), "| sim TP vs inflow TP load:", cc("sim", "TP_t"), "\n")
cat("  obs TP vs particulate load:", cc("obs", "part_t"), "| sim TP vs particulate load:", cc("sim", "part_t"), "\n")
cat("  obs TP vs inflow volume:", cc("obs", "vol_hm3"), "| sim TP vs inflow volume:", cc("sim", "vol_hm3"), "\n")
cat("  sim-obs error vs inflow TP load:", cc("err", "TP_t"), "| vs particulate load:", cc("err", "part_t"), "| vs DRP load:", cc("err", "DRP_t"), "| vs volume:", cc("err", "vol_hm3"), "\n")
cat("  slope of sim TP on inflow TP load (g/m3 per t):", round(coef(lm(sim ~ TP_t, B))[2], 5), " obs:", round(coef(lm(obs ~ TP_t, B))[2], 5), "\n")
# monthly: lake TP vs inflow load in the preceding 1-3 months
mm <- n |> group_by(ym) |> summarise(TP_t = sum(L_TP), .groups = "drop") |> arrange(ym) |> mutate(TP_3m = TP_t + lag(TP_t) + lag(TP_t, 2))
M <- tp |> group_by(ym) |> summarise(obs = mean(obs), sim = mean(sim), .groups = "drop") |> left_join(mm, by = "ym") |> filter(ym >= "2006-01")
cat("\nMonthly (n =", nrow(M), "): cor(obs, 3-month inflow TP load) =", round(cor(M$obs, M$TP_3m, use = "complete"), 2), "| cor(sim, 3-month load) =", round(cor(M$sim, M$TP_3m, use = "complete"), 2), "| cor(sim, obs) =", round(cor(M$sim, M$obs), 2), "\n")
big <- M |> arrange(desc(TP_3m)) |> head(10); cat("\nTen months with the highest preceding 3-month inflow TP load (t P): obs and sim lake TP (g/m3)\n"); print(as.data.frame(big |> mutate(across(c(obs, sim), \(x) round(x, 4)), across(c(TP_t, TP_3m), \(x) round(x, 1)))), row.names = FALSE)
# which P forms dominate the inflow TP in those wet years?
cat("\nInflow P by form, mean t P/yr: wet years (2011, 2017) vs others 2006-2021\n"); w <- ld |> filter(yr >= 2006, yr <= 2021) |> mutate(wet = yr %in% c(2011, 2017)) |> group_by(wet) |> summarise(across(c(DRP_t, DOP_t, POP_t, PIP_t, TP_t), \(x) round(mean(x), 1)), .groups = "drop"); print(as.data.frame(w))
