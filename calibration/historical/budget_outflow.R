# (1) AEME:::plot_glm_budget() of the latest output (member 46 of stage j, alum on, new AEME dev and GLM-AED).
# (2) Outflow from AEME::aeme_to_inflow() (GLM mass balance: flow-weighted concentration of the water that left the lake) against the measured Ohau Channel
#     discharge (target rotorua_flow_data) and lab results (target ohau_wq_data), with the top 5 m layer for comparison.
# Run: calibration/historical/run_hist.R with HIST_KEEP=1 HIST_TAG=budget_m46 HIST_N=1 first (writes out_budget_m46/aeme_46.rds).
suppressMessages({library(AEME); library(dplyr); library(tidyr); library(ggplot2); library(readr); library(lubridate)})
out <- "website/images/budget"; dat <- "website/data/budget"; dir.create(out, recursive = TRUE, showWarnings = FALSE); dir.create(dat, recursive = TRUE, showWarnings = FALSE)
dd <- "calibration/historical/out_budget_m46"; aeme <- readRDS(file.path(dd, "aeme_46.rds")); mem <- readRDS(file.path(dd, "member_46.rds"))$daily
# ---- (1) budget ----
bud <- AEME:::diag_glm_budget(aeme, element = c("N", "P", "C"), period = "all", include_spin_up = FALSE); saveRDS(bud, file.path(dd, "glm_budget.rds"))
p <- AEME:::plot_glm_budget(bud, units = "kg_yr"); ggsave(file.path(out, "glm_budget.png"), p, width = 15, height = 7.5, dpi = 130, bg = "white")
p2 <- AEME:::plot_glm_budget(bud, element = "P", units = "mmol_m2_d", show_residual = TRUE); ggsave(file.path(out, "glm_budget_P_areal.png"), p2, width = 8, height = 7.5, dpi = 130, bg = "white")
# ---- (2) outflow ----
DOPR <- 0.15 * 30.974 / 1000; DONR <- 9 * 14.0067 / 1000; MP <- 30.974 / 1000; MN <- 14.007 / 1000
x <- AEME::aeme_to_inflow(aeme) |> filter(Date >= "2005-07-01", Date <= "2022-06-29") |> transmute(Date, flow = HYD_flow, TP = PHS_tp + DOPR, DRP = PHS_frp, TN = NIT_tn + DONR, `NH4-N` = NIT_amm, NNN = NIT_nit, `Water Temp` = HYD_temp)
sf <- mem |> filter(Date >= "2005-07-01", Date <= "2022-06-29") |> transmute(Date, TP = TOT_tp_surf * MP + DOPR, DRP = PHS_frp_surf * MP, TN = TOT_tn_surf * MN + DONR, `NH4-N` = NIT_amm_surf * MN, NNN = NIT_nit_surf * MN, `Water Temp` = temp_surf)
# flow
fl <- targets::tar_read(rotorua_flow_data); vc <- grep("^Value", names(fl), value = TRUE)[1]; cat("measured flow column:", vc, "\n")
mf <- fl |> transmute(Date = as.Date(`Timestamp (UTC+12:00)`), q = .data[[vc]]) |> filter(is.finite(q)) |> group_by(Date) |> summarise(q = mean(q), n = n(), .groups = "drop") |> filter(n >= 12) |> mutate(obs = q * 86400)   # m3/s -> m3/d (days with at least 12 records)
cat("measured flow", format(min(mf$Date)), "to", format(max(mf$Date)), "n", nrow(mf), "| median m3/s", median(mf$q), "\n")
jf <- inner_join(x |> select(Date, sim = flow), mf |> select(Date, obs), by = "Date"); stf <- function(d) tibble(n = nrow(d), obs_mean_m3s = mean(d$obs) / 86400, sim_mean_m3s = mean(d$sim) / 86400, bias_pct = 100 * (mean(d$sim) / mean(d$obs) - 1), r = cor(d$sim, d$obs), rmse_m3s = sqrt(mean((d$sim - d$obs)^2)) / 86400, nse = 1 - sum((d$sim - d$obs)^2) / sum((d$obs - mean(d$obs))^2))
per <- \(d) as.vector(ifelse(d < "2011-01-01", "2005 to 2010", "2011 to 2022"))
sf1 <- bind_rows(jf |> stf() |> mutate(period = "All"), jf |> mutate(period = per(Date)) |> group_by(period) |> group_modify(~stf(.x)) |> ungroup()); sf1 <- sf1 |> mutate(across(where(is.numeric), \(v) signif(v, 3))); write_csv(sf1, file.path(dat, "outflow_flow_stats.csv")); print(as.data.frame(sf1), row.names = FALSE)
mm <- jf |> mutate(m = floor_date(Date, "month")) |> group_by(m) |> summarise(across(c(sim, obs), mean), .groups = "drop") |> mutate(across(c(sim, obs), \(v) v / 86400))
g1 <- ggplot(mm, aes(m)) + geom_line(aes(y = obs, colour = "Measured (Ohau Channel)"), linewidth = 0.5) + geom_line(aes(y = sim, colour = "Model (aeme_to_inflow)"), linewidth = 0.5) + scale_colour_manual(values = c("Measured (Ohau Channel)" = "black", "Model (aeme_to_inflow)" = "steelblue4")) + labs(x = NULL, y = "Outflow (m3/s), monthly mean", colour = NULL) + theme_minimal(base_size = 10) + theme(legend.position = "bottom")
g2 <- ggplot(jf |> mutate(obs = obs / 86400, sim = sim / 86400), aes(obs, sim)) + geom_abline(colour = "grey50") + geom_point(size = 0.4, alpha = 0.3, colour = "steelblue4") + labs(x = "Measured daily outflow (m3/s)", y = "Modelled daily outflow (m3/s)") + coord_equal() + theme_minimal(base_size = 10)
if (requireNamespace("patchwork", quietly = TRUE)) ggsave(file.path(out, "outflow_flow.png"), patchwork::wrap_plots(g1, g2, widths = c(2, 1)), width = 13, height = 4.8, dpi = 130, bg = "white") else { ggsave(file.path(out, "outflow_flow.png"), g1, width = 10, height = 4, dpi = 130, bg = "white"); ggsave(file.path(out, "outflow_flow_scatter.png"), g2, width = 5, height = 5, dpi = 130, bg = "white") }
# concentrations on the lab sampling dates
wq <- targets::tar_read(ohau_wq_data) |> transmute(Date = as.Date(Timestamp), variable, obs = value) |> filter(Date >= "2005-07-01", Date <= "2022-06-29", variable %in% c("TP", "DRP", "TN", "NH4-N", "NNN", "Water Temp")) |> group_by(Date, variable) |> summarise(obs = mean(obs), .groups = "drop")
L <- bind_rows(x |> select(-flow) |> pivot_longer(-Date, names_to = "variable", values_to = "sim") |> mutate(src = "Mass-balance outflow"), sf |> pivot_longer(-Date, names_to = "variable", values_to = "sim") |> mutate(src = "Top 5 m layer"))
J <- inner_join(L, wq, by = c("Date", "variable")) |> filter(!(variable == "TN" & Date < "2009-01-01")) |> mutate(period = per(Date))
sc <- function(d) tibble(n = nrow(d), obs_median = median(d$obs), sim_median = median(d$sim), bias_pct = 100 * (mean(d$sim) / mean(d$obs) - 1), r = cor(d$sim, d$obs), rmse = sqrt(mean((d$sim - d$obs)^2)))
st <- bind_rows(J |> group_by(variable, src) |> group_modify(~sc(.x)) |> ungroup() |> mutate(period = "All"), J |> group_by(variable, src, period) |> group_modify(~sc(.x)) |> ungroup()) |> mutate(across(where(is.numeric), \(v) signif(v, 3))) |> arrange(variable, period, src)
write_csv(st, file.path(dat, "outflow_conc_stats.csv")); cat("\nOutflow concentrations against the Ohau lab samples:\n"); print(as.data.frame(st), row.names = FALSE)
vl <- c(TP = "Total P (g/m3)", DRP = "DRP (g/m3)", TN = "Total N (g/m3)", `NH4-N` = "NH4-N (g/m3)", NNN = "Nitrate+nitrite N (g/m3)", `Water Temp` = "Water temperature (C)")
Lp <- L |> mutate(lab = factor(vl[variable], vl)); Op <- wq |> mutate(lab = factor(vl[variable], vl))
ggsave(file.path(out, "outflow_conc.png"), ggplot(Lp, aes(Date, sim, colour = src)) + geom_line(linewidth = 0.3, alpha = 0.8) + geom_point(data = Op, aes(Date, obs), colour = "black", size = 0.7, inherit.aes = FALSE) + facet_wrap(~lab, ncol = 1, scales = "free_y") + scale_colour_manual(values = c("Mass-balance outflow" = "firebrick", "Top 5 m layer" = "steelblue")) +
  labs(x = NULL, y = NULL, colour = NULL, subtitle = "Daily model values (lines) and Ohau Channel lab samples (points). TP and TN include the refractory offsets used in the calibration.") + theme_minimal(base_size = 10) + theme(legend.position = "bottom", plot.subtitle = element_text(size = 8)), width = 10, height = 14, dpi = 130, bg = "white")
cat("done\n")
