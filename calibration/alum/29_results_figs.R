# Figures and summary tables for website/rotorua-alum-next-steps.qmd: efficiency sensitivity (with the refractory DOP offset), in-stream reaction variants,
# lake TP vs inflow loads, and the flood-particulate sensitivity.
suppressMessages({library(dplyr); library(tidyr); library(readr); library(ggplot2)})
out <- "website/images/inflows"; dat <- "website/data/inflows"; dir.create(out, recursive = TRUE, showWarnings = FALSE); dir.create(dat, recursive = TRUE, showWarnings = FALSE)
DOPR <- 0.15 * 30.974 / 1000
th <- theme_minimal(base_size = 10) + theme(legend.position = "bottom", strip.text = element_text(face = "bold"))
sv <- function(name, p, w, h) ggsave(file.path(out, name), p, width = w, height = h, dpi = 130, bg = "white")
eff <- readRDS("calibration/alum/efficiency_sensitivity_pairs.rds")
res <- lapply(c("V0", "V1", "V2", "F50", "F0"), function(s) readRDS(file.path("calibration/alum/stream", paste0("result_", s, ".rds")))); names(res) <- c("V0", "V1", "V2", "F50", "F0")
sets <- c(setNames(lapply(eff, function(p) list(P = p)), paste0("old base, k x", names(eff))), setNames(lapply(res, function(r) list(P = r$P, pool = r$pool)), names(res)))
surf <- function(P, v) { d <- P[[v]]; d <- d[d$depth <= 5 & is.finite(d$obs) & is.finite(d$sim), ]; if (v == "PHS_tp") d$sim <- d$sim + DOPR; d }
# ---- summary table of overall fits ----
S <- bind_rows(lapply(names(sets), function(k) { P <- sets[[k]]$P; ox <- P$CHM_oxy |> filter(depth >= 17, is.finite(obs), is.finite(sim)) |> group_by(Date) |> summarise(obs = mean(obs), sim = mean(sim), .groups = "drop")
  tibble(run = k, TP_bias_pct = 100 * (mean(surf(P, "PHS_tp")$sim) / mean(surf(P, "PHS_tp")$obs) - 1), FRP_bias_pct = 100 * (mean(surf(P, "PHS_frp")$sim) / mean(surf(P, "PHS_frp")$obs) - 1),
         chl_bias_pct = 100 * (mean(surf(P, "PHY_tchla")$sim) / mean(surf(P, "PHY_tchla")$obs) - 1), anoxic_days = sum(ox$sim < 2), observed_anoxic_days = sum(ox$obs < 2)) })) |> mutate(across(where(is.numeric), \(x) round(x, 1)))
write_csv(S, file.path(dat, "run_summary.csv"))
# ---- F8: TP bias and FRP bias against the efficiency scale (old base) ----
E <- S |> filter(grepl("^old", run)) |> mutate(scale = as.numeric(sub(".*x", "", run))) |> select(scale, TP = TP_bias_pct, FRP = FRP_bias_pct, chlorophyll = chl_bias_pct) |> pivot_longer(-scale, names_to = "var", values_to = "bias")
sv("efficiency_bias.png", ggplot(E, aes(scale, bias, colour = var)) + geom_hline(yintercept = 0, colour = "grey60") + geom_line() + geom_point() + labs(x = "Scale on k_w and k_s (1 = fitted)", y = "Mean bias, surface 0-5 m (%)", colour = NULL, subtitle = "Old base; TP includes the refractory DOP offset") + th, 7, 4)
# ---- F9: surface TP by period, obs vs the runs ----
per <- function(y) cut(y, c(2004, 2008, 2012, 2016, 2019, 2022), labels = c("2005-08", "2009-12", "2013-16", "2017-19", "2020-22"))
sel <- c("old base, k x1", "V0", "V1", "V2"); tp <- bind_rows(lapply(sel, function(k) surf(sets[[k]]$P, "PHS_tp") |> mutate(run = k))); tp$per <- per(tp$yr)
A <- tp |> group_by(run, per) |> summarise(sim = mean(sim), .groups = "drop"); O <- tp |> filter(run == "V1") |> group_by(per) |> summarise(obs = mean(obs), .groups = "drop")
sv("tp_by_period.png", ggplot(A, aes(per, sim, fill = run)) + geom_col(position = "dodge") + geom_point(data = O, aes(per, obs), inherit.aes = FALSE, shape = 18, size = 4, colour = "black") + labs(x = NULL, y = "Surface TP (g P/m3)", fill = NULL, subtitle = "Diamonds: observed") + th, 8, 4)
# ---- F10: annual TP and the inflow load; error vs load ----
n <- read_csv("data/raw/flows/rotorua_inflows_populated_noalum.csv", show_col_types = FALSE); n$Date <- as.Date(n$Date); n$yr <- as.integer(format(n$Date, "%Y"))
ld <- n |> group_by(yr) |> summarise(TP_t = sum(TP * VOL / 1e6), .groups = "drop")
an <- function(k) surf(sets[[k]]$P, "PHS_tp") |> group_by(yr) |> summarise(obs = mean(obs), sim = mean(sim), .groups = "drop") |> mutate(run = k)
Y <- bind_rows(an("V1"), an("F50") , an("F0")) |> left_join(ld, by = "yr") |> filter(yr >= 2006, yr <= 2021); wet <- c(2011, 2012, 2017, 2018)
y1 <- Y |> filter(run == "V1")
p1 <- ggplot(y1, aes(TP_t, sim - obs, label = yr)) + geom_hline(yintercept = 0, colour = "grey60") + geom_point(aes(colour = yr %in% wet), size = 2) + geom_text(vjust = -0.8, size = 3) + geom_smooth(method = "lm", se = FALSE, colour = "black", linewidth = 0.4) +
  scale_colour_manual(values = c("grey40", "firebrick"), guide = "none") + labs(x = "Annual inflow TP load (t P/yr)", y = "Simulated minus observed surface TP (g/m3)", subtitle = "V1 run; red: wet years") + th
p2 <- Y |> mutate(run = factor(run, c("V1", "F50", "F0"), c("flood POP, PIP x1", "x0.5", "x0"))) |> bind_rows(Y |> filter(run == "V1") |> mutate(run = "observed", sim = obs)) |>
  ggplot(aes(yr, sim, colour = run, linewidth = run == "observed")) + annotate("rect", xmin = wet - 0.5, xmax = wet + 0.5, ymin = -Inf, ymax = Inf, alpha = 0.12) + geom_line() + scale_linewidth_manual(values = c(0.5, 1.2), guide = "none") + labs(x = NULL, y = "Annual mean surface TP (g/m3)", colour = NULL, subtitle = "Grey: wet years") + th
sv("tp_vs_load.png", gridExtra::grid.arrange(p1, p2, ncol = 2), 11, 4.5)
# ---- tables: annual, flood scaling summary ----
W <- Y |> mutate(err = sim - obs, wet = yr %in% wet) |> group_by(run) |> summarise(wet_year_error = round(mean(err[wet]), 4), other_year_error = round(mean(err[!wet]), 4), annual_rmse = round(sqrt(mean(err^2)), 4), year_correlation = round(cor(sim, obs), 2), .groups = "drop") |>
  mutate(run = factor(run, c("V1", "F50", "F0"), c("V1 (flood particulate P x1)", "F50 (x0.5)", "F0 (x0)"))) |> arrange(run)
fx <- S |> filter(run %in% c("V1", "F50", "F0")) |> mutate(run = factor(run, c("V1", "F50", "F0"), levels(W$run))) |> select(run, FRP_bias_pct, chl_bias_pct, anoxic_days)
write_csv(left_join(W, fx, by = "run"), file.path(dat, "flood_scaling_summary.csv"))
write_csv(y1 |> mutate(obs = round(obs, 4), sim = round(sim, 4), error = round(sim - obs, 4), TP_load_t = round(TP_t, 1)) |> select(yr, obs, sim, error, TP_load_t), file.path(dat, "annual_tp_v1.csv"))
cat("done\n"); print(as.data.frame(S)); print(as.data.frame(W))
