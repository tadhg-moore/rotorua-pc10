# Figures for website/rotorua-alum-next-steps.qmd (calibration story): scorecard across runs, annual TP by run, objective trajectories by stage,
# seasonal FRP / chlorophyll before and after stage s, anoxic days by year before and after stage o3, parameter positions in their ranges (stages s and o3).
# No model runs: reads calibration/alum/stream/result_*.rds and calibration/step2*/runs/step2_ies.rds, step2_best.rds.
suppressMessages({library(dplyr); library(tidyr); library(ggplot2); library(readr)})
out <- "website/images/calibration"; dat <- "website/data/calibration"; dir.create(out, recursive = TRUE, showWarnings = FALSE); dir.create(dat, recursive = TRUE, showWarnings = FALSE)
DOPR <- 0.15 * 30.974 / 1000; wet <- c(2011, 2012, 2017, 2018)
th <- theme_minimal(base_size = 10) + theme(legend.position = "bottom", strip.text = element_text(face = "bold"))
sv <- function(n, p, w, h) ggsave(file.path(out, n), p, width = w, height = h, dpi = 130, bg = "white")
runs <- c(V1 = "V1: stage o", Q100 = "q", Q2 = "q2", R1 = "r", P2 = "p2", P3 = "p3", P4 = "p4", S1 = "s", O3 = "o3", J1 = "j")
R <- lapply(names(runs), function(s) readRDS(file.path("calibration/alum/stream", paste0("result_", s, ".rds")))$P); names(R) <- names(runs)
surf <- function(P, v) { d <- P[[v]]; d <- d[d$depth <= 5 & is.finite(d$obs) & is.finite(d$sim), ]; if (v == "PHS_tp") d$sim <- d$sim + DOPR; d }
anoxdays <- function(P) P$CHM_oxy |> filter(depth >= 17, is.finite(obs), is.finite(sim)) |> group_by(Date) |> summarise(obs = mean(obs), sim = mean(sim), .groups = "drop")
# ---- scorecard ----
S <- bind_rows(lapply(names(R), function(k) { P <- R[[k]]; tp <- surf(P, "PHS_tp"); an <- tp |> filter(yr >= 2006, yr <= 2021) |> group_by(yr) |> summarise(obs = mean(obs), sim = mean(sim), .groups = "drop") |> mutate(err = sim - obs, wet = yr %in% wet)
  tn <- P$NIT_tn; tn <- tn[tn$depth <= 5 & is.finite(tn$obs) & is.finite(tn$sim), ]; ox <- anoxdays(P)
  tibble(run = k, `TP bias (%)` = 100 * mean(an$err) / mean(an$obs), `FRP bias (%)` = 100 * (mean(surf(P, "PHS_frp")$sim) / mean(surf(P, "PHS_frp")$obs) - 1), `Chlorophyll bias (%)` = 100 * (mean(surf(P, "PHY_tchla")$sim) / mean(surf(P, "PHY_tchla")$obs) - 1),
         `Total N bias (%)` = 100 * (mean(tn$sim) / mean(tn$obs) - 1), `Wet-year TP error (g/m3)` = mean(an$err[an$wet]), `Annual TP RMSE (g/m3)` = sqrt(mean(an$err^2)), `Year-to-year TP correlation` = cor(an$sim, an$obs), `Anoxic days (obs 239)` = sum(ox$sim < 2)) }))
write_csv(S |> mutate(across(where(is.numeric), \(x) round(x, 4))), file.path(dat, "run_scorecard.csv"))
L <- S |> mutate(stage = factor(runs[run], runs)) |> pivot_longer(-c(run, stage), names_to = "metric", values_to = "value") |> mutate(metric = factor(metric, names(S)[-1]))
ref <- tibble(metric = factor(c("TP bias (%)", "FRP bias (%)", "Chlorophyll bias (%)", "Total N bias (%)", "Wet-year TP error (g/m3)", "Annual TP RMSE (g/m3)", "Year-to-year TP correlation", "Anoxic days (obs 239)"), names(S)[-1]), y = c(0, 0, 0, 0, 0, 0, 1, 239))
sv("scorecard.png", ggplot(L, aes(stage, value, group = 1)) + geom_hline(data = ref, aes(yintercept = y), colour = "grey55", linetype = 2) + geom_line(colour = "steelblue4") + geom_point(colour = "steelblue4", size = 1.6) + facet_wrap(~metric, scales = "free_y", ncol = 4) +
     labs(x = "Calibration stage (in order)", y = NULL, subtitle = "Dashed line: perfect fit (anoxic days: observed 239). Surface 0-5 m, 2006-2021; TP includes the refractory DOP offset.") + th + theme(axis.text.x = element_text(angle = 45, hjust = 1)), 11, 5.5)
# ---- annual TP by run ----
sel <- c("V1", "Q100", "Q2", "P4", "O3"); A <- bind_rows(lapply(sel, function(k) surf(R[[k]], "PHS_tp") |> filter(yr >= 2006, yr <= 2021) |> group_by(yr) |> summarise(v = mean(sim), .groups = "drop") |> mutate(run = runs[k])), surf(R$V1, "PHS_tp") |> filter(yr >= 2006, yr <= 2021) |> group_by(yr) |> summarise(v = mean(obs), .groups = "drop") |> mutate(run = "observed"))
A$run <- factor(A$run, c("observed", runs[sel]))
sv("annual_tp_by_stage.png", ggplot(A, aes(yr, v, colour = run, linewidth = run == "observed")) + annotate("rect", xmin = wet - 0.5, xmax = wet + 0.5, ymin = -Inf, ymax = Inf, alpha = 0.12) + geom_line() + scale_linewidth_manual(values = c(0.5, 1.4), guide = "none") +
     scale_colour_manual(values = c("black", "grey60", "#d95f02", "#7570b3", "#1b9e77", "#e7298a")) + labs(x = NULL, y = "Annual mean surface TP (g/m3)", colour = NULL, subtitle = "Grey: wet years. The wet-year overshoot shrinks stage by stage; 2021 (obs) is missed by every run.") + th, 9, 4)
# ---- objective trajectories ----
st <- c(q = "step2q", q2 = "step2q2", r = "step2r", p2 = "step2p2", p3 = "step2p3", p4 = "step2p4", s = "step2s", o3 = "step2o3", j = "step2j")
Tj <- bind_rows(lapply(names(st), function(k) { x <- readRDS(file.path("calibration", st[[k]], "runs", "step2_ies.rds")); bind_rows(lapply(seq_along(x$hist), function(i) { p <- x$hist[[i]]$phi; p <- p[is.finite(p) & p > 0]; tibble(stage = k, iter = i - 1, best = min(p), median = median(p)) })) }))
write_csv(Tj |> mutate(across(c(best, median), \(x) round(x, 3))), file.path(dat, "objective_by_iteration.csv"))
sv("objective_trajectories.png", ggplot(pivot_longer(Tj, c(best, median), names_to = "stat", values_to = "phi") |> mutate(stage = factor(stage, names(st))), aes(iter, phi, colour = stat)) + geom_line() + geom_point(size = 1.2) + facet_wrap(~stage, nrow = 2, scales = "free_y") +
     scale_colour_manual(values = c(best = "firebrick", median = "grey40")) + labs(x = "Iteration (0 = prior ensemble)", y = "Objective (mean squared residual)", colour = NULL, subtitle = "Each stage has its own objective and start member, so the levels are not comparable across panels.") + th, 10, 5)
# ---- seasonal FRP and chlorophyll before / after stage s ----
sea <- bind_rows(lapply(c("P4", "S1"), function(k) bind_rows(lapply(c("PHS_frp", "PHY_tchla"), function(v) surf(R[[k]], v) |> filter(yr >= 2006, yr <= 2021) |> group_by(mo) |> summarise(sim = mean(sim), obs = mean(obs), .groups = "drop") |> mutate(var = v, run = runs[k])))))
sv("seasonal_frp_chl.png", ggplot(sea |> pivot_longer(c(sim, obs), names_to = "src", values_to = "v") |> mutate(line = ifelse(src == "obs", "observed", run), var = factor(var, c("PHS_frp", "PHY_tchla"), c("Surface FRP (g P/m3)", "Surface chlorophyll-a (ug/L)"))) |> distinct(var, mo, line, v), aes(mo, v, colour = line)) + geom_line() + geom_point(size = 1) +
     facet_wrap(~var, scales = "free_y") + scale_x_continuous(breaks = 1:12) + scale_colour_manual(values = c(observed = "black", p4 = "#7570b3", s = "#1b9e77")) + labs(x = "Month", y = NULL, colour = NULL, subtitle = "Mean annual cycle 2006-2021 (southern hemisphere: summer Dec-Feb)") + th, 9, 3.8)
# ---- anoxic days by year, before / after stage o3 ----
ay <- bind_rows(lapply(c("P4", "S1", "O3"), function(k) anoxdays(R[[k]]) |> mutate(yr = as.integer(format(Date, "%Y"))) |> group_by(yr) |> summarise(days = sum(sim < 2), .groups = "drop") |> mutate(run = runs[k])), anoxdays(R$P4) |> mutate(yr = as.integer(format(Date, "%Y"))) |> group_by(yr) |> summarise(days = sum(obs < 2), .groups = "drop") |> mutate(run = "observed"))
ay$run <- factor(ay$run, c("observed", "p4", "s", "o3"))
sv("anoxic_days_stages.png", ggplot(ay, aes(factor(yr), days, fill = run)) + geom_col(position = "dodge", width = 0.8) + scale_fill_manual(values = c("black", "#7570b3", "#1b9e77", "#e7298a")) + labs(x = NULL, y = "Days with bottom DO < 2 g/m3 (sampled days)", fill = NULL,
     subtitle = "Stage s raises productivity and anoxia; stage o3 pulls it back part of the way") + th + theme(axis.text.x = element_text(angle = 45, hjust = 1)), 10, 4)
# ---- parameter positions in their ranges (stages s and o3) ----
to_unit <- function(v, r) if (identical(r$scale, "log") && r$min * r$max > 0) { lo <- log(min(abs(r$min), abs(r$max))); hi <- log(max(abs(r$min), abs(r$max))); (log(abs(v)) - lo) / (hi - lo) } else (v - r$min) / (r$max - r$min)
pp <- bind_rows(lapply(c(s = "step2s", o3 = "step2o3"), function(d) { x <- readRDS(file.path("calibration", d, "runs", "step2_ies.rds")); b <- readRDS(file.path("calibration", d, "runs", "step2_best.rds")); v <- x$tab[x$vary, ]
  tibble(par = paste0(sub("^aed_[a-z_0-9]+/", "", v$name), ifelse(is.na(v$group), ifelse(is.na(v$index), "", paste0(" z", v$index)), paste0(" ", v$group))), start = vapply(seq_len(nrow(v)), \(j) to_unit(v$value[j], v[j, ]), 1), best = b$U_best) }), .id = "stage")
pp$par <- factor(pp$par, unique(pp$par))
sv("parameter_positions.png", ggplot(pp, aes(y = par)) + geom_vline(xintercept = c(0, 1), colour = "grey70") + geom_segment(aes(x = start, xend = best, yend = par), colour = "grey50", arrow = arrow(length = unit(0.12, "cm"))) + geom_point(aes(x = best, colour = best < 0.05 | best > 0.95), size = 1.8) +
     facet_wrap(~stage, scales = "free_y") + scale_colour_manual(values = c("steelblue4", "firebrick"), labels = c("inside range", "at a bound"), name = NULL) + labs(x = "Position in the allowed range (0 = lower bound, 1 = upper bound; log scale where used)", y = NULL, subtitle = "Arrow: start member to best member") + th, 10, 4.5)
cat("done\n"); print(as.data.frame(S |> mutate(across(where(is.numeric), \(x) round(x, 3)))))
