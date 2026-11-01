# Alum-on forward run: FRP / TP / chlorophyll (surface and bottom) observed vs simulated by year, and the alum effect.
#   Rscript calibration/alum/01_alum_fit.R      (ALUM_RUN = alum)
suppressMessages({library(dplyr); library(tidyr); library(ggplot2)}); source("calibration/csps_metrics.R")
run <- Sys.getenv("ALUM_RUN", "alum"); out <- file.path("website", "images", "alum_calib"); dir.create(out, showWarnings = FALSE, recursive = TRUE)
res <- readRDS(file.path("calibration", "step2", "forward", run, "forward_aeme.rds"))
P <- csps_pair(res, vars = c("PHS_frp", "PHS_tp", "PHY_tchla", "NIT_tn"))
P$PHS_tp$sim <- P$PHS_tp$sim + 9.0 * 0 + 0.15 * 30.974 / 1000       # refractory DOP added to simulated TP
yr <- function(d, lo, hi, nm) d |> filter(depth >= lo, depth <= hi) |> group_by(yr) |> summarise(n = n(), obs = mean(obs), sim = mean(sim), .groups = "drop") |> mutate(var = nm)
Y <- bind_rows(yr(P$PHS_frp, 0, 5, "FRP surface"), yr(P$PHS_frp, 17, 99, "FRP bottom"), yr(P$PHS_tp, 0, 5, "TP surface"), yr(P$PHY_tchla, 0, 5, "Chl surface"))
cat("Annual means (g/m3 for P; ug/L chl), observed vs simulated:\n"); print(Y |> filter(yr >= 2005) |> mutate(across(c(obs, sim), \(x) signif(x, 3))) |> pivot_wider(id_cols = yr, names_from = var, values_from = c(obs, sim)) |> as.data.frame(), row.names = FALSE)
S <- Y |> filter(yr >= 2007) |> group_by(var) |> summarise(obs = mean(obs), sim = mean(sim), r_annual = cor(obs, sim), bias_pct = 100 * (mean(sim) - mean(obs)) / mean(obs))
cat("\n2007-2022 means and across-year correlation:\n"); print(as.data.frame(S |> mutate(across(where(is.numeric), \(x) signif(x, 3)))), row.names = FALSE)
long <- Y |> filter(yr >= 2000) |> pivot_longer(c(obs, sim), names_to = "what", values_to = "value")
ggsave(file.path(out, sprintf("annual_%s.png", run)), ggplot(long, aes(yr, value, colour = what)) + geom_line() + geom_point(size = 1) + geom_vline(xintercept = c(2006.4, 2010), linetype = 3) +
         facet_wrap(~var, scales = "free_y") + scale_colour_manual(values = c(obs = "black", sim = "#d95f02")) + labs(x = NULL, y = NULL, colour = NULL) + theme_minimal(base_size = 9), width = 9, height = 6, dpi = 120, bg = "white")
w <- bind_rows(P$PHS_frp |> filter(depth <= 5) |> mutate(var = "FRP surface"), P$PHS_frp |> filter(depth >= 17) |> mutate(var = "FRP bottom")) |> filter(yr >= 2005) |> group_by(var, Date) |> summarise(obs = mean(obs), sim = mean(sim), .groups = "drop") |>
  pivot_longer(c(obs, sim), names_to = "what", values_to = "frp")
ggsave(file.path(out, sprintf("frp_series_%s.png", run)), ggplot(w, aes(Date, frp, colour = what)) + geom_point(data = w |> filter(what == "obs"), size = 0.6) + geom_line(data = w |> filter(what == "sim"), linewidth = 0.3) +
         facet_wrap(~var, ncol = 1, scales = "free_y") + scale_colour_manual(values = c(obs = "black", sim = "#d95f02")) + labs(y = "FRP (g/m3)", colour = NULL) + theme_minimal(base_size = 9), width = 10, height = 6, dpi = 120, bg = "white")
saveRDS(list(Y = Y, S = S), file.path(out, sprintf("alum_fit_%s.rds", run)))
