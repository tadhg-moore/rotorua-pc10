# Historical ensemble (2005-2022): envelopes against observations, per-member metrics, anoxia by year.
suppressMessages({library(dplyr); library(tidyr); library(ggplot2); library(readr)})
tg <- Sys.getenv("HIST_TAG", "o3"); sfx <- if (tg == "o3") "" else paste0("_", tg); out <- "website/images/historical"; dat <- "website/data/historical"; dir.create(out, recursive = TRUE, showWarnings = FALSE); dir.create(dat, recursive = TRUE, showWarnings = FALSE)
DOPR <- 0.15 * 30.974 / 1000; MP <- 30.974 / 1000
f <- list.files(file.path("calibration/historical", paste0("out_", tg)), "^member_.*[.]rds$", full.names = TRUE); M <- lapply(f, readRDS); cat(length(M), "members\n")
th <- theme_minimal(base_size = 10) + theme(legend.position = "bottom")
sv <- function(n, p, w, h) ggsave(file.path(out, n), p, width = w, height = h, dpi = 130, bg = "white")
# ---- observations from the pair tables (first member) ----
obs <- function(v, cond) { d <- M[[1]]$P[[v]]; d <- d[cond(d) & is.finite(d$obs), ]; d |> group_by(Date) |> summarise(obs = mean(obs), .groups = "drop") |> mutate(month = as.Date(format(Date, "%Y-%m-01"))) }
# ---- per-member metrics (surface 0-5 m pairs) ----
met <- bind_rows(lapply(M, function(m) { s <- function(v) { d <- m$P[[v]]; d <- d[d$depth <= 5 & is.finite(d$obs) & is.finite(d$sim), ]; if (v == "PHS_tp") d$sim <- d$sim + DOPR; d }
  ox <- m$P$CHM_oxy |> filter(depth >= 17, is.finite(obs), is.finite(sim)) |> group_by(Date) |> summarise(obs = mean(obs), sim = mean(sim), .groups = "drop")
  tp <- s("PHS_tp"); an <- tp |> filter(yr >= 2006, yr <= 2021) |> group_by(yr) |> summarise(obs = mean(obs), sim = mean(sim), .groups = "drop")
  tibble(member = m$member, phi = m$phi, TP_bias = 100 * (mean(tp$sim) / mean(tp$obs) - 1), FRP_bias = 100 * (mean(s("PHS_frp")$sim) / mean(s("PHS_frp")$obs) - 1), chl_bias = 100 * (mean(s("PHY_tchla")$sim) / mean(s("PHY_tchla")$obs) - 1),
         TP_annual_rmse = sqrt(mean((an$sim - an$obs)^2)), TP_year_r = cor(an$sim, an$obs), anoxic_days = sum(ox$sim < 2), obs_anoxic_days = sum(ox$obs < 2), DO_bottom_bias = mean(ox$sim - ox$obs)) })) |> mutate(across(where(is.numeric), \(x) round(x, 3)))
write_csv(met, file.path(dat, paste0("member_metrics", sfx, ".csv"))); cat("\nPer-member metrics:\n"); print(as.data.frame(met), row.names = FALSE)
cat("\nEnsemble summary (mean, min, max):\n"); print(as.data.frame(met |> select(-member) |> summarise(across(everything(), list(mean = mean, min = min, max = max))) |> pivot_longer(everything(), names_sep = "_(?=[^_]+$)", names_to = c("metric", "stat")) |> pivot_wider(names_from = stat, values_from = value)))
# ---- monthly envelopes: daily surface means against observed monthly means ----
dl <- bind_rows(lapply(M, \(m) m$daily |> mutate(member = m$member))) |> filter(Date >= "2005-01-01") |> mutate(month = as.Date(format(Date, "%Y-%m-01")))
chk <- dl |> filter(member == M[[1]]$member) |> summarise(tp_daily_mean = mean(TOT_tp_surf, na.rm = TRUE)); cat("\nTOT_tp_surf mean (raw units):", signif(chk$tp_daily_mean, 3), "\n")
env <- function(col, scale, add = 0) dl |> group_by(member, month) |> summarise(v = mean(.data[[col]], na.rm = TRUE) * scale + add, .groups = "drop") |> group_by(month) |> summarise(lo = min(v), q1 = quantile(v, .25), med = median(v), q3 = quantile(v, .75), hi = max(v), .groups = "drop")
panel <- function(col, scale, add, v, cond, lab) { e <- env(col, scale, add); o <- obs(v, cond)
  ggplot(e, aes(month)) + geom_ribbon(aes(ymin = lo, ymax = hi), fill = "steelblue", alpha = 0.25) + geom_ribbon(aes(ymin = q1, ymax = q3), fill = "steelblue", alpha = 0.4) + geom_line(aes(y = med), colour = "steelblue4", linewidth = 0.4) +
    geom_point(data = o, aes(month, obs), size = 0.8, colour = "black") + labs(x = NULL, y = lab) + th }
p1 <- panel("TOT_tp_surf", MP, DOPR, "PHS_tp", \(d) d$depth <= 5, "Surface TP (g P/m3)"); p2 <- panel("PHS_frp_surf", MP, 0, "PHS_frp", \(d) d$depth <= 5, "Surface FRP (g P/m3)")
p3 <- panel("PHY_tchla_surf", 1, 0, "PHY_tchla", \(d) d$depth <= 5, "Surface chlorophyll-a (ug/L)"); p4 <- panel("OXY_oxy_bot", 0.032, 0, "CHM_oxy", \(d) d$depth >= 17, "Bottom DO (g/m3)")
sv(paste0("envelopes", sfx, ".png"), gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 1), 9, 11)
# ---- anoxic days by year ----
ay <- dl |> group_by(member, yr = as.integer(format(Date, "%Y"))) |> summarise(anox = sum(OXY_oxy_bot * 0.032 < 2, na.rm = TRUE), .groups = "drop") |> group_by(yr) |> summarise(lo = min(anox), med = median(anox), hi = max(anox), .groups = "drop")
oy <- (M[[1]]$P$CHM_oxy |> filter(depth >= 17, is.finite(obs)) |> group_by(Date) |> summarise(obs = mean(obs), .groups = "drop") |> mutate(yr = as.integer(format(Date, "%Y"))) |> group_by(yr) |> summarise(obs_days_below2 = sum(obs < 2), n_obs = n(), .groups = "drop"))
sv(paste0("anoxia_by_year", sfx, ".png"), ggplot(ay, aes(yr)) + geom_ribbon(aes(ymin = lo, ymax = hi), fill = "steelblue", alpha = 0.3) + geom_line(aes(y = med), colour = "steelblue4") + geom_col(data = oy, aes(yr, obs_days_below2), alpha = 0.35, width = 0.5) + labs(x = NULL, y = "Days with bottom DO < 2 g/m3 (lines: all model days; bars: observed days only)") + th, 8, 4)
write_csv(ay, file.path(dat, paste0("anoxic_days_by_year", sfx, ".csv"))); write_csv(oy, file.path(dat, paste0("observed_low_do_days_by_year", sfx, ".csv")))
cat("done\n")
