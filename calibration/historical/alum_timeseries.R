# Alum on/off: (1) monthly time series of the ensemble with and without alum against the observations,
# (2) observed against modelled before and after dosing started (ensemble-mean simulation at each observation), both runs.
# Dosing: Utuhina from 2006-06-15, Puarenga from 2010-01-14 (first dosed day in the inflow file); "before" = to 2006-06-14, "after" = 2006-06-15 onwards.
suppressMessages({library(dplyr); library(tidyr); library(ggplot2); library(readr)})
out <- "website/images/historical"; dat <- "website/data/historical"; TAG <- Sys.getenv("HIST_TAG", "j50")
DOPR <- 0.15 * 30.974 / 1000; MP <- 30.974 / 1000; D0 <- as.Date("2006-06-15"); D1 <- as.Date("2010-01-14")
RUN <- c(a = "With observed alum", n = "No alum"); COL <- c("With observed alum" = "steelblue4", "No alum" = "firebrick"); FIL <- c("With observed alum" = "steelblue", "No alum" = "firebrick")
rd <- function(tag, k) { f <- list.files(file.path("calibration/historical", paste0("out_", tag)), "^member_.*[.]rds$", full.names = TRUE); lapply(f, \(x) { m <- readRDS(x); list(member = m$member, daily = m$daily, P = m$P) }) }
MA <- rd(TAG); MN <- rd(paste0(TAG, "_noalum")); cat(length(MA), length(MN), "members\n")
mem <- intersect(sapply(MA, `[[`, "member"), sapply(MN, `[[`, "member")); MA <- MA[sapply(MA, `[[`, "member") %in% mem]; MN <- MN[sapply(MN, `[[`, "member") %in% mem]
# ---- 1. monthly time series ----
mon <- function(M, run) bind_rows(lapply(M, \(m) m$daily |> filter(Date >= "2005-01-01") |> transmute(member = m$member, month = as.Date(format(Date, "%Y-%m-01")), TP = TOT_tp_surf * MP + DOPR, FRP = PHS_frp_surf * MP, chl = PHY_tchla_surf, DObot = OXY_oxy_bot * 0.032) |>
  group_by(member, month) |> summarise(across(c(TP, FRP, chl, DObot), mean), .groups = "drop") |> mutate(run = run)))
Y <- bind_rows(mon(MA, RUN[["a"]]), mon(MN, RUN[["n"]])) |> pivot_longer(c(TP, FRP, chl, DObot), names_to = "var", values_to = "v") |> group_by(run, var, month) |> summarise(lo = min(v), q1 = quantile(v, .25), med = median(v), q3 = quantile(v, .75), hi = max(v), .groups = "drop")
lab <- c(TP = "Surface TP (g P/m3)", FRP = "Surface FRP (g P/m3)", chl = "Surface chlorophyll-a (ug/L)", DObot = "Bottom DO (g/m3)"); Y$var <- factor(Y$var, names(lab), lab)
P1 <- MA[[1]]$P; ob <- function(v, cond, add = 0) { d <- P1[[v]]; d <- d[cond(d) & is.finite(d$obs), ]; d |> mutate(month = as.Date(format(Date, "%Y-%m-01"))) |> group_by(month) |> summarise(obs = mean(obs) + add, .groups = "drop") }
O <- bind_rows(ob("PHS_tp", \(d) d$depth <= 5) |> mutate(var = lab[["TP"]]), ob("PHS_frp", \(d) d$depth <= 5) |> mutate(var = lab[["FRP"]]), ob("PHY_tchla", \(d) d$depth <= 5) |> mutate(var = lab[["chl"]]), ob("CHM_oxy", \(d) d$depth >= 17) |> mutate(var = lab[["DObot"]])) |> mutate(var = factor(var, lab))
ggsave(file.path(out, "alum_timeseries.png"), ggplot(Y, aes(month)) + geom_vline(xintercept = c(D0, D1), linetype = 2, colour = "grey40") +
  geom_ribbon(aes(ymin = lo, ymax = hi, fill = run), alpha = 0.18) + geom_ribbon(aes(ymin = q1, ymax = q3, fill = run), alpha = 0.3) + geom_line(aes(y = med, colour = run), linewidth = 0.4) + geom_point(data = O, aes(month, obs), size = 0.8, inherit.aes = FALSE) +
  facet_wrap(~var, ncol = 1, scales = "free_y") + scale_colour_manual(values = COL) + scale_fill_manual(values = FIL) + labs(x = NULL, y = NULL, colour = NULL, fill = NULL, subtitle = paste0("Monthly means, ", length(mem), " joint-ensemble members (line: median; dark band: interquartile range; light band: full range).
Points: observed monthly means. Dashed lines: dosing starts (Utuhina 2006-06-15, Puarenga 2010-01-14).")) +
  theme_minimal(base_size = 10) + theme(legend.position = "bottom", plot.subtitle = element_text(size = 8)), width = 10, height = 11, dpi = 130, bg = "white")
# ---- 2. observed against modelled, before and after dosing ----
vv <- list(TP = list("PHS_tp", \(d) d$depth <= 5, DOPR), FRP = list("PHS_frp", \(d) d$depth <= 5, 0), chl = list("PHY_tchla", \(d) d$depth <= 5, 0), DObot = list("CHM_oxy", \(d) d$depth >= 17, 0))
pr <- function(M, run) bind_rows(lapply(names(vv), \(k) { s <- vv[[k]]; bind_rows(lapply(M, \(m) { d <- m$P[[s[[1]]]]; d <- d[s[[2]](d) & is.finite(d$obs) & is.finite(d$sim), c("Date", "depth", "obs", "sim")]; d$sim <- d$sim + s[[3]]; d$member <- m$member; d })) |> mutate(var = k, run = run) }))
Pp <- bind_rows(pr(MA, RUN[["a"]]), pr(MN, RUN[["n"]]))
E <- Pp |> group_by(run, var, Date, depth) |> summarise(obs = first(obs), sim = mean(sim), lo = min(sim), hi = max(sim), .groups = "drop") |> mutate(period = factor(ifelse(Date < D0, "Before dosing (2005 to mid-2006)", "After dosing (mid-2006 to 2022)"), c("Before dosing (2005 to mid-2006)", "After dosing (mid-2006 to 2022)")))
st <- function(d) tibble(n = nrow(d), bias_pct = 100 * (mean(d$sim) / mean(d$obs) - 1), rmse = sqrt(mean((d$sim - d$obs)^2)), r = if (nrow(d) > 2) cor(d$sim, d$obs) else NA)
S <- E |> group_by(var, period, run) |> group_modify(~st(.x)) |> ungroup() |> mutate(across(where(is.numeric), \(x) signif(x, 3)))
# member range of the bias (each member alone)
SB <- Pp |> mutate(period = ifelse(Date < D0, "Before dosing (2005 to mid-2006)", "After dosing (mid-2006 to 2022)")) |> group_by(var, period, run, member) |> summarise(b = 100 * (mean(sim) / mean(obs) - 1), .groups = "drop") |> group_by(var, period, run) |> summarise(bias_min = min(b), bias_max = max(b), .groups = "drop") |> mutate(across(where(is.numeric), \(x) signif(x, 3)))
S <- left_join(S, SB, by = c("var", "period", "run")); write_csv(S, file.path(dat, "alum_obs_vs_sim_stats.csv")); print(as.data.frame(S), row.names = FALSE)
L <- E |> mutate(var = factor(var, names(lab), lab), run = factor(run, RUN)); A <- S |> mutate(var = factor(var, names(lab), lab), run = factor(run, RUN), period = factor(period, levels(E$period)), txt = sprintf("%s: bias %+.0f%%, r %.2f (n %d)", ifelse(run == RUN[["a"]], "with", "no alum"), bias_pct, r, n))
lim <- L |> group_by(var, period) |> summarise(lo = min(c(obs, sim)), hi = max(c(obs, sim)), .groups = "drop")
A <- A |> left_join(lim, by = c("var", "period")) |> mutate(yy = ifelse(run == RUN[["a"]], hi - 0.04 * (hi - lo), hi - 0.11 * (hi - lo)), xx = lo)
B <- bind_rows(transmute(lim, var, period, obs = lo, sim = lo), transmute(lim, var, period, obs = hi, sim = hi))
ggsave(file.path(out, "alum_obs_vs_sim.png"), ggplot(L, aes(obs, sim, colour = run)) + geom_blank(data = B, aes(obs, sim), inherit.aes = FALSE) + geom_abline(colour = "grey50") + geom_point(alpha = 0.4, size = 0.9) + geom_text(data = A, aes(x = xx, y = yy, label = txt, colour = run), hjust = 0, size = 2.6, inherit.aes = FALSE, show.legend = FALSE) +
  facet_wrap(vars(var, period), ncol = 2, scales = "free") + scale_colour_manual(values = COL) + labs(x = "Observed", y = "Modelled (ensemble mean at each observation)", colour = NULL, subtitle = "Grey line: 1:1. Surface (0 to 5 m) for TP, FRP and chlorophyll; bottom (17 m and deeper) for DO. TP includes the refractory DOP offset.") + theme_minimal(base_size = 10) + theme(legend.position = "bottom", plot.subtitle = element_text(size = 8), strip.text = element_text(size = 8)), width = 10, height = 12, dpi = 130, bg = "white")
cat("done\n")
