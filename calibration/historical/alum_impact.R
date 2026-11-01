# Alum impact: the same 20 joint-ensemble members run 2005-2022 with the observed alum dosing (out_j) and with no alum (out_j_noalum, base_Q50_noalum.rds).
# Impact = with alum minus without alum, member by member, so the spread is the parameter uncertainty in the effect itself.
suppressMessages({library(dplyr); library(tidyr); library(ggplot2); library(readr)})
out <- "website/images/historical"; dat <- "website/data/historical"; dir.create(out, recursive = TRUE, showWarnings = FALSE); dir.create(dat, recursive = TRUE, showWarnings = FALSE)
DOPR <- 0.15 * 30.974 / 1000; MP <- 30.974 / 1000; MN <- 14.007 / 1000
rd <- function(tag) { f <- list.files(file.path("calibration/historical", paste0("out_", tag)), "^member_.*[.]rds$", full.names = TRUE)
  bind_rows(lapply(f, \(x) { m <- readRDS(x); m$daily |> mutate(member = m$member, run = tag) })) |> filter(Date >= "2005-01-01") |>
    transmute(run, member, Date, yr = as.integer(format(Date, "%Y")), mo = as.integer(format(Date, "%m")), TP = TOT_tp_surf * MP + DOPR, FRP = PHS_frp_surf * MP, chl = PHY_tchla_surf, TN = TOT_tn_surf * MN, DObot = OXY_oxy_bot * 0.032, TPbot = TOT_tp_bot * MP + DOPR, FRPbot = PHS_frp_bot * MP) }
TAG <- Sys.getenv("HIST_TAG", "j50"); A <- rd(TAG); N <- rd(paste0(TAG, "_noalum")); mem <- intersect(unique(A$member), unique(N$member)); cat("paired members:", length(mem), "\n")
A <- filter(A, member %in% mem); N <- filter(N, member %in% mem)
D <- inner_join(A, N, by = c("member", "Date", "yr", "mo"), suffix = c("_a", "_n"))
ann <- D |> group_by(member, yr) |> summarise(across(c(TP_a, TP_n, FRP_a, FRP_n, chl_a, chl_n, TN_a, TN_n, TPbot_a, TPbot_n, FRPbot_a, FRPbot_n), mean), anox_a = sum(DObot_a < 2), anox_n = sum(DObot_n < 2), DObot_a = mean(DObot_a), DObot_n = mean(DObot_n), .groups = "drop")
smr <- function(a, n) tibble(with = mean(a), without = mean(n), diff = mean(a - n), pct = 100 * mean(a - n) / mean(n))
tab <- bind_rows(lapply(list(c("Surface TP (g/m3)", "TP"), c("Surface FRP (g/m3)", "FRP"), c("Surface chlorophyll-a (ug/L)", "chl"), c("Surface total N (g/m3)", "TN"), c("Bottom TP (g/m3)", "TPbot"), c("Bottom FRP (g/m3)", "FRPbot"), c("Anoxic days per year (bottom DO < 2)", "anox"), c("Mean bottom DO (g/m3)", "DObot")),
  \(v) { s <- ann |> group_by(member) |> summarise(a = mean(.data[[paste0(v[2], "_a")]]), n = mean(.data[[paste0(v[2], "_n")]]), .groups = "drop"); r <- smr(s$a, s$n)
         bind_cols(metric = v[1], r, diff_min = min(s$a - s$n), diff_max = max(s$a - s$n), pct_min = min(100 * (s$a - s$n) / s$n), pct_max = max(100 * (s$a - s$n) / s$n)) })) |> mutate(across(where(is.numeric), \(x) signif(x, 3)))
write_csv(tab, file.path(dat, "alum_impact_summary.csv")); cat("\nImpact of alum, 2005-2022, ensemble mean (member range):\n"); print(as.data.frame(tab), row.names = FALSE)
# by period (dosing began around 2006-2007; high-dose years)
per <- function(y) cut(y, c(2004, 2010, 2016, 2022), c("2005-2010", "2011-2016", "2017-2022"))
bp <- ann |> mutate(period = per(yr)) |> group_by(member, period) |> summarise(dTP = mean(TP_a - TP_n), pTP = 100 * mean(TP_a - TP_n) / mean(TP_n), dFRP = mean(FRP_a - FRP_n), dchl = mean(chl_a - chl_n), pchl = 100 * mean(chl_a - chl_n) / mean(chl_n), danox = mean(anox_a - anox_n), .groups = "drop") |>
  group_by(period) |> summarise(across(c(dTP, pTP, dFRP, dchl, pchl, danox), list(mean = mean, min = min, max = max)), .groups = "drop") |> mutate(across(where(is.numeric), \(x) signif(x, 3)))
write_csv(bp, file.path(dat, "alum_impact_by_period.csv")); cat("\nBy period:\n"); print(as.data.frame(bp), row.names = FALSE)
# summer (Dec-Mar) chlorophyll
sc <- D |> filter(mo %in% c(12, 1, 2, 3)) |> group_by(member) |> summarise(a = mean(chl_a), n = mean(chl_n), .groups = "drop"); cat("\nSummer (Dec-Mar) chlorophyll: with", round(mean(sc$a), 2), "without", round(mean(sc$n), 2), "change", round(100 * mean(sc$a - sc$n) / mean(sc$n), 1), "%\n")
# observations for the figure (first member's pair table, alum run)
M1 <- readRDS(list.files(file.path("calibration/historical", paste0("out_", TAG)), "^member_.*[.]rds$", full.names = TRUE)[1])$P
ob <- function(v, cond, add = 0) { d <- M1[[v]]; d <- d[cond(d) & is.finite(d$obs), ]; d |> mutate(yr = as.integer(format(Date, "%Y"))) |> group_by(yr) |> summarise(obs = mean(obs) + add, .groups = "drop") }
O <- bind_rows(ob("PHS_tp", \(d) d$depth <= 5) |> mutate(v = "Annual surface TP (g P/m3)"), ob("PHY_tchla", \(d) d$depth <= 5) |> mutate(v = "Annual surface chlorophyll-a (ug/L)"), ob("PHS_frp", \(d) d$depth <= 5) |> mutate(v = "Annual surface FRP (g P/m3)"))
anox_obs <- M1$CHM_oxy |> filter(depth >= 17, is.finite(obs)) |> group_by(Date) |> summarise(obs = mean(obs), .groups = "drop") |> mutate(yr = as.integer(format(Date, "%Y"))) |> group_by(yr) |> summarise(obs = sum(obs < 2), .groups = "drop") |> mutate(v = "Days with bottom DO < 2 g/m3")
L <- ann |> select(member, yr, TP_a, TP_n, chl_a, chl_n, FRP_a, FRP_n, anox_a, anox_n) |> pivot_longer(-c(member, yr), names_to = c("var", "run"), names_sep = "_", values_to = "val") |>
  mutate(v = c(TP = "Annual surface TP (g P/m3)", chl = "Annual surface chlorophyll-a (ug/L)", FRP = "Annual surface FRP (g P/m3)", anox = "Days with bottom DO < 2 g/m3")[var], run = c(a = "With observed alum", n = "No alum")[run]) |>
  group_by(v, run, yr) |> summarise(lo = min(val), med = median(val), hi = max(val), .groups = "drop")
lv <- c("Annual surface TP (g P/m3)", "Annual surface FRP (g P/m3)", "Annual surface chlorophyll-a (ug/L)", "Days with bottom DO < 2 g/m3"); L$v <- factor(L$v, lv)
Ob <- bind_rows(O, anox_obs |> rename(obs = obs)) |> mutate(v = factor(v, lv))
ggsave(file.path(out, "alum_impact_annual.png"), ggplot(L, aes(yr, colour = run, fill = run)) + geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.2, colour = NA) + geom_line(aes(y = med)) + geom_point(data = Ob, aes(yr, obs), inherit.aes = FALSE, size = 1.4) +
  facet_wrap(~v, scales = "free_y", ncol = 2) + scale_colour_manual(values = c("With observed alum" = "steelblue4", "No alum" = "firebrick")) + scale_fill_manual(values = c("With observed alum" = "steelblue", "No alum" = "firebrick")) +
  labs(x = NULL, y = NULL, colour = NULL, fill = NULL, subtitle = paste0("Joint ensemble (", length(mem), " members: median and range), 2005-2022. Points: observations (anoxic days: observed sampled days only).")) + theme_minimal(base_size = 10) + theme(legend.position = "bottom"), width = 10, height = 6, dpi = 130, bg = "white")
dd <- ann |> transmute(member, yr, `Surface TP` = 100 * (TP_a - TP_n) / TP_n, `Surface FRP` = 100 * (FRP_a - FRP_n) / FRP_n, `Chlorophyll-a` = 100 * (chl_a - chl_n) / chl_n) |> pivot_longer(-c(member, yr), names_to = "var", values_to = "pct") |> group_by(var, yr) |> summarise(lo = min(pct), med = median(pct), hi = max(pct), .groups = "drop")
ggsave(file.path(out, "alum_impact_pct.png"), ggplot(dd, aes(yr)) + geom_hline(yintercept = 0, colour = "grey60") + geom_ribbon(aes(ymin = lo, ymax = hi), fill = "steelblue", alpha = 0.3) + geom_line(aes(y = med), colour = "steelblue4") + facet_wrap(~var, nrow = 1) +
  labs(x = NULL, y = "Change from alum (% of the no-alum value)", subtitle = "Median and range across the 20 members") + theme_minimal(base_size = 10), width = 10, height = 3.3, dpi = 130, bg = "white")
write_csv(ann |> mutate(across(where(is.numeric), \(x) signif(x, 4))), file.path(dat, "alum_impact_annual.csv")); cat("done\n")
# ---- whole-lake phosphorus: mass in the water column with and without alum (volume-weighted lake means x lake volume) ----
rl <- function(tag) { f <- list.files(file.path("calibration/historical", paste0("out_", tag)), "^member_.*[.]rds$", full.names = TRUE)
  bind_rows(lapply(f, \(x) { m <- readRDS(x); d <- m$daily; if (!"TOT_tp_lake" %in% names(d)) return(NULL); d |> filter(Date >= "2005-01-01") |> transmute(member = m$member, Date, yr = as.integer(format(Date, "%Y")), Vol, TPm = TOT_tp_lake * Vol * 30.974 / 1e9, ALAm = ALU_ala_lake * Vol * 30.974 / 1e9, ALPm = ALU_alp_lake * Vol * 30.974 / 1e9, FRPm = PHS_frp_lake * Vol * 30.974 / 1e9) })) }
LA <- rl(TAG); LN <- rl(paste0(TAG, "_noalum"))
if (nrow(LA) && nrow(LN)) {
  W <- inner_join(LA, LN, by = c("member", "Date", "yr"), suffix = c("_a", "_n")); wy <- W |> group_by(member, yr) |> summarise(dTP_t = mean(TPm_a - TPm_n), TP_a_t = mean(TPm_a), TP_n_t = mean(TPm_n), ALA_t = mean(ALAm_a), ALP_t = mean(ALPm_a), dFRP_t = mean(FRPm_a - FRPm_n), Vol = mean(Vol_a), .groups = "drop")
  wy <- wy |> mutate(dTP_gm3 = dTP_t * 1e6 / Vol); write_csv(wy |> mutate(across(where(is.numeric), \(x) signif(x, 4))), file.path(dat, "alum_impact_lake_p.csv"))
  cat("\nWhole-lake water-column TP (t P), ensemble mean over 2006-2022: with", round(mean(wy$TP_a_t[wy$yr >= 2006])), " without", round(mean(wy$TP_n_t[wy$yr >= 2006])), " difference", round(mean(wy$dTP_t[wy$yr >= 2006])), "t (", round(100 * mean(wy$dTP_t[wy$yr >= 2006]) / mean(wy$TP_n_t[wy$yr >= 2006]), 1), "%)\n")
  cat("Alum-Al as P-equivalent in the water column (ALU_ala t P-equiv, ALU_alp bound t P), mean 2006-2022:", round(mean(wy$ALA_t[wy$yr >= 2006]), 1), round(mean(wy$ALP_t[wy$yr >= 2006]), 1), "\n")
  cat("Lake TP difference by year (t P), ensemble median:\n"); print(wy |> group_by(yr) |> summarise(med = round(median(dTP_t), 1), lo = round(min(dTP_t), 1), hi = round(max(dTP_t), 1)), n = 20)
  # annual P retained by the alum effect: change in the with-minus-without TP mass difference between January of successive years is not a flux; give the standing difference only.
}
# ---- is the ensemble big enough? running mean and bootstrap interval of the mean alum effect against the number of members ----
eff <- ann |> group_by(member) |> summarise(TP = 100 * mean(TP_a - TP_n) / mean(TP_n), FRP = 100 * mean(FRP_a - FRP_n) / mean(FRP_n), chl = 100 * mean(chl_a - chl_n) / mean(chl_n), anox = mean(anox_a - anox_n), .groups = "drop") |> arrange(member)
set.seed(1); ns <- 5:nrow(eff)
cv <- bind_rows(lapply(names(eff)[-1], \(v) bind_rows(lapply(ns, \(k) { b <- replicate(1000, mean(sample(eff[[v]], k, replace = TRUE))); tibble(var = v, n = k, mean = mean(eff[[v]][seq_len(k)]), lo = quantile(b, .025), hi = quantile(b, .975)) }))))
write_csv(cv |> mutate(across(where(is.numeric), \(x) signif(x, 4))), file.path(dat, "alum_impact_convergence.csv"))
ggsave(file.path(out, "alum_impact_convergence.png"), ggplot(cv |> mutate(var = factor(var, c("TP", "FRP", "chl", "anox"), c("Surface TP (%)", "Surface FRP (%)", "Chlorophyll-a (%)", "Anoxic days per year"))), aes(n)) + geom_ribbon(aes(ymin = lo, ymax = hi), fill = "steelblue", alpha = 0.3) + geom_line(aes(y = mean), colour = "steelblue4") + facet_wrap(~var, scales = "free_y", nrow = 1) +
  labs(x = "Number of members (ranked by objective)", y = "Mean alum effect", subtitle = "Line: mean of the first n members. Band: 95% bootstrap interval of the mean for a sample of n members.") + theme_minimal(base_size = 10), width = 10, height = 3.3, dpi = 130, bg = "white")
cat("\nMean effect and 95% interval of the mean (bootstrap) at the full ensemble and at n = 20:\n"); print(as.data.frame(cv |> filter(n %in% c(20, max(n))) |> mutate(across(where(is.numeric), \(x) round(x, 2)))), row.names = FALSE)
# ---- P retained in the lake because of alum (budget) ----
# Inflow P is the same in both runs (bound P is part of the load), so  retention(alum) - retention(no alum) = (export_n - export_a) - (storage change_a - storage change_n).
# Export = surface TP x outflow volume; outflow volume is taken as the total inflow volume (evaporation and rain roughly cancel; stated as an approximation).
if (nrow(LA) && nrow(LN)) {
  bi <- AEME::inflows(readRDS("calibration/alum/stream/base_Q50.rds"))$data
  qv <- bind_rows(lapply(bi, \(d) tibble(Date = as.Date(d$Date), q = d$HYD_flow))) |> group_by(Date) |> summarise(q = sum(q, na.rm = TRUE), .groups = "drop") |> mutate(yr = as.integer(format(Date, "%Y")))
  Wd <- D |> select(member, Date, yr, TP_a, TP_n) |> inner_join(qv |> select(Date, q), by = "Date") |> group_by(member, yr) |> summarise(exp_a = sum(TP_a * q) / 1e6, exp_n = sum(TP_n * q) / 1e6, qyr = sum(q), .groups = "drop")   # g/m3 x m3 = g; /1e6 = t
  S <- W |> group_by(member, yr) |> summarise(s_a = TPm_a[which.max(Date)], s_n = TPm_n[which.max(Date)], .groups = "drop") |> arrange(member, yr) |> group_by(member) |> mutate(ds_a = s_a - dplyr::lag(s_a, default = 0), ds_n = s_n - dplyr::lag(s_n, default = 0)) |> ungroup()
  R <- inner_join(Wd, S, by = c("member", "yr")) |> mutate(retained_by_alum_t = (exp_n - exp_a) - (ds_a - ds_n), export_cut_t = exp_n - exp_a, storage_cut_t = s_n - s_a)
  write_csv(R |> mutate(across(where(is.numeric), \(x) signif(x, 4))), file.path(dat, "alum_impact_p_budget.csv"))
  rs <- R |> filter(yr >= 2006) |> group_by(member) |> summarise(ret = mean(retained_by_alum_t), exp = mean(export_cut_t), sto = last(storage_cut_t), .groups = "drop")
  cat("\nP retained in the lake because of alum, mean 2006-2022 (t P/yr): ensemble mean", round(mean(rs$ret), 1), "range", round(min(rs$ret), 1), "to", round(max(rs$ret), 1), "| export reduction", round(mean(rs$exp), 1), "t/yr | standing water-column TP reduction at the end", round(mean(rs$sto), 1), "t\n")
  ry <- R |> group_by(yr) |> summarise(lo = min(retained_by_alum_t), med = median(retained_by_alum_t), hi = max(retained_by_alum_t), .groups = "drop"); print(ry |> mutate(across(-yr, \(x) round(x, 1))), n = 20)
  ggsave(file.path(out, "alum_p_retention.png"), ggplot(ry, aes(yr)) + geom_ribbon(aes(ymin = lo, ymax = hi), fill = "steelblue", alpha = 0.3) + geom_line(aes(y = med), colour = "steelblue4") + geom_hline(yintercept = 16, linetype = 2, colour = "grey40") +
    annotate("text", x = 2005.3, y = 45, label = "Dashed line: about 16 t P/yr (Hamilton et al. 2015; ERI 128)", hjust = 0, size = 3) + labs(x = NULL, y = "P retained in the lake because of alum (t P per year)", subtitle = "Median and range across members; budget from the lake outlet export and the change in water-column TP") + theme_minimal(base_size = 10), width = 8, height = 4, dpi = 130, bg = "white")
}
