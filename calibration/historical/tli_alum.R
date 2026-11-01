# Trophic level index (NZ TLI, Burns et al. 2000; the AEME formulas) from the joint ensemble with and without alum, and from the observations.
#   components: TLc (chlorophyll-a), TLn (TN), TLp (TP), TLse (Secchi); TLI3 = mean(TLc, TLn, TLp); TLI4 = mean(TLc, TLn, TLp, TLse)
#   Annual means by TLI year (1 July to 30 June, labelled by the year it ends in, as AEME's tli_year), top 5 m as the epilimnion.
#   Model: daily top-5 m means; TP + refractory DOP and TN + refractory DON as in the calibration objective; Secchi = 1.7 / Kd (Kd = model extinction coefficient, top 5 m; the factor used in calibration/light).
#   Observations: Site 2 and Site 5 samples from the top 5 m (Secchi from all samples), mean by date and then by TLI year, at least 4 sampling dates per year (AEME's tli_min_obs_per_year).
suppressMessages({library(dplyr); library(tidyr); library(ggplot2); library(readr)})
out <- "website/images/historical"; dat <- "website/data/historical"; TAG <- Sys.getenv("TLI_TAG", "j50x")
DOPR <- 0.15 * 30.974 / 1000; DONR <- 9 * 14.0067 / 1000; MP <- 30.974 / 1000; MN <- 14.007 / 1000; KS <- 1.7; MINOBS <- 4
tli_c <- \(x) 2.22 + 2.54 * log10(pmax(x, 1e-6)); tli_n <- \(x) -3.61 + 3.01 * log10(pmax(x * 1000, 1e-6)); tli_p <- \(x) 0.218 + 2.92 * log10(pmax(x * 1000, 1e-6)); tli_s <- \(x) 5.10 + 2.27 * log10(pmax(1 / x - 1 / 40, 1e-6))
tliyr <- \(d) as.integer(format(d, "%Y")) + as.integer(as.integer(format(d, "%m")) >= 7)
comp <- function(d) d |> mutate(TLc = tli_c(chl), TLn = tli_n(TN), TLp = tli_p(TP), TLse = tli_s(secchi), TLI3 = (TLc + TLn + TLp) / 3, TLI4 = (TLc + TLn + TLp + TLse) / 4)
# ---- model ----
mod <- function(tag, run) { f <- list.files(file.path("calibration/historical", paste0("out_", tag)), "^member_.*[.]rds$", full.names = TRUE)
  bind_rows(lapply(f, \(x) { m <- readRDS(x); d <- m$daily; if (!"extc_surf" %in% names(d)) stop("no extc in ", x); d |> filter(Date >= "2005-07-01") |> transmute(member = m$member, yr = tliyr(Date), chl = PHY_tchla_surf, TN = TOT_tn_surf * MN + DONR, TP = TOT_tp_surf * MP + DOPR, secchi = KS / extc_surf) |>
    group_by(member, yr) |> summarise(across(c(chl, TN, TP, secchi), mean), .groups = "drop") })) |> comp() |> mutate(run = run) }
RUNS <- c("With observed alum", "No alum"); Ma <- mod(TAG, RUNS[1]); Mn <- mod(paste0(TAG, "_noalum"), RUNS[2]); M <- bind_rows(Ma, Mn) |> filter(yr >= 2006, yr <= 2022); cat(length(unique(M$member)), "members\n")
# ---- observations ----
cd <- targets::tar_read(chem_data) |> mutate(Date = as.Date(Date))
od <- cd |> filter(DepthFrom <= 5 | is.na(DepthFrom)) |> group_by(Date) |> summarise(chl = mean(`CHLA (mg/m^3)`, na.rm = TRUE), TN = mean(`TN (mg/m^3)`, na.rm = TRUE) / 1000, TP = mean(`TP (mg/m^3)`, na.rm = TRUE) / 1000, .groups = "drop") |>
  full_join(cd |> group_by(Date) |> summarise(secchi = mean(`Secchi (m)`, na.rm = TRUE), .groups = "drop"), by = "Date") |> mutate(across(c(chl, TN, TP, secchi), \(x) ifelse(is.nan(x), NA, x)), yr = tliyr(Date))
O <- od |> filter(yr >= 2006, yr <= 2022) |> group_by(yr) |> summarise(across(c(chl, TN, TP, secchi), \(x) if (sum(!is.na(x)) >= MINOBS) mean(x, na.rm = TRUE) else NA_real_), n_secchi = sum(!is.na(secchi)), .groups = "drop") |> comp() |> mutate(run = "Observed")
write_csv(bind_rows(O, M |> group_by(run, yr) |> summarise(across(c(chl, TN, TP, secchi, TLc, TLn, TLp, TLse, TLI3, TLI4), median), .groups = "drop")) |> mutate(across(where(is.numeric), \(x) signif(x, 4))), file.path(dat, "tli_annual.csv"))
vars <- c("TLc", "TLn", "TLp", "TLse", "TLI3", "TLI4"); vl <- c(TLc = "TLc (chlorophyll-a)", TLn = "TLn (total N)", TLp = "TLp (total P)", TLse = "TLse (Secchi)", TLI3 = "TLI3", TLI4 = "TLI4")
# ---- figure 1: annual components and TLI by scenario with observations ----
E <- M |> pivot_longer(all_of(vars), names_to = "var", values_to = "v") |> group_by(run, var, yr) |> summarise(lo = min(v), q1 = quantile(v, .25), med = median(v), q3 = quantile(v, .75), hi = max(v), .groups = "drop") |> mutate(var = factor(var, vars, vl[vars]), run = factor(run, RUNS))
Ol <- O |> pivot_longer(all_of(vars), names_to = "var", values_to = "v") |> mutate(var = factor(var, vars, vl[vars]))
COL <- c("With observed alum" = "steelblue4", "No alum" = "firebrick"); FIL <- c("With observed alum" = "steelblue", "No alum" = "firebrick")
ggsave(file.path(out, "tli_annual.png"), ggplot(E, aes(yr)) + geom_ribbon(aes(ymin = lo, ymax = hi, fill = run), alpha = 0.18) + geom_ribbon(aes(ymin = q1, ymax = q3, fill = run), alpha = 0.3) + geom_line(aes(y = med, colour = run)) + geom_line(data = Ol, aes(yr, v), inherit.aes = FALSE, colour = "black", linewidth = 0.4) + geom_point(data = Ol, aes(yr, v), inherit.aes = FALSE, size = 1.5) +
  facet_wrap(~var, ncol = 2, scales = "free_y") + scale_colour_manual(values = COL) + scale_fill_manual(values = FIL) + labs(x = "TLI year (July to June, labelled by the year it ends)", y = "Index", colour = NULL, fill = NULL, subtitle = "Joint ensemble (median, interquartile and full range) with the observed alum dosing and with no alum; black: observations.") +
  theme_minimal(base_size = 10) + theme(legend.position = "bottom", plot.subtitle = element_text(size = 8)), width = 10, height = 10, dpi = 130, bg = "white")
# ---- figure 2: divergence between the scenarios (with minus without alum), member by member ----
Dv <- inner_join(Ma, Mn, by = c("member", "yr"), suffix = c("_a", "_n")) |> filter(yr >= 2006, yr <= 2022); dv <- sapply(vars, \(v) Dv[[paste0(v, "_a")]] - Dv[[paste0(v, "_n")]]) |> as.data.frame() |> mutate(member = Dv$member, yr = Dv$yr) |> pivot_longer(all_of(vars), names_to = "var", values_to = "d")
dvs <- dv |> group_by(var, yr) |> summarise(lo = min(d), q1 = quantile(d, .25), med = median(d), q3 = quantile(d, .75), hi = max(d), .groups = "drop") |> mutate(var = factor(var, vars, vl[vars]))
ggsave(file.path(out, "tli_divergence.png"), ggplot(dvs, aes(yr)) + geom_hline(yintercept = 0, colour = "grey60") + geom_ribbon(aes(ymin = lo, ymax = hi), fill = "steelblue", alpha = 0.2) + geom_ribbon(aes(ymin = q1, ymax = q3), fill = "steelblue", alpha = 0.35) + geom_line(aes(y = med), colour = "steelblue4") + facet_wrap(~var, ncol = 2, scales = "free_y") +
  labs(x = "TLI year (July to June)", y = "TLI with alum minus TLI without alum", subtitle = "Median, interquartile range and full range across members. Negative: alum lowers the trophic level.") + theme_minimal(base_size = 10) + theme(plot.subtitle = element_text(size = 8)), width = 10, height = 9, dpi = 130, bg = "white")
# ---- table ----
per <- \(y) cut(y, c(2005, 2010, 2016, 2022), c("2006-2010", "2011-2016", "2017-2022"))
tb <- bind_rows(M |> mutate(src = run), O |> mutate(member = 0, src = "Observed")) |> mutate(period = per(yr)) |> group_by(src, member, period) |> summarise(across(all_of(vars), \(x) mean(x, na.rm = TRUE)), .groups = "drop") |> group_by(src, period) |> summarise(across(all_of(vars), list(mean = mean, lo = min, hi = max)), .groups = "drop")
write_csv(tb |> mutate(across(where(is.numeric), \(x) signif(x, 4))), file.path(dat, "tli_by_period.csv"))
dd <- Dv |> mutate(period = per(yr)) |> group_by(member, period) |> summarise(across(all_of(paste0(vars, "_a")), mean), across(all_of(paste0(vars, "_n")), mean), .groups = "drop"); for (v in vars) dd[[v]] <- dd[[paste0(v, "_a")]] - dd[[paste0(v, "_n")]]
dsm <- dd |> group_by(period) |> summarise(across(all_of(vars), list(mean = mean, lo = min, hi = max)), .groups = "drop"); write_csv(dsm |> mutate(across(where(is.numeric), \(x) signif(x, 4))), file.path(dat, "tli_divergence_by_period.csv"))
cat("\nMean TLI by period (ensemble mean; observed):\n"); print(as.data.frame(tb |> select(src, period, ends_with("_mean")) |> mutate(across(where(is.numeric), \(x) round(x, 2)))), row.names = FALSE)
cat("\nWith minus without alum, mean (min, max) by period:\n"); print(as.data.frame(dsm |> mutate(across(where(is.numeric), \(x) round(x, 2)))), row.names = FALSE)
b <- M |> inner_join(O |> select(yr, all_of(vars)) |> rename_with(\(x) paste0(x, "_obs"), all_of(vars)), by = "yr") |> group_by(run, member) |> summarise(TLI4_bias = mean(TLI4 - TLI4_obs, na.rm = TRUE), TLI3_bias = mean(TLI3 - TLI3_obs, na.rm = TRUE), TLI4_r = cor(TLI4, TLI4_obs, use = "complete.obs"), .groups = "drop") |> group_by(run) |> summarise(across(c(TLI3_bias, TLI4_bias, TLI4_r), list(mean = mean, lo = min, hi = max)), .groups = "drop")
cat("\nAgainst the observed annual TLI:\n"); print(as.data.frame(b |> mutate(across(where(is.numeric), \(x) round(x, 2)))), row.names = FALSE); cat("observed Secchi years with data:", sum(!is.na(O$TLse)), "of", nrow(O), "\ndone\n")
