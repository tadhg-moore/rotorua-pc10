# Uncertainty of the observed annual TLI and a like-for-like model comparison (extends tli_alum.R).
#   1. Sampling error from the model: the model sampled on the dates the lake was actually sampled (each variable's own dates, at least 4 per TLI year)
#      against the model's full-year mean. Difference = what a snapshot design does to an annual mean, in the model world.
#   2. Bootstrap of the observed annual means over the sampling dates within each TLI year (B draws), carried through the TLI formulas.
#   Then: ensemble sampled on the observation dates vs observed (with the bootstrap band), bias and correlation with intervals, band overlap.
suppressMessages({library(dplyr); library(tidyr); library(ggplot2); library(readr)})
out <- "website/images/historical"; dat <- "website/data/historical"; TAG <- Sys.getenv("TLI_TAG", "j50x"); B <- 1000; set.seed(42)
DOPR <- 0.15 * 30.974 / 1000; DONR <- 9 * 14.0067 / 1000; MP <- 30.974 / 1000; MN <- 14.007 / 1000; KS <- 1.7; MINOBS <- 4
tli_c <- \(x) 2.22 + 2.54 * log10(pmax(x, 1e-6)); tli_n <- \(x) -3.61 + 3.01 * log10(pmax(x * 1000, 1e-6)); tli_p <- \(x) 0.218 + 2.92 * log10(pmax(x * 1000, 1e-6)); tli_s <- \(x) 5.10 + 2.27 * log10(pmax(1 / x - 1 / 40, 1e-6))
tliyr <- \(d) as.integer(format(d, "%Y")) + as.integer(as.integer(format(d, "%m")) >= 7)
comp <- function(d) d |> mutate(TLc = tli_c(chl), TLn = tli_n(TN), TLp = tli_p(TP), TLse = tli_s(secchi), TLI3 = (TLc + TLn + TLp) / 3, TLI4 = (TLc + TLn + TLp + TLse) / 4)
V <- c("chl", "TN", "TP", "secchi"); vars <- c("TLc", "TLn", "TLp", "TLse", "TLI3", "TLI4"); vl <- c(TLc = "TLc (chlorophyll-a)", TLn = "TLn (total N)", TLp = "TLp (total P)", TLse = "TLse (Secchi)", TLI3 = "TLI3", TLI4 = "TLI4")
RUNS <- c("With observed alum", "No alum"); COL <- c("With observed alum" = "steelblue4", "No alum" = "firebrick"); FIL <- c("With observed alum" = "steelblue", "No alum" = "firebrick")
# ---- observations at date level (as tli_alum.R) ----
cd <- targets::tar_read(chem_data) |> mutate(Date = as.Date(Date))
od <- cd |> filter(DepthFrom <= 5 | is.na(DepthFrom)) |> group_by(Date) |> summarise(chl = mean(`CHLA (mg/m^3)`, na.rm = TRUE), TN = mean(`TN (mg/m^3)`, na.rm = TRUE) / 1000, TP = mean(`TP (mg/m^3)`, na.rm = TRUE) / 1000, .groups = "drop") |>
  full_join(cd |> group_by(Date) |> summarise(secchi = mean(`Secchi (m)`, na.rm = TRUE), .groups = "drop"), by = "Date") |> mutate(across(all_of(V), \(x) ifelse(is.nan(x), NA, x)), yr = tliyr(Date)) |> filter(yr >= 2006, yr <= 2022)
OL <- od |> pivot_longer(all_of(V), names_to = "v", values_to = "x") |> filter(!is.na(x)) |> group_by(yr, v) |> filter(n() >= MINOBS) |> ungroup()   # usable date-level observations
ndates <- OL |> count(yr, v) |> pivot_wider(names_from = v, values_from = n); write_csv(ndates, file.path(dat, "tli_obs_ndates.csv"))
Obs <- OL |> group_by(yr, v) |> summarise(x = mean(x), .groups = "drop") |> pivot_wider(names_from = v, values_from = x) |> comp()
# ---- model daily, both runs ----
mod <- function(tag, run) { f <- list.files(file.path("calibration/historical", paste0("out_", tag)), "^member_.*[.]rds$", full.names = TRUE)
  bind_rows(lapply(f, \(x) { m <- readRDS(x); m$daily |> filter(Date >= "2005-07-01") |> transmute(member = m$member, Date, yr = tliyr(Date), chl = PHY_tchla_surf, TN = TOT_tn_surf * MN + DONR, TP = TOT_tp_surf * MP + DOPR, secchi = KS / extc_surf) |> filter(yr >= 2006, yr <= 2022) })) |> mutate(run = run) }
Dm <- bind_rows(mod(TAG, RUNS[1]), mod(paste0(TAG, "_noalum"), RUNS[2])); cat(n_distinct(Dm$member), "members\n")
full <- Dm |> group_by(run, member, yr) |> summarise(across(all_of(V), mean), .groups = "drop") |> comp()
# sampled on the observation dates of each variable
odl <- od |> select(Date, yr, all_of(V)); smp <- bind_rows(lapply(V, \(v) {
  dd <-odl |> filter(!is.na(.data[[v]])) |> select(Date, yr) |> group_by(yr) |> filter(n() >= MINOBS) |> ungroup()
  Dm |> semi_join(dd, by = c("Date", "yr")) |> group_by(run, member, yr) |> summarise(val = mean(.data[[v]]), .groups = "drop") |> mutate(v = v) })) |> pivot_wider(names_from = v, values_from = val) |> comp()
# ---- 1. sampling error (sampled minus full-year), per member-year ----
SE <- inner_join(smp, full, by = c("run", "member", "yr"), suffix = c("_s", "_f")); se <- bind_rows(lapply(vars, \(v) tibble(run = SE$run, member = SE$member, yr = SE$yr, var = v, err = SE[[paste0(v, "_s")]] - SE[[paste0(v, "_f")]]))) |> filter(is.finite(err))
sest <- se |> group_by(run, var) |> summarise(n = n(), mean_err = mean(err), sd_err = sd(err), p05 = quantile(err, .05), p95 = quantile(err, .95), mean_abs = mean(abs(err)), .groups = "drop")
write_csv(sest |> mutate(across(where(is.numeric), \(x) signif(x, 3))), file.path(dat, "tli_sampling_error.csv")); cat("\nSampling error of the annual TLI (model sampled on observation dates minus full-year mean):\n"); print(as.data.frame(sest |> mutate(across(where(is.numeric), \(x) round(x, 3)))), row.names = FALSE)
sey <- se |> group_by(run, var, yr) |> summarise(sd_err = sd(err), mean_err = mean(err), .groups = "drop") |> filter(run == RUNS[1]); write_csv(sey |> mutate(across(where(is.numeric), \(x) signif(x, 3))), file.path(dat, "tli_sampling_error_by_year.csv"))
# ---- 2. bootstrap of the observed annual means ----
bs <- OL |> group_by(yr, v) |> reframe(b = 1:B, m = replicate(B, mean(sample(x, replace = TRUE)))) |> pivot_wider(names_from = v, values_from = m) |> comp()
ob <- bind_rows(lapply(vars, \(k) bs |> group_by(yr) |> summarise(var = k, lo = quantile(.data[[k]], .05, na.rm = TRUE), hi = quantile(.data[[k]], .95, na.rm = TRUE), sd = sd(.data[[k]], na.rm = TRUE), .groups = "drop")))
Ol <- Obs |> pivot_longer(all_of(vars), names_to = "var", values_to = "v") |> left_join(ob, by = c("yr", "var")); write_csv(Ol |> select(yr, var, v, lo, hi, sd) |> mutate(across(where(is.numeric), \(x) signif(x, 4))), file.path(dat, "tli_obs_bootstrap.csv"))
cat("\nMedian bootstrap sd of the observed annual TLI (index units):\n"); print(as.data.frame(Ol |> group_by(var) |> summarise(median_sd = round(median(sd, na.rm = TRUE), 3), max_sd = round(max(sd, na.rm = TRUE), 3))), row.names = FALSE)
# ---- 3. figure: ensemble sampled on the observation dates vs observed with its band ----
pv <- \(d) d |> pivot_longer(all_of(vars), names_to = "var", values_to = "v")
Es <- pv(smp) |> filter(is.finite(v)) |> group_by(run, var, yr) |> summarise(lo = min(v), q1 = quantile(v, .25), med = median(v), q3 = quantile(v, .75), hi = max(v), .groups = "drop") |> mutate(var = factor(var, vars, vl[vars]), run = factor(run, RUNS))
Op <- Ol |> mutate(var = factor(var, vars, vl[vars]))
ggsave(file.path(out, "tli_annual_unc.png"), ggplot(Es, aes(yr)) + geom_ribbon(aes(ymin = lo, ymax = hi, fill = run), alpha = 0.18) + geom_ribbon(aes(ymin = q1, ymax = q3, fill = run), alpha = 0.3) + geom_line(aes(y = med, colour = run)) +
  geom_errorbar(data = Op, aes(yr, ymin = lo, ymax = hi), inherit.aes = FALSE, width = 0.3, colour = "black") + geom_point(data = Op, aes(yr, v), inherit.aes = FALSE, size = 1.4) +
  facet_wrap(~var, ncol = 2, scales = "free_y") + scale_colour_manual(values = COL) + scale_fill_manual(values = FIL) + labs(x = "TLI year (July to June, labelled by the year it ends)", y = "Index", colour = NULL, fill = NULL, subtitle = "Model sampled on the dates each variable was observed (median, interquartile and full range of members) against the observed annual value with its 90% bootstrap interval over sampling dates.") +
  theme_minimal(base_size = 10) + theme(legend.position = "bottom", plot.subtitle = element_text(size = 8)), width = 10, height = 10, dpi = 130, bg = "white")
sg <- se |> filter(var %in% c("TLc", "TLn", "TLp", "TLse", "TLI4")) |> mutate(var = factor(var, c("TLc", "TLn", "TLp", "TLse", "TLI4"), vl[c("TLc", "TLn", "TLp", "TLse", "TLI4")]), run = factor(run, RUNS))
ggsave(file.path(out, "tli_sampling_error.png"), ggplot(sg, aes(factor(yr), err, fill = run)) + geom_hline(yintercept = 0, colour = "grey60") + geom_boxplot(outlier.size = 0.4, linewidth = 0.3, width = 0.7) + facet_wrap(~var, ncol = 1, scales = "free_y") + scale_fill_manual(values = FIL) +
  labs(x = "TLI year", y = "TLI sampled on observation dates minus full-year mean", fill = NULL, subtitle = "What the actual sampling design does to the model's annual TLI, by member (boxes: 50 members).") + theme_minimal(base_size = 10) + theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, vjust = 0.5), plot.subtitle = element_text(size = 8)), width = 10, height = 11, dpi = 130, bg = "white")
# ---- 4. bias and correlation with intervals; band overlap ----
ens <- function(d, run) pv(d) |> filter(run == !!run, is.finite(v)) |> group_by(var, yr) |> summarise(m = median(v), lo = min(v), hi = max(v), .groups = "drop")
bsl <- bs |> pivot_longer(all_of(vars), names_to = "var", values_to = "o")
res <- bind_rows(lapply(RUNS, \(r) bind_rows(lapply(c(sampled = "sampled", full = "full"), \(basis) { E <- ens(if (basis == "sampled") smp else full, r)
  bind_rows(lapply(c("TLI3", "TLI4", "TLp", "TLc", "TLn", "TLse"), \(k) { e <- E |> filter(var == k); j <- inner_join(e, Obs |> select(yr, o = all_of(k)), by = "yr") |> filter(is.finite(o)); jb <- inner_join(e, bsl |> filter(var == k), by = c("var", "yr")) |> filter(is.finite(o))
    bb <- jb |> group_by(b) |> summarise(bias = mean(m - o), r = if (n() > 3) cor(m, o) else NA_real_, .groups = "drop")
    tibble(run = r, basis = basis, var = k, years = nrow(j), bias = mean(j$m - j$o), bias_lo = quantile(bb$bias, .05, na.rm = TRUE), bias_hi = quantile(bb$bias, .95, na.rm = TRUE), r = cor(j$m, j$o), r_lo = quantile(bb$r, .05, na.rm = TRUE), r_hi = quantile(bb$r, .95, na.rm = TRUE)) })) }), .id = NULL)))
write_csv(res |> mutate(across(where(is.numeric), \(x) signif(x, 3))), file.path(dat, "tli_skill_intervals.csv")); cat("\nBias and correlation of the ensemble median against observed annual TLI (obs bootstrap 90% interval):\n"); print(as.data.frame(res |> filter(var %in% c("TLI3", "TLI4", "TLp", "TLc")) |> mutate(across(where(is.numeric), \(x) round(x, 2)))), row.names = FALSE)
ov <- bind_rows(lapply(RUNS, \(r) ens(smp, r) |> mutate(run = r))) |> inner_join(Ol |> select(yr, var, lo_o = lo, hi_o = hi, v), by = c("yr", "var")) |> filter(is.finite(v)) |> mutate(overlap = lo <= hi_o & hi >= lo_o, within_sd = v >= lo & v <= hi)
ovs <- ov |> group_by(run, var) |> summarise(years = n(), overlap = mean(overlap), obs_in_ens_range = mean(within_sd), .groups = "drop"); write_csv(ovs |> mutate(across(where(is.numeric), \(x) signif(x, 3))), file.path(dat, "tli_band_overlap.csv"))
cat("\nShare of years where the observed 90% band overlaps the sampled-ensemble range (overlap), and the observed value lies inside it:\n"); print(as.data.frame(ovs |> mutate(across(where(is.numeric), \(x) round(x, 2)))), row.names = FALSE); cat("done\n")
