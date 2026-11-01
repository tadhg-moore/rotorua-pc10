# Step 1 of the thesis-stoichiometry calibration: fit the in-stream removal rule to the stream observations alone (no model run).
#   Rscript calibration/alum/62_stream_fit.R   -> website/data/h1/stream_fit_*.csv, website/images/h1/stream_fit.png
# Utuhina: the inflow P is measured downstream of the dosing point, so removal f_obs = 1 - DRP_with / DRP_untreated on dosed days (with-alum file = measured, no-alum file = reconstructed from the pre-dosing monthly DRP:TP, Hamilton et al. 2015 p36).
#   Both files are daily series interpolated from monthly samples, so the days are not independent: the intervals use a bootstrap over calendar months.
# Puarenga: no independent data in the repository (the McIntosh spot samples of Hamilton et al. 2015 Fig 20 are not available); the with-alum file is built from the ramp itself, which is shown below.
# Rule (H2 form): f = min(f_max, eps * R), R = dose / untreated DRP (mol Al per mol P), eps in mol P bound per mol Al (the thesis: about 1). Also fitted: a flat f and a smooth saturating rule f = f_max * (1 - exp(-eps R / f_max)).
suppressMessages({library(AEME); library(dplyr); library(tidyr); library(readr); library(ggplot2)})
dat <- "website/data/h1"; img <- "website/images/h1"; for (d in c(dat, img)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
MMOL <- 1000 / 30.974; START <- as.Date("2007-01-01"); END <- as.Date("2022-06-29"); set.seed(1)
inf <- AEME::inflows(readRDS("calibration/alum/stream/base_Q50.rds"))$data
rd <- function(f) read_csv(file.path("data/raw/flows", f), show_col_types = FALSE) |> mutate(Date = as.Date(Date)) |> select(Inflow, Date, Q = VOL, DRP, TP)
W <- rd("rotorua_inflows_populated_withalum.csv"); N <- rd("rotorua_inflows_populated_noalum.csv")
mk <- function(s) { d <- inf[[s]]; d$Date <- as.Date(d$Date); dose <- data.frame(Date = d$Date, dose = d$ALU_ala + d$ALU_alp)
  inner_join(W |> filter(Inflow == s) |> select(Date, Q, DRP_w = DRP), N |> filter(Inflow == s) |> select(Date, DRP_n = DRP), by = "Date") |> inner_join(dose, by = "Date") |>
    filter(Date >= START, Date <= END, dose > 0, DRP_n > 0.005) |> mutate(stream = s, R = dose / (DRP_n * MMOL), f_obs = pmin(pmax(1 - DRP_w / DRP_n, 0), 1), al_kgd = dose * Q * 26.98 / 1e6, month = format(Date, "%Y-%m")) }
U <- mk("Utuhina"); P <- mk("Puarenga")
cat("Utuhina dosed days:", nrow(U), " months:", n_distinct(U$month), " mean f_obs", round(mean(U$f_obs), 3), "\n")
# ---- data summary by dose ratio ----
rb <- c(0, 1, 2, 5, 10, 23, 50, Inf); sumR <- function(x) x |> mutate(Rbin = cut(R, rb, include.lowest = TRUE)) |> group_by(stream, Rbin) |> summarise(days = n(), R_mean = mean(R), al_kgd_mean = mean(al_kgd), f_sd = sd(f_obs), f_obs = mean(f_obs), .groups = "drop")
tabR <- bind_rows(sumR(U), sumR(P)); print(as.data.frame(tabR |> mutate(across(where(is.numeric), \(z) signif(z, 3)))), row.names = FALSE)
# ---- the Puarenga with-alum file is the ramp: fit it back ----
ramp <- 0.9 * pmin(1, P$R / 23); cat("Puarenga with-alum file vs 0.9*min(1, R/23): RMSE", signif(sqrt(mean((P$f_obs - ramp)^2)), 3), " max |diff|", signif(max(abs(P$f_obs - ramp)), 3), "\n")
# ---- models fitted to the Utuhina ----
Mflat <- function(p, R) rep(p[1], length(R)); Mlin <- function(p, R) pmin(p[2], p[1] * R); Mexp <- function(p, R) p[2] * (1 - exp(-p[1] * R / p[2]))
fitm <- function(M, start, lower, upper, d) { o <- optim(start, function(p) sum((d$f_obs - M(p, d$R))^2), method = "L-BFGS-B", lower = lower, upper = upper); o$par }
spec <- list(flat = list(M = Mflat, s = 0.5, lo = 0, up = 1), linear_cap = list(M = Mlin, s = c(0.3, 0.8), lo = c(0.001, 0.2), up = c(3, 1)), saturating = list(M = Mexp, s = c(0.3, 0.8), lo = c(0.001, 0.2), up = c(3, 1)))
fits <- lapply(spec, \(m) fitm(m$M, m$s, m$lo, m$up, U))
months <- unique(U$month); B <- 500
boot <- lapply(names(spec), \(k) { t(replicate(B, simplify = "matrix", { ms <- sample(months, replace = TRUE); d <- bind_rows(lapply(ms, \(m) U[U$month == m, ])); fitm(spec[[k]]$M, spec[[k]]$s, spec[[k]]$lo, spec[[k]]$up, d) })) }); names(boot) <- names(spec)
rmse <- function(M, p, d) sqrt(mean((d$f_obs - M(p, d$R))^2))
ref <- list(`previous: flat 0.45` = \(R) rep(0.45, length(R)), `previous: ramp 0.9 min(1, R/23)` = \(R) 0.9 * pmin(1, R / 23), `H1: min(0.9, 1 R)` = \(R) pmin(0.9, R))
res <- bind_rows(lapply(names(spec), \(k) { p <- fits[[k]]; b <- boot[[k]]; if (nrow(b) == 1) b <- matrix(as.numeric(b), ncol = 1); data.frame(rule = k, eps = if (k == "flat") NA else p[1], eps_lo = if (k == "flat") NA else quantile(b[, 1], 0.05), eps_hi = if (k == "flat") NA else quantile(b[, 1], 0.95),
  f_max = if (k == "flat") p[1] else p[2], fmax_lo = quantile(b[, if (k == "flat") 1 else 2], 0.05), fmax_hi = quantile(b[, if (k == "flat") 1 else 2], 0.95), rmse = rmse(spec[[k]]$M, p, U), n_par = length(p)) }),
  bind_rows(lapply(names(ref), \(k) data.frame(rule = k, eps = NA, eps_lo = NA, eps_hi = NA, f_max = NA, fmax_lo = NA, fmax_hi = NA, rmse = sqrt(mean((U$f_obs - ref[[k]](U$R))^2)), n_par = 0))))
res$mean_bias <- c(sapply(names(spec), \(k) mean(spec[[k]]$M(fits[[k]], U$R) - U$f_obs)), sapply(ref, \(f) mean(f(U$R) - U$f_obs)))
cat("\nUtuhina fits (5-95% from a bootstrap over months):\n"); print(as.data.frame(res |> mutate(across(where(is.numeric), \(z) signif(z, 3)))), row.names = FALSE)
write_csv(res |> mutate(across(where(is.numeric), \(z) signif(z, 4))), file.path(dat, "stream_fit_utuhina.csv")); write_csv(tabR |> mutate(across(where(is.numeric), \(z) signif(z, 4))), file.path(dat, "stream_fit_byR.csv"))
# ---- what the Puarenga summaries in Hamilton et al. (2015) constrain ----
# f >= 0.8 at a mass ratio Al:PO4-P of about 20 (R = 23 mol/mol): eps * 23 >= 0.8  ->  eps >= 0.035 for f_max = 0.9; the ramp is the lower bound of eps
cat("\nPuarenga: eps lower bound from 'more than 80% reduction at R = 23':", round(0.8 / 23, 3), "(ramp: ", round(0.9 / 23, 3), ") ; thesis ~0.98\n")
# ---- figure ----
Rg <- exp(seq(log(0.2), log(100), length.out = 200)); cv <- bind_rows(lapply(names(spec), \(k) data.frame(R = Rg, f = spec[[k]]$M(fits[[k]], Rg), rule = paste("fit:", k))), lapply(names(ref), \(k) data.frame(R = Rg, f = ref[[k]](Rg), rule = k)))
pts <- U |> mutate(Rbin = cut(R, rb, include.lowest = TRUE)) |> group_by(Rbin) |> summarise(R = mean(R), f = mean(f_obs), lo = quantile(f_obs, 0.1), hi = quantile(f_obs, 0.9), n = n(), .groups = "drop")
ggsave(file.path(img, "stream_fit.png"), ggplot() + geom_point(data = U, aes(R, f_obs), colour = "grey70", size = 0.5, alpha = 0.5) + geom_linerange(data = pts, aes(R, ymin = lo, ymax = hi), colour = "black") + geom_point(data = pts, aes(R, f), colour = "black", size = 2) +
  geom_line(data = cv, aes(R, f, colour = rule, linetype = grepl("^fit", rule)), linewidth = 0.7) + scale_x_log10() + scale_linetype_manual(values = c(`TRUE` = "solid", `FALSE` = "dashed"), guide = "none") + coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Dose ratio R = Al dose / untreated DRP (mol/mol)", y = "Fraction of DRP removed", colour = NULL, subtitle = "Utuhina, dosed days 2007 to 2022-06. Grey: days; black: mean and 10 to 90% range by dose-ratio bin.") + theme_minimal(base_size = 10) + theme(legend.position = "bottom", plot.subtitle = element_text(size = 8)) + guides(colour = guide_legend(ncol = 2)),
  width = 7.5, height = 5, dpi = 130, bg = "white")
cat("done\n")
