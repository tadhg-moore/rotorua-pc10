# Step 2 performance plots and tables: start values vs the best ensemble member, calibration and held-out years.
#   Rscript calibration/step2/step2_report.R
source("calibration/step2/step2_common.R")
suppressPackageStartupMessages({library(ggplot2); library(dplyr); library(tidyr)})
out <- Sys.getenv("S2_OUT", file.path("website", "images", "step2"))
lab0 <- if (STEP2B) "Step 2b start" else "Start values" 
dir.create(out, recursive = TRUE, showWarnings = FALSE)
ies <- readRDS(file.path(SA_DIR, "step2_ies.rds")); best <- readRDS(file.path(SA_DIR, "step2_best.rds"))
tab <- ies$tab; vary <- ies$vary; hist <- ies$hist
b <- step2_build("sa_base")
VARS2 <- c(STEP2_VARS$variable, "HYD_temp", "CHM_oxy")
lab <- c(PHY_tchla = "Chlorophyll-a (ug/L)", PHS_tp = "Total P (g/m3)", PHS_frp = "Filterable reactive P (g/m3)",
         NIT_tn = "Total N (g/m3)", NIT_nit = "Nitrate (g/m3)", NIT_amm = "Ammonium (g/m3)")
run_one <- function(param) {
  res <- aemetools::run_aeme_param(b$aeme, param = param[, PCOLS], model = MODEL, path = b$path, return_aeme = TRUE)
  sim <- lapply(names(lab), function(v) {
    d <- AEME::get_var(res, model = MODEL, var_sim = v, use_obs = FALSE, remove_spin_up = TRUE)
    d <- d[d$depth <= 5, ]; d <- aggregate(value ~ Date, d, mean); d$Date <- as.Date(d$Date)
    if (v == "NIT_tn") d$value <- d$value + DONR_GM3
    if (v == "PHS_tp") d$value <- d$value + DOPR_GM3
    d$var <- v; d })
  P <- csps_pair(res, vars = VARS2)
  list(sim = bind_rows(sim), P = P)
}
r0 <- run_one(best$best |> mutate(value = tab$value))        # start values (the table's start column)
r1 <- run_one(best$best)
saveRDS(list(start = r0, best = r1), file.path(SA_DIR, "step2_report_runs.rds"))

# ---- 1. time series: surface observations (points) vs daily surface model mean ------------------
obs <- bind_rows(lapply(names(lab), function(v) { d <- r1$P[[v]]; d <- d[d$depth <= 5, ]
  d |> group_by(Date) |> summarise(value = mean(obs), .groups = "drop") |> mutate(var = v) }))
sim <- bind_rows(r0$sim |> mutate(run = lab0), r1$sim |> mutate(run = "Best member"))
obs$panel <- factor(lab[obs$var], lab); sim$panel <- factor(lab[sim$var], lab)
p1 <- ggplot() +
  geom_line(data = sim, aes(Date, value, colour = run), linewidth = 0.35, alpha = 0.9) +
  geom_point(data = obs, aes(Date, value), size = 0.7, colour = "grey20") +
  geom_vline(xintercept = as.Date("2004-01-01"), linetype = 2, colour = "grey50") +
  facet_wrap(~panel, ncol = 1, scales = "free_y") +
  scale_colour_manual(values = setNames(c("#d95f02", "#1b9e77"), c(lab0, "Best member")), name = NULL) +
  labs(x = NULL, y = NULL, subtitle = "Points: observed surface (0-5 m). Lines: daily surface model mean. Dashed line: start of the held-out years.") +
  theme_minimal(base_size = 10) + theme(legend.position = "top")
ggsave(file.path(out, "series.png"), p1, width = 10, height = 12, dpi = 120, bg = "white")

# ---- 2. annual means of chlorophyll, total P and total N: observed vs best member ---------------
ann <- bind_rows(lapply(c("PHY_tchla", "PHS_tp", "NIT_tn"), function(v) {
  list(r0$P[[v]] |> mutate(run = lab0), r1$P[[v]] |> mutate(run = "Best member")) |>
    bind_rows() |> filter(depth <= 5) |> mutate(var = v) }))
ann$sim <- ann$sim + ifelse(ann$var == "NIT_tn", DONR_GM3, ifelse(ann$var == "PHS_tp", DOPR_GM3, 0))
A <- ann |> group_by(var, run, yr) |> summarise(n = n(), obs = mean(obs), sim = mean(sim), .groups = "drop") |> filter(n >= 3)
A <- pivot_longer(A, c(obs, sim), names_to = "src", values_to = "value") |>
  filter(!(src == "obs" & run == lab0)) |> mutate(series = ifelse(src == "obs", "Observed", run))
A$panel <- factor(lab[A$var], lab)
p2 <- ggplot(A, aes(yr, value, colour = series, group = series)) + geom_line(linewidth = 0.6) + geom_point(size = 1.4) +
  geom_vline(xintercept = 2003.5, linetype = 2, colour = "grey50") + facet_wrap(~panel, ncol = 1, scales = "free_y") +
  scale_colour_manual(values = setNames(c("grey20", "#d95f02", "#1b9e77"), c("Observed", lab0, "Best member")), name = NULL) +
  labs(x = NULL, y = NULL, subtitle = "Annual means over sampling dates with at least 3 samples") + theme_minimal(base_size = 10) + theme(legend.position = "top")
ggsave(file.path(out, "annual.png"), p2, width = 9, height = 8, dpi = 120, bg = "white")

# ---- 3. observed vs simulated scatter (surface samples), best member ---------------------------
sc <- bind_rows(lapply(names(lab), function(v) { d <- r1$P[[v]]; d <- d[d$depth <= 5, ]
  d$sim <- d$sim + ifelse(v == "NIT_tn", DONR_GM3, ifelse(v == "PHS_tp", DOPR_GM3, 0))
  d$var <- v; d$window <- ifelse(d$Date <= CAL_END2, "Calibration", "Held out"); d[, c("obs", "sim", "var", "window")] }))
sc$panel <- factor(lab[sc$var], lab)
p3 <- ggplot(sc, aes(obs, sim, colour = window)) + geom_abline(colour = "grey60") + geom_point(size = 0.8, alpha = 0.6) +
  facet_wrap(~panel, scales = "free", ncol = 3) + scale_x_log10() + scale_y_log10() +
  scale_colour_manual(values = c(Calibration = "#1b9e77", "Held out" = "#7570b3"), name = NULL) +
  labs(x = "Observed", y = "Simulated", subtitle = "Best member, surface samples, log axes; line = 1:1") + theme_minimal(base_size = 10) + theme(legend.position = "top")
ggsave(file.path(out, "scatter.png"), p3, width = 10, height = 6.5, dpi = 120, bg = "white")

# ---- 4. residuals: start vs best member, calibration and held-out ------------------------------
R <- data.frame(metric = STEP2_NAMES, start_cal = best$baseline_Ycal, best_cal = best$best_Ycal,
                start_val = best$baseline_Yval, best_val = best$best_Yval)
write.csv(R, file.path(out, "residuals.csv"), row.names = FALSE)
RL <- pivot_longer(R, -metric, names_to = c("run", "window"), names_sep = "_", values_to = "value") |>
  mutate(run = ifelse(run == "start", lab0, "Best member"), window = ifelse(window == "cal", "Calibration years", "Held-out years"),
         metric = factor(metric, rev(STEP2_NAMES)))
p4 <- ggplot(RL, aes(value, metric, colour = run)) + geom_vline(xintercept = 0, colour = "grey60") + geom_point(size = 1.8) +
  facet_wrap(~window) + scale_colour_manual(values = setNames(c("#d95f02", "#1b9e77"), c(lab0, "Best member")), name = NULL) +
  labs(x = "Residual (model minus observed, in tolerance units; 0 is a perfect fit)", y = NULL) + theme_minimal(base_size = 9) + theme(legend.position = "top")
ggsave(file.path(out, "residuals.png"), p4, width = 10, height = 7, dpi = 120, bg = "white")

# ---- 5. error score by iteration -----------------------------------------------------------------
ph <- data.frame(iteration = seq_along(hist) - 1, median_cal = sapply(hist, \(h) median(h$phi, na.rm = TRUE)),
                 best_cal = sapply(hist, \(h) min(h$phi, na.rm = TRUE)), median_val = sapply(hist, \(h) median(rowMeans(h$Yval^2), na.rm = TRUE)))
write.csv(ph, file.path(out, "phi.csv"), row.names = FALSE)
phl <- pivot_longer(ph, -iteration, names_to = "series", values_to = "phi")
p5 <- ggplot(phl, aes(iteration, phi, colour = series)) + geom_line() + geom_point() +
  scale_colour_manual(values = c(median_cal = "black", best_cal = "#1b9e77", median_val = "#7570b3"),
                      labels = c(median_cal = "Ensemble median, calibration", best_cal = "Best member, calibration", median_val = "Ensemble median, held out"), name = NULL) +
  geom_hline(yintercept = mean(best$baseline_Ycal^2), linetype = 3, colour = "grey50") + labs(x = "Iteration (0 = prior ensemble)", y = "Error score phi") +
  theme_minimal(base_size = 10) + theme(legend.position = "top")
ggsave(file.path(out, "phi.png"), p5, width = 7, height = 4, dpi = 120, bg = "white")

# ---- 6. where the best member sits in each parameter range, and how much the data constrained it -
fin <- hist[[length(hist)]]$U
pp <- data.frame(parameter = paste0(tab$name[vary], ifelse(is.na(tab$group[vary]), "", paste0(" (", tab$group[vary], ")")),
                                    ifelse(is.na(tab$index[vary]) | !is.na(tab$group[vary]), "", paste0(" z", tab$index[vary]))) |> sub(pattern = "^[a-z_]+/", replacement = ""),
                 position = best$U_best, sd_ratio = apply(fin, 2, sd) / 0.289)
pp$parameter <- factor(pp$parameter, rev(pp$parameter))
p6 <- ggplot(pp, aes(position, parameter, size = 1 - sd_ratio)) + geom_vline(xintercept = c(0, 1), colour = "grey70") +
  geom_point(colour = "#1b9e77") + scale_size_continuous(range = c(1, 5), name = "Constrained\n(1 - sd ratio)") + xlim(0, 1) +
  labs(x = "Best member position within the parameter range (0 = lower bound, 1 = upper bound)", y = NULL) + theme_minimal(base_size = 9)
ggsave(file.path(out, "parameters.png"), p6, width = 8, height = 7, dpi = 120, bg = "white")
write.csv(cbind(pp, start = tab$value[vary], best = best$best$value[vary], min = tab$min[vary], max = tab$max[vary]), file.path(out, "params.csv"), row.names = FALSE)
cat("done\n")
