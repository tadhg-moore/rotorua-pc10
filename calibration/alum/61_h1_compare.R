# Compare the stream-step hypotheses on the best member (member 46 of stage j): previous (McIntosh ramp + Utuhina 0.45), H1 (thesis 1:1 in both streams), H1b (thesis in the Puarenga only).
# Needs the runs from run_hist.R (HIST_KEEP=1, HIST_N=1): out_budget_m46 (previous), out_h1_m46_H1, out_h1_m46_H1b, and calibration/alum/60_h1_base.R.
#   Rscript calibration/alum/61_h1_compare.R   -> website/data/h1/h1_compare.csv, website/data/h1/h1_fit_annual.csv, website/images/h1/h1_fit.png
suppressMessages({library(AEME); library(dplyr); library(tidyr); library(readr); library(ggplot2)})
dat <- "website/data/h1"; img <- "website/images/h1"; for (d in c(dat, img)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
MP <- 30.974; MAL <- 26.98; DOPR <- 0.15 * MP / 1000; START <- as.Date("2006-01-01"); END <- as.Date("2022-06-29")
runs <- c("Previous (ramp)" = "calibration/historical/out_budget_m46", "H1 (thesis, both)" = "calibration/historical/out_h1_m46_H1", "H1b (thesis, Puarenga)" = "calibration/historical/out_h1_m46_H1b")
COL <- c("Previous (ramp)" = "grey40", "H1 (thesis, both)" = "firebrick", "H1b (thesis, Puarenga)" = "#b0246b")
rows <- list(); fit <- list(); ann <- list()
for (nm in names(runs)) { d <- runs[[nm]]; a <- readRDS(file.path(d, "aeme_46.rds")); m <- readRDS(file.path(d, "member_46.rds"))
  inf <- AEME::inflows(a)$data; S <- bind_rows(lapply(c("Utuhina", "Puarenga"), \(s) { x <- inf[[s]]; data.frame(stream = s, Date = as.Date(x$Date), Q = x$HYD_flow, dose = x$ALU_ala + x$ALU_alp, B = x$ALU_alp) })) |> filter(Date >= START, Date <= END)
  yrs <- as.numeric(difftime(END, START, units = "days")) / 365.25
  Al_t <- sum(S$dose * S$Q) * MAL / 1e9; Bstr_t <- sum(S$B * S$Q) * MP / 1e9
  nc <- ncdf4::nc_open(AEME::get_model_outfile(a)$glm_aed[["output"]]); tm <- ncdf4::ncvar_get(nc, "time"); orig <- as.Date(strsplit(ncdf4::ncatt_get(nc, "time", "units")$value, " ")[[1]][3]); dd <- orig + floor(tm / 24)
  ns <- ncdf4::ncvar_get(nc, "NS"); gm <- function(v) { x <- ncdf4::ncvar_get(nc, v); x[!is.finite(x) | abs(x) > 1e20] <- 0; x }
  Vl <- gm("V"); valid <- row(Vl) <= matrix(ns, nrow(Vl), length(ns), byrow = TRUE); Vl[!valid] <- 0; Uw <- colSums(gm("ALU_uptake_w") * Vl)
  za <- ncdf4::ncvar_get(nc, "zarea")[1:4, ]; za[!is.finite(za)] <- 0; Us <- colSums({x <- ncdf4::ncvar_get(nc, "ALU_uptake_s_Z")[1:4, ]; x[!is.finite(x)] <- 0; x} * za); ncdf4::nc_close(nc)
  k <- dd >= START & dd <= END; Uw_t <- sum(Uw[k]) * MP / 1e9; Us_t <- sum(Us[k]) * MP / 1e9
  o <- AEME::aeme_to_inflow(a) |> mutate(Date = as.Date(Date)) |> filter(Date >= "2011-01-01", Date <= "2021-12-31"); exp_t <- sum(o$HYD_flow * (o$PHS_tp + DOPR)) / 1e6 / (as.numeric(difftime(as.Date("2021-12-31"), as.Date("2011-01-01"), units = "days") + 1) / 365.25)
  rows[[nm]] <- data.frame(run = nm, Al_applied_t_yr = Al_t / yrs, P_bound_stream_t_yr = Bstr_t / yrs, P_bound_lake_water_t_yr = Uw_t / yrs, P_bound_lake_bed_t_yr = Us_t / yrs, P_bound_total_t_yr = (Bstr_t + Uw_t + Us_t) / yrs,
    AlP_stream = Al_t / Bstr_t * MP / MAL, AlP_lake = (Al_t - Bstr_t * MAL / MP) / (Uw_t + Us_t) * MP / MAL, AlP_whole = Al_t / (Bstr_t + Uw_t + Us_t) * MP / MAL, export_2011_21_t_yr = exp_t)
  for (v in c("PHS_tp", "PHS_frp", "PHY_tchla")) { p <- m$P[[v]] |> filter(depth <= 5, Date >= START, Date <= END, is.finite(obs), is.finite(sim)); if (v == "PHS_tp") p$sim <- p$sim + DOPR
    fit[[paste(nm, v)]] <- data.frame(run = nm, variable = v, n = nrow(p), bias_pct = 100 * (mean(p$sim) / mean(p$obs) - 1), rmse = sqrt(mean((p$sim - p$obs)^2)), r = cor(p$sim, p$obs))
    ann[[paste(nm, v)]] <- p |> group_by(yr) |> summarise(obs = mean(obs), sim = mean(sim), .groups = "drop") |> mutate(run = nm, variable = v) } }
tab <- bind_rows(rows); fit <- bind_rows(fit); ann <- bind_rows(ann)
write_csv(tab |> mutate(across(where(is.numeric), \(x) signif(x, 4))), file.path(dat, "h1_compare.csv")); write_csv(fit |> mutate(across(where(is.numeric), \(x) signif(x, 4))), file.path(dat, "h1_fit.csv")); write_csv(ann |> mutate(across(where(is.numeric), \(x) signif(x, 4))), file.path(dat, "h1_fit_annual.csv"))
print(as.data.frame(tab |> mutate(across(where(is.numeric), \(x) signif(x, 3)))), row.names = FALSE); print(as.data.frame(fit |> mutate(across(where(is.numeric), \(x) signif(x, 3)))), row.names = FALSE)
lab <- c(PHS_tp = "Surface TP (g/m3, with refractory DOP)", PHS_frp = "Surface FRP (g/m3)", PHY_tchla = "Surface chlorophyll (mg/m3)")
ob <- ann |> filter(run == names(runs)[1]) |> transmute(variable, yr, value = obs, run = "Observed")
g <- bind_rows(ann |> transmute(variable, yr, value = sim, run), ob) |> mutate(variable = factor(lab[variable], lab), run = factor(run, c("Observed", names(runs))))
ggsave(file.path(img, "h1_fit.png"), ggplot(g, aes(yr, value, colour = run)) + geom_line(aes(linewidth = run == "Observed")) + geom_point(size = 0.8) + facet_wrap(~variable, scales = "free_y", ncol = 1) + scale_linewidth_manual(values = c(`TRUE` = 1, `FALSE` = 0.5), guide = "none") +
  scale_colour_manual(values = c(Observed = "black", COL)) + labs(x = NULL, y = "Annual mean of the surface (0 to 5 m) samples", colour = NULL) + theme_minimal(base_size = 10) + theme(legend.position = "bottom"), width = 7.5, height = 7, dpi = 130, bg = "white")
cat("done\n")
