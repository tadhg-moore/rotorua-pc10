# Best member of each lake alum refit (calibration/historical/launch_refit_members.ps1) against the stage j member 46: P bound by alum, Al:P ratios (whole, lake, and the Puarenga stream on days below the cap), export, and the surface fit.
#   Rscript calibration/alum/65_refit_members.R   -> website/data/h1/refit_members.csv, refit_fit.csv
suppressMessages({library(AEME); library(dplyr); library(tidyr); library(readr)})
dat <- "website/data/h1"; img <- "website/images/h1"; for (d in c(dat, img)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
MP <- 30.974; MAL <- 26.98; DOPR <- 0.15 * MP / 1000; START <- as.Date("2006-01-01"); END <- as.Date("2022-06-29")
MMOL <- 1000 / MP
H <- "calibration/historical/"
runs <- list("H0 refit (ramp)" = c(paste0(H, "out_refit_H0"), 1), "H1b refit (thesis, Puarenga)" = c(paste0(H, "out_refit_H1b"), 1), "H1b + beta 1.43 refit" = c(paste0(H, "out_refit_H1b_B143"), 1 / 0.7))
rows <- list(); fit <- list()
for (nm in names(runs)) { d <- runs[[nm]][1]; beta <- as.numeric(runs[[nm]][2]); af <- list.files(d, "^aeme_.*rds$", full.names = TRUE)[1]; a <- readRDS(af); m <- readRDS(sub("aeme_", "member_", af))
  inf <- AEME::inflows(a)$data; S <- bind_rows(lapply(c("Utuhina", "Puarenga"), \(s) { x <- inf[[s]]; data.frame(stream = s, Date = as.Date(x$Date), Q = x$HYD_flow, dose = x$ALU_ala + x$ALU_alp, B = x$ALU_alp, frp0 = x$PHS_frp * MMOL + x$ALU_alp) })) |> filter(Date >= START, Date <= END)
  yrs <- as.numeric(difftime(END, START, units = "days")) / 365.25
  Al_t <- sum(S$dose * S$Q) * MAL / 1e9 / beta; Bstr_t <- sum(S$B * S$Q) * MP / 1e9
  Pz <- S |> filter(stream == "Puarenga", dose > 0, frp0 > 0); atcap <- Pz$B >= 0.89 * Pz$frp0; un <- Pz[!atcap, ]
  nc <- ncdf4::nc_open(AEME::get_model_outfile(a)$glm_aed[["output"]]); tm <- ncdf4::ncvar_get(nc, "time"); orig <- as.Date(strsplit(ncdf4::ncatt_get(nc, "time", "units")$value, " ")[[1]][3]); dd <- orig + floor(tm / 24)
  ns <- ncdf4::ncvar_get(nc, "NS"); gm <- function(v) { x <- ncdf4::ncvar_get(nc, v); x[!is.finite(x) | abs(x) > 1e20] <- 0; x }
  Vl <- gm("V"); valid <- row(Vl) <= matrix(ns, nrow(Vl), length(ns), byrow = TRUE); Vl[!valid] <- 0; Uw <- colSums(gm("ALU_uptake_w") * Vl)
  za <- ncdf4::ncvar_get(nc, "zarea")[1:4, ]; za[!is.finite(za)] <- 0; Us <- colSums({x <- ncdf4::ncvar_get(nc, "ALU_uptake_s_Z")[1:4, ]; x[!is.finite(x)] <- 0; x} * za); ncdf4::nc_close(nc)
  k <- dd >= START & dd <= END; Uw_t <- sum(Uw[k]) * MP / 1e9; Us_t <- sum(Us[k]) * MP / 1e9
  o <- AEME::aeme_to_inflow(a) |> mutate(Date = as.Date(Date)) |> filter(Date >= "2011-01-01", Date <= "2021-12-31"); exp_t <- sum(o$HYD_flow * (o$PHS_tp + DOPR)) / 1e6 / (as.numeric(difftime(as.Date("2021-12-31"), as.Date("2011-01-01"), units = "days") + 1) / 365.25)
  rows[[nm]] <- data.frame(run = nm, Al_applied_t_yr = Al_t / yrs, P_bound_stream_t_yr = Bstr_t / yrs, P_bound_lake_water_t_yr = Uw_t / yrs, P_bound_lake_bed_t_yr = Us_t / yrs, P_bound_total_t_yr = (Bstr_t + Uw_t + Us_t) / yrs,
    AlP_lake = (Al_t - Bstr_t * MAL / MP / beta) / (Uw_t + Us_t) * MP / MAL, AlP_whole = Al_t / (Bstr_t + Uw_t + Us_t) * MP / MAL,
    Puarenga_at_cap_pct = 100 * mean(atcap), Puarenga_AlP_below_cap = sum(un$dose * un$Q) / beta / sum(un$B * un$Q) * MP / MAL * 1, export_2011_21_t_yr = exp_t)
  for (v in c("PHS_tp", "PHS_frp", "PHY_tchla")) { p <- m$P[[v]] |> filter(depth <= 5, Date >= START, Date <= END, is.finite(obs), is.finite(sim)); if (v == "PHS_tp") p$sim <- p$sim + DOPR
    fit[[paste(nm, v)]] <- data.frame(run = nm, variable = v, n = nrow(p), bias_pct = 100 * (mean(p$sim) / mean(p$obs) - 1), rmse = sqrt(mean((p$sim - p$obs)^2)), r = cor(p$sim, p$obs)) } }
tab <- bind_rows(rows); fit <- bind_rows(fit)
write_csv(tab |> mutate(across(where(is.numeric), \(x) signif(x, 4))), file.path(dat, "refit_members.csv")); write_csv(fit |> mutate(across(where(is.numeric), \(x) signif(x, 4))), file.path(dat, "refit_fit.csv"))
print(as.data.frame(tab |> mutate(across(where(is.numeric), \(x) signif(x, 3)))), row.names = FALSE); print(as.data.frame(fit |> mutate(across(where(is.numeric), \(x) signif(x, 3)))), row.names = FALSE)
