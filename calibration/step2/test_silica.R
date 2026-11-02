# What does a realistic silica supply do? AEME has no inflow_param(); the inflow concentrations are changed with
# AEME::get_inflows() / set_inflows() on the base Aeme object, and the lake's initial silica in the AED config.
# Runs the 2e best member over a short window at several inflow SIL_rsi values (mmol Si/m3) and compares diatoms, chlorophyll
# and the nutrient fit.
#   Rscript calibration/step2/test_silica.R      (SI_STAGE = e, SI_VALUES = "1,50,150,300", SI_START / SI_STOP)
stage <- Sys.getenv("SI_STAGE", "e")
Sys.setenv(S2_STAGE = stage, S2_DIR = file.path("calibration", "step2", "silica_test"))
source("calibration/step2/step2_common.R"); suppressMessages(library(ncdf4))
SA_START <- Sys.getenv("SI_START", "1990-01-01"); SA_STOP <- Sys.getenv("SI_STOP", "1997-12-31")
vals <- as.numeric(strsplit(Sys.getenv("SI_VALUES", "1,50,150,300"), ",")[[1]])
out <- file.path("calibration", "step2", "silica_test"); dir.create(out, showWarnings = FALSE, recursive = TRUE)
best <- readRDS(file.path(switch(stage, b = "calibration/step2b/runs", e = "calibration/step2e/runs", "calibration/step2/runs"), "step2_best.rds"))$best
base0 <- readRDS("calibration/step2/aeme_base_1990_v2.rds")

case <- function(si) {
  inf <- AEME::get_inflows(base0); stopifnot(is.list(inf))
  inf <- lapply(inf, function(d) { if ("SIL_rsi" %in% names(d)) d$SIL_rsi <- si; d })
  f <- file.path(out, sprintf("base_si%g.rds", si)); saveRDS(AEME::set_inflows(base0, inf), f); SA_BASE <<- f
  b <- step2_build("si")
  cfg <- AEME::configuration(b$aeme); cfg[["glm_aed"]][["bgc"]][["aed"]][["aed_silica"]][["rsi_initial"]] <- si
  ip <- cfg[["glm_aed"]][["hydrodynamic"]][["init_profiles"]]; v <- matrix(ip$wq_init_vals, nrow = ip$num_depths); colnames(v) <- ip$wq_names; v[, "SIL_rsi"] <- si
  cfg[["glm_aed"]][["hydrodynamic"]][["init_profiles"]][["wq_init_vals"]] <- as.vector(v)
  AEME::configuration(b$aeme) <- cfg; AEME::write_configuration(aeme = b$aeme, model = MODEL, path = b$path, include_boundary = FALSE)
  chk <- read.csv(list.files(b$path, "inflow_Hamurana.csv", recursive = TRUE, full.names = TRUE)[1]); cat("  built inflow SIL_rsi (Hamurana):", chk$SIL_rsi[1], "\n")
  res <- aemetools::run_aeme_param(b$aeme, param = best[, PCOLS], model = MODEL, path = b$path, return_aeme = TRUE)
  stopifnot(inherits(res, "Aeme"))
  P <- csps_pair(res, vars = c("PHY_tchla", "PHS_tp", "PHS_frp", "NIT_nit", "NIT_amm", "NIT_tn"))
  P$PHS_tp$sim <- P$PHS_tp$sim + DOPR_GM3; P$NIT_tn$sim <- P$NIT_tn$sim + DONR_GM3
  fit <- do.call(rbind, lapply(names(P), function(v) { d <- P[[v]]; d <- d[d$depth <= 5, ]
    data.frame(si = si, var = v, obs = mean(d$obs), sim = mean(d$sim), logbias = mean(log(d$sim + .003) - log(d$obs + .003))) }))
  x <- nc_open(file.path(AEME::get_lake_dir(res, b$path), "glm_aed", "output", "output.nc")); on.exit(nc_close(x))
  t0 <- ncatt_get(x, "time", "units")$value; orig <- as.Date(strsplit(t0, " ")[[1]][3]); tm <- ncvar_get(x, "time")
  dts <- if (grepl("hour", t0)) orig + floor(tm / 24) else orig + tm; NS <- ncvar_get(x, "NS"); keep <- dts >= as.Date(SA_START)
  top <- function(v) { m <- ncvar_get(x, v); sapply(seq_along(NS), function(i) m[NS[i], i])[keep] }
  summ <- data.frame(si = si, rsi_surface = mean(top("SIL_rsi")), diatom_C = mean(top("PHY_diatom")), cyano_C = mean(top("PHY_cyano")), green_C = mean(top("PHY_green")),
                     chl_mean = mean(top("PHY_tchla")), chl_p90 = quantile(top("PHY_tchla"), .9), fSi_diatom = mean(top("PHY_diatom_fSil")))
  list(fit = fit, summ = summ)
}
R <- lapply(vals, function(s) { cat("silica", s, "mmol/m3\n"); flush.console(); case(s) })
S <- do.call(rbind, lapply(R, `[[`, "summ")); F <- do.call(rbind, lapply(R, `[[`, "fit"))
cat("\nPhytoplankton by silica supply (surface layer, mean over the run; fSi = diatom silica limitation, 1 = none):\n"); print(signif(S, 3), row.names = FALSE)
cat("\nSurface (0-5 m) nutrient / chlorophyll fit vs observations:\n"); F[, 3:5] <- lapply(F[, 3:5], signif, 3); print(tidyr::pivot_wider(F, id_cols = var, names_from = si, values_from = c(sim, logbias), names_sep = "_si") |> as.data.frame(), row.names = FALSE)
write.csv(S, file.path(out, "silica_phyto.csv"), row.names = FALSE); write.csv(F, file.path(out, "silica_fit.csv"), row.names = FALSE)
