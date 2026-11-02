# Silica under the corrected light: the 2f best member with the inflow / lake silica set by overwriting SIL_rsi in the built inflow files (AEME's own
# unit handling multiplies large values by 16.64), over a short window. Does a silica supply let diatoms carry the winter-spring chlorophyll and nitrate uptake?
#   Rscript calibration/step2/test_silica_f.R     (SI_VALUES = "1,50,150,300")
Sys.setenv(S2_STAGE = "f", S2_DIR = file.path("calibration", "step2", "silica_test_f"))
source("calibration/step2/step2_common.R"); suppressMessages({library(ncdf4); library(dplyr)})
SA_START <- Sys.getenv("SI_START", "1998-01-01"); SA_STOP <- Sys.getenv("SI_STOP", "2003-12-31")
vals <- as.numeric(strsplit(Sys.getenv("SI_VALUES", "1,50,150,300"), ",")[[1]]); out <- file.path("calibration", "step2", "silica_test_f"); dir.create(out, showWarnings = FALSE, recursive = TRUE)
best <- readRDS("calibration/step2f/runs/step2_best.rds")$best
case <- function(si) {
  b <- step2_build("sf")
  for (f in list.files(b$path, "^inflow_.*[.]csv$", recursive = TRUE, full.names = TRUE)) { d <- read.csv(f, check.names = FALSE); if ("SIL_rsi" %in% names(d)) { d$SIL_rsi <- si; write.csv(d, f, row.names = FALSE, quote = FALSE) } }
  cfg <- AEME::configuration(b$aeme); cfg[["glm_aed"]][["bgc"]][["aed"]][["aed_silica"]][["rsi_initial"]] <- si
  ip <- cfg[["glm_aed"]][["hydrodynamic"]][["init_profiles"]]; v <- matrix(ip$wq_init_vals, nrow = ip$num_depths); colnames(v) <- ip$wq_names; v[, "SIL_rsi"] <- si
  cfg[["glm_aed"]][["hydrodynamic"]][["init_profiles"]][["wq_init_vals"]] <- as.vector(v); AEME::configuration(b$aeme) <- cfg
  AEME::write_configuration(aeme = b$aeme, model = MODEL, path = b$path, include_boundary = FALSE)
  for (f in list.files(b$path, "^inflow_.*[.]csv$", recursive = TRUE, full.names = TRUE)) { d <- read.csv(f, check.names = FALSE); if ("SIL_rsi" %in% names(d)) { d$SIL_rsi <- si; write.csv(d, f, row.names = FALSE, quote = FALSE) } }   # write_configuration may rewrite the inflows
  chk <- read.csv(list.files(b$path, "inflow_Hamurana.csv", recursive = TRUE, full.names = TRUE)[1]); cat("  inflow SIL_rsi in file:", chk$SIL_rsi[1], "\n")
  res <- aemetools::run_aeme_param(b$aeme, param = best[, PCOLS], model = MODEL, path = b$path, return_aeme = TRUE); stopifnot(inherits(res, "Aeme"))
  x <- nc_open(file.path(AEME::get_lake_dir(res, b$path), "glm_aed", "output", "output.nc")); on.exit(nc_close(x))
  t0 <- ncatt_get(x, "time", "units")$value; orig <- as.Date(strsplit(t0, " ")[[1]][3]); tm <- ncvar_get(x, "time"); dts <- if (grepl("hour", t0)) orig + floor(tm / 24) else orig + tm
  NS <- ncvar_get(x, "NS"); Z <- ncvar_get(x, "z"); keep <- dts >= as.Date(SA_START) + 550
  up5 <- function(v) { m <- ncvar_get(x, v); sapply(seq_along(NS), function(i) { n <- NS[i]; h <- Z[seq_len(n), i]; th <- diff(c(0, h)); k <- (h[n] - (h - th / 2)) <= 5; sum(m[seq_len(n)[k], i] * th[k]) / sum(th[k]) })[keep] }
  mo <- as.integer(format(dts[keep], "%m")); S <- data.frame(mo = mo, chl = up5("PHY_tchla"), nit = up5("NIT_nit"), di = up5("PHY_diatom"), cy = up5("PHY_cyano"), gr = up5("PHY_green"), rsi = up5("SIL_rsi"))
  list(si = si, tab = S |> group_by(mo) |> summarise(across(everything(), mean), .groups = "drop") |> mutate(si = si))
}
R <- lapply(vals, function(s) { cat("silica", s, "\n"); flush.console(); case(s) }); A <- bind_rows(lapply(R, `[[`, "tab"))
cat("\nAnnual means by silica supply:\n"); print(A |> group_by(si) |> summarise(chl = mean(chl), nit = mean(nit), diatom_C = mean(di), cyano_C = mean(cy), green_C = mean(gr), rsi = mean(rsi)) |> mutate(across(where(is.numeric), \(v) signif(v, 3))) |> as.data.frame(), row.names = FALSE)
cat("\nMonthly chlorophyll (ug/L) and nitrate (g/m3, ~mmol*0.014) by silica:\n"); print(tidyr::pivot_wider(A |> mutate(chl = round(chl, 1), nit = round(nit * 0.014007, 4)), id_cols = mo, names_from = si, values_from = c(chl, nit)) |> as.data.frame(), row.names = FALSE)
write.csv(A, file.path(out, "silica_f.csv"), row.names = FALSE)
