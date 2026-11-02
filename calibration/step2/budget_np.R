# N and P budgets (t/yr, 1991-2005) of whatever run is in calibration/step2/runs/build/sa_base (the last model run made there).
#   Rscript calibration/step2/budget_np.R
suppressMessages({library(ncdf4); library(AEME); library(dplyr)})
S2D <- Sys.getenv("S2_DIR", "calibration/step2/runs")
x <- nc_open(file.path(S2D, "build/sa_base/LID11133_rotorua/glm_aed/output/output.nc"))
cat("outflow-related output variables:\n")
for (v in grep("out|over|seep|inflow|evap|rain", names(x$var), ignore.case = TRUE, value = TRUE)) {
  vv <- x$var[[v]]; cat(sprintf("  %-18s %-14s %s\n", v, vv$units, vv$longname)) }
t0 <- ncatt_get(x, "time", "units")$value; orig <- as.Date(strsplit(t0, " ")[[1]][3]); tm <- ncvar_get(x, "time")
dts <- if (grepl("hour", t0)) orig + floor(tm / 24) else orig + tm
NS <- ncvar_get(x, "NS"); Z <- ncvar_get(x, "z")
res <- readRDS("calibration/step2/runs/step2_baseline_aeme_v2.rds"); hyps <- AEME::get_hypsograph(res)
keep <- which(dts >= as.Date("1991-01-01") & dts <= as.Date("2005-12-31")); ny <- 15
vol <- matrix(0, nrow(Z), ncol(Z))
for (i in seq_len(ncol(Z))) { n <- NS[i]; h <- Z[seq_len(n), i]; th <- diff(c(0, h)); dep <- (h - th / 2) - h[n]
  vol[seq_len(n), i] <- th * approx(hyps$depth, hyps$area, xout = dep, rule = 2)$y }
M <- function(v) tryCatch(ncvar_get(x, v), error = function(e) NULL)
integ <- function(v) { m <- M(v); if (is.null(m)) return(NULL); colSums(m * vol, na.rm = TRUE) }
sa <- M("surface_area"); ov <- M("overflow_vol")                               # m3/d
yr <- function(r, mw) mean(r[keep]) * mw * 365.25 * 1e-9                       # mmol/d -> t/yr (mw in g/mol)
conc_out <- function(v) { m <- M(v); sapply(keep, function(i) m[NS[i], i]) }           # surface layer, mmol/m3

inf <- targets::tar_read(rotorua_inflow_list_matrix)[[1]]; inf$Rainfall <- NULL
I <- bind_rows(inf, .id = "stream") |> filter(Date >= as.Date("1991-01-01"), Date <= as.Date("2005-12-31"))
vsum <- function(cols) sum(rowSums(I[, cols, drop = FALSE]) * I$HYD_flow) / 1e6 / ny

# ---------------- nitrogen ----------------
cat("\n=== NITROGEN (t N/yr) ===\n")
tn <- integ("TOT_tn"); cat("lake TN stock start/end (t):", round(tn[keep[1]] * 14.0067e-9), round(tn[tail(keep, 1)] * 14.0067e-9), "\n")
N <- c(inflow = vsum(c("NIT_amm", "NIT_nit", "NIT_don", "NIT_pon")),
       sediment_amm_flux = mean(M("SDF_Fsed_amm")[keep]) * mean(sa[keep]) * 14.0067e-9 * 365.25,
       outflow_overflow = mean(ov[keep] * conc_out("TOT_tn")) * 14.0067e-9 * 365.25,
       denitrification = yr(integ("NIT_denit"), 14.0067), pon_settling_if_N = -yr(integ("OGM_pon_set"), 14.0067),
       phyto_settling_N = -sum(sapply(c("PHY_green_set_n", "PHY_cyano_set_n", "PHY_diatom_set_n"), function(v) { m <- integ(v); if (is.null(m)) 0 else yr(m, 14.0067) })))
print(round(N, 1))
# ---------------- phosphorus ----------------
cat("\n=== PHOSPHORUS (t P/yr) ===\n")
tp <- integ("TOT_tp"); if (!is.null(tp)) cat("lake TP stock start/end (t):", round(tp[keep[1]] * 30.974e-9), round(tp[tail(keep, 1)] * 30.974e-9), "\n")
P <- c(inflow = vsum(c("PHS_frp", "PHS_dop", "PHS_pop", "PHS_pip")),
       sediment_frp_flux = mean(M("SDF_Fsed_frp")[keep]) * mean(sa[keep]) * 30.974e-9 * 365.25)
if (!is.null(M("TOT_tp"))) P["outflow_overflow"] <- mean(ov[keep] * conc_out("TOT_tp")) * 30.974e-9 * 365.25
for (v in c("OGM_pop_set", "PHS_pip_set", "PHS_frp_ads", "PHS_pip", "PHY_green_set_p", "PHY_cyano_set_p", "OGM_pop_hyd", "OGM_dop_min")) {
  m <- integ(v); if (!is.null(m)) P[v] <- yr(m, 30.974) }
print(round(P, 1))
cat("\nP-related variables present:\n"); print(grep("PHS|pop|dop|pip|ads", names(x$var), value = TRUE, ignore.case = TRUE))
cat("\nsettling velocities in aed.nml:\n"); f <- list.files(file.path(S2D, "build/sa_base"), "aed.nml", recursive = TRUE, full.names = TRUE)[1]
print(grep("w_pom|w_po4ads|settling|Fsed|fsed|w_p ", readLines(f), value = TRUE))
