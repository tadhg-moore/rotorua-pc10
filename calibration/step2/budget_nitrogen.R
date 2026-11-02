# Nitrogen budget of the step 2 baseline run, 1991-2005, from the GLM output (t N / yr).
#   Rscript calibration/step2/budget_nitrogen.R   (needs calibration/step2/runs from run_step2_baseline / diag_nitrogen)
suppressMessages({library(ncdf4); library(AEME); library(dplyr)})
x <- nc_open("calibration/step2/runs/build/sa_base/LID11133_rotorua/glm_aed/output/output.nc")
t0 <- ncatt_get(x, "time", "units")$value; orig <- as.Date(strsplit(t0, " ")[[1]][3])
tm <- ncvar_get(x, "time"); dts <- if (grepl("hour", t0)) orig + floor(tm / 24) else orig + tm
NS <- ncvar_get(x, "NS"); Z <- ncvar_get(x, "z")
res <- readRDS("calibration/step2/runs/step2_baseline_aeme_v2.rds"); hyps <- AEME::get_hypsograph(res)
keep <- which(dts >= as.Date("1991-01-01") & dts <= as.Date("2005-12-31")); ny <- 15
mm2t <- 14.0067e-9                                  # mmol N -> tonnes N
M <- function(v) ncvar_get(x, v)
vars <- list(denit = "NIT_denit", nitrif = "NIT_nitrif", don_min = "OGM_don_min", pon_hyd = "OGM_pon_hyd", tn = "TOT_tn")
A <- lapply(vars, M)
vol <- matrix(0, nrow(Z), ncol(Z))
for (i in seq_len(ncol(Z))) { n <- NS[i]; h <- Z[seq_len(n), i]; th <- diff(c(0, h)); dep <- (h - th / 2) - h[n]
  a <- approx(hyps$depth, hyps$area, xout = dep, rule = 2)$y; vol[seq_len(n), i] <- th * a }
lv <- M("lake_volume"); cat("layer-summed volume vs lake_volume (mean ratio):", signif(mean(colSums(vol)[keep] / lv[keep]), 3), "\n")
integ <- function(m) colSums(m * vol, na.rm = TRUE)         # mmol N / d  (rate)  or mmol N (stock)
tn_stock <- integ(A$tn)[keep] * mm2t; cat("lake TN stock (t N): start", round(tn_stock[1]), " end", round(tn_stock[length(keep)]), " mean", round(mean(tn_stock)), "\n")
cat("mean lake TN (g/m3):", signif(mean(tn_stock * 1e6 / lv[keep]), 3), "\n")
rate <- sapply(c("denit", "nitrif", "don_min", "pon_hyd"), function(k) mean(integ(A[[k]])[keep]) * mm2t * 365.25)
cat("\nwater-column rates, t N/yr:\n"); print(round(rate))
sa <- M("surface_area")
fl <- sapply(c("SDF_Fsed_amm", "SDF_Fsed_nit"), function(v) mean(M(v)[keep]) * mean(sa[keep]) * mm2t * 365.25)
cat("\nsediment-water flux (+ = into water), t N/yr:\n"); print(round(fl))
# outflow / seepage
ov <- M("tot_outflow_vol"); cat("\nmean daily outflow vol m3/d:", signif(mean(ov[keep]), 3), " seepage:", signif(mean(M("seepage_vol")[keep]), 3), " overflow:", signif(mean(M("overflow_vol")[keep]), 3), "\n")
tn_surf <- sapply(keep, function(i) A$tn[NS[i], i])
cat("outflow load approx (t N/yr):", round(mean(ov[keep] * tn_surf * 14.0067e-6 / 1000 * 1e-0) * 365.25 * 1e-3 * 1e3 / 1e3), "\n")
# inflow load
inf <- targets::tar_read(rotorua_inflow_list_matrix)[[1]]; inf$Rainfall <- NULL
I <- bind_rows(inf, .id = "stream") |> filter(Date >= as.Date("1991-01-01"), Date <= as.Date("2005-12-31"))
ld <- I |> mutate(N = (NIT_amm + NIT_nit + NIT_don + NIT_pon) * HYD_flow / 1e6) |> summarise(across(c(N), sum), vol = sum(HYD_flow))
cat("\ninflow N load t/yr:", round(ld$N / ny), "  inflow vol m3/yr:", signif(ld$vol / ny, 3), "\n")
sed <- tryCatch(M("OGM_ton_sed"), error = function(e) NULL); if (!is.null(sed)) cat("sediment TON (mmol/m2) first/last:", signif(sed[keep[1]], 3), signif(sed[tail(keep, 1)], 3), "\n")
# settling losses to the sediment (rates in mmol/m3/d, integrated over the water column)
cat("\nsettling / uptake terms, t N/yr:\n")
for (v in c("OGM_pon_set", "PHY_green_set_n", "PHY_green_gpp_n", "PHY_green_mor_n", "NIT_dnra", "NIT_anammox")) {
  m <- tryCatch(M(v), error = function(e) NULL); if (is.null(m)) next
  cat(sprintf("%-18s %s  %8.1f\n", v, ncatt_get(x, v, "units")$value, mean(integ(m)[keep]) * mm2t * 365.25)) }
cat("PON settling in C units converted at C:N 106/16 would be:", round(mean(integ(M("OGM_pon_set"))[keep]) * mm2t * 365.25 * 16 / 106), "t N/yr\n")
