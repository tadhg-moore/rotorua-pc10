# Evaluation for the Morris sensitivity analysis on the latest setup (joint-stage best member, V1 inflows, flood x 0.5 base, alum on, 2005-2022).
# Sourced by calibration/sa/sa_latest.R (main process and workers) after calibration/step2/step2_common.R (with the joint-stage environment).
# One model run -> named numeric vector of
#   cal_*, val_*  : the 27 scaled residuals of the joint-stage objective in the calibration (2006-06 to 2018-06) and held-out (2018-07 to 2022-06) windows, and phi_cal, phi_val (mean square)
#   sig_*, st_*   : CSPS process / system signatures and state errors (csps_metrics.R) over the whole record, on paired obs/sim days
#   x_*           : physical-unit summaries of the daily model output (all days, 2006-07 to 2022-06): means, seasonal means, anoxic days, stratification, production, alum pools, year-to-year TP
MP_ <- 30.974 / 1000; MN_ <- 14.007 / 1000; OX_ <- 32 / 1000

sa_daily <- function(res) { f <- AEME::get_model_outfile(res)$glm_aed[["output"]]; nc <- ncdf4::nc_open(f); on.exit(ncdf4::nc_close(nc))
  z <- ncdf4::ncvar_get(nc, "z"); ns <- ncdf4::ncvar_get(nc, "NS"); tm <- ncdf4::ncvar_get(nc, "time"); t0 <- ncdf4::ncatt_get(nc, "time", "units")$value; orig <- as.Date(strsplit(t0, " ")[[1]][3]); dd <- if (grepl("hour", t0)) orig + floor(tm / 24) else orig + tm
  nt <- length(ns); nl <- nrow(z); valid <- row(z) <= matrix(ns, nl, nt, byrow = TRUE); zv <- ifelse(valid, z, NA)
  zmax <- apply(zv, 2, max, na.rm = TRUE); zmin <- apply(zv, 2, min, na.rm = TRUE); ms <- valid & z >= matrix(zmax - 5, nl, nt, byrow = TRUE); mb <- valid & z <= matrix(zmin + 3, nl, nt, byrow = TRUE)
  keep <- c("temp", "OXY_oxy", "PHS_frp", "NIT_amm", "NIT_nit", "TOT_tp", "TOT_tn", "PHY_tchla", "PHY_cyano", "PHY_green", "PHY_diatom", "PHY_gpp", "extc", "TOT_tss", "ALU_ala", "ALU_alp")
  out <- data.frame(Date = dd)
  for (v in intersect(keep, names(nc$var))) { a <- tryCatch(ncdf4::ncvar_get(nc, v), error = function(e) NULL); if (is.null(a) || !is.matrix(a) || nrow(a) != nl || ncol(a) != nt) next
    a[!is.finite(a) | abs(a) > 1e20] <- NA; ok <- function(m) m & !is.na(a)
    out[[paste0(v, "_surf")]] <- colSums(ifelse(ok(ms), a, 0)) / pmax(colSums(ok(ms)), 1); out[[paste0(v, "_bot")]] <- colSums(ifelse(ok(mb), a, 0)) / pmax(colSums(ok(mb)), 1) }
  out }

sa_extras <- function(D) {
  D <- D[D$Date >= as.Date("2006-07-01") & D$Date <= as.Date("2022-06-29"), ]
  if (nrow(D) < 365) return(numeric())
  mo <- as.integer(format(D$Date, "%m")); yc <- as.integer(format(D$Date, "%Y")); wy <- yc + (mo >= 7); summer <- mo %in% c(12, 1:4)
  g <- function(n, f = 1, add = 0) if (n %in% names(D)) D[[n]] * f + add else rep(NA_real_, nrow(D))
  tp <- g("TOT_tp_surf", MP_, DOPR_GM3); frp <- g("PHS_frp_surf", MP_); tn <- g("TOT_tn_surf", MN_, DONR_GM3); nh4 <- g("NIT_amm_surf", MN_); no3 <- g("NIT_nit_surf", MN_)
  chl <- g("PHY_tchla_surf"); oxb <- g("OXY_oxy_bot", OX_); ts <- g("temp_surf"); tb <- g("temp_bot")
  cy <- g("PHY_cyano_surf"); ph <- cy + g("PHY_green_surf") + g("PHY_diatom_surf")
  m <- function(x, i = TRUE) suppressWarnings(mean(x[i], na.rm = TRUE))
  ann <- function(x) tapply(x, yc, mean, na.rm = TRUE); atp <- ann(tp); wet <- as.integer(names(atp)) %in% c(2011, 2012, 2017, 2018)
  yrs <- as.integer(names(atp)); ok <- yrs >= 2007 & yrs <= 2021
  ex <- c(x_tp_mean = m(tp), x_frp_mean = m(frp), x_tn_mean = m(tn), x_nh4_mean = m(nh4), x_no3_mean = m(no3), x_chl_mean = m(chl),
          x_chl_summer = m(chl, summer), x_chl_p90 = unname(quantile(chl, 0.9, na.rm = TRUE)), x_chl_gt10 = m(chl > 10), x_chl_gt20 = m(chl > 20),
          x_cyano_frac = m(cy) / m(ph), x_frp_summer = m(frp, summer), x_tp_summer = m(tp, summer), x_nh4_bot_summer = m(g("NIT_amm_bot", MN_), summer), x_frp_bot_summer = m(g("PHS_frp_bot", MP_), summer),
          x_kd_mean = m(g("extc_surf")), x_tss_mean = m(g("TOT_tss_surf")), x_temp_surf = m(ts), x_temp_bot = m(tb), x_dT_mean = m(ts - tb), x_f_strat = m(ts - tb > 1), x_f_strat_summer = m((ts - tb > 1), summer),
          x_oxy_bot_mean = m(oxb), x_oxy_bot_min = suppressWarnings(min(oxb, na.rm = TRUE)), x_anox_days = sum(oxb < 2, na.rm = TRUE) / length(unique(wy)),
          x_gpp_mean = m(g("PHY_gpp_surf")), x_ala_mean = m(g("ALU_ala_surf")), x_alp_mean = m(g("ALU_alp_surf")),
          x_tp_annual_sd = sd(atp[ok]), x_tp_wet_minus_dry = if (any(wet & ok) && any(!wet & ok)) mean(atp[wet & ok]) - mean(atp[!wet & ok]) else NA_real_,
          x_tp_trend = if (sum(ok) >= 5) unname(coef(lm(atp[ok] ~ yrs[ok]))[2]) else NA_real_,
          x_frp_early = m(frp, yc <= 2010), x_frp_late = m(frp, yc >= 2015), x_tp_early = m(tp, yc <= 2010), x_tp_late = m(tp, yc >= 2015), x_chl_early = m(chl, yc <= 2010), x_chl_late = m(chl, yc >= 2015))
  ex[!is.finite(ex)] <- NA_real_; ex }

sa_latest_eval <- function(aeme, path, param) {
  res <- tryCatch(aemetools::run_aeme_param(aeme, param = param[, PCOLS], model = MODEL, path = path, return_aeme = TRUE), error = function(e) { message("run_aeme_param error: ", conditionMessage(e)); NULL })
  if (is.null(res) || identical(res, 999) || !inherits(res, "Aeme")) return(NULL)
  Pall <- csps_pair(res); P <- Pall[intersect(names(Pall), c("PHS_tp", "PHS_frp", "PHY_tchla", "NIT_tn", "HYD_temp", "CHM_oxy"))]
  if (is.null(P$PHS_tp) && is.null(P$PHS_frp)) return(NULL)
  Al_sim <- al_sim_from_nc(res); end <- as.Date("2022-06-29")
  cal <- step3_resid(P, Al_sim, DOSE_START, CAL_END3); val <- step3_resid(P, Al_sim, CAL_END3 + 1, end)
  out <- c(setNames(cal, paste0("cal_", names(cal))), setNames(val, paste0("val_", names(val))), phi_cal = mean(cal^2), phi_val = mean(val^2))
  sig <- tryCatch(csps_signatures(Pall, "sim"), error = function(e) numeric()); if (length(sig)) out <- c(out, setNames(sig, paste0("sig_", names(sig))))
  st <- tryCatch(csps_state(Pall), error = function(e) data.frame())
  if (nrow(st)) for (i in seq_len(nrow(st))) { k <- paste0("st_", st$variable[i], "_", gsub("[^A-Za-z]+", "", st$region[i])); out[paste0(k, "_bias")] <- st$bias[i]; out[paste0(k, "_rmse")] <- st$RMSE[i] }
  D <- tryCatch(sa_daily(res), error = function(e) { message("daily failed: ", conditionMessage(e)); NULL })
  if (!is.null(D)) out <- c(out, sa_extras(D))
  out
}
