# Alum-stage objective (copied from calibration/step3/step3_common.R so it can be used inside the step 2 framework without re-sourcing sa_common.R).
# FRP terms are tightened for the alum-on calibration to FRP (S2_STAGE=alum).
DOSE_START <- as.Date("2006-06-01"); CAL_END3 <- as.Date(Sys.getenv("S2_CALEND3", "2018-06-30"))
AL_OBS <- readRDS("calibration/step3/al_obs.rds")          # mg/m3 aluminium in the lake water, 2011-2015
AL_MG_PER_MMOL <- 26.98                                    # ALU_ala is mmol Al/m3
DONR_GM3 <- 9 * 14.0067 / 1000; DOPR_GM3 <- 0.15 * 30.974 / 1000   # refractory DON / DOP missing from TN / TP totals
STEP3_NAMES <- c("TP_bias", "TP_rmse", "TP_annual_rmse", "TP_slope", "FRP_bias", "FRP_rmse", "FRP_bottom_summer_bias",
                 "chl_bias", "chl_summer_bias", "TN_bias", "Al_rmse", "temp_bias", "temp_rmse", "oxy_bias", "oxy_bottom_bias")
Q_EXTRA <- identical(Sys.getenv("S2_STAGE"), "q") || identical(Sys.getenv("S2_TPTERMS"), "1")            # particulate-P stage: annual TP terms for the wet years (inflow volume > 580 hm3/yr: 2011, 2012, 2017, 2018)
if (Q_EXTRA) STEP3_NAMES <- c(STEP3_NAMES, "TP_wet_bias", "TP_year_r")
WET_YR <- c(2011, 2012, 2017, 2018)
# Joint stage (S2_JOINT=1): adds bottom-oxygen / anoxia terms (as the oxygen stage), annual total-N terms and the thermocline depth (from the temperature profiles) to the alum-stage objective
JOINT <- identical(Sys.getenv("S2_JOINT"), "1")
if (JOINT) { Q_EXTRA <- TRUE; STEP3_NAMES <- unique(c(STEP3_NAMES, "TP_wet_bias", "TP_year_r", "do_bias", "do_rmse", "anox_bias", "anox_rmse", "domin_bias", "doslope_bias", "TN_annual_bias", "TN_year_r", "thermo_bias", "thermo_rmse")) }
thermo_depth <- function(dep, tmp) { o <- order(dep); dep <- dep[o]; tmp <- tmp[o]; if (length(dep) < 5 || diff(range(tmp)) < 3) return(NA_real_)      # stratified profiles only (top-bottom difference >= 3 C)
  g <- abs(diff(tmp) / diff(dep)); i <- which.max(g); (dep[i] + dep[i + 1]) / 2 }
PENALTY3 <- 5; MIN_PAIRS3 <- 8
lr3 <- function(d, fl) log(d$sim + fl) - log(d$obs + fl)

step3_resid <- function(P, Al_sim, lo, hi) {
  y <- setNames(rep(0, length(STEP3_NAMES)), STEP3_NAMES)
  sub <- function(d) if (is.null(d)) NULL else d[d$Date >= lo & d$Date <= hi, ]
  tp <- sub(P$PHS_tp); frp <- sub(P$PHS_frp); chl <- sub(P$PHY_tchla); tn <- sub(P$NIT_tn)
  if (!is.null(tp)) tp$sim <- tp$sim + DOPR_GM3
  if (!is.null(tn)) tn$sim <- tn$sim + DONR_GM3
  s <- function(d) d[d$depth <= 5, ]
  if (!is.null(tp) && nrow(s(tp)) >= MIN_PAIRS3) { d <- s(tp); e <- lr3(d, 0.002)
    y["TP_bias"] <- mean(e) / 0.15; y["TP_rmse"] <- sqrt(mean(e^2)) / 0.3
    a <- aggregate(cbind(obs, sim) ~ yr, d, mean); cnt <- table(d$yr); a <- a[cnt[as.character(a$yr)] >= 6, ]
    if (nrow(a) >= 3) y["TP_annual_rmse"] <- sqrt(mean((a$sim - a$obs)^2)) / 0.01
    if (nrow(a) >= 4) y["TP_slope"] <- (coef(lm(sim ~ yr, a))[2] - coef(lm(obs ~ yr, a))[2]) / 0.002
    if (Q_EXTRA) { w <- a[a$yr %in% WET_YR, ]; if (nrow(w) >= 1) y["TP_wet_bias"] <- mean(w$sim - w$obs) / 0.005
      if (nrow(a) >= 5) y["TP_year_r"] <- (1 - suppressWarnings(cor(a$sim, a$obs))) / 0.5 } }
  if (!is.null(frp) && nrow(s(frp)) >= MIN_PAIRS3) { d <- s(frp); e <- lr3(d, 0.002)
    y["FRP_bias"] <- mean(e) / 0.3; y["FRP_rmse"] <- sqrt(mean(e^2)) / 0.7
    b <- frp[frp$depth >= 17 & frp$mo %in% c(12, 1:4), ]
    if (nrow(b) >= 5) y["FRP_bottom_summer_bias"] <- mean(lr3(b, 0.002)) / 0.5 }
  if (!is.null(chl) && nrow(s(chl)) >= MIN_PAIRS3) { d <- s(chl); y["chl_bias"] <- mean(lr3(d, 1)) / 0.5
    m <- d[d$mo %in% c(12, 1:4), ]; if (nrow(m) >= 4) y["chl_summer_bias"] <- mean(lr3(m, 1)) / 0.5 }
  if (!is.null(tn) && nrow(s(tn)) >= MIN_PAIRS3) y["TN_bias"] <- mean(lr3(s(tn), 0.02)) / 0.3
  if (JOINT) {
    if (!is.null(tn) && nrow(s(tn)) >= MIN_PAIRS3) { d <- s(tn); a <- aggregate(cbind(obs, sim) ~ yr, d, mean); cnt <- table(d$yr); a <- a[cnt[as.character(a$yr)] >= 6, ]
      if (nrow(a) >= 3) y["TN_annual_bias"] <- mean(log((a$sim + 0.02) / (a$obs + 0.02))) / 0.15
      if (nrow(a) >= 5) y["TN_year_r"] <- (1 - suppressWarnings(cor(a$sim, a$obs))) / 0.5 }
    ox <- sub(P$CHM_oxy); if (!is.null(ox)) ox <- ox[ox$depth >= 17, ]                     # bottom oxygen / anoxia: daily mean of >= 17 m, observed days only
    if (!is.null(ox) && nrow(ox) >= 20) { B <- aggregate(cbind(obs, sim) ~ Date, ox, mean); B$wy <- as.integer(format(B$Date, "%Y")) + as.integer(as.integer(format(B$Date, "%m")) >= 7)
      y["do_bias"] <- mean(B$sim - B$obs) / 0.5; y["do_rmse"] <- sqrt(mean((B$sim - B$obs)^2)) / 1.0
      W <- do.call(rbind, lapply(split(B, B$wy), function(x) { if (nrow(x) < 60) return(NULL); w <- x$wy[1]
        sl <- function(col) { z <- x[x$Date >= as.Date(paste0(w - 1, "-10-15")) & x$Date <= as.Date(paste0(w, "-02-15")), ]; if (nrow(z) < 30) NA_real_ else unname(coef(lm(z[[col]] ~ as.numeric(z$Date)))[2]) }
        data.frame(oa = sum(x$obs < 2), sa = sum(x$sim < 2), om = min(x$obs), sm = min(x$sim), os = sl("obs"), ss = sl("sim")) }))
      if (!is.null(W) && nrow(W) >= 2) { e <- log1p(W$sa) - log1p(W$oa); y["anox_bias"] <- mean(e) / 0.7; y["anox_rmse"] <- sqrt(mean(e^2)) / 1.0
        y["domin_bias"] <- mean(W$sm - W$om) / 1.0; y["doslope_bias"] <- if (sum(!is.na(W$os) & !is.na(W$ss)) >= 2) mean(W$ss - W$os, na.rm = TRUE) / 0.02 else 0 } }
    tt <- sub(P$HYD_temp)                                                                     # thermocline depth of the stratified profiles (largest gradient), observed vs simulated at the observed depths
    if (!is.null(tt) && nrow(tt) >= 100) { td <- do.call(rbind, lapply(split(tt, tt$Date), function(x) data.frame(o = thermo_depth(x$depth, x$obs), s = thermo_depth(x$depth, x$sim)))); td <- td[is.finite(td$o) & is.finite(td$s), ]
      if (nrow(td) >= 10) { y["thermo_bias"] <- mean(td$s - td$o) / 3; y["thermo_rmse"] <- sqrt(mean((td$s - td$o)^2)) / 5 } } }
  # aluminium in the water: only the variation matters (the observations include natural background Al), so compare
  # the observed values with the simulated ones after removing the mean offset
  if (!is.null(Al_sim)) { m <- merge(AL_OBS[AL_OBS$Date >= lo & AL_OBS$Date <= hi, ], Al_sim, by = "Date")
    if (nrow(m) >= 8) { off <- mean(m$al - m$sim); y["Al_rmse"] <- sqrt(mean((m$sim + off - m$al)^2)) / 5 } }
  g <- function(v, bs, nm) { d <- sub(P[[v]]); if (is.null(d) || nrow(d) < 50) return(invisible())
    y[paste0(nm, "_bias")] <<- mean(d$sim - d$obs) / bs; if (nm == "temp") y["temp_rmse"] <<- sqrt(mean((d$sim - d$obs)^2)) / 1.0
    if (nm == "oxy") { b <- d[d$depth >= 17, ]; if (nrow(b) >= 20) y["oxy_bottom_bias"] <<- mean(b$sim - b$obs) / 1 } }
  g("HYD_temp", 0.5, "temp"); g("CHM_oxy", 1.0, "oxy")
  y[!is.finite(y)] <- PENALTY3
  y
}

# AEME's get_var() does not know the alum variables, so read ALU_ala (mmol Al/m3 when the dose ratio is 1) from the GLM
# output file: daily mean over the top 5 m
al_sim_from_nc <- function(res) tryCatch({
  f <- AEME::get_model_outfile(res)$glm_aed[["output"]]; nc <- ncdf4::nc_open(f); on.exit(ncdf4::nc_close(nc))
  a <- ncdf4::ncvar_get(nc, "ALU_ala"); z <- ncdf4::ncvar_get(nc, "z"); ns <- ncdf4::ncvar_get(nc, "NS"); tm <- ncdf4::ncvar_get(nc, "time")
  t0 <- ncdf4::ncatt_get(nc, "time", "units")$value; orig <- as.Date(strsplit(t0, " ")[[1]][3])
  d <- if (grepl("hour", t0)) orig + floor(tm / 24) else orig + tm
  v <- vapply(seq_along(ns), function(i) { h <- z[seq_len(ns[i]), i]; mean(a[seq_len(ns[i]), i][h >= max(h) - 5], na.rm = TRUE) }, numeric(1))
  data.frame(Date = d, sim = v * AL_MG_PER_MMOL / as.numeric(Sys.getenv("S2_BETA", "1")))   # S2_BETA: mol P bound per mol Al (capacity = beta x Al dose, see calibration/alum/63_stoich_base.R)
}, error = function(e) NULL)

# one model run -> list(cal, val) residual vectors; NULL on failure
step3_eval <- function(aeme, path, param) {
  res <- tryCatch(aemetools::run_aeme_param(aeme, param = param[, PCOLS], model = MODEL, path = path, return_aeme = TRUE),
                  error = function(e) NULL)
  if (is.null(res) || identical(res, 999) || !inherits(res, "Aeme")) return(NULL)
  P <- csps_pair(res, vars = c("PHS_tp", "PHS_frp", "PHY_tchla", "NIT_tn", "HYD_temp", "CHM_oxy"))
  if (is.null(P$PHS_tp) && is.null(P$PHS_frp)) return(NULL)
  Al_sim <- al_sim_from_nc(res)
  end <- as.Date("2022-06-29")
  list(cal = step3_resid(P, Al_sim, DOSE_START, CAL_END3), val = step3_resid(P, Al_sim, CAL_END3 + 1, end))
}
