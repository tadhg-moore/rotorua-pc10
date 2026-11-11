# Observation-based targets ("signatures") for the Lake Rotorua model.
#
# The same function computes each signature from observations or from model
# output, so a model run can be scored against the observed values:
#
#   obs <- wq_target_signatures(prof_daily, thermal_daily, chem_long, hod, hflux)
#   sim <- wq_target_signatures(<same inputs built from model output>)
#   score_wq_targets(obs, sim)
#
# Inputs (identical layout for observations and model):
#   prof_daily    date, t_surf, t_bot, do_bot          daily surface (<= 1.5 m) and
#                                                      bottom (>= 18 m) temperature and DO
#   thermal_daily date, d_temp, thermo_depth           from thermal_structure()
#   chem_long     site, layer (INT/HYP/BOT), date,     discrete chemistry, mg/m3
#                 variable (tp, tn, drp, nh4, nnn,     (model: sample the output at the
#                 chla, secchi), value                 observed dates and depths)
#   hod           oxygen_depletion() output            (optional)
#   hflux         hypolimnetic_flux() output           (optional)
#
# Tolerances are the year-to-year standard deviation of the signature (or a
# robust spread, IQR/1.35, for skewed and event-based ones; on a log scale for
# bottom maxima, AHOD and accumulation fluxes), with a floor, so a
# model within one tolerance is within the lake's own interannual variability.

wq_target_definitions <- function() {
  tibble::tribble(
    ~metric, ~priority, ~theme, ~label, ~measure, ~period,
    # --- Priority 1: physics, oxygen and the alum-era phosphorus response
    "strat_frac_djf", 1, "Stratification", "Share of summer days stratified",
      "Share of Dec–Feb days with surface − bottom ≥ 1 °C, median of years", "2007–2026",
    "strat_longest_spell", 1, "Stratification", "Longest stratified spell",
      "Longest run of stratified days (Nov–Mar), median of years (d)", "2007–2026",
    "thermo_depth_djf", 1, "Stratification", "Summer thermocline depth",
      "Median thermocline depth on stratified Dec–Feb days, median of years (m)", "2007–2026",
    "t_surf_djf", 1, "Temperature", "Summer surface temperature",
      "Mean Dec–Feb surface (≤ 1.5 m) temperature, median of years (°C)", "2007–2026",
    "anox_frac_summer", 1, "Oxygen", "Share of summer days anoxic",
      "Share of Nov–Mar days with bottom DO < 2 g/m³, median of years", "2007–2026",
    "do_bot_min", 1, "Oxygen", "Annual minimum bottom DO",
      "Minimum daily bottom DO in each year, median of years (g/m³)", "2007–2026",
    "tp_int_prealum", 1, "Phosphorus", "Surface TP before alum",
      "Mean of annual INT TP means, both sites (mg/m³)", "2001–2006",
    "tp_int_alum", 1, "Phosphorus", "Surface TP under alum",
      "Mean of annual INT TP means (mg/m³)", "2012–2020",
    "tp_alum_ratio", 1, "Phosphorus", "Alum response of TP",
      "tp_int_alum / tp_int_prealum", "2001–2020",
    "tp_pause_response", 1, "Phosphorus", "TP response to the Puarenga dosing pause",
      "Mean INT TP 2021–2022 minus mean 2017–2020 (mg/m³)", "2017–2022",
    "chl_int_alum", 1, "Phytoplankton", "Surface chl-a under alum",
      "Mean of annual INT chl-a means (mg/m³)", "2012–2020",
    "tli3_alum", 1, "Trophic state", "TLI3 under alum",
      "Mean annual lake TLI3 (Burns), INT samples", "2012–2020",
    # --- Priority 2: internal loading, nitrogen and seasonality
    "tn_int_alum", 2, "Nitrogen", "Surface TN under alum",
      "Mean of annual INT TN means (mg/m³)", "2012–2020",
    "tn_int_recent", 2, "Nitrogen", "Surface TN, recent",
      "Mean of annual INT TN means (mg/m³)", "2023–2026",
    "bot_nh4_max", 2, "Internal loading", "Summer bottom NH₄ maximum",
      "Annual maximum BOT NH₄ (mean of sites), median of years (mg/m³)", "2007–2026",
    "bot_drp_max", 2, "Internal loading", "Summer bottom DRP maximum",
      "Annual maximum BOT DRP (mean of sites), median of years (mg/m³)", "2007–2026",
    "excess_nh4_jf", 2, "Internal loading", "Bottom NH₄ excess in Jan–Feb",
      "Median Jan–Feb BOT − INT NH₄ on paired dates (mg/m³)", "2007–2026",
    "excess_drp_jf", 2, "Internal loading", "Bottom DRP excess in Jan–Feb",
      "Median Jan–Feb BOT − INT DRP on paired dates (mg/m³)", "2007–2026",
    "do_decline_rate", 2, "Internal loading", "Bottom DO decline when stratified",
      "Median slope of bottom DO in stratified periods (g/m³/d)", "2007–2026",
    "ahod", 2, "Internal loading", "Areal hypolimnetic O₂ demand",
      "Median AHOD of stratified periods (g O₂/m²/d)", "2007–2026",
    "hyp_flux_nh4", 2, "Internal loading", "Hypolimnetic NH₄ accumulation",
      "Median positive accumulation flux (mg N/m²/d; obs is a lower bound)", "2003–2026",
    "hyp_flux_drp", 2, "Internal loading", "Hypolimnetic DRP accumulation",
      "Median positive accumulation flux (mg P/m²/d; obs is a lower bound)", "2003–2026",
    "month_max_tp", 2, "Seasonality", "Timing of the surface TP peak",
      "Month of maximum monthly-median INT TP (1 = Jan)", "2007–2026",
    "month_max_nh4", 2, "Seasonality", "Timing of the surface NH₄ peak",
      "Month of maximum monthly-median INT NH₄", "2007–2026",
    "month_max_nnn", 2, "Seasonality", "Timing of the NOx peak",
      "Month of maximum monthly-median INT NOx", "2007–2026",
    # --- Priority 3: secondary patterns
    "nnn_winter", 3, "Nitrogen", "Winter NOx",
      "Median Jul–Oct INT NOx (mg/m³)", "2016–2026",
    "tn_tp_alum", 3, "Stoichiometry", "TN:TP ratio",
      "Median INT TN:TP (molar)", "2012–2020",
    "secchi_alum", 3, "Clarity", "Secchi depth",
      "Mean of annual mean Secchi depths (m)", "2012–2020",
    "chl_trend_recent", 3, "Phytoplankton", "Recent chl-a trend",
      "Seasonal Sen slope of INT chl-a (% of median per year)", "2016–2026",
    "tp_trend_recent", 3, "Phosphorus", "Recent TP trend",
      "Seasonal Sen slope of INT TP (% of median per year)", "2016–2026"
  )
}

#' Paired time-series skill measures that need simulated values at observed
#' times (not signatures). Listed in the target table with acceptance levels.
wq_pointwise_definitions <- function() {
  tibble::tribble(
    ~metric, ~priority, ~theme, ~label, ~measure, ~acceptance,
    "skill_t_surf", 1, "Temperature", "Daily surface temperature (buoy)", "RMSE and bias", "RMSE ≤ 1.0 °C, |bias| ≤ 0.5 °C",
    "skill_t_bot", 1, "Temperature", "Daily bottom temperature (buoy)", "RMSE and bias", "RMSE ≤ 1.5 °C",
    "skill_do_bot", 1, "Oxygen", "Daily bottom DO (buoy)", "RMSE; hit rate of anoxic days (DO < 2)", "RMSE ≤ 2 g/m³; hit rate ≥ 0.5",
    "skill_tp_int", 1, "Phosphorus", "Monthly INT TP", "KGE; Spearman ρ of annual means", "KGE ≥ 0.3; ρ ≥ 0.5",
    "skill_chl_int", 2, "Phytoplankton", "Monthly INT chl-a", "KGE on log values", "KGE ≥ 0.2",
    "skill_tn_int", 2, "Nitrogen", "Monthly INT TN", "KGE", "KGE ≥ 0.3",
    "skill_bot_nh4", 2, "Internal loading", "BOT NH₄ and DRP", "Spearman ρ of annual maxima", "ρ ≥ 0.4"
  )
}

# value + tolerance from a vector of yearly values
.yr_stat <- function(x, fun = stats::median) {
  x <- x[is.finite(x)]
  if (length(x) < 2) return(c(value = NA_real_, tol = NA_real_, n = length(x)))
  v <- fun(x)
  c(value = v, tol = max(stats::sd(x), 0.1 * abs(v), 1e-6), n = length(x))
}
.spread_stat <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 3) return(c(value = NA_real_, tol = NA_real_, n = length(x)))
  v <- stats::median(x)
  c(value = v, tol = max(stats::IQR(x) / 1.35, 0.1 * abs(v), 0.05), n = length(x))
}

# log-scale value + tolerance (tolerance is a log-ratio; z = log(sim/obs) / tol)
.log_stat <- function(x) {
  x <- x[is.finite(x) & x > 0]
  if (length(x) < 3) return(c(value = NA_real_, tol = NA_real_, n = length(x)))
  c(value = stats::median(x), tol = max(stats::IQR(log(x)) / 1.35, log(1.25)), n = length(x))
}
log_scale_metrics <- c("bot_nh4_max", "bot_drp_max", "hyp_flux_nh4", "hyp_flux_drp", "ahod")

#' Compute all signatures (see wq_target_definitions()) from one set of inputs
wq_target_signatures <- function(prof_daily, thermal_daily, chem_long, hod = NULL, hflux = NULL) {
  out <- list()
  put <- function(m, s) out[[m]] <<- s
  mon <- function(d) as.integer(format(d, "%m"))
  hy <- wq_hy

  # ---- profiles
  pd <- prof_daily |>
    dplyr::left_join(thermal_daily |> dplyr::select(date, d_temp_th = d_temp, thermo_depth), by = "date") |>
    dplyr::mutate(d_temp = dplyr::coalesce(t_surf - t_bot, d_temp_th), hy = hy(date), m = mon(date))
  djf <- pd |> dplyr::filter(m %in% c(12, 1, 2))
  sumr <- pd |> dplyr::filter(m %in% c(11, 12, 1, 2, 3))
  by_hy <- function(d, f, min_n = 45) {
    sp <- split(d, d$hy); sp <- sp[vapply(sp, nrow, 1L) >= min_n]
    vapply(sp, \(x) suppressWarnings(as.numeric(f(x))), numeric(1))
  }
  put("strat_frac_djf", .yr_stat(by_hy(djf |> dplyr::filter(is.finite(d_temp)), \(d) mean(d$d_temp >= 1))))
  put("strat_longest_spell", .yr_stat(by_hy(sumr |> dplyr::filter(is.finite(d_temp)), \(d) {
    e <- run_events(d$date, d$d_temp >= 1); if (nrow(e)) max(e$duration) else 0 }, 60)))
  put("thermo_depth_djf", .yr_stat(by_hy(djf, \(d) stats::median(d$thermo_depth[d$d_temp >= 1], na.rm = TRUE), 30)))
  put("t_surf_djf", .yr_stat(by_hy(djf |> dplyr::filter(is.finite(t_surf)), \(d) mean(d$t_surf))))
  put("anox_frac_summer", .spread_stat(by_hy(sumr |> dplyr::filter(is.finite(do_bot)), \(d) mean(d$do_bot < 2), 60)))
  put("do_bot_min", .spread_stat(by_hy(pd |> dplyr::filter(is.finite(do_bot)), \(d) min(d$do_bot), 200)))

  # ---- chemistry (annual means of INT, both sites pooled per year)
  ch <- chem_long |> dplyr::mutate(hy = hy(date), m = mon(date))
  ann_int <- function(v) ch |> dplyr::filter(layer == "INT", variable == v) |>
    dplyr::group_by(hy) |> dplyr::filter(dplyr::n_distinct(m) >= 6) |>
    dplyr::summarise(x = mean(value), .groups = "drop")
  per_mean <- function(a, hys) { x <- a$x[a$hy %in% hys]; .yr_stat(x, mean) }
  tp <- ann_int("tp"); tn <- ann_int("tn"); chl <- ann_int("chla"); sec <- ann_int("secchi")
  s_pre <- per_mean(tp, 2001:2006); s_alum <- per_mean(tp, 2012:2020)
  put("tp_int_prealum", s_pre); put("tp_int_alum", s_alum)
  r <- s_alum[["value"]] / s_pre[["value"]]
  put("tp_alum_ratio", c(value = r, tol = r * sqrt((s_alum[["tol"]] / s_alum[["value"]])^2 / max(s_alum[["n"]], 1) +
                                                      (s_pre[["tol"]] / s_pre[["value"]])^2 / max(s_pre[["n"]], 1)) * 2,
                         n = s_alum[["n"]] + s_pre[["n"]]))
  a <- tp$x[tp$hy %in% 2021:2022]; b <- tp$x[tp$hy %in% 2017:2020]
  put("tp_pause_response", c(value = mean(a) - mean(b),
                             tol = max(stats::sd(tp$x[tp$hy %in% 2012:2020]), 1), n = length(a) + length(b)))
  put("chl_int_alum", per_mean(chl, 2012:2020))
  put("tn_int_alum", per_mean(tn, 2012:2020))
  put("tn_int_recent", per_mean(tn, 2023:2026))
  put("secchi_alum", per_mean(sec, 2012:2020))
  wide <- ch |> dplyr::filter(layer == "INT", variable %in% c("chla", "tp", "tn", "secchi")) |>
    dplyr::group_by(site, layer, date, variable) |> dplyr::summarise(value = mean(value), .groups = "drop") |>
    tidyr::pivot_wider(names_from = variable, values_from = value)
  for (v in c("chla", "tp", "tn", "secchi")) if (!v %in% names(wide)) wide[[v]] <- NA_real_
  tl <- tli_annual(wide) |> dplyr::filter(site == "Lake (mean of sites)", hy %in% 2012:2020)
  put("tli3_alum", .yr_stat(tl$tli3, mean))

  # bottom maxima (mean of sites per year)
  botmax <- function(v) ch |> dplyr::filter(layer == "BOT", variable == v, hy >= 2007) |>
    dplyr::group_by(site, hy) |> dplyr::summarise(mx = max(value), .groups = "drop") |>
    dplyr::group_by(hy) |> dplyr::summarise(mx = mean(mx), .groups = "drop") |> dplyr::pull(mx)
  put("bot_nh4_max", .log_stat(botmax("nh4")))
  put("bot_drp_max", .log_stat(botmax("drp")))
  excess <- function(v) {
    e <- ch |> dplyr::filter(variable == v, layer %in% c("BOT", "INT"), m %in% 1:2, hy >= 2007) |>
      dplyr::group_by(site, date, hy, layer) |> dplyr::summarise(value = mean(value), .groups = "drop") |>
      tidyr::pivot_wider(names_from = layer, values_from = value) |>
      dplyr::filter(is.finite(BOT), is.finite(INT))
    yr <- e |> dplyr::group_by(hy) |> dplyr::summarise(x = stats::median(BOT - INT), .groups = "drop")
    s <- .spread_stat(yr$x); s[["value"]] <- stats::median(e$BOT - e$INT); s
  }
  put("excess_nh4_jf", excess("nh4")); put("excess_drp_jf", excess("drp"))

  # seasonality (month of maximum of the monthly-median climatology, alum era)
  peak_month <- function(v) {
    cl <- ch |> dplyr::filter(layer == "INT", variable == v, hy >= 2007) |>
      dplyr::group_by(m) |> dplyr::summarise(x = stats::median(value), .groups = "drop")
    c(value = cl$m[which.max(cl$x)], tol = 1, n = nrow(cl))
  }
  put("month_max_tp", peak_month("tp")); put("month_max_nh4", peak_month("nh4"))
  put("month_max_nnn", peak_month("nnn"))
  nw <- ch |> dplyr::filter(layer == "INT", variable == "nnn", m %in% 7:10, hy >= 2017) |>
    dplyr::group_by(hy) |> dplyr::summarise(x = stats::median(value), .groups = "drop")
  s <- .yr_stat(nw$x); s[["value"]] <- stats::median(ch$value[ch$layer == "INT" & ch$variable == "nnn" &
                                                             ch$m %in% 7:10 & ch$hy >= 2017])
  put("nnn_winter", s)
  rt <- ch |> dplyr::filter(layer == "INT", variable %in% c("tn", "tp"), hy %in% 2012:2020) |>
    dplyr::group_by(site, date, variable) |> dplyr::summarise(value = mean(value), .groups = "drop") |>
    tidyr::pivot_wider(names_from = variable, values_from = value) |>
    dplyr::mutate(r = (tn / 14.007) / (tp / 30.974), hy = hy(date)) |> dplyr::filter(is.finite(r))
  s <- .yr_stat(tapply(rt$r, rt$hy, stats::median)); s[["value"]] <- stats::median(rt$r)
  put("tn_tp_alum", s)
  trend <- function(v) {
    d <- ch |> dplyr::filter(layer == "INT", variable == v, date >= as.Date("2016-07-01"))
    r <- seasonal_mk(d$date, d$value)
    if (is.null(r)) c(value = NA_real_, tol = NA_real_, n = 0) else
      c(value = r$pct_per_yr, tol = max(abs(r$pct_per_yr) / max(abs(r$z), 1), 0.5), n = r$years)
  }
  put("chl_trend_recent", trend("chla")); put("tp_trend_recent", trend("tp"))

  # ---- event-based internal loading
  if (!is.null(hod) && nrow(hod)) {
    h <- hod[hod$rate > 0 & hod$r2 >= 0.5, ]
    s <- .spread_stat(-h$rate); put("do_decline_rate", s)
    put("ahod", .log_stat(h$ahod))
  }
  if (!is.null(hflux) && nrow(hflux)) {
    put("hyp_flux_nh4", .log_stat(hflux$flux[hflux$variable == "nh4" & hflux$flux > 0]))
    put("hyp_flux_drp", .log_stat(hflux$flux[hflux$variable == "drp" & hflux$flux > 0]))
  }

  vals <- dplyr::bind_rows(lapply(names(out), \(m) tibble::tibble(
    metric = m, value = unname(out[[m]]["value"]), tol = unname(out[[m]]["tol"]), n = unname(out[[m]]["n"]))))
  wq_target_definitions() |> dplyr::left_join(vals, by = "metric")
}

#' Score a model run against the observed signatures
#'
#' z = (sim - obs) / tol. |z| <= 1: within interannual variability (good),
#' 1-2: acceptable, > 2: poor. Lower-bound targets (hypolimnetic accumulation)
#' count as met when the model is at or above the observed value. The overall
#' score is the priority-weighted mean of z^2 (weights 3, 2, 1).
score_wq_targets <- function(obs, sim) {
  lower_bound <- c("hyp_flux_nh4", "hyp_flux_drp")
  obs |>
    dplyr::select(metric, priority, theme, label, obs = value, tol) |>
    dplyr::left_join(sim |> dplyr::select(metric, sim = value), by = "metric") |>
    dplyr::mutate(
      z = ifelse(metric %in% log_scale_metrics, log(pmax(sim, 1e-6) / obs) / tol, (sim - obs) / tol),
      z = ifelse(metric %in% lower_bound & z > 0, 0, z),
      z = ifelse(grepl("^month_max", metric), {
        d <- abs(sim - obs) %% 12; pmin(d, 12 - d) / tol }, z),
      rating = dplyr::case_when(is.na(z) ~ "not scored", abs(z) <= 1 ~ "good",
                                abs(z) <= 2 ~ "acceptable", TRUE ~ "poor"),
      weight = c(3, 2, 1)[priority]
    )
}

score_wq_summary <- function(sc) {
  sc |> dplyr::filter(is.finite(z)) |>
    dplyr::group_by(priority) |>
    dplyr::summarise(n = dplyr::n(), good = sum(abs(z) <= 1), acceptable = sum(abs(z) > 1 & abs(z) <= 2),
                     poor = sum(abs(z) > 2), mean_z2 = mean(z^2), .groups = "drop") |>
    dplyr::bind_rows(sc |> dplyr::filter(is.finite(z)) |>
                       dplyr::summarise(priority = NA, n = dplyr::n(), good = sum(abs(z) <= 1),
                                        acceptable = sum(abs(z) > 1 & abs(z) <= 2), poor = sum(abs(z) > 2),
                                        mean_z2 = stats::weighted.mean(z^2, weight)))
}

#' Paired skill measures (for the pointwise targets)
kge_skill <- function(obs, sim) {
  ok <- is.finite(obs) & is.finite(sim); o <- obs[ok]; s <- sim[ok]
  if (length(o) < 5) return(NA_real_)
  1 - sqrt((stats::cor(o, s) - 1)^2 + (stats::sd(s) / stats::sd(o) - 1)^2 + (mean(s) / mean(o) - 1)^2)
}
rmse_skill <- function(obs, sim) { ok <- is.finite(obs) & is.finite(sim); sqrt(mean((sim[ok] - obs[ok])^2)) }

#' Build the target inputs from model profile output
#'
#' @param sim long model output: date, var_sim, depth (m below surface), value
#'   (AED units: g/m3 for nutrients, mg/m3 chl-a), e.g. from the AEME parquet
#'   output (`output_raw`), one model and member.
#' @param obs_chem observed chemistry (wq_lake_long layout); the model is
#'   sampled on the same site/layer/date/variable combinations.
#' @param hyps hypsograph from hyps_below_surface().
#' @param from first date used (spin-up excluded).
#' @param int_max,hyp_range,bot_min depth (m) of the INT, HYP and BOT layers.
#' @return list(prof_daily, thermal_daily, chem_long, hod, hflux) ready for
#'   wq_target_signatures().
model_target_inputs <- function(sim, obs_chem, hyps, from = as.Date("2000-07-01"),
                                int_max = 8, hyp_range = c(12, 18), bot_min = 18) {
  sim <- sim[sim$date >= from, ]
  g <- function(v) sim[sim$var_sim == v, c("date", "depth", "value")]
  tmp <- g("HYD_temp"); oxy <- g("CHM_oxy")
  lay <- function(d, lo, hi) d |> dplyr::filter(depth >= lo, depth <= hi) |>
    dplyr::group_by(date) |> dplyr::summarise(v = mean(value), .groups = "drop")
  prof <- lay(tmp, 0, 1.5) |> dplyr::rename(t_surf = v) |>
    dplyr::left_join(lay(tmp, bot_min, 99) |> dplyr::rename(t_bot = v), by = "date") |>
    dplyr::left_join(lay(oxy, bot_min, 99) |> dplyr::rename(do_bot = v), by = "date")
  th <- tmp |> dplyr::transmute(source = "model", date, depth_m = depth, temp_c = value) |>
    thermal_structure(hyps, source) |> dplyr::select(date, d_temp, thermo_depth)

  # chemistry sampled like the observations (mg/m3)
  vmap <- c(tp = "PHS_tp", tn = "NIT_tn", drp = "PHS_frp", nh4 = "NIT_amm", nnn = "NIT_nit", chla = "PHY_tchla")
  mult <- c(tp = 1000, tn = 1000, drp = 1000, nh4 = 1000, nnn = 1000, chla = 1)
  rng <- list(INT = c(0, int_max), HYP = hyp_range, BOT = c(bot_min, 99))
  keys <- obs_chem |> dplyr::filter(variable %in% names(vmap), date >= from) |>
    dplyr::distinct(site, layer, date, variable)
  chem <- dplyr::bind_rows(lapply(names(vmap), function(nm) {
    d <- g(vmap[[nm]]); if (!nrow(d)) return(NULL)
    k <- mult[[nm]]
    dplyr::bind_rows(lapply(names(rng), function(L) lay(d, rng[[L]][1], rng[[L]][2]) |>
                              dplyr::transmute(date, layer = L, variable = nm, value = v * k)))
  }))
  chem_long <- keys |> dplyr::inner_join(chem, by = c("date", "layer", "variable"))

  # events and fluxes, computed exactly as for the observations
  daily <- prof |> dplyr::mutate(d_temp = t_surf - t_bot)
  strat <- run_events(daily$date, daily$d_temp > 1, min_len = 3, max_gap = 2)
  hod <- oxygen_depletion(daily, th, strat, hyps)
  ctd_like <- tidyr::crossing(site = unique(obs_chem$site), th) |>
    dplyr::left_join(prof |> dplyr::select(date, t_bot), by = "date")
  hflux <- hypolimnetic_flux(chem_long, ctd_like, hyps)
  list(prof_daily = prof, thermal_daily = th, chem_long = chem_long, hod = hod, hflux = hflux)
}
