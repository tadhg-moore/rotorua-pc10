# Diagnose a Kw (light extinction coefficient) or Secchi depth time series and
# recommend how to parameterise light extinction for a GLM(-AED) run: a single
# static Kw, a monthly Kw climatology, or a fully dated time-varying Kw forcing.
#
# Only base R + stats are required.

#' Recommend a light extinction parameterisation for GLM
#'
#' @param date Date vector (or coercible via `as.Date`), one per observation.
#' @param kw Numeric vector of light extinction coefficients (Kd, m^-1). Supply
#'   either `kw` or `secchi`, not both.
#' @param secchi Numeric vector of Secchi depths (m). Converted to Kd via
#'   `Kd = secchi_coef / secchi`.
#' @param secchi_coef Coefficient used to convert Secchi depth to Kd (default
#'   1.7, the commonly used Poole & Atkins mid-range value). Override if a
#'   site-specific Secchi:Kd relationship is known.
#' @param min_n Minimum number of observations required before any seasonal or
#'   interannual pattern is considered resolvable (default 12).
#' @param min_years Minimum span of distinct years required to assess
#'   interannual variability (default 3).
#' @param max_median_gap_days Maximum acceptable median sampling gap (days)
#'   for a monthly climatology to be considered resolvable (default 60).
#' @param interannual_r2_threshold Fraction of total variance explained by
#'   year, above which interannual variability is judged to dominate over any
#'   recurring seasonal cycle (default 0.30).
#' @param seasonal_ratio_threshold Ratio of max:min monthly mean (after
#'   removing each year's own mean) above which a recurring seasonal cycle is
#'   judged strong enough to justify a monthly climatology (default 1.3, i.e.
#'   a >30% swing).
#'
#' @return An object of class `light_recommendation` (a list) with the
#'   recommendation, supporting diagnostics, and (where relevant) a monthly
#'   Kw climatology table ready to use as GLM forcing.
#' @export
recommend_light_parameterisation <- function(date,
                                             kw = NULL,
                                             secchi = NULL,
                                             secchi_coef = 1.7,
                                             min_n = 12,
                                             min_years = 3,
                                             max_median_gap_days = 60,
                                             interannual_r2_threshold = 0.30,
                                             seasonal_ratio_threshold = 1.3) {
  
  if (is.null(kw) == is.null(secchi)) {
    stop("Supply exactly one of `kw` or `secchi`.")
  }
  
  date <- as.Date(date)
  
  if (!is.null(secchi)) {
    if (length(secchi) != length(date)) {
      stop("`date` and `secchi` must be the same length.")
    }
    if (any(secchi <= 0, na.rm = TRUE)) {
      stop("`secchi` must be strictly positive.")
    }
    kw <- secchi_coef / secchi
    input_type <- "secchi"
  } else {
    if (length(kw) != length(date)) {
      stop("`date` and `kw` must be the same length.")
    }
    if (any(kw <= 0, na.rm = TRUE)) {
      stop("`kw` must be strictly positive.")
    }
    input_type <- "kw"
  }
  
  ok <- stats::complete.cases(date, kw)
  n_dropped <- sum(!ok)
  date <- date[ok]
  kw <- kw[ok]
  
  ord <- order(date)
  date <- date[ord]
  kw <- kw[ord]
  
  n_obs <- length(kw)
  if (n_obs < 3) {
    stop("Need at least 3 valid observations to give a recommendation.")
  }
  
  years <- as.integer(format(date, "%Y"))
  months <- as.integer(format(date, "%m"))
  n_years <- length(unique(years))
  span_days <- as.numeric(max(date) - min(date))
  gaps <- diff(as.numeric(date))
  median_gap_days <- if (length(gaps) > 0) stats::median(gaps) else NA_real_
  max_gap_days <- if (length(gaps) > 0) max(gaps) else NA_real_
  
  mean_kw <- mean(kw)
  median_kw <- stats::median(kw)
  sd_kw <- if (n_obs > 1) stats::sd(kw) else NA_real_
  cv_kw <- if (!is.na(sd_kw)) sd_kw / mean_kw else NA_real_
  
  # Fraction of total variance explained by year (interannual regime shifts
  # vs. a recurring seasonal cycle).
  interannual_r2 <- NA_real_
  if (n_years >= 2) {
    grand_mean <- mean(kw)
    year_means <- tapply(kw, years, mean)
    ss_between <- sum((year_means[as.character(years)] - grand_mean)^2)
    ss_total <- sum((kw - grand_mean)^2)
    interannual_r2 <- if (ss_total > 0) ss_between / ss_total else NA_real_
  }
  
  # Pooled monthly climatology (raw).
  monthly <- data.frame(
    month = 1:12,
    n = as.integer(tapply(kw, factor(months, levels = 1:12), length)),
    mean = as.numeric(tapply(kw, factor(months, levels = 1:12), mean)),
    sd = as.numeric(tapply(kw, factor(months, levels = 1:12),
                           function(x) if (length(x) > 1) stats::sd(x) else NA_real_))
  )
  monthly$n[is.na(monthly$n)] <- 0L
  
  present_means <- monthly$mean[!is.na(monthly$mean)]
  seasonal_ratio_raw <- if (length(present_means) > 0) {
    max(present_means) / min(present_means)
  } else {
    NA_real_
  }
  
  # Detrended seasonal signal: remove each year's own mean before pooling by
  # month, so a real recurring seasonal cycle can be told apart from
  # interannual regime shifts / one-off bloom or turbidity events.
  seasonal_ratio_detrended <- NA_real_
  if (n_years >= 2) {
    year_means <- tapply(kw, years, mean)
    kw_detrended <- kw / year_means[as.character(years)]
    month_ratio <- tapply(kw_detrended, factor(months, levels = 1:12), mean)
    month_ratio <- month_ratio[!is.na(month_ratio)]
    if (length(month_ratio) > 0) {
      seasonal_ratio_detrended <- max(month_ratio) / min(month_ratio)
    }
  }
  
  flags <- list(
    sufficient_n = n_obs >= min_n,
    sufficient_years = n_years >= min_years,
    sufficient_monthly_resolution = !is.na(median_gap_days) && median_gap_days <= max_median_gap_days,
    interannual_dominant = !is.na(interannual_r2) && interannual_r2 >= interannual_r2_threshold,
    seasonal_signal_present = !is.na(seasonal_ratio_detrended) && seasonal_ratio_detrended >= seasonal_ratio_threshold
  )
  
  # --- Decision logic ---------------------------------------------------
  if (!flags$sufficient_n) {
    recommendation <- "static"
    reason <- sprintf(
      paste0("Only %d valid observation(s) supplied (need >= %d). Too little ",
             "data to resolve any seasonal or interannual pattern."),
      n_obs, min_n
    )
  } else if (!flags$sufficient_years) {
    recommendation <- "static_or_single_year_monthly"
    reason <- sprintf(
      paste0("Data span only %d year(s) (need >= %d to separate a recurring ",
             "seasonal cycle from interannual variability). If the model run ",
             "covers exactly the sampled period, a monthly pattern from this ",
             "single year is usable; otherwise use the static mean/median Kw ",
             "(%.3f / %.3f m^-1)."),
      n_years, min_years, mean_kw, median_kw
    )
  } else if (!flags$sufficient_monthly_resolution) {
    recommendation <- "static"
    reason <- sprintf(
      paste0("Median sampling gap is %.0f days (> %.0f day threshold), too ",
             "sparse to build a reliable monthly climatology. Recommend a ",
             "static Kw = %.3f m^-1 (mean) or %.3f m^-1 (median, more robust ",
             "to bloom/turbidity outliers)."),
      median_gap_days, max_median_gap_days, mean_kw, median_kw
    )
  } else if (flags$interannual_dominant) {
    recommendation <- "dated_timeseries"
    reason <- sprintf(
      paste0("Year explains %.0f%% of total variance in Kw, more than the ",
             "seasonal cycle does (interannual regime shifts / trends ",
             "dominate over any recurring monthly pattern, detrended ",
             "seasonal ratio = %.2fx). A generic monthly climatology would ",
             "blend these regimes together and misrepresent most individual ",
             "years. Recommend driving GLM with the actual dated ",
             "observations (interpolated/held constant between sampling ",
             "dates) for the specific years being simulated, rather than a ",
             "synthetic 'typical year' climatology. If a single ",
             "representative value is still needed (e.g. for scenario runs ",
             "outside the observed period), use the static mean/median ",
             "(%.3f / %.3f m^-1) with the caveat that it will still miss ",
             "whichever regime the simulated period actually falls into."),
      100 * interannual_r2, seasonal_ratio_detrended, mean_kw, median_kw
    )
  } else if (!flags$seasonal_signal_present) {
    recommendation <- "static"
    reason <- sprintf(
      paste0("Interannual variability is limited (year R^2 = %.0f%%) and the ",
             "detrended seasonal swing is modest (%.2fx across months). A ",
             "monthly time-varying Kw is unlikely to add real skill over a ",
             "static value. Recommend static Kw = %.3f m^-1 (mean) or ",
             "%.3f m^-1 (median)."),
      100 * ifelse(is.na(interannual_r2), 0, interannual_r2),
      ifelse(is.na(seasonal_ratio_detrended), 1, seasonal_ratio_detrended),
      mean_kw, median_kw
    )
  } else {
    recommendation <- "monthly_climatology"
    reason <- sprintf(
      paste0("Sampling is frequent enough (median gap %.0f days) and spans ",
             "%d years with a real recurring seasonal cycle (detrended ",
             "seasonal ratio = %.2fx) that is not swamped by interannual ",
             "variability (year R^2 = %.0f%%). Recommend a monthly Kw ",
             "climatology (see `$monthly_climatology`) as a time-varying GLM ",
             "forcing. If the model run covers the actual sampled period, ",
             "the dated observed time series is still preferable to the ",
             "climatology."),
      median_gap_days, n_years, seasonal_ratio_detrended, 100 * interannual_r2
    )
  }
  
  out <- list(
    recommendation = recommendation,
    reason = reason,
    input_type = input_type,
    secchi_coef = if (input_type == "secchi") secchi_coef else NA_real_,
    n_obs = n_obs,
    n_dropped = n_dropped,
    date_range = range(date),
    n_years = n_years,
    median_gap_days = median_gap_days,
    max_gap_days = max_gap_days,
    mean_kw = mean_kw,
    median_kw = median_kw,
    sd_kw = sd_kw,
    cv_kw = cv_kw,
    interannual_r2 = interannual_r2,
    seasonal_ratio_raw = seasonal_ratio_raw,
    seasonal_ratio_detrended = seasonal_ratio_detrended,
    flags = flags,
    monthly_climatology = monthly,
    data = data.frame(date = date, kw = kw)
  )
  class(out) <- "light_recommendation"
  out
}

#' @export
print.light_recommendation <- function(x, ...) {
  cat("Light extinction parameterisation recommendation\n")
  cat(strrep("-", 50), "\n", sep = "")
  cat(sprintf("Input:              %s (n = %d, %d dropped for NA/ordering)\n",
              x$input_type, x$n_obs, x$n_dropped))
  cat(sprintf("Date range:         %s to %s (%d distinct year(s))\n",
              x$date_range[1], x$date_range[2], x$n_years))
  cat(sprintf("Median sample gap:  %.0f days (max %.0f days)\n",
              x$median_gap_days, x$max_gap_days))
  cat(sprintf("Kw:                 mean %.3f, median %.3f, sd %.3f, CV %.2f (m^-1)\n",
              x$mean_kw, x$median_kw, x$sd_kw, x$cv_kw))
  if (!is.na(x$interannual_r2)) {
    cat(sprintf("Interannual R^2:    %.0f%% of variance explained by year\n",
                100 * x$interannual_r2))
  }
  if (!is.na(x$seasonal_ratio_detrended)) {
    cat(sprintf("Seasonal ratio:     %.2fx (max:min monthly mean, detrended by year)\n",
                x$seasonal_ratio_detrended))
  }
  cat("\nRecommendation: ", switch(x$recommendation,
                                   static = "STATIC Kw",
                                   static_or_single_year_monthly = "STATIC Kw (or single-year monthly pattern)",
                                   dated_timeseries = "DATED TIME-VARYING Kw (use observed time series directly)",
                                   monthly_climatology = "MONTHLY Kw CLIMATOLOGY (time-varying)"
  ), "\n\n", sep = "")
  cat(strwrap(x$reason, width = 78), sep = "\n")
  if (x$recommendation == "monthly_climatology") {
    cat("\nMonthly climatology:\n")
    print(x$monthly_climatology, row.names = FALSE)
  }
  invisible(x)
}

#' @export
plot.light_recommendation <- function(x, ...) {
  op <- graphics::par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
  on.exit(graphics::par(op))
  
  plot(x$data$date, x$data$kw, type = "p", pch = 16, col = "steelblue",
       xlab = "Date", ylab = expression(K[d]~(m^-1)), main = "Observed Kw")
  graphics::abline(h = x$mean_kw, lty = 2, col = "grey40")
  
  mc <- x$monthly_climatology
  plot(mc$month, mc$mean, type = "b", pch = 16, col = "darkorange",
       xlab = "Month", ylab = expression(K[d]~(m^-1)), main = "Monthly climatology",
       ylim = range(c(mc$mean - mc$sd, mc$mean + mc$sd), na.rm = TRUE))
  graphics::arrows(mc$month, mc$mean - mc$sd, mc$month, mc$mean + mc$sd,
                   angle = 90, code = 3, length = 0.03, col = "darkorange")
  invisible(x)
}

# ---------------------------------------------------------------------------
# Decompose an observed Kw/Secchi series into a background (non-biotic) term
# and a biotic term attributable to phytoplankton (Chla) and/or non-algal
# particulates (turbidity), so the background term can be used as the
# `Kw_file` forcing when running GLM with AED's `bioshade_feedback` on.
# Rationale: GLM-AED adds the two additively (extc(i) = Kw + localext, see
# glm_aed.F90::update_light) — if the prescribed Kw already contains the
# biotic signal, running with bioshade_feedback double-counts it.
# ---------------------------------------------------------------------------

#' Decompose observed Kw into background and biotic components
#'
#' Fits `Kd ~ Chla + Turbidity` (whichever predictors are supplied) and uses
#' it to split each observed Kd into a background term (water + CDOM +
#' anything not explained by Chla/turbidity) and a biotic term. The
#' background term is what should be supplied as GLM's `Kw`/`Kw_file` when
#' AED's `bioshade_feedback` is switched on, since AED will reconstruct the
#' biotic term dynamically from its own simulated state.
#'
#' @param date Date vector (or coercible via `as.Date`).
#' @param kw Numeric vector of Kd (m^-1). Supply either `kw` or `secchi`.
#' @param secchi Numeric vector of Secchi depths (m), converted via
#'   `Kd = secchi_coef / secchi`.
#' @param secchi_coef Secchi-to-Kd conversion coefficient (default 1.7).
#' @param chla Numeric vector of chlorophyll-a (mg/m^3), same length as
#'   `date`, or `NULL` to omit.
#' @param turbidity Numeric vector of turbidity (NTU), same length as `date`,
#'   or `NULL` to omit. Used as a proxy for non-algal/mineral particulate
#'   extinction; note it is not independent of Chla (algae itself scatters
#'   light and contributes to a turbidity reading), so treat the individual
#'   coefficients cautiously when both predictors are supplied.
#' @param min_n Minimum number of complete (date, kw, predictor) triples
#'   required to fit the regression (default 10). Fewer than this and the
#'   function stops rather than returning an unreliable fit.
#' @param background_floor Minimum physically plausible background Kd
#'   (m^-1); background values from the regression that fall below this are
#'   clipped and counted (default 0.02, roughly pure-water + a little CDOM).
#'
#' @return An object of class `kw_decomposition`: the fitted `lm` model,
#'   diagnostics (R^2, coefficients, predictor collinearity), and a
#'   `data.frame` with the observed Kw split into `biotic_kw` and
#'   `background_kw` per date.
#' @export
decompose_kw_biotic <- function(date,
                                kw = NULL,
                                secchi = NULL,
                                secchi_coef = 1.7,
                                chla = NULL,
                                turbidity = NULL,
                                min_n = 10,
                                background_floor = 0.02) {
  
  if (is.null(kw) == is.null(secchi)) {
    stop("Supply exactly one of `kw` or `secchi`.")
  }
  if (is.null(chla) && is.null(turbidity)) {
    stop("Supply at least one of `chla` or `turbidity` to estimate the biotic component.")
  }
  
  date <- as.Date(date)
  n <- length(date)
  
  if (!is.null(secchi)) {
    if (length(secchi) != n) stop("`date` and `secchi` must be the same length.")
    kw <- secchi_coef / secchi
  } else {
    if (length(kw) != n) stop("`date` and `kw` must be the same length.")
  }
  if (!is.null(chla) && length(chla) != n) {
    stop("`date` and `chla` must be the same length.")
  }
  if (!is.null(turbidity) && length(turbidity) != n) {
    stop("`date` and `turbidity` must be the same length.")
  }
  
  dat <- data.frame(date = date, kw = kw)
  predictor_names <- character(0)
  if (!is.null(chla))      { dat$chla <- chla;             predictor_names <- c(predictor_names, "chla") }
  if (!is.null(turbidity)) { dat$turbidity <- turbidity;   predictor_names <- c(predictor_names, "turbidity") }
  
  dat <- dat[stats::complete.cases(dat), , drop = FALSE]
  dat <- dat[order(dat$date), ]
  n_used <- nrow(dat)
  n_dropped <- n - n_used
  
  if (n_used < min_n) {
    stop(sprintf(
      "Only %d complete (date, kw, %s) observations (need >= %d). Cannot fit a reliable decomposition.",
      n_used, paste(predictor_names, collapse = ", "), min_n
    ))
  }
  
  form <- stats::reformulate(predictor_names, response = "kw")
  fit <- stats::lm(form, data = dat)
  co <- stats::coef(fit)
  smry <- stats::summary.lm(fit)
  
  negative_coefs <- predictor_names[co[predictor_names] < 0]
  
  collinearity <- NA_real_
  if (length(predictor_names) == 2) {
    collinearity <- stats::cor(dat$chla, dat$turbidity, use = "complete.obs")
  }
  
  biotic_kw <- as.numeric(as.matrix(dat[, predictor_names, drop = FALSE]) %*%
                            co[predictor_names])
  background_kw <- dat$kw - biotic_kw
  
  n_clipped <- sum(background_kw < background_floor)
  background_kw_raw <- background_kw
  background_kw <- pmax(background_kw, background_floor)
  
  dat$biotic_kw <- biotic_kw
  dat$background_kw_raw <- background_kw_raw
  dat$background_kw <- background_kw
  
  out <- list(
    model = fit,
    predictor_names = predictor_names,
    coefficients = co,
    r2 = smry$r.squared,
    adj_r2 = smry$adj.r.squared,
    p_values = smry$coefficients[, "Pr(>|t|)"],
    n_used = n_used,
    n_dropped = n_dropped,
    negative_coefs = negative_coefs,
    collinearity = collinearity,
    background_floor = background_floor,
    n_clipped = n_clipped,
    background_intercept = unname(co["(Intercept)"]),
    mean_biotic_kw = mean(biotic_kw),
    mean_background_kw = mean(background_kw),
    biotic_fraction = mean(biotic_kw) / mean(dat$kw),
    data = dat
  )
  class(out) <- "kw_decomposition"
  out
}

#' @export
print.kw_decomposition <- function(x, ...) {
  cat("Kw decomposition: background vs. biotic extinction\n")
  cat(strrep("-", 50), "\n", sep = "")
  cat(sprintf("Model:              kw ~ %s\n", paste(x$predictor_names, collapse = " + ")))
  cat(sprintf("n used:             %d (%d dropped for missing values)\n", x$n_used, x$n_dropped))
  cat(sprintf("R^2 / adj R^2:      %.3f / %.3f\n", x$r2, x$adj_r2))
  cat("\nCoefficients (p-value):\n")
  for (nm in names(x$coefficients)) {
    cat(sprintf("  %-12s %10.5f   (p = %.4f)\n", nm, x$coefficients[nm], x$p_values[nm]))
  }
  if (length(x$negative_coefs) > 0) {
    cat(sprintf("\nWARNING: negative coefficient(s) for %s — physically implausible ",
                paste(x$negative_coefs, collapse = ", ")))
    cat("(more biomass/turbidity should not reduce extinction). Likely collinearity\n")
    cat("or too few observations across a limited biomass range; treat the split with caution.\n")
  }
  if (!is.na(x$collinearity) && abs(x$collinearity) > 0.6) {
    cat(sprintf("\nNOTE: chla and turbidity are correlated (r = %.2f) in this dataset — the\n",
                x$collinearity))
    cat("individual coefficients are not well separated; interpret the *combined* biotic\n")
    cat("term as more reliable than either coefficient alone.\n")
  }
  cat(sprintf("\nMean total Kw:      %.3f m^-1\n", mean(x$data$kw)))
  cat(sprintf("Mean biotic Kw:     %.3f m^-1 (%.0f%% of total)\n",
              x$mean_biotic_kw, 100 * x$biotic_fraction))
  cat(sprintf("Mean background Kw: %.3f m^-1 (intercept = %.3f)\n",
              x$mean_background_kw, x$background_intercept))
  if (x$n_clipped > 0) {
    cat(sprintf("\nNOTE: %d of %d background estimates were below the %.3f m^-1 floor and\n",
                x$n_clipped, x$n_used, x$background_floor))
    cat("were clipped (see `$data$background_kw_raw` for the unclipped values) — this can\n")
    cat("indicate the regression is over-attributing extinction to the biotic term on\n")
    cat("some dates (e.g. collinearity, or a bloom event outside the fitted range).\n")
  }
  cat("\nUse `$data$background_kw` (dated) as the GLM `Kw_file` forcing when running with\n")
  cat("AED's bioshade_feedback on. To check whether it should be static, monthly, or\n")
  cat("fully dated, pass it on to:\n")
  cat("  recommend_light_parameterisation(date = x$data$date, kw = x$data$background_kw)\n")
  invisible(x)
}

#' @export
plot.kw_decomposition <- function(x, ...) {
  op <- graphics::par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
  on.exit(graphics::par(op))
  
  plot(x$data$kw, stats::fitted(x$model), pch = 16, col = "steelblue",
       xlab = expression("Observed "*K[d]~(m^-1)), ylab = expression("Fitted "*K[d]~(m^-1)),
       main = sprintf("Model fit (R^2 = %.2f)", x$r2))
  graphics::abline(0, 1, lty = 2, col = "grey40")
  
  plot(x$data$date, x$data$kw, type = "p", pch = 16, col = "grey50",
       xlab = "Date", ylab = expression(K[d]~(m^-1)), main = "Observed vs. decomposed Kw",
       ylim = range(c(x$data$kw, x$data$background_kw, x$data$biotic_kw)))
  graphics::points(x$data$date, x$data$background_kw, pch = 16, col = "steelblue")
  graphics::points(x$data$date, x$data$biotic_kw, pch = 16, col = "forestgreen")
  graphics::legend("topleft", legend = c("Observed (total)", "Background", "Biotic"),
                   col = c("grey50", "steelblue", "forestgreen"), pch = 16, bty = "n", cex = 0.8)
  invisible(x)
}
