#' Inflows for a transient climate run, paired with the transient weather
#'
#' Each simulated date takes its inflow from the same calendar day of the ERA5
#' source year that supplied that date's weather (`year_map`), so flow and
#' rainfall stay physically paired. Source years with no inflow record (the
#' inflow series ends 2022-06-30) are replaced by a seeded draw from the years
#' that have one, matching leap-year class; the weather-flow pairing is lost for
#' those years only.
#'
#' Climate response (skipped when `deltas` is NULL, i.e. the control):
#'  - `HYD_flow` is multiplied by the monthly `MET_pprain` ratio of that year
#'    (concentrations are held, so loads scale with flow);
#'  - `HYD_temp` gets `temp_damp` x the monthly `MET_tmpair` shift (streams warm
#'    less than air; 0.5 matches calibration/scenarios30).
#' Point-source inflows (`fixed`, e.g. geothermal, WWTP) are left unscaled.
#'
#' Alum (`alum_mode = "concentration"`, the default): the dosed inflows' ALU_ala
#' *concentration* is held at the latest year of the record (`alum_ref`,
#' 2021-07-01 to 2022-06-30), repeated by day of year in every simulated year and
#' every branch, whatever the flow. This avoids mapping a pre-dosing weather year
#' onto a zero dose. Note the historical record shows dosing is load-based (dose
#' load is uncorrelated with flow; concentration falls as flow rises), so holding
#' concentration makes the alum load scale with the scenario's flow.
#' `alum_mode = "load"` instead replaces the dose *load* (ALU_ala x HYD_flow) by
#' its mean-by-day-of-year over `alum_ref` and divides by the new flow (the
#' earlier setting, used with a 2016-2022 reference).
#' Other nutrient concentrations are carried over from the source year as-is,
#' so they reflect that year's historical loading (a known limitation).
#'
#' @param inflows named list of inflow data frames (Date, HYD_flow, HYD_temp, ...)
#' @param year_map output of build_transient_year_map()
#' @param deltas one gcm/scenario of gcm_transient_deltas_df, or NULL (control)
#' @param from,to dates to cover (include the spin-up)
#' @param alum_names inflows that receive alum
#' @param alum_ref length-2 Date vector, period defining the dose climatology
#' @param alum_mode "concentration" (hold the alum_ref ALU_ala concentration) or "load"
#' @param temp_damp fraction of the air-temperature shift given to stream temperature
#' @param fixed inflows whose flow and temperature do not respond to climate
#' @param seed RNG seed for substitute source years
#' @return named list of inflow data frames on the target dates
build_transient_inflows <- function(inflows, year_map, deltas = NULL, from, to,
                                    alum_names = c("Puarenga", "Utuhina"),
                                    alum_ref = as.Date(c("2021-07-01", "2022-06-30")),
                                    alum_mode = c("concentration", "load"),
                                    temp_damp = 0.5, fixed = c("Geothermal", "WWTP"),
                                    seed = 42) {
  alum_mode <- match.arg(alum_mode)
  is_leap <- function(y) (y %% 4 == 0 & y %% 100 != 0) | y %% 400 == 0
  inflows$Rainfall <- NULL
  from <- as.Date(from); to <- as.Date(to)

  # source years that have a full inflow record
  yrs <- as.integer(format(inflows[[1]]$Date, "%Y"))
  n <- table(yrs)
  have <- as.integer(names(n))[n == ifelse(is_leap(as.integer(names(n))), 366L, 365L)]

  ym <- year_map
  bad <- !ym$source_year %in% have
  if (any(bad)) {
    withr::with_seed(seed, {
      for (i in which(bad)) {
        pool <- have[is_leap(have) == is_leap(ym$target_year[i])]
        ym$source_year[i] <- pool[sample.int(length(pool), 1)]
      }
    })
    message(sum(bad), " of ", nrow(ym), " weather years had no inflow record; ",
            "substituted source years (weather-flow pairing lost for those).")
  }

  dates <- seq(from, to, by = "day")
  ty <- as.integer(format(dates, "%Y"))
  md <- format(dates, "%m-%d")
  src_year <- ym$source_year[match(ty, ym$target_year)]
  if (anyNA(src_year)) stop("year_map does not cover ", from, " to ", to, call. = FALSE)
  src_date <- as.Date(paste0(src_year, "-", md))
  mon <- as.integer(format(dates, "%m"))

  factor_for <- function(var) {
    if (is.null(deltas)) return(NULL)
    d <- deltas[deltas$variable == var, ]
    f <- d$delta[match(paste(ty, mon), paste(d$year, d$month))]
    if (anyNA(f)) stop("missing transient deltas for ", var, call. = FALSE)
    f
  }
  f_rain <- factor_for("MET_pprain")
  f_temp <- factor_for("MET_tmpair")

  lapply(stats::setNames(names(inflows), names(inflows)), function(nm) {
    d <- inflows[[nm]]
    out <- d[match(src_date, as.Date(d$Date)), , drop = FALSE]
    out$Date <- dates
    if ("ALU_ala" %in% names(d) && nm %in% alum_names) {
      r <- as.Date(d$Date) >= alum_ref[1] & as.Date(d$Date) <= alum_ref[2]
      v <- if (alum_mode == "load") d$ALU_ala[r] * d$HYD_flow[r] else d$ALU_ala[r]
      clim <- tapply(v, format(as.Date(d$Date)[r], "%m-%d"), mean, na.rm = TRUE)
      if (!"02-29" %in% names(clim) && "02-28" %in% names(clim))   # reference year has no leap day
        clim <- c(clim, "02-29" = unname(clim["02-28"]))
      out$ALU_ala <- as.numeric(clim[md])      # a concentration, or temporarily a load
      out$ALU_ala[is.na(out$ALU_ala)] <- 0
    }
    if (!nm %in% fixed) {
      if (!is.null(f_rain)) out$HYD_flow <- out$HYD_flow * f_rain
      if (!is.null(f_temp)) out$HYD_temp <- out$HYD_temp + temp_damp * f_temp
    }
    if (alum_mode == "load" && "ALU_ala" %in% names(out) && nm %in% alum_names)
      out$ALU_ala <- ifelse(out$HYD_flow > 0, out$ALU_ala / out$HYD_flow, 0)
    out
  })
}
