# Comparing met forcing sources (buoy, airport/NIWA, ERA5, PC10 BOPRC
# stations). Everything upstream of this already agrees on a wide
# Date + MET_* layout (see buoy_to_aeme_met.R, niwa_hourly_to_aeme_met.R,
# read_era5_hourly_met via era5_hourly_met, pc10_climate_to_aeme_met.R), so
# comparison just means: aggregate each source to daily, stack them long,
# and score everything against the buoy (the only source actually measured
# on the lake) over whatever window each pair overlaps.

# Collapse an hourly (or daily) wide MET_* data frame to one row per day.
# Rain is accumulated (sum); everything else is averaged. A day with no
# valid readings collapses to NA rather than NaN/0.
aggregate_met_daily <- function(df) {
  df <- df |> dplyr::mutate(Date = as.Date(Date))

  sum_vars <- intersect("MET_pprain", names(df))
  mean_vars <- setdiff(names(df), c("Date", sum_vars))

  na_mean <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
  na_sum  <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)

  df |>
    dplyr::group_by(Date) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(mean_vars), na_mean),
      dplyr::across(dplyr::all_of(sum_vars), na_sum),
      .groups = "drop"
    )
}

# Stack a named list of wide met data frames (any timestep) into one long
# daily table: Date, variable, value, source.
met_daily_long <- function(sources) {
  dplyr::bind_rows(lapply(names(sources), function(nm) {
    aggregate_met_daily(sources[[nm]]) |>
      tidyr::pivot_longer(-Date, names_to = "variable", values_to = "value") |>
      dplyr::mutate(source = nm)
  }))
}

#' Compare met forcing sources against a reference (the lake buoy by
#' default), variable by variable.
#'
#' @param ... named wide met data frames, e.g.
#'   `compare_met_sources(buoy = rotorua_buoy_met_aeme_hr,
#'   airport = niwa_met_hourly_aeme, era5 = era5_hourly_met,
#'   pc10 = pc10_climate_met)`
#' @param vars variables to score (must exist in at least the reference and
#'   one other source)
#' @param reference name (from `...`) to treat as ground truth; comparisons
#'   are only computed for the other sources against this one
#'
#' @return list with `long` (all sources, tidy daily), `wide` (one column
#'   per source, for the compared variables only) and `stats` (one row per
#'   variable x non-reference source: n, overlap window, bias, MAE, RMSE, r)
compare_met_sources <- function(...,
                                vars = c("MET_tmpair", "MET_humrel",
                                        "MET_pprain"),
                                reference = "buoy") {
  sources <- list(...)
  stopifnot(reference %in% names(sources))

  long <- met_daily_long(sources)

  wide <- long |>
    dplyr::filter(variable %in% vars) |>
    tidyr::pivot_wider(names_from = source, values_from = value)

  other_sources <- setdiff(names(sources), reference)

  stats <- lapply(vars, function(v) {
    lapply(other_sources, function(src) {
      cols <- c("Date", reference, src)
      if (!all(c(reference, src) %in% names(wide))) return(NULL)
      d <- wide[wide$variable == v, cols]
      names(d) <- c("Date", "ref", "val")
      d <- d[stats::complete.cases(d), ]
      if (nrow(d) < 2) {
        return(data.frame(variable = v, source = src, n = nrow(d),
                          start = as.Date(NA), end = as.Date(NA),
                          bias = NA_real_, mae = NA_real_, rmse = NA_real_,
                          r = NA_real_))
      }
      data.frame(
        variable = v, source = src, n = nrow(d),
        start = min(d$Date), end = max(d$Date),
        bias = mean(d$val - d$ref),
        mae = mean(abs(d$val - d$ref)),
        rmse = sqrt(mean((d$val - d$ref)^2)),
        r = stats::cor(d$val, d$ref)
      )
    }) |> dplyr::bind_rows()
  }) |> dplyr::bind_rows()

  list(long = long, wide = wide, stats = stats)
}

plot_met_source_comparison <- function(comparison, window = NULL,
                                       vars = c("MET_tmpair", "MET_humrel",
                                               "MET_pprain")) {
  d <- comparison$long |> dplyr::filter(variable %in% vars)
  if (!is.null(window)) {
    d <- d |> dplyr::filter(Date >= window[1], Date <= window[2])
  }

  ggplot2::ggplot(d, ggplot2::aes(Date, value, colour = source)) +
    ggplot2::geom_line(na.rm = TRUE, alpha = 0.8) +
    ggplot2::facet_wrap(~variable, ncol = 1, scales = "free_y") +
    ggplot2::labs(x = NULL, y = NULL, colour = NULL) +
    ggplot2::theme_bw()
}
