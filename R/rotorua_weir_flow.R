rotorua_weir_flow <- function(level_m, n_stoplogs = 0L, cd = 1) {
  if (!is.numeric(level_m)) {
    stop("`level_m` must be numeric.", call. = FALSE)
  }
  if (any(is.infinite(level_m))) {
    stop("`level_m` must not contain infinite values.", call. = FALSE)
  }
  if (
    !is.numeric(n_stoplogs) ||
      anyNA(n_stoplogs) ||
      any(!is.finite(n_stoplogs)) ||
      any(n_stoplogs < 0 | n_stoplogs > 3 | n_stoplogs != floor(n_stoplogs))
  ) {
    stop("`n_stoplogs` must contain whole numbers from 0 to 3.", call. = FALSE)
  }
  if (length(n_stoplogs) != 1L && length(n_stoplogs) != length(level_m)) {
    stop("`n_stoplogs` must have length 1 or match `level_m`.", call. = FALSE)
  }
  if (
    !is.numeric(cd) ||
      length(cd) != 1L ||
      !is.finite(cd) ||
      cd <= 0
  ) {
    stop("`cd` must be a single, positive, finite number.", call. = FALSE)
  }

  lower_crest_m <- 278.166 + 0.270 * n_stoplogs
  lower_head_m <- pmax(level_m - lower_crest_m, 0)
  upper_head_m <- pmax(level_m - 279.316, 0)

  1.705 * cd * (
    4.6 * lower_head_m^1.5 +
      (26 - 4.6) * upper_head_m^1.5
  )
}

generate_rotorua_weir_curves <- function(
  level_range_m = c(279.6, 280.5),
  by_m = 0.01,
  cd = 1
) {
  if (
    !is.numeric(level_range_m) ||
      length(level_range_m) != 2L ||
      any(!is.finite(level_range_m)) ||
      level_range_m[1] > level_range_m[2]
  ) {
    stop(
      "`level_range_m` must be two finite, increasing level values.",
      call. = FALSE
    )
  }
  if (!is.numeric(by_m) || length(by_m) != 1L || !is.finite(by_m) || by_m <= 0) {
    stop("`by_m` must be a single, positive, finite number.", call. = FALSE)
  }

  levels_m <- seq(level_range_m[1], level_range_m[2], by = by_m)
  if (tail(levels_m, 1) < level_range_m[2]) {
    levels_m <- c(levels_m, level_range_m[2])
  }

  curves <- expand.grid(
    level_m = levels_m,
    n_stoplogs = 0:3
  )
  curves$flow_m3s <- rotorua_weir_flow(
    level_m = curves$level_m,
    n_stoplogs = curves$n_stoplogs,
    cd = cd
  )
  curves
}
