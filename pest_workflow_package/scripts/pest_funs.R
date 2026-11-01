# ============================================================================
# Project calibration helpers.
#
# Functions promoted to aemetools (>= 0.3.0.9000) and removed from here:
#
#   with_log()            -> aemetools::set_param_log()
#   freeze()              -> aemetools::freeze_param()
#   carry()               -> aemetools::carry_param()  (thin wrapper kept below,
#                            it restricts to the stage's kept set)
#   check_param_targets() -> aemetools::check_param_targets(param, aeme, model)
#                            NOTE: the signature changed - it now takes a built
#                            aeme object, not (p, aed, phyto). Update the call
#                            site in the main script.
#   port_free() / next_pest_port() / PORT_BASE
#                         -> aemetools now binds a free PANTHER port itself;
#                            create_sen_control()/create_pest_control() take
#                            `port` as a search start-point, so no explicit
#                            port juggling is needed.
# ============================================================================

# ============================================================================
# Stage-1 fallback: lakes with no water-level observations
# ============================================================================

#' Synthetic LKE_lvlwtr "observations" for a lake that has none.
#'
#' Stage 1 exists to fit the water balance, and calib_aeme() hard-requires
#' at least one row of observed level to build a PEST observation group -
#' without it stage 1 cannot run at all ("observations(aeme)$level has no
#' rows"). Rather than skip water balance calibration for such a lake
#' entirely, anchor it to a target that needs no observed series: the level
#' should return to roughly the same value at the same point in the annual
#' cycle, year after year, i.e. no long-term trend.
#'
#' One point per year, all at the SAME calendar day (the simulation start
#' date's anniversary) and all targeting the SAME reference value, is the
#' key choice: comparing same-phase-to-same-phase means the outcome depends
#' only on the level's long-term drift, never on the shape or amplitude of
#' its seasonal cycle - nothing here pulls at any other time of year, so
#' natural seasonal variation is entirely free. It intentionally does NOT
#' constrain the trajectory to be flat, only to not have drifted when the
#' calendar comes back around.
#'
#' MUST be called on the BUILT, ALREADY-RUN object (after fresh_build()'s
#' check run), and its result used only for the calibration step that
#' follows - never fed back into a fresh build. Two reasons:
#'
#'   1. build_aeme() itself reads observations(aeme)$level, inside
#'      resolve_water_level(), to help correct/estimate outflows before the
#'      model ever runs. Handing it fabricated data there does not just add
#'      a PEST target, it changes how the water balance is constructed.
#'   2. The reference value defaults to the model's OWN simulated LKE_lvlwtr
#'      near the start of the post-spin-up period (via get_var(), which
#'      needs a run to already exist) rather than input(aeme)$init_depth.
#'      They are not interchangeable: this project has lakes whose level
#'      convention is a DEPTH (single digits) and at least one, confirmed by
#'      testing, whose hypsograph is an absolute ELEVATION (tens) -
#'      init_depth is a depth in both cases, so using it directly as the
#'      target for an elevation-basis lake fails resolve_water_level()'s own
#'      range check ("Observed range: 12.41 to 12.41. Hypsograph range: 66.11
#'      to 83.52."). Reading the model's own simulated series sidesteps the
#'      question of which convention this particular lake uses entirely.
#'
#' Weighted the same as every other lake's real LKE_lvlwtr observations (via
#' the ordinary set_weights() call in run_stage()) rather than specially
#' downweighted: because there are only 2-3 points here against hundreds for
#' a lake with real data, it naturally cannot dominate phi the way a dense
#' observed series can (see the stage-2 note on LKE_lvlwtr, above) - the
#' sparsity alone does the downweighting.
#'
#' The anchor values carry a tiny deterministic +/-`jitter_frac` alternation
#' (default +/-0.05%% of `ref`) rather than being all EXACTLY equal. This is
#' not cosmetic: calib_aeme()'s per-realisation "best" pick uses KGE, whose
#' correlation term is undefined (0/0) against an observed series with zero
#' variance. Confirmed on LID49307: PEST's native phi improved normally
#' (3415 -> 3180) while every realisation's KGE came back NaN -> the
#' pipeline's NA sentinel -> get_best_params() returned NA for every stage-1
#' parameter -> carry_param() crashed trying to warn about it (a real
#' aemetools bug, but this is what triggered it). The alternation nets to
#' ~zero across the anchors, so it adds no systematic drift of its own - it
#' only keeps KGE well-defined.
#'
#' @param aeme BUILT, ALREADY-RUN aeme object (e.g. fresh_build()'s return).
#' @param model model key, e.g. "glm_aed".
#' @param period the aeme_sim_period from suggest_sim_period()/set_sim_period().
#' @param ref reference level. Defaults to this lake's own simulated
#'   LKE_lvlwtr at the first available day of `period` (post spin-up).
#' @param min_points abort if the simulation period is too short to place at
#'   least this many annual anchors (default 2 - a single point cannot
#'   constrain a trend at all).
#' @param jitter_frac +/- fraction of `ref` to alternate by (default 0.0005,
#'   i.e. 0.05%). Set to 0 to disable - understanding it breaks KGE-based fit
#'   extraction for this target, per the note above.
#' Trim an Aeme object's own forcing data (meteorology + every inflow) to an
#' absolute date range, so a shorter simulation period actually simulates
#' fewer days instead of just moving where SCORING starts within an
#' unchanged, still-full-length run.
#'
#' `AEME::set_time(start=, stop=, spin_up=)` alone does NOT shorten a run:
#' `suggest_sim_period()` always resolves its own window from the forcing
#' data's own native availability (`earliest <- window$start + spin_up`,
#' where `window$start` sits near the forcing series' actual beginning, not
#' whatever `start=` was passed to `set_time()`), using `spin_up` only to
#' decide how much of that unchanged range to exclude from scoring. This was
#' tried directly (2026-09-21, see pest_calib_v4.R's START_PHYS/SPIN_UP_PHYS
#' history) and confirmed ineffective - the resolved period stayed full-
#' length regardless. Genuinely shortening runtime/output size requires the
#' underlying met/inflow data itself to not extend past the desired window
#' BEFORE `suggest_sim_period()` ever sees it - which is what this function
#' does, directly on `input(aeme)$meteo` and every `inflows(aeme)` stream.
#'
#' Call this BEFORE `AEME::set_time()`/`suggest_sim_period()`/
#' `set_sim_period()`, on the same aeme0 those functions will run against.
#'
#' @param aeme a built/loaded Aeme object with inflows already attached (via
#'   get_inflows()/add_inflows(), as every lake in this pipeline already is
#'   before this point).
#' @param start,stop Date or character - the absolute forcing window to
#'   keep. Should already include however much spin-up you intend to use
#'   BEFORE the analysis window - this trims the RAW forcing series, not
#'   the scored/spin-up split (that split still happens via `spin_up=` in
#'   the usual `set_time()` call made right after this).
#' @returns `aeme` with `input(aeme)$meteo` and every inflow's data frame
#'   trimmed to `[start, stop]`.
trim_forcing_period <- function(aeme, start, stop) {
  start <- as.Date(start); stop <- as.Date(stop)

  inp <- AEME::input(aeme)
  met_date <- as.Date(inp$meteo$Date)
  inp$meteo <- inp$meteo[met_date >= start & met_date <= stop, , drop = FALSE]
  AEME::input(aeme) <- inp

  infl <- get_inflows(aeme)
  infl <- lapply(infl, function(df) {
    d <- as.Date(df$Date)
    df[d >= start & d <= stop, , drop = FALSE]
  })
  aeme <- aeme |> remove_inflow(all = TRUE) |> add_inflows(infl)
  aeme
}

#' Pick the densest available `window_years`-long observation window for
#' THIS lake's own record, adaptively - not every lake has data for every
#' target variable (e.g. some have no Schmidt stability samples at all, or
#' no water-level record), so this scores only the variables that actually
#' have at least `min_obs` observations SOMEWHERE in this lake's record,
#' rather than requiring all of them and returning a degenerate all-zero
#' window for a lake missing one entirely. Mirrors the manual analysis
#' that picked Rototoa's own 2021-2023 window (see
#' pest_calib_v4.R's ANALYSIS_START header) but automated and per-lake.
#'
#' Combine with `trim_forcing_period()` (this function only picks the
#' dates) and, ideally, `AEME::suggest_sim_period()` on the trimmed result
#' for fine placement/sparse-year trimming within the chosen window -
#' `suggest_sim_period()` alone cannot do this job itself: it always
#' resolves its window from the forcing data's OWN full native range, only
#' using `spin_up` to decide how much of that unchanged range to exclude
#' from scoring (see trim_forcing_period()'s own header for the
#' 2026-09-21 finding this was confirmed on).
#'
#' @param aeme untrimmed Aeme object with inflows already attached
#'   (get_inflows()/add_inflows(), as this pipeline always does before
#'   this point) and observations present.
#' @param vars_sim character vector of AEME variables to score density
#'   for. `LKE_vol`/`LKE_lvlwtr` is handled separately via
#'   `observations(aeme)$level`, not `$lake`, matching how the rest of
#'   this pipeline treats water level.
#' @param window_years numeric, target scored-window length in years.
#' @param spin_up_years numeric, spin-up to reserve BEFORE the window -
#'   only affects the returned `forcing_start`, not the density scoring.
#' @param step_years numeric, how far to slide between candidate windows.
#' @param min_obs a variable needs at least this many observations
#'   SOMEWHERE in the lake's whole record to be scored at all; a variable
#'   with fewer than this contributes no usable signal to any window
#'   choice, so it's dropped from scoring rather than zeroing out every
#'   candidate window because of a variable this lake structurally lacks.
#' @returns list(forcing_start, forcing_stop, window_start, window_end,
#'   vars_used, candidates), or NULL if this lake has no forcing data at
#'   all / no target variable clears `min_obs` anywhere (caller should
#'   fall back to the long/default period in that case).
pick_dense_window <- function(aeme, vars_sim, window_years = 2,
                              spin_up_years = 2, step_years = 1,
                              min_obs = 8) {
  met <- AEME::input(aeme)$meteo
  if (is.null(met) || nrow(met) == 0) return(NULL)
  force_start <- as.Date(min(met$Date, na.rm = TRUE))
  force_end   <- as.Date(max(met$Date, na.rm = TRUE))

  obs <- AEME::observations(aeme)
  d <- obs$lake
  lake_vars <- setdiff(vars_sim, c("LKE_vol", "LKE_lvlwtr"))
  if (!is.null(d)) {
    d$Date <- as.Date(d$Date)
    d <- d[!is.na(d$value) & d$var_aeme %in% lake_vars, ]
  }
  needs_lvl <- any(c("LKE_vol", "LKE_lvlwtr") %in% vars_sim)
  lvl <- obs$level
  if (!is.null(lvl) && needs_lvl) {
    lvl$Date <- as.Date(lvl$Date)
    lvl <- lvl[!is.na(lvl$value), ]
    if (nrow(lvl) == 0) lvl <- NULL
  } else lvl <- NULL

  usable_vars <- if (!is.null(d) && nrow(d) > 0) {
    tab <- table(d$var_aeme)
    names(tab)[tab >= min_obs]
  } else character(0)
  if (length(usable_vars) == 0 && is.null(lvl)) return(NULL)

  full_start <- force_start; full_end <- force_end
  if (length(usable_vars) > 0) {
    dv <- d[d$var_aeme %in% usable_vars, ]
    full_start <- max(full_start, min(dv$Date, na.rm = TRUE))
    full_end   <- min(full_end, max(dv$Date, na.rm = TRUE))
  }
  if (!is.null(lvl)) {
    full_start <- max(full_start, min(lvl$Date, na.rm = TRUE))
    full_end   <- min(full_end, max(lvl$Date, na.rm = TRUE))
  }
  if (!is.finite(full_start) || !is.finite(full_end) || full_start >= full_end) return(NULL)

  win_days <- round(window_years * 365)
  starts <- seq(full_start, max(full_start, full_end - win_days), by = paste(step_years, "years"))
  if (length(starts) == 0) starts <- full_start

  cand <- do.call(rbind, lapply(starts, function(s) {
    e <- s + win_days
    counts <- if (length(usable_vars) > 0) {
      vapply(usable_vars, function(v) sum(d$var_aeme == v & d$Date >= s & d$Date <= e),
            integer(1))
    } else integer(0)
    lvl_n <- if (!is.null(lvl)) sum(lvl$Date >= s & lvl$Date <= e) else NA_integer_
    data.frame(start = s, end = e,
              min_n = if (length(counts) > 0) min(counts) else NA_integer_,
              total_n = sum(counts) + (if (is.na(lvl_n)) 0 else lvl_n),
              lvl_n = lvl_n)
  }))

  # Prefer windows with a real (non-zero) level record when this lake's
  # target set needs one at all - an emptier-but-real-level window beats a
  # denser one that would force LKE_vol onto synthesize_level_target()'s
  # synthetic fallback instead of real data (confirmed the right call for
  # Rototoa's own window choice - see ANALYSIS_START's header).
  if (needs_lvl && !is.null(lvl) && any(cand$lvl_n > 0, na.rm = TRUE)) {
    cand <- cand[is.na(cand$lvl_n) | cand$lvl_n > 0, ]
  }
  # min_n can be NA when this lake has no usable $lake-observation
  # variable at all (level-only target set) - fall back to ranking by
  # total_n/lvl_n alone in that case.
  # unary "-" is undefined for Date, so rank recency via as.numeric().
  start_num <- as.numeric(cand$start)
  ord <- if (all(is.na(cand$min_n))) {
    order(-cand$total_n, -start_num)
  } else {
    order(-cand$min_n, -cand$total_n, -start_num)
  }
  best <- cand[ord, ][1, ]

  list(window_start = best$start, window_end = best$end,
      forcing_start = best$start - round(spin_up_years * 365),
      forcing_stop = best$end,
      vars_used = usable_vars, candidates = cand)
}

#' Build the SAME Aeme object a lake's stage-1/2/3 checkpoints were
#' actually calibrated against - required because a checkpoint stores its
#' PEST metadata (best/kept/phi/cal_ctrl/cal_id) but not a runnable Aeme
#' object itself. Which PERIOD to rebuild with differs by lake, by this
#' project's own history:
#'
#'   - Rototoa (LID50270) is the one lake ever calibrated against the
#'     LONG period (START/STOP/SPIN_UP, ~2009-2023/5yr spin-up) - every
#'     stage-1/2/3 checkpoint under `s{1,2,3}_LID50270_rototoa.rds` was
#'     fit against it.
#'   - every other lake's stage-1/2 checkpoint comes from the 9-lake
#'     physics run (2026-09-25), which used pick_dense_window()'s own
#'     per-lake ADAPTIVE short window (2yr scored / 2yr spin-up defaults)
#'     instead - see pest_calib_v4.R's USE_SHORT_PERIOD block. This is
#'     reconstructed here rather than hardcoded per lake:
#'     pick_dense_window() takes no random seed and scores purely from
#'     this lake's own observation record, so calling it again with the
#'     same arguments reproduces the SAME window the original run picked,
#'     as long as the underlying observation data hasn't changed since.
#'
#' Moved here from analyse_pest_calib_v4.R (2026-09-25) so
#' pest_calib_v4_downstream.R can reuse it too, for rebuilding an
#' upstream lake's own calibrated physics ahead of converting its outflow
#' into a downstream lake's inflow - one definition, not two copies that
#' could silently drift apart.
#'
#' @param source_aeme_list a list of built Aeme objects to search (e.g.
#'   `targets::tar_read(aeme_baseline_upstream)`), each with a real
#'   `AEME::lake(x)$id`.
#' @param lake_tag e.g. "LID50270_rototoa" - the LID prefix (before the
#'   first "_") identifies the lake within `source_aeme_list`.
#' @param targets_sim AEME variables to resolve the period against -
#'   should match what that lake's stage 1/2 actually targeted.
build_lake_aeme0 <- function(source_aeme_list, lake_tag, targets_sim) {
  lid <- sub("^(LID[0-9]+)_.*$", "\\1", lake_tag)
  ids <- vapply(source_aeme_list, function(a) AEME::lake(a)$id, character(1))
  hit <- which(ids == lid)
  if (length(hit) == 0)
    stop("build_lake_aeme0(): no entry for ", lid,
        " (from lake_tag '", lake_tag, "') in source_aeme_list.")
  aeme0 <- source_aeme_list[[hit[1]]]
  inflows <- get_inflows(aeme0)
  inflows <- lapply(inflows, function(df) df[, !colnames(df) %in% "NCS_tss"])
  aeme0 <- aeme0 |> remove_inflow(all = TRUE) |> add_inflows(inflows)

  # 2026-10-01: every lake now runs the SAME full period Rototoa always
  # used (forcing data confirmed to cover 2002-2023 for every lake checked,
  # well beyond this window), so dashboard outputs (ensembles, scenarios,
  # baseline outflows) are directly comparable across all 14 lakes. This
  # does NOT change what period a lake's parameters were CALIBRATED
  # against (pest_calib_v6.R calls pick_dense_window() itself, independent
  # of this function) - only the forward-run period used to exercise those
  # already-fit parameters here and in the ensemble/scenario/outflow
  # scripts that call build_lake_aeme0().
  START <- "2014-07-01"; STOP <- "2023-06-30"; SPIN_UP <- 365 * 5
  aeme0_full <- AEME::set_time(aeme0, start = START, stop = STOP, spin_up = SPIN_UP)
  period_full <- suggest_sim_period(aeme0_full, vars_sim = targets_sim,
                                    spin_up = ceiling(365 * 1.5))
  AEME::set_sim_period(aeme0_full, period_full)
}

#' Does this lake have at least one real water-level observation actually
#' falling INSIDE `period` - not just somewhere in its raw record?
#'
#' The original guard around every synthesize_level_target() call site
#' (pest_calib_v4.R) only checked `nrow(observations(aeme)$level) == 0` -
#' true only when a lake has NO level data anywhere, ever. That missed a
#' real case: a lake WITH real level data, none of which happens to fall
#' inside a short, adaptively-chosen window (pick_dense_window()) -
#' confirmed 2026-09-25 on Pehiakura Small (lake 7 of the 9-lake physics
#' run), which failed stage 1 outright ("No observations fall within the
#' simulation period") because its real level record existed but sat
#' entirely outside the ~2-year window chosen for its other variables.
#' calib_aeme() cannot target LKE_vol with zero in-period observations
#' either way, so this needs the SAME synthetic-anchor fallback - just
#' triggered on the right condition.
#'
#' @param aeme a built Aeme object.
#' @param period a period (list/data.frame with `start`/`stop`, as
#'   returned by suggest_sim_period()/set_sim_period()).
#' @returns logical.
has_level_obs_in_period <- function(aeme, period) {
  lvl <- AEME::observations(aeme)$level
  if (is.null(lvl) || nrow(lvl) == 0) return(FALSE)
  d <- as.Date(lvl$Date)
  any(d >= as.Date(period$start) & d <= as.Date(period$stop) & !is.na(lvl$value))
}

synthesize_level_target <- function(aeme, model, period, ref = NULL,
                                    min_points = 2, jitter_frac = 0.0005) {
  start <- as.Date(period$start); stop <- as.Date(period$stop)
  if (is.null(ref)) {
    sim <- AEME::get_var(aeme, model = model, var_sim = "LKE_lvlwtr",
                         remove_spin_up = TRUE)
    sim <- sim[!is.na(sim$value), ]
    if (nrow(sim) == 0)
      stop("synthesize_level_target(): model has no simulated LKE_lvlwtr to ",
          "read a reference level from - was `aeme` built and run first?")
    ref <- sim$value[which.min(abs(sim$Date - start))]
  }
  # Kept as a 1-day inset (harmless safety margin), but this was NOT the
  # actual bug - confirmed 2026-09-25 (Pehiakura Small/lake 7): moving the
  # anchors off the exact period boundary alone did not fix stage 1. The
  # REAL cause, found afterward: pest_obs_table() (aemetools, write_pst.R)
  # only reads observations(aeme)$level when the target is LITERALLY
  # "LKE_lvlwtr" (`want_lvl <- "LKE_lvlwtr" %in% vars_sim`). Stage 1/2
  # target "LKE_vol", which pest_obs_table() instead expects to find
  # ALREADY DERIVED into observations(aeme)$lake - a conversion AEME's own
  # calc_lake_obs_deriv() does ONCE, upstream, from observations(aeme)
  # $level via calc_V_glm(), before aeme_baseline_upstream is ever built
  # and cached. Writing only to $level here (the ORIGINAL version of this
  # function) updated data calc_lake_obs_deriv() would use if it ran again
  # - but it never runs again inside pest_calib_v4.R, so a synthesized
  # level never became a synthesized volume, and calib_aeme() found zero
  # LKE_vol rows in $lake regardless. Fixed below by deriving and writing
  # the matching LKE_vol row into $lake directly, replicating
  # calc_lake_obs_deriv()'s own conversion exactly (same calc_V_glm() call,
  # same bed-relative-to-absolute-elevation adjustment) so both paths -
  # an explicit LKE_lvlwtr target and an LKE_vol target - see synthetic
  # data.
  dates <- seq(start + 1, stop - 1, by = "1 year")
  if (length(dates) < min_points)
    stop("synthesize_level_target(): simulation period (", start, " to ", stop,
        ", ", round(as.numeric(stop - start) / 365.25, 1), " yr) is too short ",
        "for ", min_points, " annual anchor points.")
  jit <- rep_len(c(1, -1), length(dates)) * abs(ref) * jitter_frac
  df <- data.frame(Date = dates, var_aeme = "LKE_lvlwtr", value = ref + jit)

  # calc_V_glm() (AEME, internal) expects absolute water-surface elevation
  # (hyps$elev's own datum), but `ref`/`df$value` came from
  # get_var(var_sim = "LKE_lvlwtr"), which - like all GLM-AED model output
  # - is height above the lake BED, not absolute elevation. Add
  # min(hyps$elev) to convert, exactly as calc_V_glm()'s own docstring
  # instructs ("LKE_lvlwtr as returned by get_var ... must have
  # min(hyps$elev) added before being passed here").
  hyps <- AEME::input(aeme)$hypsograph
  vol_df <- data.frame(
    Date = dates, var_aeme = "LKE_vol", depth = NA_real_,
    value = AEME:::calc_V_glm(depth = df$value + min(hyps$elev), hyps = hyps)
  )

  obs <- AEME::observations(aeme)
  obs$level <- df
  # Replace any pre-existing LKE_vol rows (there should be none - this
  # path only runs when the lake has no usable in-period level/volume
  # data at all - but guard against a stale partial row rather than
  # duplicate it) before adding the synthesized ones.
  obs$lake <- if (is.null(obs$lake)) {
    vol_df
  } else {
    dplyr::bind_rows(obs$lake[obs$lake$var_aeme != "LKE_vol", , drop = FALSE], vol_df)
  }
  AEME::observations(aeme) <- obs
  message("  no water-level observations - synthesized ", nrow(df),
          " annual same-phase LKE_lvlwtr anchors around ", round(ref, 2),
          " m (+/-", round(100 * jitter_frac, 3), "%, so KGE stays defined) at ",
          paste(format(dates, "%Y-%m-%d"), collapse = ", "),
          " (and the matching derived LKE_vol rows in observations(aeme)",
          "$lake, since that - not $level - is what an LKE_vol-target ",
          "stage actually scores against) - stage 1 still constrains ",
          "long-term drift, without fighting seasonal variation")
  aeme
}

# ============================================================================
# Table readers and appliers
# ============================================================================

#' AEME variable names for AED's own ("OXY_oxy" -> "CHM_oxy",
#' "OGM_doc" -> "CAR_doc"). Names AEME does not rename pass through.
aeme_var <- function(v) {
  e <- new.env()
  utils::data("key_naming", package = "AEME", envir = e)
  kn <- as.data.frame(e$key_naming)
  kn <- kn[!is.na(kn$glm_aed) & nzchar(kn$glm_aed), ]
  map <- stats::setNames(kn$var_aeme, kn$glm_aed)
  out <- unname(map[v])
  out[is.na(out)] <- v[is.na(out)]
  unique(out)
}

#' One row per (switch, value) pair.
#'
#' The shipped table is part specification, part documentation: a `name` can
#' carry several pipe-separated keys sharing one block prefix, and a
#' `required_value` can be a choice ("1 or 2", "0 or 1 per group") that only
#' the modeller can settle. Those come from SWITCH_CHOICE.
#'
#' Read positionally rather than with read.csv(): the trailing `enables` and
#' `note` fields carry unquoted commas, so the table is ragged and read.csv()
#' shifts those rows into nonsense. Only the first three fields are load
#' bearing and those are quoted properly, so take them by position and treat
#' everything after `enables` as free text.
read_switches <- function(file) {
  lines <- readLines(file, warn = FALSE)
  lines <- lines[nzchar(trimws(lines))]
  fields <- function(l) scan(text = l, what = "", sep = ",", quote = "\"",
                             quiet = TRUE, strip.white = TRUE,
                             comment.char = "", blank.lines.skip = FALSE)
  hdr <- fields(lines[1])
  need <- c("file", "name", "required_value")
  if (!identical(hdr[seq_along(need)], need))
    stop(file, " must start with the columns ", paste(need, collapse = ", "),
         "; found ", paste(utils::head(hdr, 3), collapse = ", "))

  do.call(rbind, lapply(lines[-1], function(l) {
    f <- fields(l)
    if (length(f) < 3 || !nzchar(f[3]))
      stop("Switch row has no required_value: ", l)
    keys <- trimws(strsplit(f[2], "|", fixed = TRUE)[[1]])
    vals <- trimws(strsplit(f[3], "|", fixed = TRUE)[[1]])
    has_blk <- grepl("/", keys, fixed = TRUE)
    if (has_blk[1] && any(!has_blk)) {
      blk <- sub("/.*$", "", keys[1])
      keys[!has_blk] <- paste0(blk, "/", keys[!has_blk])
    }
    if (length(vals) != length(keys)) vals <- rep(trimws(f[3]), length(keys))
    data.frame(file = f[1], key = keys, value_txt = vals,
               note = paste(f[-(1:4)], collapse = ", "),
               stringsAsFactors = FALSE)
  }))
}

#' Coerce a switch value to the type AED expects: logical for `.true.`,
#' numeric (possibly a vector) for comma-separated numbers, character
#' otherwise. print.nml() turns these back into `.true.` / `1, 2, 3` /
#' `'a','b'` on write.
switch_value <- function(txt) {
  v <- trimws(txt)
  if (grepl("^\\.?(true|false)\\.?$", v, ignore.case = TRUE))
    return(grepl("true", v, ignore.case = TRUE))
  parts <- trimws(strsplit(v, ",", fixed = TRUE)[[1]])
  num <- suppressWarnings(as.numeric(parts))
  if (!anyNA(num)) return(num)
  parts
}



#' A required_value the table leaves to the modeller.
switch_open <- function(txt) grepl("(^| )or( |$)|per group", txt)

#' Column positions of `groups` within aed_phyto_pars.csv, which is what
#' aed_phytoplankton/the_phytos indexes.
phyto_cols <- function(phyto, groups) {
  cols <- names(phyto)[-1]
  idx <- match(groups, cols)
  if (anyNA(idx))
    stop("aed_phyto_pars.csv has no column for: ",
         paste(groups[is.na(idx)], collapse = ", "),
         ". Available: ", paste(cols, collapse = ", "))
  # input_model_parameters() picks the column with grepl(), so a group name
  # that is a substring of another column would be written to both.
  amb <- groups[vapply(groups, function(g) sum(grepl(g, cols, fixed = TRUE)),
                       integer(1)) > 1L]
  if (length(amb) > 0)
    stop("Group name(s) ", paste(amb, collapse = ", "), " match more than one ",
         "column of aed_phyto_pars.csv; AEME writes parameters by grepl() and ",
         "would update all of them.")
  idx
}

#' The block a bare nml key lives in.
aed_block_of <- function(aed, key) {
  hit <- which(vapply(aed, function(b)
    tolower(key) %in% tolower(names(b)), logical(1)))
  if (length(hit) == 0) NA_character_ else names(aed)[hit[1]]
}

#' The &aed_models list: union of what the build already activated (AEME
#' derives that from the simulated variables, and it carries dependencies such
#' as aed_totals) with what the switch table requires, dropped to the modules
#' that actually have a namelist block, in AED's execution order.
aed_models <- function(aed, required) {
  have <- trimws(unlist(strsplit(as.character(aed[["aed_models"]][["models"]]),
                                 ",", fixed = TRUE)))
  want <- trimws(as.character(required))
  no_block <- setdiff(want, names(aed))
  if (length(no_block) > 0)
    warning("aed.nml has no namelist block for ",
            paste(no_block, collapse = ", "),
            " - left inactive (AED aborts on a module with no &block).",
            call. = FALSE)
  mods <- union(have, setdiff(want, no_block))
  ord <- c("aed_sedflux", "aed_tracer", "aed_noncohesive", "aed_oxygen",
           "aed_carbon", "aed_silica", "aed_nitrogen", "aed_phosphorus",
           "aed_organic_matter", "aed_phytoplankton", "aed_zooplankton",
           "aed_macrophyte", "aed_totals")
  c(ord[ord %in% mods], setdiff(mods, ord))
}

#' Settle the switch table against a built aed.nml / aed_phyto_pars.csv.
#'
#' Zone counts, num_phytos/the_phytos/settling and dbase are properties of
#' this build rather than constants, so they are derived here and the table's
#' literal is ignored.
#'
#' @return list(aed, phyto, report).
resolve_switches <- function(aed, phyto, switches, groups, n_zones,
                             choices = SWITCH_CHOICE, skip = SWITCH_SKIP,
                             strict = STRICT_TABLES) {

  pcol <- phyto_cols(phyto, groups)
  rep_rows <- list()
  note <- function(key, status, value = "") {
    rep_rows[[length(rep_rows) + 1L]] <<-
      data.frame(key = key, status = status,
                 value = paste(format(value, trim = TRUE), collapse = ", "),
                 stringsAsFactors = FALSE)
  }

  derived <- list(
    "aed_sed_const2d/n_zones"      = n_zones,
    "aed_sed_const2d/active_zones" = seq_len(n_zones),
    "aed_phytoplankton/num_phytos" = length(groups),
    "aed_phytoplankton/the_phytos" = pcol,
    "aed_phytoplankton/settling"   =
      rep(as.numeric(choices[["aed_phytoplankton/settling"]]), length(groups)),
    # the table gives the bare filename; GLM needs the path GLM was built with
    "aed_phytoplankton/dbase"      = aed[["aed_phytoplankton"]][["dbase"]]
  )

  for (i in seq_len(nrow(switches))) {
    key <- switches$key[i]
    if (key %in% skip) { note(key, "skipped"); next }

    if (key %in% names(derived)) {
      val <- derived[[key]]; status <- "derived"
    } else if (key %in% names(choices)) {
      val <- choices[[key]]; status <- "chosen"
    } else if (switch_open(switches$value_txt[i])) {
      msg <- paste0("Switch ", key, " has an open value (\"",
                    switches$value_txt[i], "\"); add it to SWITCH_CHOICE.")
      if (strict) stop(msg)
      warning(msg, call. = FALSE); note(key, "unresolved"); next
    } else {
      val <- switch_value(switches$value_txt[i]); status <- "applied"
    }

    if (identical(switches$file[i], "aed.nml")) {
      if (identical(key, "models")) {
        val <- aed_models(aed, val); status <- "derived"
      }
      blk <- if (grepl("/", key, fixed = TRUE)) sub("/.*$", "", key)
      else aed_block_of(aed, key)
      nm <- tolower(sub("^.*/", "", key))
      if (is.na(blk) || is.null(aed[[blk]])) { note(key, "missing"); next }
      hit <- match(nm, tolower(names(aed[[blk]])))
      if (is.na(hit)) { note(key, "missing"); next }
      aed[[blk]][[names(aed[[blk]])[hit]]] <- val
      note(key, status, val)

    } else if (identical(switches$file[i], "aed_phyto_pars.csv")) {
      p <- sub("^phyto/", "", key)
      r <- match(p, phyto[[1]])
      if (is.na(r)) { note(key, "missing"); next }
      only <- PHYTO_SWITCH_GROUPS[[p]]
      for (g in groups) {
        phyto[[g]][r] <- if (is.null(only) || g %in% only) val[1] else 0
      }
      note(key, status,
           if (is.null(only)) val[1] else paste0(val[1], " (", paste(only, collapse = "/"), ")"))

    } else {
      note(key, "missing")
    }
  }

  report <- do.call(rbind, rep_rows)
  bad <- report$key[report$status == "missing"]
  if (length(bad) > 0) {
    msg <- paste0("Switches with no matching key in the built model: ",
                  paste(bad, collapse = ", "))
    if (strict) stop(msg)
    warning(msg, call. = FALSE)
  }

  list(aed = aed, phyto = phyto, report = report)
}

#' Apply the switch table to a built model.
#'
#' Writes through the aeme object's configuration, not straight to disk: every
#' later call that persists the configuration (apply_trim_output() inside
#' calib_aeme() and sa_aeme(), for one) re-writes aed.nml and
#' aed_phyto_pars.csv from that object, so a change made only on disk would be
#' silently reverted the moment calibration starts.
apply_switches <- function(aeme, path, switches, groups, n_zones, ...) {
  cfg <- AEME::configuration(aeme)
  aed <- cfg[[MODEL]][["bgc"]][["aed"]]
  phyto <- cfg[[MODEL]][["bgc"]][["aed_phyto_pars"]]
  if (is.null(aed) || is.null(phyto))
    stop("apply_switches(): no AED configuration on this build - use_bgc = TRUE?")

  res <- resolve_switches(aed = aed, phyto = phyto, switches = switches,
                          groups = groups, n_zones = n_zones, ...)

  cfg[[MODEL]][["bgc"]][["aed"]] <- res$aed
  cfg[[MODEL]][["bgc"]][["aed_phyto_pars"]] <- res$phyto
  AEME::configuration(aeme) <- cfg
  AEME::write_configuration(aeme = aeme, model = MODEL, path = path,
                            include_boundary = FALSE)

  list(aeme = aeme, report = res$report)
}

#' The calibration parameter table, normalised for AEME.
read_param_table <- function(file, model = MODEL) {
  raw <- utils::read.csv(file, stringsAsFactors = FALSE)
  need <- c(PCOLS, "module", "var_sim")
  miss <- setdiff(need, names(raw))
  if (length(miss) > 0)
    stop("Missing column(s) in ", file, ": ", paste(miss, collapse = ", "))
  raw <- raw[raw$model == model, , drop = FALSE]

  # aed_phyto_pars.csv/aed_zoop_pars.csv rows are keyed on that file's
  # p_name column, and input_model_parameters() matches it exactly - the
  # "phyto/"/"zoop/" qualifier has to come off or the write is a silent
  # no-op (confirmed 2026-09-17: zoop/ rows written through unstripped
  # never reached the built aed_zoop_pars.csv at all, no error raised).
  ph <- raw$file == "aed_phyto_pars.csv"
  raw$name[ph] <- sub("^phyto/", "", raw$name[ph])
  zp <- raw$file == "aed_zoop_pars.csv"
  raw$name[zp] <- sub("^zoop/", "", raw$name[zp])

  hit <- match(raw$name, names(PARAM_ALIAS))
  raw$name[!is.na(hit)] <- unname(PARAM_ALIAS[hit[!is.na(hit)]])

  # var_sim is a "|" list in AED's variable names; calibration works in AEME's.
  raw$vars <- lapply(strsplit(raw$var_sim, "|", fixed = TRUE),
                     function(v) aeme_var(trimws(v)))

  raw$group[!nzchar(raw$group)] <- NA_character_
  raw$index <- suppressWarnings(as.numeric(raw$index))
  raw$model <- model
  raw
}

#' Zoned parameters are written to the model as an ARRAY with one value per
#' zone: the table carries one row per name, so fan it out. A single row where
#' the model expects three produces a short array and the model dies a few
#' hundred days in, with no warning.
expand_zones <- function(p, n) {
  zoned <- p$file == "aed.nml" & grepl("^aed_sed_const2d/", p$name)
  if (!any(zoned)) return(p)
  base <- p[zoned, , drop = FALSE]
  rbind(p[!zoned, , drop = FALSE],
        do.call(rbind, lapply(seq_len(n), function(i) {
          d <- base; d$index <- i; d
        })))
}

#' Phytoplankton rows the 3 dp write-out cannot resolve.
phyto_unresolved <- function(p, digits = PHYTO_CSV_DIGITS,
                             min_steps = PHYTO_MIN_STEPS) {
  p$file == "aed_phyto_pars.csv" & (p$max - p$min) < min_steps * 10^(-digits)
}

#' Template values the aed_phyto_pars.csv round-trip already damages, whether
#' or not they are being calibrated. Anything listed here is silently changed
#' every time the configuration is written.
phyto_precision_report <- function(phyto, digits = PHYTO_CSV_DIGITS) {
  nums <- names(phyto)[vapply(phyto, is.numeric, logical(1))]
  out <- lapply(nums, function(cn) {
    v <- phyto[[cn]]
    ch <- which(!is.na(v) & v != round(v, digits))
    if (length(ch) == 0) return(NULL)
    data.frame(p_name = phyto[[1]][ch], group = cn, value = v[ch],
               written = round(v[ch], digits), stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

#' Structural localizer from the table's var_sim column: which parameters are
#' allowed to respond to which target.
prior_localizer <- function(p, vars_sim) {
  nf <- encode_param(p$group, p$name, p$index)
  out <- lapply(vars_sim, function(v)
    unique(nf[vapply(p$vars, function(x) v %in% x, logical(1))]))
  names(out) <- vars_sim
  out[vapply(out, length, integer(1)) > 0]
}

#' Shipped parameter rows, by name.
shipped <- function(nms) as.data.frame(POOL[POOL$name %in% nms, ])

#' A parameter row built from scratch, for anything the shipped tables do not
#' carry. Bounds default to +/- `fac` of the current value.
prow <- function(file, name, value, min = NULL, max = NULL, fac = 0.5,
                 group = NA_character_, index = NA_real_) {
  if (is.null(min)) min <- value - abs(value) * fac
  if (is.null(max)) max <- value + abs(value) * fac
  data.frame(model = MODEL, file = file, name = name, value = value,
             min = min, max = max, group = group, index = index,
             stringsAsFactors = FALSE)
}

#' Repeat a row once per sediment zone. Zoned parameters are written to the
#' model as an ARRAY with one value per zone: a single row where the model
#' expects three produces a short array and the model dies a few hundred
#' days in, with no warning. GLM's &sediment block and AED's sediment block
#' declare their zone counts separately and need not agree.
per_zone <- function(df, n) do.call(rbind, lapply(seq_len(n), function(i) {
  d <- df; d$index <- i; d
}))

#' Map each GLM sediment zone to the HYD_temp sub-region it should be
#' force-linked to in run_stage()'s pvm localizer, by physical position.
#' Zone 1 is the BOTTOM-most zone (0 to zone_heights[1] m above the lake
#' bed) and the last zone reaches the water surface (see AEME's
#' run_glm_aed_diagnostics.R "Coordinate system" comment: "zone_heights:
#' upper boundaries of sediment zones measured as height above the lake
#' BED"). So a bottom-zone sediment parameter (e.g. sed_temp_mean) should
#' influence bot_temp specifically, the top zone should influence
#' surf_temp, and any zones strictly between them fall to meta_temp -
#' mirroring where each zone actually sits under the water column, rather
#' than trusting a short Morris screen to infer it (the same reasoning
#' force_keep already exists for - see mixing/coef_mix_hyp's own force_keep
#' entry). A single zone spans the whole sediment column and physically
#' abuts the deepest water, so it links to bot_temp only.
#' @param n_zones GLM sediment zone count for this lake.
#' @param has_meta logical, does this lake's own region set actually
#'   include a meta_temp band (see temp_bands()'s `has_meta` - a lake too
#'   shallow for a distinct middle band drops it entirely, see that
#'   function's own header)? When FALSE, middle zones are split between
#'   bot_temp/surf_temp by physical proximity instead of linked to a
#'   region that doesn't exist for this lake - confirmed necessary
#'   2026-09-25: force_keep-ing a middle sediment zone to "meta_temp" for
#'   a lake whose region set never defined one left an orphan pvm_list key
#'   with no matching sub-region.
#' @returns named list, zone index (as character, "1".."n_zones") -> region
#'   name - the shape run_stage()'s force_keep expects for a per-zone entry.
sed_zone_regions <- function(n_zones, has_meta = TRUE) {
  if (n_zones <= 1) return(stats::setNames(list("bot_temp"), "1"))
  if (isTRUE(has_meta)) {
    regions <- rep("meta_temp", n_zones)
  } else {
    mid <- ceiling(n_zones / 2)
    regions <- c(rep("bot_temp", mid), rep("surf_temp", n_zones - mid))
  }
  regions[1] <- "bot_temp"
  regions[n_zones] <- "surf_temp"
  stats::setNames(as.list(regions), as.character(seq_len(n_zones)))
}

#' Depth-adaptive surf/meta/bot temperature band boundaries.
#'
#' The original stage-2/3 region definitions used FIXED absolute-metre
#' offsets from the surface/bed (surf_temp = 0-2m, meta_temp = 2.5 to
#' z-4.5m, bot_temp = z-4 to z) - tuned against, and only ever run
#' against, Rototoa (z = 26.06m). Confirmed broken the first time this
#' pipeline ran a shallower lake (2026-09-25, Tomarata/Spectacle, both
#' error "Regions surf_temp and meta_temp both target HYD_temp and
#' overlap"): for a lake shallow enough that z - 4.5 < 2.5, meta_temp's
#' own depth_range = c(2.5, z-4.5) has its bounds in the WRONG order:
#' every consumer of depth_range takes min()/max() of it (never [1]/[2]
#' directly - see e.g. .region_var_indices()/pest_obs_table()'s own depth
#' filters), so an inverted range silently renormalises to
#' c(z-4.5, 2.5) - identical to, and so colliding with, surf_temp's own
#' c(0, 2) band once max(z-4.5, 0) exceeds 0.
#'
#' Scales the same three bands proportionally to `z` instead, with floors/
#' ceilings chosen so a deep lake (z >= ~13m) reproduces the ORIGINAL
#' fixed values exactly (verified: z = 26.06 gives surf_hi = 2, bot_lo =
#' 22.06, meta = c(2.5, 21.56) - byte-identical to the hardcoded values
#' every Rototoa run this session used) while a shallow lake shrinks all
#' three bands smoothly and drops meta_temp entirely once there is under
#' 1m of column left for it, rather than emitting an inverted/overlapping
#' range.
#'
#' @param z numeric, this lake's init_depth (AEME::input(aeme)$init_depth).
#' @returns list(surf_hi, bot_lo, has_meta, meta_lo, meta_hi) - the
#'   boundary values a caller assembles into its own region list (this
#'   function does not build region lists itself, since stage 2 and stage
#'   3 attach different weights/months to the same bands).
temp_bands <- function(z) {
  surf_hi <- min(2, 0.3 * z)
  bot_lo  <- max(z - 4, 0.7 * z)
  meta_lo <- surf_hi + 0.5
  meta_hi <- bot_lo - 0.5
  has_meta <- (meta_hi - meta_lo) >= 0.5
  list(surf_hi = surf_hi, bot_lo = bot_lo, has_meta = has_meta,
      meta_lo = meta_lo, meta_hi = meta_hi)
}

#' Repeat a row once per phytoplankton group. For aed_phyto_pars.csv the
#' `group` column selects the column of the file and `name` selects the row.
per_phyto <- function(df, groups) do.call(rbind, lapply(groups, function(g) {
  d <- df; d$group <- g; d
}))

add_name_full <- function(p) {
  # Guard against nrow(p) == 0: encode_param()/paste0() does NOT recycle to
  # length 0 just because group/name/index are empty - the literal "/","[",
  # "]" separators are themselves length-1, so paste0() recycles to length
  # 1, producing a phantom one-row "/[]" name_full on an otherwise-empty
  # frame (confirmed directly, 2026-09-30: this broke rbind() downstream
  # with "replacement has 1 row, data has 0" for every lake without
  # sediment flux data, since sedflux_frozen is legitimately 0 rows for
  # them). A caller building an optional frozen/adjustable set that can be
  # empty (e.g. freeze_sediment_flux_data() for a lake with no measured
  # values) needs add_name_full() to just pass an empty frame through.
  if (nrow(p) == 0) {
    p$name_full <- character(0)
    return(p)
  }
  p$name_full <- encode_param(p$group, p$name, p$index)
  p
}

#' Freeze aed_sed_const2d/aed_oxygen/aed_nitrogen/aed_phosphorus sediment
#' flux parameters to real measured values, for the small subset of lakes
#' that have them (data/processed/sediment_flux_params.csv - Fsed_oxy/
#' Ksed_oxy/theta_sed_oxy/Fsed_frp/Ksed_frp/Fsed_amm/Ksed_amm, sourced
#' externally, NOT the generic per-lake "Tier 2 obs_adjusted" estimate
#' fresh_build() derives from water-column concentrations for every lake).
#'
#' 2026-09-30: discovered this file existed but was wired into nothing -
#' every lake, including the 3 (Tomarata/Spectacle/Slipper) it actually
#' covers, was instead free-calibrating fsed_amm/fsed_frp against
#' bottom-water nutrient concentrations that are themselves heavily
#' censored/sparse at most lakes (the same data quality problem behind
#' this session's worst NIT_amm/PHS_frp fits). For a lake with a REAL
#' measured flux value, inferring it indirectly from noisy downstream
#' concentration data throws away better information sitting unused.
#'
#' Freezes (min = max = value) rather than narrow()ing: these are field
#' measurements, not a posterior from a previous calibration stage - there
#' is no "uncertainty band from an ensemble" to narrow to, and treating a
#' measurement as adjustable-within-bounds would let PEST talk itself out
#' of a real observation to chase a different target's residual (the
#' exact failure mode found in this session's sed_temp_mean-vs-NCS_ss1
#' investigation).
#'
#' Fsed_* are per-GLM-zone in ptab (expand_zones() already duplicated them
#' with index = 1..n_zones) - the single measured value is applied
#' identically to every zone (no zone-resolved measurement exists to do
#' otherwise; a documented simplification, not an assumption hidden in
#' code). Ksed_*/theta_sed_oxy are lake-wide (index = NA), matching the
#' measurement directly. A lake/parameter combination with NA in the
#' source CSV (e.g. Tomarata's Fsed_frp/Ksed_frp) is left alone entirely -
#' still free-calibrated, exactly as before - rather than freezing to a
#' missing value.
#'
#' @param ptab the stage's adjustable parameter table (PCOLS format,
#'   already expand_zones()'d).
#' @param lake_name the `lake` column value in sediment_flux_params.csv
#'   (e.g. "Tomarata") - NOT the LID/tag used elsewhere in this pipeline,
#'   since that CSV was authored independently with plain lake names.
#' @param csv_path path to sediment_flux_params.csv.
#' @returns list(adjustable = ptab with frozen rows removed,
#'   frozen = PCOLS-format data.frame of the newly-frozen rows, possibly
#'   0 rows if this lake has no entry or every value is NA).
freeze_sediment_flux_data <- function(ptab, lake_name,
                                      csv_path = file.path("data", "processed",
                                                           "sediment_flux_params.csv")) {
  empty_frozen <- ptab[0, , drop = FALSE]
  if (!file.exists(csv_path)) return(list(adjustable = ptab, frozen = empty_frozen))
  sfp <- utils::read.csv(csv_path, stringsAsFactors = FALSE)
  sfp <- sfp[sfp$lake == lake_name & !is.na(sfp$value), ]
  if (!nrow(sfp)) return(list(adjustable = ptab, frozen = empty_frozen))

  par_map <- c(
    Fsed_oxy = "aed_sed_const2d/fsed_oxy", Ksed_oxy = "aed_oxygen/ksed_oxy",
    theta_sed_oxy = "aed_oxygen/theta_sed_oxy",
    Fsed_frp = "aed_sed_const2d/fsed_frp", Ksed_frp = "aed_phosphorus/ksed_frp",
    Fsed_amm = "aed_sed_const2d/fsed_amm", Ksed_amm = "aed_nitrogen/ksed_amm",
    Fsed_nit = "aed_sed_const2d/fsed_nit", Ksed_nit = "aed_nitrogen/ksed_nit"
  )
  sfp$aed_name <- par_map[sfp$par]
  sfp <- sfp[!is.na(sfp$aed_name), ]

  frozen_rows <- list()
  keep <- rep(TRUE, nrow(ptab))
  for (i in seq_len(nrow(sfp))) {
    hit <- ptab$name == sfp$aed_name[i]
    if (!any(hit)) next
    rows <- ptab[hit, , drop = FALSE]
    rows$value <- sfp$value[i]
    rows$min <- sfp$value[i]
    rows$max <- sfp$value[i]
    frozen_rows[[length(frozen_rows) + 1]] <- rows
    keep[hit] <- FALSE
    message(sprintf("  freeze_sediment_flux_data: %s -> %.4g (measured, %s)",
                    sfp$aed_name[i], sfp$value[i], lake_name))
  }
  frozen <- if (length(frozen_rows)) do.call(rbind, frozen_rows) else empty_frozen
  list(adjustable = ptab[keep, , drop = FALSE], frozen = frozen)
}

#' Checkpoint each stage's result to `<CHECKPOINT_ROOT>/sN.rds`, so a lost
#' session or a failure in a later step can be resumed without re-running
#' the solver. `<CHECKPOINT_ROOT>/sN.rds` sits outside anything
#' fresh_build() unlink()s. The RDS complements the on-disk results DB +
#' kept PEST directory (under ROOT, not CHECKPOINT_ROOT - see the ROOT/
#' CHECKPOINT_ROOT split comment in pest_calib_v3.R); it does not replace
#' them, and re-entry still needs that same working directory.
stage_rds <- function(n, aeme) {
fname <- paste0("s", n, "_", basename(AEME::get_lake_dir(aeme)), ".rds")
    file.path(CHECKPOINT_ROOT, fname)
}
run_or_load <- function(n, aeme, expr) {
  f <- stage_rds(n, aeme)
  if (file.exists(f)) {
    message("stage ", n, ": resumed from ", f)
    return(readRDS(f))
  }
  res <- force(expr)
  saveRDS(res, f)
  # Clean up the raw PEST directory only AFTER the lightweight checkpoint
  # has been successfully written to disk - i.e. only on genuine success,
  # never on a crash (a crashed run never reaches this line, so its
  # directory - the auto-versioned cal_s<stage>_vN one - survives intact
  # for a future restart_from, exactly as intended). Once the checkpoint
  # exists, the per-iteration ensembles/netCDFs in the raw directory (the
  # actual disk-space cost, per-run into the GB range for a BGC stage) are
  # redundant with what's already captured in `res$best`/`res$phi`/etc. -
  # keeping them around after success only recreates the disk-space
  # crisis from earlier this session for no ongoing benefit.
  pest_dir <- tryCatch(res$cal_ctrl$pest_dir, error = function(e) NULL)
  if (!is.null(pest_dir) && dir.exists(pest_dir)) {
    ok <- tryCatch({ unlink(pest_dir, recursive = TRUE); TRUE },
                   error = function(e) FALSE)
    if (ok) message("  stage ", n, ": checkpoint saved - removed raw PEST directory ", pest_dir)
    else message("  stage ", n, ": checkpoint saved, but could not remove ", pest_dir, " (left in place)")
  }
  res
}

#' NSE/KGE recomputed with censored (detection-floor) observations excluded.
#'
#' AEME::assess_aeme() pools every observation for a variable, including
#' ones pinned at a lab detection limit - for a heavily censored variable
#' (e.g. Rototoa's PHS_frp: 75% of 171 raw observations sit at exactly
#' 0.001 g/m3) this makes NSE's variance denominator near-zero, so a small
#' simulation bias produces an enormous, largely meaningless negative NSE
#' (-11.7, -29, ...) that says more about the metric than the fit -
#' confirmed directly this session (2026-09-28/29 phosphate-regression and
#' cross-lake-comparison diagnostics). This does NOT change what PEST
#' calibrates against (still the ad hoc down-weight/hard-drop in
#' resolve_targets()) - it only recomputes the DIAGNOSTIC metric used to
#' judge fit quality after the fact, so a cross-lake comparison isn't
#' dominated by a censoring artifact at whichever lake happens to have the
#' most detection-limited observations.
#'
#' A "censored" observation is one at or within `floor_tol` of its own
#' variable's minimum positive value in the simulation window - the same
#' detection-limit convention already used in diagnose_phosphate_
#' regression.R. Both the pooled (assess_aeme()-equivalent) and
#' uncensored-only NSE/KGE are returned side by side, with the censored
#' fraction reported explicitly, so the exclusion stays visible rather
#' than silently changing the number.
#'
#' @param aeme a completed AEME run (e.g. from run_aeme_param()).
#' @param model model name, e.g. "glm_aed".
#' @param var_sim character vector of var_aeme names to assess.
#' @param floor_tol observations <= floor_tol * min(obs[obs > 0]) count as
#'   censored. Default 1.01 (matches diagnose_phosphate_regression.R).
#' @param min_n minimum uncensored observations required to report an
#'   uncensored-only statistic; below this it's NA rather than a number
#'   computed from too few points to mean anything.
#' @returns data.frame, one row per var_sim: n_total, n_censored,
#'   pct_censored, nse_pooled, nse_uncensored, kge_pooled, kge_uncensored,
#'   n_uncensored.
assess_censored <- function(aeme, model, var_sim, floor_tol = 1.01, min_n = 5) {
  nse1 <- function(o, s) 1 - sum((o - s)^2) / sum((o - mean(o))^2)
  kge1 <- function(o, s) {
    r     <- suppressWarnings(stats::cor(o, s))
    alpha <- stats::sd(s) / stats::sd(o)
    beta  <- mean(s) / mean(o)
    1 - sqrt((r - 1)^2 + (alpha - 1)^2 + (beta - 1)^2)
  }
  out <- lapply(var_sim, function(v) {
    df <- tryCatch(AEME::get_var(aeme, model = model, var_sim = v,
                                 use_obs = TRUE, remove_spin_up = TRUE),
                  error = function(e) NULL)
    if (is.null(df) || !nrow(df)) return(NULL)
    df <- df[!is.na(df$obs) & !is.na(df$sim), ]
    if (!nrow(df)) return(NULL)
    pos <- df$obs[df$obs > 0]
    floor_val <- if (length(pos)) min(pos) else min(df$obs)
    censored <- df$obs <= floor_tol * floor_val
    d_clean <- df[!censored, ]
    uncensored_ok <- nrow(d_clean) >= min_n
    data.frame(
      var_aeme = v, n_total = nrow(df), n_censored = sum(censored),
      pct_censored = round(100 * sum(censored) / nrow(df), 1),
      nse_pooled = round(nse1(df$obs, df$sim), 3),
      nse_uncensored = if (uncensored_ok) round(nse1(d_clean$obs, d_clean$sim), 3) else NA,
      kge_pooled = round(kge1(df$obs, df$sim), 3),
      kge_uncensored = if (uncensored_ok) round(kge1(d_clean$obs, d_clean$sim), 3) else NA,
      n_uncensored = nrow(d_clean)
    )
  })
  dplyr::bind_rows(Filter(Negate(is.null), out))
}

#' Build and run the model into its own directory.
#'
#' Every stage - and every step within a stage - gets a fresh build. PEST
#' staging and input_model_parameters() both rewrite the lake directory in
#' place, so re-using one leaves a configuration that is no longer the one
#' that was built.
#'
#' build_aeme() copies AED's shipped templates whenever the lake directory is
#' absent, and initialise_aed() then sets the module list from the simulated
#' variables alone. The switch table is therefore re-applied after every
#' build, not once at the top.
#'
#' OUTLET_DEPTH: with the installed AEME (0.4.0) build_aeme() writes the
#' floating outlet at outl_elvs = lake_depth - 6 (its default), so every lake
#' drew its outflow from ~6 m above the bed - deep hypolimnetic water - instead
#' of skimming the surface as a natural stream outlet does. There is no
#' project-side outlet config (update_aeme.R / build_aeme have no outlet arg),
#' and nothing in the aeme object carries a "-1 m below surface" spec that a
#' rebuild honours (checked: outflows()$outflow_lvl is empty, a fresh
#' build_aeme() overwrites the object's outl_elvs with its own depth-6
#' default). flt_off_sw is TRUE, so outl_elvs is metres BELOW the CURRENT
#' surface, recomputed every timestep - the intent (2026-09-04 decision) is a
#' near-surface skim. Patched on the aeme object so every later
#' write_configuration() (apply_switches(), apply_trim_output() in
#' calib_aeme()/sa_aeme()) keeps it. Outflow VOLUME is unaffected (prescribed
#' by bcs/outflow_wbal.csv x the stage-1 outflow factor); only the exported
#' temperature/chemistry, and the lake's own hypolimnetic budget, change.
#'
#' A floating outlet crashed Te Kanae even at 0.2 m: AEME's build-time
#' validation only checks outl_elvs against the lake's STATIC crest-base
#' depth, never against how low the level actually runs, and this lake's
#' level genuinely dries to 0 in the last month of the 9-year record
#' (2023-05-28 on, confirmed by reproducing aeme_fwd.rds's forward run
#' directly - level 1-4 m for weeks, then 0). ANY positive floating depth is
#' invalid once level hits 0, so no depth value fixes this - only a FIXED
#' (non-floating) outlet does, because it doesn't recompute relative to a
#' surface that can vanish. OUTLET_OVERRIDE (by lake id, from
#' pest_calib.R) switches specific lakes to outlet_type = 1 (fixed,
#' flt_off_sw = .false.) at an absolute elevation instead of the floating
#' skim. Confirmed clean for Te Kanae (0 non-finite over the full 9-year
#' run, vs 43/1986 floating) at elev = base_elev + its stage-2 median
#' simulated level - i.e. a sill at its typical stand, which naturally stops
#' producing outflow during a drawdown instead of chasing a vanishing
#' surface. Only overridden lakes change; the rest keep the floating skim.
OUTLET_DEPTH <- 0.2      # outl_elvs: metres below the water surface (floating)

fix_outlet_depth <- function(aeme, path, depth = OUTLET_DEPTH) {
  cfg <- AEME::configuration(aeme)
  o <- cfg[[MODEL]]$hydrodynamic$outflow
  if (is.null(o) || is.null(o$outl_elvs)) return(aeme)
  n   <- length(o$outl_elvs)
  lid <- tryCatch(AEME::lake(aeme)$id, error = function(e) NA_character_)
  ov  <- if (exists("OUTLET_OVERRIDE") && !is.na(lid)) OUTLET_OVERRIDE[[lid]] else NULL

  if (!is.null(ov)) {
    old <- o$outl_elvs
    cfg[[MODEL]]$hydrodynamic$outflow$outl_elvs   <- rep(ov$elev, n)
    cfg[[MODEL]]$hydrodynamic$outflow$flt_off_sw  <- rep(FALSE, n)
    cfg[[MODEL]]$hydrodynamic$outflow$outlet_type <- rep(1L, n)
    AEME::configuration(aeme) <- cfg
    AEME::write_configuration(aeme = aeme, model = MODEL, path = path,
                              include_boundary = FALSE)
    message("  outlet OVERRIDE for ", lid, ": fixed elevation ", ov$elev,
            " (was floating ", paste(signif(old, 4), collapse = ","), ")")
    return(aeme)
  }

  old <- o$outl_elvs
  cfg[[MODEL]]$hydrodynamic$outflow$outl_elvs <- rep(depth, n)
  AEME::configuration(aeme) <- cfg
  AEME::write_configuration(aeme = aeme, model = MODEL, path = path,
                            include_boundary = FALSE)
  message("  outlet depth: outl_elvs ",
          paste(signif(old, 4), collapse = ","), " -> ", depth,
          " m below surface")
  aeme
}

#' Per-lake GLM &mixing namelist override, read by fix_mixing() below.
#'
#' Pupuke (a comparatively deep, ~53 m maar lake) showed a genuine GLM bug
#' under the shipped default: deep_mixing=2's energy budget is too weak (at
#' the default coef_mix_hyp=0.5) to ever fully re-entrain this depth of
#' water column once stratified, so the seasonally-oxygenated depth shrinks
#' monotonically year over year (48.9 m in 2013 -> 15.1 m by 2022 in the
#' unmodified baseline, confirmed via a 5-variant mixing-scheme experiment
#' on the default model, 2026-09-20). Switching to deep_mixing=1 (GLM's
#' simpler deep-mixing energy scheme) held the oxygenated depth rock-steady
#' at ~52 m every year with no other change; quadrupling coef_mix_hyp under
#' deep_mixing=2 gave the identical result (confirming the mechanism), but
#' deep_mixing=1 is simpler and needs no accompanying justification for an
#' inflated coefficient. Only Pupuke is overridden; every other lake keeps
#' AEME's shipped mixing defaults (frozen, not calibrated - see
#' pest_calib_v3.R's stage-2 adjustable-parameter comment).
fix_mixing <- function(aeme, path) {
  lid <- tryCatch(AEME::lake(aeme)$id, error = function(e) NA_character_)
  ov  <- if (exists("MIXING_OVERRIDE") && !is.na(lid)) MIXING_OVERRIDE[[lid]] else NULL
  if (is.null(ov)) return(aeme)

  ld <- AEME::get_lake_dir(aeme, path = path)
  nml_file <- file.path(ld, MODEL, "glm4.nml")
  nml <- AEME::read_nml(nml_file)
  old <- AEME::get_nml_value(nml, ov$name, warn = FALSE)
  nml <- AEME::set_nml(nml, arg_name = ov$name, arg_val = ov$value)
  AEME::write_nml(nml, nml_file)

  # File-only edit was not enough (found 2026-09-22): aemetools::
  # run_aeme_param()'s input_model_parameters() call regenerates glm4.nml
  # from THIS aeme OBJECT's own stored configuration on every forward run
  # - not from whatever is already on disk - so a file-only patch here
  # gets silently reverted by the very next PEST realisation (confirmed
  # directly: fix_mixing()'s own log line showed "deep_mixing 2 -> 1" at
  # build time, but the file read back "= 2" after a single
  # run_aeme_param() call). This almost certainly means the original
  # Pupuke MIXING_OVERRIDE entry never actually took effect during real
  # PEST calibration runs either - only in a one-off post-build check
  # that used the object directly before any run_aeme_param()/
  # calib_aeme()/sa_aeme() call touched it. Mirror the edit into the
  # in-memory config (hydrodynamic/mixing block) so it survives.
  cfg <- AEME::configuration(aeme)
  cfg[[MODEL]]$hydrodynamic$mixing[[ov$name]] <- ov$value
  AEME::configuration(aeme) <- cfg

  message("  mixing OVERRIDE for ", lid, ": ", ov$name, " ", old, " -> ", ov$value)
  aeme
}

#' @param switches the switch table to apply when use_bgc is TRUE - defaults
#'   to the global SWITCHES (full BGC suite). Pass PHYS_SWITCHES (models
#'   reduced to "aed_sedflux" only) for a physics-only stage that still
#'   needs GLM's sed_heat_model = 2 scheme - see the fix_mixing()/
#'   sed_heat_model comment block above stage 1 in pest_calib_v4.R for why
#'   use_bgc = FALSE alone is not equivalent (sed_heat_model silently
#'   downgrades to 1 without an active WQ module, a real, non-trivial
#'   thermal difference - confirmed directly, mean 1.98degC / max 6.9degC
#'   on Rototoa - not just a formality). Confirmed aed_sedflux alone
#'   reproduces the full-BGC run's temperature bit-for-bit (2026-09-22).
#' Sediment/particle (aed_noncohesive) defaults, corrected 2026-10-05.
#'
#' In AED a NEGATIVE w_ss is settling and a positive one is buoyancy (same convention as the phytoplankton
#' w_p). The AEME template, and therefore every Aeme object, carried w_ss = 0.5, 0.03 (buoyant), so particles
#' floated to the surface and left through the near-surface outlet; the solids calibrations compensated with
#' tau_0/epsilon. Spectacle showed it: surface solids 4,888 -> 4 mg/L with the sign flipped. Also fs (bed mass
#' fractions) summed to 2 (1, 1), doubling resuspension, and epsilon (g/m2/s) was 0.02. These are the values
#' build_aeme() writes into aed.nml (it takes them from the object's stored configuration, not from the files
#' under aeme/). PEST_NO_NC_FIX=1 restores the old defaults (control experiments only).
NC_DEFAULTS <- list(w_ss = c(-0.5, -0.03), fs = c(0.6, 0.4), epsilon = 0.0003, tau_0 = c(0.08, 0.05))
fix_noncohesive <- function(aeme, path) {
  if (nzchar(Sys.getenv("PEST_NO_NC_FIX"))) return(aeme)
  # 1. the built aed.nml (build_aeme() copies the AEME package template, not the files under aeme/)
  ld <- AEME::get_lake_dir(aeme, path = path)
  f <- file.path(ld, MODEL, "aed", "aed.nml")
  if (file.exists(f)) {
    l <- readLines(f)
    s <- grep("^&aed_noncohesive", l); e <- s + which(trimws(l[s:length(l)]) == "/")[1] - 1L
    for (nm in names(NC_DEFAULTS)) {
      i <- s + which(grepl(paste0("^ *", nm, " *="), l[s:e])) - 1L
      if (length(i) == 1) l[i] <- sprintf("   %s = %s", nm, paste(format(NC_DEFAULTS[[nm]], scientific = FALSE, trim = TRUE), collapse = ", "))
    }
    writeLines(l, f)
  }
  # 2. mirror into the object: run_aeme_param() regenerates the files from the stored configuration
  cfg <- AEME::configuration(aeme)
  nc <- cfg[[MODEL]]$bgc$aed$aed_noncohesive
  if (!is.null(nc)) {
    for (nm in names(NC_DEFAULTS)) nc[[nm]] <- NC_DEFAULTS[[nm]]
    cfg[[MODEL]]$bgc$aed$aed_noncohesive <- nc
    AEME::configuration(aeme) <- cfg
  }
  aeme
}

fresh_build <- function(tag, vars_sim, use_bgc, n_zones = NULL, switches = NULL) {
  p <- file.path(ROOT, tag)
  unlink(p, recursive = TRUE)
  dir.create(p, recursive = TRUE, showWarnings = FALSE)
  mc <- AEME::set_vars_sim(AEME::get_model_controls(use_bgc = use_bgc),
                           vars_sim = vars_sim)
  a <- AEME::build_aeme(path = p, aeme = aeme0, model = MODEL,
                        model_controls = mc, ext_elev = 5, use_bgc = use_bgc)
  a <- fix_outlet_depth(a, p)
  a <- fix_mixing(a, p)
  a <- fix_noncohesive(a, p)
  if (use_bgc) {
    if (is.null(n_zones))
      n_zones <- AEME::configuration(a)[[MODEL]]$hydrodynamic$sediment$n_zones
    if (is.null(switches)) switches <- SWITCHES
    sw <- apply_switches(aeme = a, path = p, switches = switches,
                         groups = PHYTO_GROUPS, n_zones = n_zones)
    a <- sw$aeme
    message("  switches applied to ", tag, ":")
    print(sw$report, row.names = FALSE)
  }
  a <- AEME::run_aeme(aeme = a, model = MODEL, path = p)
  list(aeme = a, path = p)
}

# ---- one stage -------------------------------------------------------------

#' Screen with Morris, select, localise, then calibrate with pestpp-ies.
#'
#' @param adjustable,frozen parameter dataframes.
#' @param targets AEME variables to calibrate against.
#' @param sub_regions named list for create_sen_control(); names must contain
#'   an underscore.
#' @param screen FALSE skips the screen - right for a handful of parameters,
#'   and required for a water-level-only stage, which the SA path cannot
#'   index (it has no gridded variable).
#' @param rebuild function returning a fresh list(aeme, path) for the
#'   calibration step.
#' @param prior_pvm named list (target -> name_full) declaring which
#'   parameters may respond to which target, from the table's var_sim
#'   column. Intersected with the Morris result when screening, used on its
#'   own when not.
#' @param weights named vector for calib_aeme(); defaults to 1 throughout.
#' Delete the agent_NN working copies under a finished PEST run directory.
#'
#' PANTHER gives every agent its own copy of the whole lake directory: 27 of
#' them account for 10.6 GB of an 11 GB stage-3 tree. Everything the readers
#' need - the .pst, the .rec, and the per-iteration .par.csv / .obs.csv
#' ensembles - lives in the master directory (440 MB). Keeping the agents
#' would make per-lake PEST directories cost ~15 GB a lake instead of ~1 GB.
prune_pest_agents <- function(dir, keep = KEEP_PEST_AGENTS) {
  if (isTRUE(keep) || !dir.exists(dir)) return(invisible(0L))
  ag <- list.dirs(dir, recursive = TRUE, full.names = TRUE)
  ag <- ag[grepl("agent_[0-9]+$", ag)]
  if (length(ag) > 0) unlink(ag, recursive = TRUE, force = TRUE)
  invisible(length(ag))
}

# ============================================================================
# Adaptive targets - constrain only the variables a lake actually has data for
# ============================================================================

#' Which of a candidate variable set can this lake's data support as a
#' calibration target, and how hard.
#'
#' calib_aeme() hard-errors on a target with no observation inside the
#' (per-lake trimmed) simulation window, and a target whose observations are
#' all at the analytical detection limit makes KGE degenerate (its correlation
#' term is ~0/0), which lets one variable's phi term run away with the whole
#' objective (a lake's near-zero nitrate did exactly this: stage-3 phi 283 vs
#' ~12 elsewhere). So each candidate is checked against this lake's own
#' observations and either dropped or given a weight that reflects how much
#' signal it carries.
#'
#' @param aeme built aeme object (its observations()$lake table is read).
#' @param candidates character vector of AEME variable names to consider.
#' @param period aeme_sim_period (list with $start / $stop) - the window
#'   calib_aeme() will actually score.
#' @param sub_region_defs named list of create_sen_control() sub-regions; each
#'   entry is list(var=, month=, depth_range=).
#' @param base_weights named numeric; per-variable weight ceiling (default 1).
#' @param min_obs a variable with fewer than this many in-window observations
#'   is dropped (default 8).
#' Data-density weight cap: `n` observations in-window scale linearly to a
#' weight of `n/25`, floored so a thin-but-surviving target still counts for
#' something and capped at `cap` (a hand-set importance ceiling). Factored
#' out of resolve_targets() (below) so stage 2's whole-lake targets - which
#' have no BGC sub-region structure to run through resolve_targets() itself -
#' can share the exact same "how much data backs this target" formula
#' instead of a second, drifting reimplementation.
#'
#' @param n in-window observation count.
#' @param cap importance ceiling (default 1).
#' @param floor minimum weight for a target that still has some data (default 0.25).
n_scale_weight <- function(n, cap = 1, floor = 0.25) max(floor, min(cap, n / 25))

#' @return list(vars, sub_regions, weights) filtered/scaled for this lake.
resolve_targets <- function(aeme, candidates, period, sub_region_defs,
                            base_weights = NULL, min_obs = 8) {
  o  <- AEME::observations(aeme)$lake
  t0 <- as.Date(period$start); t1 <- as.Date(period$stop)
  o$.d <- as.Date(o$Date)
  o$.dep <- if (!is.null(o$depth_mid)) o$depth_mid
            else if (!is.null(o$depth_from)) (o$depth_from + o$depth_to) / 2
            else if (!is.null(o$depth)) o$depth else NA_real_

  keep_w <- setNames(rep(NA_real_, length(candidates)), candidates)
  for (v in candidates) {
    cap <- if (!is.null(base_weights) && v %in% names(base_weights))
      base_weights[[v]] else 1
    ov <- o[o$var_aeme == v & !is.na(o$value) & o$.d >= t0 & o$.d <= t1, ]
    n  <- nrow(ov)
    if (n < min_obs) {                                       # hard drop: no data
      message(sprintf("  resolve_targets: %s dropped - %d obs in window (< %d)",
                      v, n, min_obs)); next
    }
    val <- ov$value
    if (stats::sd(val, na.rm = TRUE) <= 1e-6) {              # hard drop: constant
      message("  resolve_targets: ", v, " dropped - observations are constant"); next
    }
    lo <- min(val[val > 0], na.rm = TRUE)                    # ~ detection limit
    at_floor <- mean(val <= 1.01 * lo, na.rm = TRUE)
    if (at_floor > 0.85) {                                   # hard drop: censored
      message(sprintf("  resolve_targets: %s dropped - %.0f%% of obs at the detection floor",
                      v, 100 * at_floor)); next
    }
    censored <- at_floor > 0.4 || diff(range(val, na.rm = TRUE)) < 3 * lo
    keep_w[v] <- if (censored) {
      message(sprintf("  resolve_targets: %s down-weighted to a nudge - %.0f%% censored",
                      v, 100 * at_floor))
      min(cap, 0.3)
    } else {
      n_scale_weight(n, cap = cap)                           # weight scales with data
    }
  }
  vars <- names(keep_w)[!is.na(keep_w)]

  # keep a sub-region only if its variable survived, it has some observation
  # inside its own depth x month box (else the SA metric is undefined), AND
  # those observations aren't themselves degenerate (constant/near-constant)
  # WITHIN this narrower box - even a variable that passed the LAKE-WIDE
  # constant-value check above (line ~1320) can still have a sub-region
  # (a specific month x depth slice) where every value happens to be
  # identical, e.g. a detection-floor value that only shows up in summer.
  # Confirmed as a real, not hypothetical, failure mode 2026-10-01: Te
  # Kanae's surf_amm sub-region (summer, 0-2m) had all 8 NIT_amm
  # observations exactly 0.005 even though the lake-wide NIT_amm record
  # was NOT constant (so the line-1320 guard never triggered) - PEST++'s
  # own weight-from-variance calculation for that degenerate sub-region
  # then produced a weight around 5e17, taking phi to ~1e30 and making the
  # whole stage-4 objective meaningless. Same 1e-6 threshold as the
  # lake-wide check, just applied to the sub-region's own observation set.
  sr <- sub_region_defs[vapply(sub_region_defs, function(s) {
    if (!s$var %in% vars) return(FALSE)
    m  <- as.integer(format(o$.d, "%m"))
    hit <- o$var_aeme == s$var & !is.na(o$value) & o$.d >= t0 & o$.d <= t1 &
      m %in% s$month
    if (!all(is.na(o$.dep)) && !is.null(s$depth_range))
      hit <- hit & (is.na(o$.dep) |
                    (o$.dep >= min(s$depth_range) & o$.dep <= max(s$depth_range)))
    if (sum(hit, na.rm = TRUE) < 3) return(FALSE)
    if (stats::sd(o$value[hit], na.rm = TRUE) <= 1e-6) {
      message("  resolve_targets: sub-region for ", s$var,
             " dropped - observations within this month/depth box are constant")
      return(FALSE)
    }
    # Second, more precise pass: the raw date/depth filter above can still
    # pass a sub-region whose variance comes ENTIRELY from observations the
    # model doesn't actually simulate (confirmed on Te Kanae's own
    # surf_amm - 9 raw candidate rows had real variance, sd=0.24, but only
    # 8 of them survive get_var()'s own model-simulated-period alignment,
    # and those 8 happen to be exactly the constant 0.005 subset). Re-check
    # variance on the EXACT obs set get_var(remove_spin_up=TRUE) returns -
    # the same alignment aemetools' own pest_obs_table() uses - not just
    # the raw date/depth filter above.
    gv <- tryCatch(AEME::get_var(aeme, model = MODEL, var_sim = s$var,
                                 use_obs = TRUE, remove_spin_up = TRUE),
                  error = function(e) NULL)
    if (!is.null(gv) && nrow(gv) > 0) {
      gm <- as.integer(format(as.Date(gv$Date), "%m"))
      ghit <- !is.na(gv$obs) & gm %in% s$month
      if (!is.null(s$depth_range) && !is.null(gv$depth) && !all(is.na(gv$depth)))
        ghit <- ghit & (is.na(gv$depth) |
                        (gv$depth >= min(s$depth_range) & gv$depth <= max(s$depth_range)))
      if (sum(ghit, na.rm = TRUE) >= 3 &&
          stats::sd(gv$obs[ghit], na.rm = TRUE) <= 1e-6) {
        message("  resolve_targets: sub-region for ", s$var,
               " dropped - observations are constant once aligned to the",
               " model's own simulated period")
        return(FALSE)
      }
    }
    TRUE
  }, logical(1))]

  list(vars = vars,
       sub_regions = sr,
       weights = keep_w[vars])
}

#' Data-density weight caps for stage 2's whole-lake targets.
#'
#' Stage 2 has no BGC sub-region/depth-month box structure and its targets
#' live across two different observation tables (LKE_lvlwtr in
#' observations()$level, the rest in $lake), so it can't call
#' resolve_targets() directly - this is the same n_scale_weight() formula,
#' applied without resolve_targets()'s hard-drop: stage 2 declares all of
#' `vars` unconditionally (calib_aeme()'s sub_regions list references every
#' one of them), so a target thin on data still gets a target, just capped
#' at the floor rather than removed.
#'
#' Used ALONGSIDE balance_weights()'s baseline-bias equalisation, not
#' instead of it: n_scale_weight() answers "how much data backs this
#' target", balance_weights() answers "is its baseline phi share
#' proportionate" - this supplies the `cap` balance_weights() is handed.
#'
#' @param aeme built aeme object for stage 2 (pre- or post-forward-run;
#'   only its observations() are read).
#' @param vars character vector of stage-2 target variable names.
#' @param period aeme_sim_period (list with $start / $stop).
#' @param max_cap named numeric; per-variable importance ceiling.
#' @param min_obs below this, the cap is floored (not dropped - see above)
#'   and a warning is logged.
#' @return named numeric, one cap per var in `vars`.
stage2_n_caps <- function(aeme, vars, period, max_cap, min_obs = 8) {
  t0 <- as.Date(period$start); t1 <- as.Date(period$stop)
  obs <- AEME::observations(aeme)
  caps <- vapply(vars, function(v) {
    tbl <- if (identical(v, "LKE_lvlwtr")) obs$level else obs$lake
    if (is.null(tbl) || !nrow(tbl)) return(0.25)
    d <- as.Date(tbl$Date)
    n <- sum(tbl$var_aeme == v & !is.na(tbl$value) & d >= t0 & d <= t1, na.rm = TRUE)
    cap <- if (v %in% names(max_cap)) max_cap[[v]] else 1
    if (n < min_obs) {
      message(sprintf("  stage2_n_caps: %s has only %d obs in window (< %d) - ",
                      v, n, min_obs),
              "flooring its cap rather than dropping it (stage 2 needs it as a declared target)")
      return(0.25)
    }
    n_scale_weight(n, cap = cap)
  }, numeric(1))
  names(caps) <- vars
  caps
}

# ============================================================================
# Using the failed forward runs: size the iES ensemble to the feasible fraction
# ============================================================================

#' Estimate what fraction of a wide biogeochemical prior GLM-AED can actually
#' integrate, from evidence already on disk.
#'
#' 60-80% of independent prior draws for a ~90-parameter BGC stage land on a
#' combination the model cannot integrate (typically a phytoplankton group
#' that blooms and drives a state negative); the forward run then writes no
#' output and pestpp-ies loses that realisation before the first update, so the
#' posterior has ended up with as few as 5-13 members. Rather than fight the
#' crash rate, measure it and oversample the prior so the *surviving* ensemble
#' is large enough. Order of preference for the estimate:
#'   1. a previous iES run's .rec  ("N ... runs failed during evaluation of the
#'      initial parameter ensemble") - the cleanest signal;
#'   2. the Morris screen's own run table (fraction of evaluations that
#'      returned a finite metric);
#'   3. a conservative default.
feasible_fraction <- function(cal_dir = NULL, sen = NULL, default = 0.35) {
  rec <- if (!is.null(cal_dir)) list.files(cal_dir, "\\.rec$", recursive = TRUE,
                                           full.names = TRUE) else character()
  for (f in rec) {
    ln <- tryCatch(readLines(f, warn = FALSE), error = function(e) character())
    hit <- grep("runs failed during evaluation of the initial parameter ensemble",
                ln, value = TRUE)
    tot <- grep("drawing parameter realizations:", ln, value = TRUE)
    if (length(hit) && length(tot)) {
      nfail <- as.numeric(sub(".*?([0-9]+) .*", "\\1", hit[1]))
      ndraw <- as.numeric(sub(".*: *([0-9]+).*", "\\1", tot[1]))
      if (is.finite(nfail) && is.finite(ndraw) && ndraw > 0)
        return(max(0.1, min(1, 1 - nfail / ndraw)))
    }
  }
  est <- tryCatch({
    if (is.null(sen) || is.null(sen$runs)) NA_real_ else {
      r <- sen$runs
      key <- paste(r$gen, r$run, sep = "/")
      ok <- tapply(r$fit_value, key,
                   function(x) any(is.finite(x) & abs(x) < 900))
      if (length(ok)) max(0.1, min(1, mean(ok, na.rm = TRUE))) else NA_real_
    }
  }, error = function(e) NA_real_)
  if (is.finite(est)) est else default
}

#' iES draw size: grow ies_num_reals so ~`target` realisations survive the
#' initial forward-run cull, bounded by [floor, cap].
adaptive_num_reals <- function(feas, target, floor_n, cap_n) {
  n <- ceiling(target / max(0.1, feas))
  as.integer(min(cap_n, max(floor_n, n)))
}

#' The single realisation with the lowest TOTAL (weighted) phi at the
#' final iteration - i.e. an actual, internally-consistent parameter
#' vector that was evaluated together in one forward run.
#'
#' NOT aemetools::get_best_params(): that function picks each parameter's
#' value INDEPENDENTLY, taking whichever row (across every realisation and
#' every iteration) had the best per-parameter `fit_value` for that
#' parameter's own linked target. That is a per-parameter selection, not a
#' per-realisation one - it can and does assemble a Frankenstein parameter
#' set that was never run together, and it is largely blind to the
#' `weights` argument actually used to solve this stage (each parameter is
#' still scored against its own target's fit history regardless of how the
#' stage weighted that target overall). Confirmed 2026-09-21/22 on
#' Rototoa's merged stage 1: across three runs with markedly different
#' LKE_lvlwtr weights (0.375, 0.5, 1.0), get_best_params() returned
#' numerically IDENTICAL parameter values (outflow, MET_wndspd, rh_factor,
#' ...) every time, while the run's own aeme.phi.group.csv showed the
#' actual best realisation's LKE_lvlwtr phi contribution moving by three
#' orders of magnitude (~1854 -> ~0.6) as the weight increased - i.e. the
#' calibration WAS working, get_best_params() just wasn't reporting it.
#' Directly checked: the true best realisation (matching cal$phi$min
#' exactly) had outflow=0.904, MET_wndspd=1.039 - nothing like
#' get_best_params()'s 1.199 / 1.167.
#'
#' Reads aemetools::read_pest_phi_group() for EVERY iteration's
#' per-realisation, per-observation-group phi (already reflects the
#' stage's actual weights, since pestpp-ies computes it that way), sums
#' across groups to get each realisation's true total phi within its own
#' iteration, and picks the best (realisation, iteration) pair BY MINIMUM
#' PHI ACROSS THE WHOLE RUN - not just the last iteration. iES culls
#' high-phi realisations and shrinks parameter variance every iteration
#' (see NOPTMAX's own comment in pest_calib_v4.R/v6.R), which improves fit
#' up to a point and then over-narrows: confirmed directly on a real run
#' (Rototoa stage 3, 2026-09-26/27), whose phi trajectory (11390 -> 2741 ->
#' 2458 -> 1838 -> 1547 -> 1446 -> 1197 -> 1235 -> 1374) bottomed out at
#' iteration 6, then got WORSE at 7 and 8 - the OLD version of this
#' function (`last_it <- max(pg$iteration)`, unconditional) would have
#' silently used iteration 8's demonstrably worse result instead of
#' iteration 6's actual best, with no warning. Then pulls that
#' realisation's actual parameter vector from
#' aemetools::read_pest_ensemble(..., type = "par"). `file`/`min`/`max`
#' are not carried in the ensemble file (only name_full + value), so
#' they're recovered by matching name/group/index back onto `adj` - the
#' same adjustable-parameter table this stage was built from.
#'
#' Takes `calib` (aemetools::read_calib()'s return), NOT the raw
#' `cal_ctrl`: create_pest_control()'s `pest_dir` is relative, resolved
#' against the lake directory only once PANTHER starts, so
#' .pest_locate() (which read_pest_phi_group()/read_pest_ensemble() both
#' call) cannot find it from a bare cal_ctrl after the fact - confirmed
#' directly (2026-09-22), matching that function's own error hint ("Pass
#' the object from read_calib instead"). calib carries the resolved
#' absolute path.
best_realisation_params <- function(adj, calib) {
  pg <- aemetools::read_pest_phi_group(calib)
  real_col <- if ("par_realization" %in% names(pg)) "par_realization" else "obs_realization"

  tot <- stats::aggregate(pg$phi, by = list(iteration = pg$iteration,
                                            realisation = pg[[real_col]]), FUN = sum)
  names(tot) <- c("iteration", "realisation", "phi")
  best_row <- tot[which.min(tot$phi), ]
  best_it   <- best_row$iteration
  best_real <- as.character(best_row$realisation)
  if (best_it != max(pg$iteration)) {
    per_it_min <- stats::aggregate(tot$phi, by = list(iteration = tot$iteration), FUN = min)
    message("  best_realisation_params(): best phi (", signif(best_row$phi, 4),
           ") is at iteration ", best_it, ", not the last (",
           max(pg$iteration), ") - using iteration ", best_it,
           "'s ensemble. Per-iteration best phi: ",
           paste(sprintf("%d=%.4g", per_it_min$iteration, per_it_min$x), collapse = " -> "))
  }

  ens <- aemetools::read_pest_ensemble(calib, iteration = best_it, type = "par")
  b <- ens[ens$realisation == best_real, c("name_full", "value")]
  dec <- aemetools:::decode_param_full(b$name_full)
  b <- cbind(dec, value = b$value)

  ref <- unique(adj[, c("file", "name", "group", "index", "min", "max")])
  out <- merge(b, ref, by = c("name", "group", "index"), all.x = TRUE)
  if (any(is.na(out$file)))
    stop("best_realisation_params(): ", sum(is.na(out$file)),
         " parameter(s) from the final ensemble did not match `adj` - ",
         "name/group/index decoding likely disagrees with how `adj` was built.")
  out$model <- MODEL
  out[, PCOLS]
}

#' @param force_keep named list, parameter name -> EITHER a character vector
#'   of target region names it should be linked to (applied to every row
#'   sharing that name, e.g. every zone of a zoned parameter alike), OR a
#'   named list keyed by zone index (as character, matching the parameter
#'   table's `index` column) -> region name(s), for a zoned parameter whose
#'   correct linkage genuinely differs by zone (see sed_zone_regions()).
#'   Bypasses the Morris screen (and the localizer's "linked to no target"
#'   cut) for that parameter entirely - use for a parameter whose real
#'   sensitivity is seasonal/annual and won't show up in a short screening
#'   window (Morris screens over a much shorter window than the full
#'   calibration period -
#'   see create_sen_control()'s own window). Confirmed necessary in
#'   practice 2026-09-24: Rototoa's mixing/coef_mix_hyp was Morris-dropped
#'   as "insensitive" and sat frozen at its starting value the whole
#'   calibration, while a direct manual sweep the day before had already
#'   shown it has large, real leverage on bottom-temperature variability
#'   over the FULL period - the screen's short window likely doesn't span
#'   a full annual turnover cycle. NULL (default): no change from before.
#' @param adaptive_reals logical; grow the iES ensemble size adaptively
#'   toward IES_TARGET_SURVIVORS (see the "iES draw size" comment below)
#'   instead of using the fixed N_REALS. Default `stage == 3`, matching
#'   every pipeline through v5 (stage 3 is always the many-parameter BGC
#'   stage there) - pass explicitly for a pipeline that renumbers stages
#'   (e.g. v6) so this targets whichever stage actually needs it.
#' A deterministic signature of everything a Morris screen's result actually
#' depends on: which parameters were screened (names/bounds), what was held
#' frozen alongside them (also part of the model run, so also part of the
#' result), and what targets/sub_regions the screen was scored against.
#' Used to invalidate run_stage()'s screen checkpoint automatically when any
#' of these genuinely change between runs, rather than requiring a human to
#' remember to delete a stale cache file. Not a cryptographic hash (no
#' `digest` package dependency added for this) - a deterministic string
#' built from sorted, rounded inputs is more than sufficient to catch a
#' real change; a hash collision here would require two DIFFERENT screening
#' problems producing byte-identical sorted name/bound/target strings.
digest_screen_inputs <- function(adj, frozen, targets, sub_regions) {
  fmt <- function(df) paste(sort(paste(df$name_full, signif(df$min, 6),
                                       signif(df$max, 6), sep = "=")),
                            collapse = "|")
  paste(fmt(add_name_full(adj)), fmt(add_name_full(frozen)),
       paste(sort(targets), collapse = ","),
       paste(sort(names(sub_regions %||% list())), collapse = ","),
       sep = "::")
}

run_stage <- function(stage, aeme, path, adjustable, frozen, targets,
                      sub_regions, screen = TRUE, rebuild = NULL,
                      prior_pvm = NULL, weights = NULL, force_keep = NULL,
                      stall_minutes = NULL, adaptive_reals = (stage == 3),
                      restart_from = NULL, restart_noptmax = NULL,
                      cal_dir_suffix = "", freeze_dropped = FALSE,
                      prior_par_ensemble = NULL) {

  message("\n=== stage ", stage, ": ", paste(targets, collapse = ", "), " ===")
  adj <- add_name_full(adjustable)
  # The results DB lives at the ROOT, not under the per-stage build directory.
  # fresh_build() unlink()s <ROOT>/<tag> at the start of every lake, so a DB
  # kept inside it is destroyed by the next lake - taking every simulation
  # result, phi trajectory and observation table with it. One DB at the ROOT
  # accumulates across lakes and stages, keyed by sim_id, and stays readable
  # after the run.
  fdir <- file.path(ROOT, "calib_sa")
  keep_adj <- adj
  pvm <- NULL
  sa_id <- NA_character_
  sen <- NULL
  if (is.null(weights)) weights <- set_weights(targets)

  # PEST run directories, ABSOLUTE and keyed by lake, so they survive the next
  # lake's fresh_build(). sa_aeme_pest()/calib_aeme_pest() resolve a relative
  # pest_dir against the lake directory - which lives inside the build tree
  # that gets unlink()ed - but honour an absolute one as given. Without this,
  # read_pest_ensemble(), pest_posterior_params() and plot_pest_ensemble()
  # only ever work for the lake currently being run.
  lake_name <- basename(AEME::get_lake_dir(aeme))
  dir.create(file.path(ROOT, "pest"), recursive = TRUE, showWarnings = FALSE)
  pest_root <- normalizePath(file.path(ROOT, "pest"), winslash = "/",
                             mustWork = TRUE)
  sen_dir <- file.path(pest_root, lake_name, sprintf("sen_s%d", stage))
  # cal_dir is AUTO-VERSIONED by default (cal_dir_suffix = ""): a "fresh"
  # call NEVER overwrites a previous attempt's directory, because
  # aemetools' create_pest_control(overwrite=TRUE default) will happily
  # clear whatever directory it's given - confirmed directly as a real
  # incident (2026-09-28): a restart_from-based recovery attempt's own
  # patch got silently clobbered by pest_calib_v6.R's internal
  # source("pest_funs.R"), so it ran as an ordinary fresh call and wiped
  # the crashed run's cal_s4 directory - including the iteration-0-4 files
  # a real restart needed - before the mistake was even noticed. Relying
  # on a human (or a monkey-patch) to remember a unique suffix at exactly
  # the right moment is fragile; making overwrite structurally impossible
  # is not. An explicit cal_dir_suffix still works for anyone who wants
  # manual control (e.g. a deliberately-named "_restart" directory).
  #
  # Trade-off, stated plainly: every stage/lake combination now
  # accumulates cal_s<stage>, cal_s<stage>_v2, cal_s<stage>_v3, ... across
  # repeated runs, using more disk over the project's lifetime (this
  # session already had one full disk-space crisis from accumulated
  # scratch directories) - clean up old versions once their results are
  # safely checkpointed elsewhere, the same way v1's stage-3/4 PEST
  # directories were manually backed up and pruned earlier this session.
  if (identical(cal_dir_suffix, "")) {
    base_cal_dir <- file.path(pest_root, lake_name, sprintf("cal_s%d", stage))
    cal_dir <- base_cal_dir
    v <- 2L
    while (dir.exists(cal_dir) && length(list.files(cal_dir)) > 0) {
      cal_dir <- paste0(base_cal_dir, "_v", v)
      v <- v + 1L
    }
    if (!identical(cal_dir, base_cal_dir))
      message("  cal_dir: '", base_cal_dir, "' already has files - using '",
             cal_dir, "' instead (previous attempt preserved)")
  } else {
    cal_dir <- file.path(pest_root, lake_name, sprintf("cal_s%d%s", stage, cal_dir_suffix))
  }

  if (screen) {
    # Checkpoint the Morris screen itself, separately from the stage's
    # overall checkpoint (which only exists once the FULL stage, screen +
    # calibration, finishes). The screen alone can take the better part of
    # an hour (e.g. stage 4's own 292-run screen) - without this, any
    # restart of the calibration step (restart_from, or just re-running
    # after a crash) redoes that hour of screening every time, even though
    # nothing about the screen's own inputs changed. Confirmed as a real,
    # not hypothetical, cost: a restart attempt lost ~1 hour to exactly
    # this (2026-09-28).
    #
    # Keyed by a signature of the screen's actual inputs (adjustable/
    # frozen parameter names+bounds, targets, sub_regions) rather than
    # just stage+lake, so a genuine change to what's being screened (e.g.
    # sed_temp_mean moving from carry() to narrow() between runs)
    # automatically invalidates the cache instead of silently reusing a
    # stale, mismatched screen.
    screen_sig <- digest_screen_inputs(adj, frozen, targets, sub_regions)
    screen_ckpt_dir <- if (exists("CHECKPOINT_ROOT")) CHECKPOINT_ROOT else "checkpoints"
    screen_ckpt <- file.path(screen_ckpt_dir, sprintf("screen_s%d_%s.rds", stage, lake_name))

    cached <- NULL
    if (file.exists(screen_ckpt)) {
      cached <- tryCatch(readRDS(screen_ckpt), error = function(e) NULL)
      if (!is.null(cached) && !identical(cached$sig, screen_sig)) {
        message("  screen checkpoint found but inputs changed - re-screening")
        cached <- NULL
      }
    }

    if (!is.null(cached)) {
      message("  screen: loaded from checkpoint (", screen_ckpt, ") - skipping the Morris run")
      sa_id <- cached$sa_id; sen <- cached$sen; mu <- cached$mu
    } else {
      sen_ctrl <- create_sen_control(file_dir = fdir, morris_r = MORRIS_R,
                                     ncore = NCORE, vars_sim = sub_regions,
                                     pest_dir = sen_dir,
                                     stall_minutes = stall_minutes %||% 10)
      mean_fit <- function(df) mean(df$model, na.rm = TRUE)
      sa_id <- unname(sa_aeme(
        aeme = aeme, model = MODEL, param = add_name_full(rbind(adj[, PCOLS],
                                                                frozen[, PCOLS])),
        FUN_list = stats::setNames(lapply(targets, function(v) mean_fit), targets),
        path = path, ctrl = sen_ctrl)[[1]])

      prune_pest_agents(sen_dir)
      sen <- read_sen(ctrl = sen_ctrl, sim_id = sa_id)
      mu <- sen[[sa_id]]$indices
      mu <- mu[mu$index_type == "mu_star", ]
      dir.create(screen_ckpt_dir, recursive = TRUE, showWarnings = FALSE)
      saveRDS(list(sig = screen_sig, sa_id = sa_id, sen = sen, mu = mu), screen_ckpt)
      message("  screen: saved checkpoint to ", screen_ckpt)
      # Same rule as run_or_load()'s cal_dir cleanup: only remove the raw
      # directory once the lightweight checkpoint capturing what's needed
      # from it (sen/mu) is safely on disk. A crash before this point
      # leaves sen_dir intact for diagnosis; this line is only reached on
      # genuine screen success.
      if (dir.exists(sen_dir)) {
        ok <- tryCatch({ unlink(sen_dir, recursive = TRUE); TRUE }, error = function(e) FALSE)
        if (ok) message("  screen: removed raw directory ", sen_dir)
      }
    }

    # mu_star is in each sub-region's own units, so scale to its maximum
    # before comparing across sub-regions.
    mu$rel <- stats::ave(mu$value, mu$variable, FUN = function(v)
      if (max(v, na.rm = TRUE) > 0) v / max(v, na.rm = TRUE) else v * 0)
    sub_var <- vapply(sub_regions, function(x) x$var, character(1))
    mu$var_aeme <- sub_var[mu$variable]
    infl <- mu[mu$rel >= THR, ]

    # Keep zoned parameters as COMPLETE sets. Dropping the zones that did not
    # screen leaves a short array, and the model fails - so a zoned parameter
    # is kept or dropped on the strength of its most influential zone.
    keep_names <- unique(adj$name[adj$name_full %in% unique(infl$parameter)])
    keep_adj <- adj[adj$name %in% keep_names, ]
    message("  kept ", nrow(keep_adj), " of ", nrow(adj))
    if (nrow(keep_adj) == 0) stop("stage ", stage, ": nothing was sensitive")

    # Link by parameter name for the same reason: a zone re-added to complete
    # an array has no sensitivity of its own, and pestpp-ies treats an
    # unlinked parameter as fixed.
    #
    # Grouped by REGION name (infl$variable, e.g. "surf_temp"/"bot_temp"),
    # not the derived flat variable (infl$var_aeme, e.g. "HYD_temp")
    # (2026-09-24) - aemetools' calib_aeme()/localizer are now region-
    # aware, so a parameter can be linked to bot_temp specifically without
    # also being linked to surf_temp just because both share HYD_temp.
    # Before this change every sub-region under one variable was pooled
    # into a single pvm bucket, which is exactly what silently diluted
    # mixing/coef_mix_hyp's effect against surf_temp/meta_temp when force_keep
    # first tried linking it to a variable rather than a region.
    pvm_list <- lapply(split(infl$parameter, infl$variable), function(z) {
      nms <- unique(adj$name[adj$name_full %in% unique(z)])
      keep_adj$name_full[keep_adj$name %in% nms]
    })
    pvm_list <- pvm_list[vapply(pvm_list, length, integer(1)) > 0]

    # A Morris screen sees correlation, not mechanism: over a short design it
    # will happily link a phosphorus flux to oxygen. Where the table states
    # which variables a parameter acts on, take the intersection.
    if (!is.null(prior_pvm)) {
      pvm_list <- stats::setNames(lapply(names(pvm_list), function(v) {
        keep <- intersect(pvm_list[[v]], prior_pvm[[v]])
        if (length(keep) == 0) {
          message("  localizer: no prior overlap for ", v,
                  "; keeping the screened set")
          pvm_list[[v]]
        } else keep
      }), names(pvm_list))
    }

    # A parameter the screen kept but that ends up linked to no target (the
    # table's var_sim column contradicts a spurious Morris correlation) would
    # be a free random walk - pestpp-ies treats an unlinked adjustable
    # parameter as fixed, and pest_localizer() aborts on it. Drop those from
    # the adjustable set, whole zoned arrays together as elsewhere. Changing
    # the outlet depth shifted the stage-3 sensitivities enough that ~30 such
    # parameters appeared where the N_REALS=50 screen had none.
    linked_nf    <- unique(unlist(pvm_list, use.names = FALSE))
    linked_names <- unique(keep_adj$name[keep_adj$name_full %in% linked_nf])
    dropped      <- setdiff(unique(keep_adj$name), linked_names)
    if (length(dropped) > 0) {
      message("  localizer: dropping ", length(dropped),
              " screened parameter(s) the table links to no stage-", stage,
              " target: ", paste(utils::head(dropped, 8), collapse = ", "),
              if (length(dropped) > 8) ", ..." else "")
      keep_adj <- keep_adj[keep_adj$name %in% linked_names, ]
    }
    if (nrow(keep_adj) == 0) stop("stage ", stage, ": nothing linkable")

    # force_keep: re-admit any named parameter the Morris screen (or the
    # "linked to no target" cut just above) dropped, with an EXPLICIT
    # target linkage supplied by the caller rather than one inferred from
    # the (possibly-blind, for these parameters) screen. See this
    # argument's own doc above run_stage()'s signature for why.
    #
    # Each entry's value is either a flat character vector of region names
    # (applied to EVERY row sharing that parameter name - e.g. a scalar
    # parameter like mixing/coef_mix_hyp, or a zoned parameter that should
    # link the same way regardless of zone), or a named list keyed by zone
    # index (as character, matching adj$index) -> region name(s), for a
    # zoned parameter whose correct linkage genuinely differs BY zone (see
    # sed_zone_regions() - a bottom sediment zone should influence
    # bot_temp, not surf_temp).
    if (!is.null(force_keep)) {
      for (nm in names(force_keep)) {
        rows <- adj[adj$name == nm, , drop = FALSE]
        if (nrow(rows) == 0) {
          message("  force_keep: '", nm, "' not found in this stage's adjustable table - skipped")
          next
        }
        spec <- force_keep[[nm]]
        indexed <- is.list(spec) && !is.null(names(spec))

        was_dropped <- !nm %in% keep_adj$name
        if (was_dropped) {
          keep_adj <- rbind(keep_adj, rows)
          message("  force_keep: re-admitting '", nm,
                  "' (screened out, forced back in) - linked to ",
                  if (indexed) "per-zone regions" else paste(spec, collapse = ", "))
        }

        if (indexed) {
          for (idx_chr in names(spec)) {
            idx_rows <- rows[as.character(rows$index) == idx_chr, , drop = FALSE]
            if (nrow(idx_rows) == 0) next
            for (v in spec[[idx_chr]]) {
              pvm_list[[v]] <- unique(c(pvm_list[[v]], idx_rows$name_full))
            }
          }
        } else {
          for (v in spec) {
            pvm_list[[v]] <- unique(c(pvm_list[[v]], rows$name_full))
          }
        }
      }
    }

    # vars_sim here must match pvm_list's own keys - region names when
    # sub_regions was used for screening (as above), flat variable names
    # otherwise.
    pvm <- as_param_var_matrix(pvm_list, param = keep_adj,
                               vars_sim = if (!is.null(sub_regions)) names(sub_regions) else targets)

  } else if (!is.null(prior_pvm)) {
    pvm_list <- lapply(prior_pvm, function(z)
      intersect(z, keep_adj$name_full))
    pvm_list <- pvm_list[vapply(pvm_list, length, integer(1)) > 0]
    if (length(pvm_list) > 0)
      pvm <- as_param_var_matrix(pvm_list, param = keep_adj,
                                 vars_sim = if (!is.null(sub_regions)) names(sub_regions) else targets)
  }

  # freeze_dropped (2026-10-03): a parameter that was ADJUSTABLE in this stage but
  # screened out (Morris below threshold, or linked to no target) is, by default,
  # simply absent from the PEST control file and from `best` - so the model silently
  # falls back to its config default for it, during the calibration AND in every
  # later forward run. In Rototoa's stage 4 that dropped light/Kw, the shortwave
  # factor, coef_mix_hyp, sed_temp_mean, the sediment oxygen demand and others
  # (48 kept in the old run, 13 in the new), giving a 1.5 C cold bias and
  # over-predicted oxygen. With freeze_dropped = TRUE they stay in the control file
  # as FIXED parameters (min = max = their starting value: the narrowed value
  # carried from an earlier stage, else the table's declared start), so the model
  # runs with the value the stage was set up with and they appear in `best`.
  # pestpp-ies and the localizer ignore partrans = "fixed" parameters, so they need
  # no Morris/localizer row. Default FALSE: stages 1-2 and old checkpoints behave
  # exactly as before.
  dropped_frozen <- adj[0, , drop = FALSE]
  if (isTRUE(freeze_dropped)) {
    frozen_nf <- if (nrow(frozen) > 0) add_name_full(frozen)$name_full else character()
    dr <- adj[!adj$name %in% keep_adj$name & !adj$name_full %in% frozen_nf, , drop = FALSE]
    if (nrow(dr) > 0) {
      dr$min <- dr$value
      dr$max <- dr$value
      dropped_frozen <- dr
      message("  freeze_dropped: ", length(unique(dr$name)), " screened-out parameter(s) held FIXED at ",
              "their starting values instead of reverting to model defaults: ",
              paste(utils::head(unique(dr$name), 12), collapse = ", "),
              if (length(unique(dr$name)) > 12) ", ..." else "")
    }
  }

  if (!is.null(rebuild)) {
    b <- rebuild()
    aeme <- b$aeme
    path <- b$path
    # fdir deliberately NOT re-pointed at the new build directory - see above.
  }

  # ies_bad_phi_sigma: pestpp-ies drops any realisation whose phi exceeds
  # mean + sigma*sd each iteration. At the default 2 the stage-3 ensembles
  # collapsed to 3-12 members (see BAD_PHI_SIGMA in pest_calib.R). A per-stage
  # override, so stages 1-2 keep the tighter default.
  bps <- if (exists("BAD_PHI_SIGMA") && stage == 3) BAD_PHI_SIGMA else 2

  # Draw size. For the wide stage-3 BGC prior, most draws crash the forward
  # run, so a fixed ies_num_reals leaves a tiny posterior. Measure the feasible
  # fraction (from a previous .rec, else the Morris runs, else a default) and
  # oversample so ~IES_TARGET_SURVIVORS realisations get through the initial
  # cull. Governed by `adaptive_reals` (default: stage 3 only, matching every
  # pipeline through v5, where stage 3 is always the many-parameter BGC
  # stage) rather than a hardcoded `stage == 3` check - a pipeline that
  # renumbers stages (e.g. v6, which splits stage 3 into a small TSS-only
  # stage and moves the BGC stage to 4) needs to pass adaptive_reals
  # explicitly for whichever stage actually has the parameter count this
  # exists to serve; the default preserves every existing caller's
  # behaviour unchanged. Any run without the IES_TARGET_SURVIVORS config
  # keeps the fixed size regardless.
  n_reals <- N_REALS
  if (adaptive_reals && exists("IES_TARGET_SURVIVORS")) {
    feas <- feasible_fraction(cal_dir = cal_dir, sen = sen)
    n_reals <- adaptive_num_reals(feas, IES_TARGET_SURVIVORS, N_REALS,
                                  if (exists("IES_REALS_CAP")) IES_REALS_CAP else 500L)
    message(sprintf("  iES draw size: feasible fraction ~%.0f%% -> %d realisations (target %d survivors)",
                    100 * feas, n_reals, IES_TARGET_SURVIVORS))
  }

  cal_ctrl <- create_pest_control(
    exe = "pestpp-ies",
    noptmax = if (!is.null(restart_from)) restart_noptmax %||% 1L else NOPTMAX,
    ies_num_reals = n_reals,
    ncore = NCORE, file_dir = fdir, pest_dir = cal_dir,
    localizer = pvm, stall_minutes = stall_minutes %||% 10,
    restart_from = restart_from,
    prior_par_ensemble = prior_par_ensemble,
    pestpp_options = list(ies_lambda_mults = 1, lambda_scale_fac = 1,
                          ies_bad_phi_sigma = bps))

  # vars_sim: pass the REGION list itself (not just names) when sub_regions
  # was given - aemetools' calib_aeme() now accepts this exact shape
  # natively (2026-09-24), splitting a variable's objective into
  # independently-weighted depth/month windows (e.g. surf_temp/bot_temp)
  # instead of pooling every matched observation into one KGE. weights/
  # FUN_list stay keyed by the FLAT variable names regardless - calib_aeme()
  # reduces vars_sim to its flat form internally before checking those.
  cal_id <- unname(calib_aeme(
    aeme = aeme, path = path,
    param = set_param_log(add_name_full(rbind(keep_adj[, PCOLS],
                                              frozen[, PCOLS],
                                              dropped_frozen[, PCOLS]))),
    model = MODEL, vars_sim = if (!is.null(sub_regions)) sub_regions else targets,
    FUN_list = stats::setNames(lapply(targets, function(v) kge), targets),
    weights = weights, ctrl = cal_ctrl)[[1]])

  prune_pest_agents(cal_dir)
  calib <- read_calib(ctrl = cal_ctrl, sim_id = cal_id)
  phi <- read_pest_phi(calib)
  message("  phi: ", paste(signif(phi$min, 4), collapse = " -> "))

  # best_realisation_params() must be matched against EVERY parameter
  # actually written into the PEST control file, not just keep_adj: a
  # non-empty `frozen` (carry-forward from an earlier stage, e.g. stage
  # 2's outflow/seepage_rate/sed_heat_model carried from stage 1) is
  # rbind()ed into calib_aeme()'s param argument above, and aemetools
  # reports those fixed parameters in the posterior ensemble output too
  # ("held fixed ... and reported in the calibration output", seen in the
  # run log) - so they appear in read_pest_ensemble()'s name_full column
  # even though keep_adj never listed them. Passing only keep_adj as
  # best_realisation_params()'s `adj` argument left it unable to match
  # those rows back (merge() by name/group/index against a `ref` table
  # that simply didn't contain them), aborting with "N parameter(s) ...
  # did not match `adj`" - confirmed directly: N was always exactly
  # nrow(frozen). `kept` stays keep_adj-only (unaffected) since that's
  # what carry()/narrow() key off for what THIS stage owns.
  list(stage = stage, sa_id = sa_id, cal_id = cal_id, sen = sen,
       kept = keep_adj, pvm = pvm, phi = phi, cal_ctrl = cal_ctrl,
       n_reals = n_reals,
       frozen_dropped = dropped_frozen,
       best = add_name_full(best_realisation_params(
         rbind(keep_adj[, PCOLS], frozen[, PCOLS], dropped_frozen[, PCOLS]), calib)))
}

#' The winners of a stage, fixed for the stages that follow.
#'
#' Thin wrapper over aemetools::carry_param(): restricts the finished
#' stage's best parameters to the set that stage kept adjustable (minus
#' `except`) and freezes them (min == max == value) for the next stage.
carry <- function(res, except = character()) {
  keep <- setdiff(res$kept$name, except)
  aemetools::carry_param(res$best, names = keep)
}

#' The winners of a stage, kept adjustable but bounded by the posterior.
#'
#' The counterpart to carry(): use it for a parameter the next stage still
#' has an observation for. Bounds become the posterior `q` quantiles,
#' intersected with the prior bounds; the starting value becomes the
#' posterior median. Falls back to the stage bounds if the ensemble cannot
#' be read.
narrow <- function(res, names_keep, q = c(0.05, 0.95)) {
  b <- res$best[res$best$name %in% names_keep, , drop = FALSE]
  if (nrow(b) == 0) return(b[, PCOLS, drop = FALSE])
  b <- tryCatch({
    calib <- read_calib(ctrl = res$cal_ctrl, sim_id = res$cal_id)
    ens <- do.call(rbind, lapply(pest_posterior_params(calib), function(s)
      s[, c("name_full", "value")]))
    for (j in seq_len(nrow(b))) {
      v <- ens$value[ens$name_full == b$name_full[j]]
      if (length(v) < 3) next
      qs <- stats::quantile(v, q, na.rm = TRUE)
      b$min[j] <- max(b$min[j], min(qs))
      b$max[j] <- min(b$max[j], max(qs))
      b$value[j] <- stats::median(v, na.rm = TRUE)
    }
    b
  }, error = function(e) {
    message("  narrow(): posterior unavailable (", conditionMessage(e),
            "); keeping the stage bounds")
    b
  })
  # pest_param_table() aborts on min >= max.
  flat <- b$min >= b$max
  if (any(flat)) {
    b$min[flat] <- b$value[flat] - abs(b$value[flat]) * 0.05
    b$max[flat] <- b$value[flat] + abs(b$value[flat]) * 0.05
  }
  b[, PCOLS, drop = FALSE]
}

#' Mask a profile variable's observations to structural depth bands
#' (surface / metalimnion / bottom) before calibration, WITHOUT touching
#' the observations on the Aeme object passed in.
#'
#' Why: calib_aeme() scores a variable's whole matched-observation set as
#' one pooled KGE - it does NOT filter by the sub_regions list (that only
#' feeds create_sen_control()'s SCREENING objective, a separate mechanism
#' used to pick which PARAMETERS are sensitive, not which OBSERVATIONS the
#' actual pestpp-ies calibration scores). A profile with many more
#' observations near the surface than at depth therefore has its pooled
#' KGE dominated by whichever band happens to be densely sampled, not by
#' physical importance - and vertically adjacent depths are highly
#' autocorrelated, so "more points" there isn't "more information" the
#' way it would be for independent samples.
#'
#' Concrete evidence this matters (Rototoa, 2026-09-23/24): a severe
#' bottom-temperature over-mixing problem (variability ratio alpha=2.39,
#' KGE=-0.48 at depth) was completely invisible in the pooled whole-column
#' KGE (0.61 - looked fine) and only showed up once surface/bottom were
#' scored separately. A structural failure that size, hidden by pooling,
#' is exactly the failure mode this function exists to prevent.
#'
#' `aeme` itself is never modified - `AEME::observations<-()` replaces the
#' whole `@observations` slot on ordinary copy-on-modify S4 semantics, so
#' calling this on `aeme0` and assigning the result to a NEW name (e.g.
#' `aeme_trim <- trim_obs_profile(aeme0, ...)`) leaves `aeme0` completely
#' untouched wherever else it's used - pass `aeme_trim` to fresh_build()/
#' calib_aeme() and keep `aeme0` as the full, unmasked record for anything
#' else (plotting, other stages, other variables).
#'
#' @param aeme Aeme object; only observations()$lake is read/replaced -
#'   observations()$level (water level) is left untouched.
#' @param var AEME variable name to mask (e.g. "HYD_temp"). Rows for every
#'   other variable in observations()$lake pass through unchanged.
#' @param surf_depth,bot_depth band half-widths, in metres: "surface" is
#'   depth <= surf_depth; "bottom" is depth >= z - bot_depth, where z is
#'   the lake's own max depth (AEME::input(aeme)$init_depth) - matches the
#'   z-4/z convention this pipeline's own bot_temp sub-region already
#'   uses, so bottom scales with each lake's own depth rather than a fixed
#'   absolute value that would mean something different for a 10 m lake
#'   than a 50 m one.
#' @param keep_meta TRUE keeps everything strictly between the two bands
#'   ("metalimnion", including the thermocline) as its own group; FALSE
#'   drops that band entirely, keeping only surface + bottom.
#' @param meta_max_n NULL keeps every metalimnion observation on each
#'   date; an integer thins each date's metalimnion rows down to at most
#'   this many, keeping the ones closest to the profile's own observed
#'   midpoint depth for that date - use this if the metalimnion band
#'   itself is densely sampled enough to reintroduce the same
#'   autocorrelation/density problem this function exists to avoid.
#' @param z override the lake depth used for the bottom band (metres);
#'   NULL reads it from the built aeme via AEME::input().
#' @returns a COPY of `aeme` with observations()$lake's `var` rows
#'   filtered to the kept bands; `aeme` itself is unmodified.
trim_obs_profile <- function(aeme, var = "HYD_temp", surf_depth = 2,
                             bot_depth = 4, keep_meta = TRUE,
                             meta_max_n = NULL, z = NULL) {
  obs <- AEME::observations(aeme)
  lake <- obs$lake
  if (is.null(lake) || nrow(lake) == 0) return(aeme)

  if (is.null(z)) {
    z <- tryCatch(AEME::input(aeme)$init_depth, error = function(e) NA_real_)
    if (is.na(z) || !is.finite(z)) {
      # Fall back to the deepest hypsograph point below the surface, same
      # convention stage-3's own z <- ... fallback uses elsewhere in this
      # pipeline.
      hyp <- tryCatch(AEME::input(aeme)$hypsograph, error = function(e) NULL)
      z <- if (!is.null(hyp)) max(hyp$elev) - min(hyp$elev) else NA_real_
    }
  }
  if (is.na(z)) {
    message("  trim_obs_profile(): could not resolve lake depth for ", var,
            " - leaving observations unmasked")
    return(aeme)
  }

  is_var <- lake$var_aeme == var
  if (!any(is_var)) return(aeme)

  dep <- if (!is.null(lake$depth_mid)) lake$depth_mid
         else if (!is.null(lake$depth_from)) (lake$depth_from + lake$depth_to) / 2
         else if (!is.null(lake$depth)) lake$depth else NA_real_
  if (all(is.na(dep[is_var]))) {
    message("  trim_obs_profile(): no depth column found for ", var,
            " - leaving observations unmasked")
    return(aeme)
  }

  surf <- is_var & !is.na(dep) & dep <= surf_depth
  bot  <- is_var & !is.na(dep) & dep >= (z - bot_depth)
  meta <- is_var & !is.na(dep) & !surf & !bot
  keep <- !is_var          # every other variable: untouched
  keep <- keep | surf | bot | (keep_meta & meta)

  if (keep_meta && !is.null(meta_max_n) && any(meta)) {
    meta_idx <- which(meta)
    meta_df  <- data.frame(i = meta_idx, Date = as.Date(lake$Date[meta_idx]),
                           dep = dep[meta_idx])
    mid_by_date <- stats::ave(meta_df$dep, meta_df$Date, FUN = function(d) mean(range(d)))
    meta_df$dist <- abs(meta_df$dep - mid_by_date)
    drop_idx <- unlist(lapply(split(meta_df, meta_df$Date), function(d) {
      if (nrow(d) <= meta_max_n) return(integer(0))
      d$i[order(d$dist)][-seq_len(meta_max_n)]
    }))
    if (length(drop_idx) > 0) keep[drop_idx] <- FALSE
  }

  n_dropped <- sum(is_var) - sum(keep & is_var)
  message(sprintf(
    "  trim_obs_profile(%s): kept %d of %d obs (surface<=%.1fm: %d, bottom>=%.1fm: %d, %s: %d), dropped %d",
    var, sum(keep & is_var), sum(is_var), surf_depth, sum(surf),
    z - bot_depth, sum(bot),
    if (keep_meta) "metalimnion" else "metalimnion(dropped)",
    if (keep_meta) sum(keep & meta) else 0L, n_dropped))

  obs$lake <- lake[keep, , drop = FALSE]
  AEME::observations(aeme) <- obs
  aeme
}

#' The GLM physical-parameter calibration table (stages 1-2), one row per
#' named parameter - not scattered prow()/shipped() calls in the pipeline
#' script. See data/processed/glm_phys_calib_params.csv's own header/notes
#' column for the parameter-by-parameter reasoning.
#'
#' Extends read_param_table()'s schema with three columns read_param_table()
#' doesn't know about, because several of these parameters don't have a
#' single universal value the way BGC rate constants do:
#'   - `value_source`: "fixed" (the CSV's own value/min/max are literal
#'     numbers, used as-is) or "lake_mean" (the STARTING value must come
#'     from THIS LAKE's own built sediment config - e.g. deep-sediment
#'     mean temperature genuinely differs by lake/climate - so value/min/
#'     max in the CSV are ignored for these rows in favour of
#'     `mean(sed[[<name minus "sediment/">]])` from the built aeme).
#'   - `fac`: for a "lake_mean" row, NA means take the CSV's own literal
#'     min/max as an ABSOLUTE bound around the lake-specific value (e.g.
#'     sed_temp_amplitude's physical 0.1-6 degC range); a number means
#'     the bound is value*(1 +/- fac) instead - a RELATIVE band around
#'     the lake's own value (e.g. sed_temp_mean's +/-40%). Matches
#'     prow()'s own fac convention exactly.
#'   - `adjustable`: FALSE freezes the row (min=max=value) regardless of
#'     whatever the CSV's own min/max say - used for the sediment
#'     material-property parameters (sed_heat_Ksoil, sed_temp_depth) that
#'     are frozen at the lake's own value rather than a hand-picked
#'     literal, exactly the way carry()/frozen-stage parameters already
#'     work elsewhere in this pipeline.
#'
#' `vars`/`var_sim` here are calib_aeme()'s REGION names (surf_temp,
#' bot_temp, meta_temp, whole_lvl, ...), not raw AEME variable names -
#' this table is meant to feed the region-aware calib_aeme()/pvm
#' localizer, not the flat vars_sim form.
#'
#' Sediment rows (any name in `zoned_names`) are expanded to one row per
#' GLM sediment zone via per_zone()'s own convention (index = 1..n_zones)
#' - the CSV holds one row per parameter, not one per zone, same
#' compactness read_param_table()/expand_zones() give the BGC table.
#'
#' @param file path to the CSV (data/processed/glm_phys_calib_params.csv).
#' @param aeme a BUILT Aeme object for the lake being calibrated - read
#'   for its own sediment config on every "lake_mean" row. Pass the SAME
#'   build a stage's fresh_build() produced (b1$aeme/b2$aeme), before any
#'   PEST run touches it.
#' @param n_zones GLM sediment zone count for this lake
#'   (cfg$hydrodynamic$sediment$n_zones) - required if any zoned row is
#'   present in the table.
#' @returns data.frame with PCOLS + a `vars` list-column (one character
#'   vector of region/variable names per row), ready to rbind() into an
#'   adjustable-parameter table the same way s1_adj/s2_adj are built today.
read_phys_param_table <- function(file, aeme, n_zones = NULL) {
  raw <- utils::read.csv(file, stringsAsFactors = FALSE)
  need <- c("model", "file", "name", "group", "index", "value", "min", "max",
           "fac", "value_source", "adjustable", "module", "var_sim")
  miss <- setdiff(need, names(raw))
  if (length(miss) > 0)
    stop("Missing column(s) in ", file, ": ", paste(miss, collapse = ", "))

  raw$group[raw$group %in% c("NA", "")] <- NA_character_
  raw$index <- suppressWarnings(as.numeric(raw$index))
  raw$adjustable <- as.logical(raw$adjustable)

  cfg <- AEME::configuration(aeme)$glm_aed
  sed <- cfg$hydrodynamic$sediment

  zoned_names <- c("sediment/sed_temp_mean", "sediment/sed_temp_amplitude",
                   "sediment/sed_heat_Ksoil", "sediment/sed_temp_depth")

  rows <- lapply(seq_len(nrow(raw)), function(i) {
    r <- raw[i, ]
    val <- r$value; mn <- r$min; mx <- r$max

    if (identical(r$value_source, "lake_mean")) {
      base_name <- sub("^sediment/", "", r$name)
      if (is.null(sed) || is.null(sed[[base_name]]))
        stop("read_phys_param_table(): '", base_name,
             "' not found in this lake's built sediment config")
      val <- mean(sed[[base_name]], na.rm = TRUE)
      if (!is.na(r$fac)) {
        mn <- val - abs(val) * r$fac
        mx <- val + abs(val) * r$fac
      }
      # else: CSV's own literal min/max stand as an absolute bound
      # around this lake-specific value (e.g. sed_temp_amplitude).
    }

    if (!isTRUE(r$adjustable)) { mn <- val; mx <- val }

    data.frame(model = r$model, file = r$file, name = r$name, value = val,
              min = mn, max = mx, group = r$group, index = r$index,
              stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  out$vars <- lapply(strsplit(raw$var_sim, "|", fixed = TRUE), trimws)

  zoned <- out$name %in% zoned_names
  if (any(zoned)) {
    if (is.null(n_zones))
      stop("read_phys_param_table(): n_zones required - table has zoned sediment rows")
    base <- out[zoned, , drop = FALSE]
    out <- rbind(out[!zoned, , drop = FALSE],
                do.call(rbind, lapply(seq_len(n_zones), function(i) {
                  d <- base; d$index <- i; d
                })))
  }
  out
}
