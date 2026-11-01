library(targets)
library(tarchetypes)
library(AEME)
library(aemetools)
library(ggplot2)

# pest_funs.R moved into scripts/ (2026-10-02); run from the project root
source(Find(file.exists, c("scripts/pest_funs.R", "pest_funs.R")))

# ============================================================================
# v6 (2026-09-26): SKETCH - 4-stage restructure, Rototoa-only testbed
# ============================================================================
#
# Copied from pest_calib_v4.R at the point it had: the corrected observation
# data (LID linkage fix), R_resp/R_growth bound fixes, cyano-only
# N-fixation, PHY_cyano working as a real stage-3(now 4) target (the
# aemetools get_var_indices()/.raf_extract_var() depth-collapse fix), and
# the combined TSS+BGC "stage 4/5" single-stage design. NOT YET RUN - this
# is a sketch to test on Rototoa before any decision to port it to the
# 10-lake production pipeline (pest_calib_v5.R, a separate, already-
# established all-lakes lineage this file deliberately does not touch).
#
# What v6 changes from v4, and why:
#
#   1. TSS split out into its own numbered stage 3 (previously an ad hoc,
#      one-off diagnostic script - "stage 2b" in the tss_phyto_update
#      report, section 2.2 - never a permanent part of the pipeline).
#      Rationale (tss_phyto_update report, section 2): uncontrolled light
#      extinction from unchecked TSS/resuspension can silently dominate
#      phytoplankton dynamics before nutrients or phyto parameters ever
#      get evaluated fairly - the same reasoning that already justifies
#      keeping thermal structure (stage 2) separate from BGC.
#   2. The former single "stage 3" (temp+oxy+nutrients+TSS+chl-a+cyano,
#      all calibrated together) is renumbered "stage 4" and no longer
#      calibrates TSS parameters directly - it inherits them from the new
#      stage 3's checkpoint instead (narrow()'d, not fully carry()'d - see
#      that stage's own header comment for why full freezing was rejected).
#   3. A NEW, SA-only Morris screen of GLM's bulk heat-transfer
#      coefficients (`ce`/`ch`/`cd` in glm3.nml's &meteorology block -
#      never touched by any calibration in this project; distinct from the
#      already-calibrated MET_radswd/radlwd/tmpair/wndspd SCALING FACTORS)
#      runs after stage 2 completes, scored against stage 2's own thermal
#      targets. Deliberately NOT fed into stage 2's (or any stage's)
#      adjustable parameter set, regardless of how sensitive the screen
#      finds them - they are easily confounded with the wind/humidity
#      scaling factors already being calibrated, and the point here is
#      information (do they matter at all for this lake?), not adding
#      three more simultaneously-adjustable, weakly-identifiable
#      parameters to stage 2's own search space.
#
# UNRESOLVED / first-pass judgement calls, flagged rather than hidden:
#   - The new stage 3's SECOND (unobserved) TSS particle class parameters
#     (tau_0[2]/w_ss[2]) have no direct observational constraint (tss_
#     phyto_update report, section 2.2) - kept ADJUSTABLE in stage 3 here
#     (not carry()'d from a prior checkpoint, since v6 has none yet) but
#     flagged for narrow()/carry() once a real v6 stage-3 checkpoint exists
#     to hand off from, per the same report's section 9 item 5 caution
#     about propagating an unconstrained value as though it were identified.
#   - `aed_phosphorus`'s own adsorption parameters (kadsratio/qmax/kpo4p)
#     are deliberately left OUT of the new stage 3 entirely (no phosphorus
#     observations are in play there) and stay fully adjustable in stage 4,
#     where the phosphorus-TSS interaction found in the tss_phyto_update
#     report (section 6) can actually be evaluated against real data.
#
# ============================================================================
# v4 (2026-09-21): Rototoa-only calibration-process refinement testbed
# ============================================================================
#
# Copied from pest_calib_v3.R (which stays as the 10-lake production
# pipeline, untouched) at the point v3 had: local build/PEST tree + Dropbox
# checkpoint split (contention/corruption fix), the outlet-override and
# Pupuke deep_mixing/coef_mix_hyp fixes, NCORE tuned to the box's physical
# core count, and the full stage-1/2/3 structure documented below.
#
# Purpose: iterate on the calibration PROCESS itself (target sets,
# weighting, narrowing choices, parameter tables) against one lake chosen
# for being a clean, information-rich testbed - Rototoa: 23,892 usable
# observations across 32 variables in the sim window (second only to
# Pupuke, which is excluded from this purpose as it's mid-fix for its own
# mixing bug), the best water-level record of any upstream lake (3,287
# obs, tied with Pupuke), and already fully 3-stage-calibrated under v3 -
# so every v4 change has a real v3-Rototoa baseline to diff against
# directly, not a fresh run needed just to get one. Not entangled in
# either of the two other special investigations running elsewhere
# (Pupuke's mixing fix; Tomarata/Kawaupaka held back for the
# adjustable-vs-frozen-mixing-coefficient before/after comparison).
#
# LAKE_IDX (below, in the configuration block) is the only structural
# change from v3: every loop that iterated all 10 lakes now iterates just
# this one. SKIP_LAKES is gone (nothing left to skip). The ROOT/
# CHECKPOINT_ROOT trees are new and separate from v3's (staged-
# calibration-v4/, both locally and in Dropbox) specifically so v4
# experiments never touch or get confused with v3's Rototoa result -
# compare the two checkpoint trees directly when you want a before/after.
#
# Once a v4 change proves out here, port it back to pest_calib_v3.R for
# the other 9 lakes - this script is not meant to grow into the
# production pipeline itself.
#
# Change 1 (2026-09-21): stage-1/2 evaporation-parameter fix for the
# Rototoa water-level drift. Root cause and design in full at
# S1_NARROW_NAMES, below - summary: MET_wndspd/rh_factor (evaporation-
# relevant) move from stage-2-only to stage-1-adjustable (targeted
# against LKE_vol, stage 1's only observation), then narrow() (not
# fresh shipped()) into stage 2. Diagnosis: the observed lake is flat
# (68.2-69.0 m, 2013-2023) but simulates a ~13 m drain even under
# stage-1-best; the raw inflow+precip-outflow balance is net positive
# every year (would predict rising, not draining); evaporation - the one
# GLM-internal term not in either CSV - is the only sink large enough,
# and stage 1 had no parameter able to touch it. Also traced why stage 2
# doesn't catch this on its own despite LKE_vol already being a
# weighted stage-2 target: balance_weights() derives each target's
# weight from its baseline (stage-1-best) KGE, so an evaporation-blind
# stage 1 makes LKE_vol's own stage-2 weight look artificially
# untrustworthy before stage 2 even starts - expected to self-correct
# once stage 1 can actually fix the water balance, without touching
# balance_weights() or the LKE_vol cap directly for this first test.
#
# Change 2 (2026-09-21, same day): Change 1's narrow(s1, S1_NARROW_NAMES)
# hand-off turned out to BE the drift's actual root cause, not the fix for
# it - narrow() restarts the next stage from the posterior ENSEMBLE MEDIAN,
# not the best-fit point, and for Rototoa's MET_wndspd/rh_factor the median
# was diluted enough relative to stage 1's real best fit that stage 2 let
# the evaporation bias resurface and drain the lake to zero volume
# mid-run. A direct best-fit-value substitution fixed that specific
# symptom (verified: 0 days <=0, full date range intact) - but per-lake
# patches to narrow()'s hand-off are not a structural fix. Instead: stage 1
# (water-only) and stage 2 (thermal+water) are now MERGED into one joint
# stage, calibrating water balance and thermal structure together against
# all four physics targets at once. There is no longer an earlier stage to
# narrow from, so S1_NARROW_NAMES and the narrow(s1, ...) mechanism are
# unused (left defined, unreferenced, as a record of the diagnosis this
# fix grew out of). The merged stage is still checkpointed/labelled "s1";
# there is no "s2" checkpoint in v4 any more. Stage 3 (still gated off by
# RUN_STAGE3) now carries/narrows from this single merged s1 instead of
# combining s1+s2.
#
# Change 3 (2026-09-22): reverted Change 2's merge back to a genuine
# two-stage split - water balance alone (checkpointed "s1"), then
# thermal+water (checkpointed "s2", new in v4 again). The merge fixed the
# median-vs-best-fit hand-off bug, but created a different problem: a
# single MET_wndspd serving both the water-balance and thermal-structure
# objectives at once let the two signals pull it in conflicting
# directions, and the optimizer settled on a compromise that satisfied
# neither - confirmed directly: surface temperature bias stuck at
# ~-4.06degC across three different mixing-parameter fixes (freezing
# coef_mix_hyp for Rototoa, etc.), because the real constraint was two
# objectives fighting over one parameter, not a wrong parameter value.
# The stage-1 -> stage-2 hand-off (S1_NARROW_NAMES: MET_wndspd,
# rh_factor) explicitly carries Change 2's actual fix forward: it uses
# narrow()'s BEST-FIT value override, not the posterior median that
# caused the original drift - see the stage-2 code, just before s2_adj is
# built. Stage 3 now carries/narrows from BOTH s1 and s2 (their
# respective except = S1_NARROW_NAMES / S2_NARROW_NAMES carry-forwards),
# restoring the original pre-merge three-stage chain.

# Staged calibration of GLM-AED with PEST++ - v3
# ============================================================================
#
# Calibrates a lake in three stages, each one screened with a Morris
# sensitivity analysis, localised from that screening, and solved with
# pestpp-ies:
#
#   1. water balance     -> lake volume
#   2. thermal structure -> temperature, thermocline, stratification, water
#                           level (meteorology scaling: shortwave, longwave,
#                           wind, air temp, relative humidity)
#   3. biogeochemistry   -> dissolved O2, ammonium, nitrate, FRP,
#                           chlorophyll-a  (+ temperature anchor)
#
# Differences from pest_calib.R (v1), 2026-09-16
# ------------------------------------------------------------------------
# A forward-model check (stage-1-best params, stage-2 params held at
# DEFAULT vs CALIBRATED) showed simulated LKE_vol shifting by
# 0.12-0.19 m mean / up to 0.27 m max between the two, with a consistent
# sign and (for one lake) a drift that grew over the run - i.e. stage-2
# thermal/met parameters (evaporation-relevant: MET_radswd, rh_factor,
# MET_radlwd, wind/mixing coefficients) do move the water balance that
# stage 1 froze, contrary to v1's assumption that this coupling is
# negligible. v2 responds with two changes, both scoped to stage 2 only -
# see "Holding parameters still between stages" below for why this is not
# extended to every stage boundary:
#
#   a. WATER-BALANCE PARAMETERS ARE NARROWED, NOT FROZEN, IN STAGE 2.
#      `outflow` and `glm4.nml/outflow/seepage_rate` move from carry()
#      (frozen) to narrow() (adjustable, bounded to the stage-1 posterior)
#      - the same treatment stage 3 already gives light/Kw, for the same
#      reason: a parameter with a real, informative target left open in
#      the next stage is not "free", it is being fit jointly rather than
#      forced.
#   b. LKE_vol IS RESTORED AS A STAGE-2 TARGET, WITH A BALANCED WEIGHT.
#      v1 excluded it after it took over the stage-2 objective (99.6% of
#      phi on Spectacle) once, thermocline/stratification getting worse as
#      a result. The forward-model check traced that to a large, roughly
#      constant sim-obs elevation offset (a likely datum/reference-
#      elevation issue, not resolved here) inflating LKE_vol's
#      baseline (1-KGE) far beyond the thermal targets' - not to the
#      water balance being unconstrainable by stage-2 parameters. Rather
#      than exclude it again, stage2_baseline_kge() / balance_weights()
#      (below) measure each stage-2 target's baseline (1-KGE) from one
#      forward run at stage-1-best/stage-2-default parameters, and scale
#      weights so no target's baseline phi share can dominate the others
#      before pestpp-ies ever runs. If the underlying offset is a genuine
#      datum bug, fixing it directly would be the better long-term fix -
#      this reweighting is a safeguard against it distorting stage 2, not
#      a substitute for finding it.
#
# Differences from pest_calib_v2.R, 2026-09-17/18
# ------------------------------------------------------------------------
# Points c/d/e/f below, plus one reversion of v2 itself (2026-09-18):
# stage 2 no longer targets LKE_vol, and WBAL_NAMES (outflow,
# outflow/seepage_rate) go back to being FROZEN (carry()), not narrowed,
# into stage 2 - see the "v3 (2026-09-18)" note at vars_phys in stage 2,
# below, for why: a confirmed aemetools/calib_aeme() limitation (combining
# LKE_vol with grid-based targets makes pest_obs_table() build an empty
# date index for the grid variables), not a bug in this project's code.
# v2's stage-1->2 narrowing (this header's next paragraph) is therefore
# currently INACTIVE - kept documented since it's the right design once
# that upstream limitation is fixed or worked around differently.
#
#   c. PRE-FLIGHT VALIDATION, ONCE, BEFORE THE LAKE LOOP. Two checks that
#      were never automated anywhere in this pipeline before tonight:
#      (i) observation duplicates - three overlapping raw WQ exports are
#      bind_rows()'d with no dedup for anything except LKE_vol (fixed at
#      the source in R/update_aeme.R; this script only VERIFIES the fix
#      landed, since a stale `targets` cache would otherwise silently
#      recalibrate on ~12% duplicated observations with no error), and
#      (ii) inflow sanity (magnitude/units/completeness) - nothing in the
#      inflow chain (lake_flows_hourly_baseline.csv -> lspc_to_aeme() ->
#      aeme_baseline_upstream) validated this before. A failed inflow check
#      triggers an automatic tar_invalidate()/tar_make() regeneration -
#      expensive (cascades through the whole upstream/downstream targets
#      DAG) and deliberately run at most once per script invocation, not
#      per lake.
#   d. STAGE-2 -> 3 NARROWING EXTENDED BEYOND light/Kw. Mixing coefficients
#      and MET_wndspd/MET_radswd move from carry() (frozen) to narrow() -
#      each has a real BGC channel (stratification/turnover controls
#      oxygen and nutrient recycling; wind sets both mixing energy and gas
#      exchange; shortwave drives GPP, not just heat) and HYD_temp stays a
#      stage-3 anchor to push back on them. Pure bias-correction factors
#      (MET_tmpair, MET_radlwd, rh_factor) and sediment thermal params stay
#      frozen - see S2_NARROW_NAMES below for the full reasoning.
#   e. NIT_tn/PHS_tp ARE DIRECT STAGE-3 TARGETS. v2 excluded NIT_tn/PHS_tp
#      because the AED TOT_tn/TOT_tp diagnostics came through ~25x the
#      observations - that was the aed_nitrogen/simn2o=2 bug (denitrification
#      structurally disabled), now fixed in
#      data/processed/aed_sa_calib_params.csv. A post-fix forward-run test
#      showed sim/obs TN ~1-4x across two lakes, not diagnostic-broken any
#      more, so both go through resolve_targets() like every other stage-3
#      candidate (its existing min_obs/censoring safety net still protects
#      any lake where the data doesn't actually support them). LKE_tli3
#      (chla+TN+TP composite) was tried on the same basis but EXCLUDED again
#      (2026-09-19): its log10()-based calculation in AEME has no floor
#      against near-zero simulated concentrations, producing an intermittent
#      -Inf/NaN that aborts the whole calibration - see the vars_bgc_all
#      comment in stage 3 below for the full trail.
#   f. STAGE-2 WEIGHT CAPS ARE NOW DATA-DENSITY-SCALED, NOT HAND-SET. v2's
#      balance_weights(cap = c(LKE_vol = 0.5, ...)) used fixed numbers.
#      v3 computes those caps from stage2_n_caps() (pest_funs.R) - the same
#      n/25 formula resolve_targets() already uses for stage 3, factored
#      out as n_scale_weight() - so BOTH the baseline-bias-equalisation
#      balance_weights() already does AND "trust proportional to data
#      density" apply together, instead of picking one.
#
# The stage-3 targets follow the decision outputs (the TLI components): nitrate
# is calibrated alongside ammonium/FRP/chl-a so total N is actually
# constrained. Secchi depth is NOT a target - GLM-AED does not emit RAD_secchi
# in a calibration forward run (it is a derived quantity AEME computes only on
# the full post-processed output), so a Secchi objective makes every forward
# run return NULL. The light climate is constrained indirectly through
# chlorophyll and temperature; Secchi stays a post-hoc validation check.
# Total N / total P ARE direct targets as of v3 (point e, above) - see
# vars_bgc_all in stage 3 below. LKE_tli3 was tried and excluded again.
#
# Physics drives biogeochemistry (stratification controls oxygen and nutrient
# recycling) but barely the reverse, so fixing the water balance, then the
# thermal structure, stops nutrient errors being compensated by volume or
# mixing errors. Each stage's winners are carried into the next - see
# "Holding parameters still between stages" below.
#
# Stage 3's parameter set, and the AED/phytoplankton switches those parameters
# depend on, are read from two curated tables rather than hard-coded:
#
#   data/processed/aed_sa_calib_params.csv    what is adjusted, over what
#                                             range, and which variables it is
#                                             expected to move (var_sim)
#   data/processed/aed_sa_calib_switches.csv  what must be switched on in
#                                             aed.nml / aed_phyto_pars.csv for
#                                             those parameters to do anything
#
# Requires PEST++: run aemetools::install_pest() once.
# Runtime on 27 cores: roughly ten minutes for a one-year simulation.
stopifnot(have_pest())

# ============================================================================
# Holding parameters still between stages
# ============================================================================
#
# Three mechanisms, in descending order of how hard they hold.
#
# 1. FREEZE - aemetools::freeze_param(), via carry(): value == min == max.
#    calib_aeme() and sa_aeme() detect
#    that, write the value into the model files once through
#    input_model_parameters(), and drop the parameter from the PEST control
#    file altogether. The model sees the value; PEST never knows it existed.
#
#    Use it when the later stage carries NO observation that constrains the
#    parameter. That is the whole justification: an adjustable parameter with
#    no informative observation is not "free", it is a random walk that
#    pestpp-ies will use to absorb noise in some other variable.
#    -> stage-1 water-balance parameters in STAGE 3 ONLY as of v2 (water
#       level is not a stage-3 target), and the stage-2 mixing parameters.
#       v2 no longer freezes them in stage 2 - see NARROW below - because
#       LKE_vol is a stage-2 target again and they are exactly the
#       parameters that target can inform.
#
# 2. NARROW - narrow(): keep the parameter adjustable, shrink its bounds to
#    the previous stage's posterior quantiles, and keep an observation in the
#    target set that constrains it. This is the honest carry-forward for a
#    parameter that genuinely couples two stages. Freezing it at the posterior
#    *mode* throws away the spread pestpp-ies just spent a whole stage
#    estimating; re-opening it at its prior bounds lets a chlorophyll error be
#    "fixed" by breaking the thermal structure.
#    -> light/Kw, which sets both the light climate and the heat input.
#       Re-opening Kw is only defensible while HYD_temp stays in the stage-3
#       target set (ANCHOR_VARS below) - otherwise nothing pushes back on it.
#    -> v2 adds `outflow` and `glm4.nml/outflow/seepage_rate` in stage 2,
#       bounded to the stage-1 posterior, now that LKE_vol is back as a
#       stage-2 target to push back on them.
#
# 3. LOCALISE - the pestpp-ies localizer (param_var_matrix / ctrl$localizer).
#    Keeps the parameter adjustable but zeroes the entries linking it to
#    observation groups it has no business responding to. The right tool when
#    a parameter must stay free but must not be updated by a particular data
#    type. Here the localizer is the intersection of the Morris screen and the
#    var_sim column of the parameter table, so a spurious Morris correlation
#    cannot wire, say, fsed_frp to oxygen.
#
# Two things PEST offers that aemetools does not currently emit, worth knowing
# if you hit their use cases:
#   * PARTRANS = fixed holds a parameter while keeping it visible in the .pst
#     and in the sensitivity/residual output. pest_param_table() emits only
#     "none"/"log" and aborts on min >= max, so freeze_param() - which removes
#     the parameter entirely - is the available equivalent.
#   * PARTRANS = tied ties an array (the three sediment zones, say) to one
#     lead parameter at a fixed ratio. That would cut the zoned parameters
#     from three adjustable to one without producing the short arrays that
#     kill the model. Today the zones are all adjustable and kept as complete
#     sets.
#
# One option that IS available: create_pest_control(prior_par_ensemble = )
# takes stage N's posterior parameter ensemble as stage N+1's prior, carrying
# the full covariance rather than a box of independent bounds. Prefer it over
# narrow() once you trust the stage-2 posterior.

# ---- configuration ---------------------------------------------------------

# Separate tree from pest_calib.R (v1) and pest_calib_v2.R - v1's
# checkpoints/pest dirs may still be in use by other work, and v3's stage-2/3
# changes (narrowing, targets, weighting) are different enough from v2's
# that reusing v2's tree (never actually run - staged-calibration-v2/ does
# not exist on disk) would mix incompatible checkpoints if it ever were.
#
# Moved OFF the Dropbox-synced project tree, 2026-09-21: each PANTHER
# worker's forward_run.R writes its own full lake directory copy plus a
# multi-hundred-MB to multi-GB netCDF output per model run, and with 16+
# concurrent workers Dropbox's sync client was competing for the same
# files - individual GLM runs that take ~9s in isolation were taking
# 6-7.5 minutes under sync contention (confirmed: send/receive round trip
# via the PANTHER run manager log, not just the misleading in-run timer),
# and one payload read even hit "invalid or incomplete compressed data"
# (aeme_fwd.rds), consistent with Dropbox locking/partially uploading a
# file a worker was mid-read on. Of the tree's ~148 GB, ~131 GB was
# disposable agent_* worker copies and ~1.7 MB was the actual s1/s2/s3
# checkpoints - migrated the ~24 GB of real content (checkpoints + pruned
# per-lake pest/ master dirs, KEEP_PEST_AGENTS=FALSE) to this local path
# once; the R scripts themselves stay in Dropbox as before.
# v4: separate tree from v3 (staged-calibration-v4, not -v3), so this
# lake's process experiments never touch or get confused with v3's
# already-complete Rototoa result - see the v4 header note at the top of
# this file. Same local/Dropbox split as v3, for the same reason (sync
# contention/corruption under PANTHER's per-run file churn).
ROOT     <- Sys.getenv("PEST_CALIB_ROOT", "C:/Users/data/Desktop/ac_lake_calib/staged-calibration-v4")   # EDIT default or set PEST_CALIB_ROOT: PEST working dirs and results DB (large)
# The one thing worth Dropbox's automatic cloud backup: the tiny (~1.7 MB
# total) s1/s2/s3 checkpoint .rds files stage_rds()/run_or_load() read and
# write (pest_funs.R). Everything else stays under the local ROOT above -
# the pest/ ensembles, build trees, and all PANTHER worker churn are large
# and reproducible by re-running the (already-checkpointed) pipeline, not
# worth the sync cost or the contention risk that moved ROOT off Dropbox
# in the first place.
CHECKPOINT_ROOT <- Sys.getenv("PEST_CHECKPOINT_ROOT", file.path("staged-calibration-v4", "checkpoints"))

# 2026-10-03: PEST_LOG_PHY=1 scores stage 4 on the log1p phytoplankton variables
# (AEME PHY_ltchla / PHY_lcyano) instead of the raw PHY_tchla / PHY_cyano. The
# raw variables are still simulated and written out for diagnostics; only the
# targets, chlorophyll sub-regions, weight caps and parameter-to-target links
# are translated. Needs the AEME build that defines the PHY_l* variables.
LOG_PHY <- nzchar(Sys.getenv("PEST_LOG_PHY"))
LOG_PHY_MAP <- c(PHY_tchla = "PHY_ltchla", PHY_cyano = "PHY_lcyano")
LOG_PHY_RAW <- names(LOG_PHY_MAP)
map_log_phy <- function(x) {
  if (!LOG_PHY || is.null(x)) return(x)
  i <- x %in% names(LOG_PHY_MAP); x[i] <- unname(LOG_PHY_MAP[x[i]]); x
}
if (LOG_PHY) message("PEST_LOG_PHY: stage 4 targets PHY_tchla/PHY_cyano -> ", paste(LOG_PHY_MAP, collapse = ", "))

# 2026-10-03: PEST_REGION_TRANSFORM="PHY_tchla=log1p,PHY_cyano=log1p" gives every
# calibration / SA sub-region of those AEME variables a `transform` (aemetools'
# per-sub-region transform: observed and modelled values are put through the same
# function before scoring). Preferred over PEST_LOG_PHY (derived AEME variables):
# the variable keeps its name, so targets, regions and parameter links are untouched.
# Needs an aemetools build that understands region$transform.
REGION_TRANSFORM <- local({
  x <- Sys.getenv("PEST_REGION_TRANSFORM")
  if (!nzchar(x)) return(list())
  kv <- strsplit(strsplit(x, ",", fixed = TRUE)[[1]], "=", fixed = TRUE)
  stats::setNames(lapply(kv, function(z) trimws(z[2])), vapply(kv, function(z) trimws(z[1]), ""))
})
add_region_transform <- function(regions) {
  if (!length(REGION_TRANSFORM)) return(regions)
  for (nm in names(regions)) {
    tr <- REGION_TRANSFORM[[regions[[nm]]$var]]
    if (!is.null(tr)) regions[[nm]]$transform <- tr
  }
  regions
}
if (length(REGION_TRANSFORM)) message("PEST_REGION_TRANSFORM: ",
  paste(names(REGION_TRANSFORM), unlist(REGION_TRANSFORM), sep = " -> ", collapse = "; "))
dir.create(CHECKPOINT_ROOT, recursive = TRUE, showWarnings = FALSE)
START    <- "2014-07-01"
STOP     <- "2023-06-30"
SPIN_UP  <- 365 * 5

# Shortened analysis window (2026-09-25, user request): stage 3's full
# 2009-2023 (~14 year, 5-year-spin-up) run was taking ~10-20 min PER
# evaluation once every AED/BGC state variable's netCDF output (~1.9 GB
# per run for Rototoa, vs a few hundred MB for stages 1/2) was competing
# for disk I/O across NCORE=20 parallel workers - unsustainable for the
# ~800+ evaluations a Morris screen + pestpp-ies solve need. Chosen by
# sliding 2-year windows across every sub-region (surf/bot temp, oxy,
# amm, nit, tn, frp, tp, chla, schstb, thmcln - see
# analyse_subregion_windows.R in the session scratchpad) and picking the
# one with the fewest empty regions and highest minimum count, RESTRICTED
# to windows that retain real LKE_lvlwtr observations (level obs only
# start 2013 - an earlier, nominally "denser" window would force stage
# 1/2's water-balance target onto synthesize_level_target()'s synthetic
# fallback instead of real data, a real regression given stage 2 already
# calibrated well against the genuine record). 2021-01-01 to 2023-01-01
# won by a wide margin (10-50x the observations of every other
# level-obs-retaining candidate in nearly every region) - data density
# for Rototoa evidently improved substantially in recent years. Bottom-
# nutrient regions (bot_amm/bot_nit/bot_frp/bot_tp) stayed at/near zero
# in EVERY 2-year window tested, including this one - that reflects
# genuine, window-independent sparsity in the deep nutrient record (they
# were already the ones resolve_targets() dropped from the full 9-year
# run for lack of min_obs=8), not a cost of shortening the period.
#
# ANALYSIS_STOP/ANALYSIS_START feed trim_forcing_period() (pest_funs.R),
# which trims input(aeme)$meteo and every inflow stream to this absolute
# range BEFORE set_time()/suggest_sim_period() ever see it - passing a
# shorter start=/stop= to set_time() alone does NOT shorten a run (see
# the now-historical START_PHYS/SPIN_UP_PHYS attempt below, 2026-09-21,
# confirmed ineffective for exactly this reason). SPIN_UP_SHORT (2 years,
# per the user's own example figure) sits BEFORE ANALYSIS_START in the
# trimmed forcing range, giving a ~4 total simulated years (2019-01-01 to
# 2023-01-01) against the previous ~14.
ANALYSIS_START  <- "2021-01-01"
ANALYSIS_STOP   <- "2023-01-01"
SPIN_UP_SHORT   <- 365 * 2
FORCING_START   <- as.character(as.Date(ANALYSIS_START) - SPIN_UP_SHORT)
FORCING_STOP    <- ANALYSIS_STOP
# Opt-in switch for the block above - see its own USE_SHORT_PERIOD comment
# at the point it's read, in the per-lake loop. FALSE (the long, well-
# tested START/STOP/SPIN_UP window) unless a caller explicitly opts in.
#
# 2026-09-25 (user request): TRUE for this 9-lake physics run, now driving
# pick_dense_window() (pest_funs.R) PER LAKE rather than Rototoa's own
# hardcoded ANALYSIS_START/FORCING_START dates above (those stay defined,
# unused while this is on, as the record of Rototoa's own manual window
# choice for its stage-3 BGC work).
USE_SHORT_PERIOD  <- TRUE
SHORT_WINDOW_YEARS <- 2
SHORT_SPINUP_YEARS <- 2
# v4 (2026-09-21): shortened window for stages 1/2, ATTEMPTED then
# REVERTED - kept defined (unused) as a starting point for a correct
# version later, not wired into aeme0/period anymore (see the comment at
# the top of the lake loop where aeme0_phys used to be built).
#
# Why it didn't work: suggest_sim_period() (AEME, internal) always
# resolves ITS OWN window from the model's forcing-data availability
# (`earliest <- window$start + spin_up`, where window$start sits near the
# forcing data's native beginning, not whatever start= was passed to
# set_time()) - passing START_PHYS here had no effect, confirmed directly
# (the resolved period still covered ~2013-2023, not ~2018-2023).
# Reducing spin_up doesn't shorten the run either - it only moves where
# SCORING starts within an unchanged, still-full-length simulated range.
# A genuinely shorter run needs the underlying inflow/met boundary-
# condition data itself trimmed to a shorter absolute range before
# suggest_sim_period() ever sees it.
#
# The 5-years-scored reasoning below is still the right target if/when
# that's implemented: the Rototoa water-level drift this version exists
# to fix is small in year 1 and only reaches ~6.5 m by year 5 (at the
# observed ~1.3 m/year rate) - cut much shorter and the fix's own test
# stops being able to tell whether it worked. 1 year spin-up (vs stage
# 3's 5) would be safe for stages 1/2 specifically because they are
# use_bgc = FALSE - water/heat balance equilibrates fast; it's sediment
# nutrient pools (stage 3 only) that need the long spin-up.
START_PHYS   <- "2018-07-01"
STOP_PHYS    <- STOP
SPIN_UP_PHYS <- 365 * 1
MODEL    <- "glm_aed"
# Index into aeme_baseline_upstream / upstream_wlev_calib, both ordered the
# same way as v3's lake loop (1 Tomarata ... 10 Rototoa). Every per-lake
# section already loops `for (i in LAKE_IDX)`, so this can be a vector -
# confirmed 2026-09-25: no other code assumes a single scalar lake.
#
# 2026-09-25 (user request): the other 9 lakes (everything except Rototoa,
# index 10, already fully calibrated through stage 3), physics only
# (RUN_STAGE3 <- FALSE below) - stages 1/2 use the fixes this session
# built against Rototoa (region-native calib_aeme(), the get_var_indices()
# depth-grid fix, the generalised dry-lake guard, per-zone sediment
# force_keep) for the first time on the other 9 lakes.
# 2026-09-25 night (user request, overnight run): back to Rototoa (index
# 10) alone, RUN_STAGE3 <- TRUE - the combined TSS+BGC "stage 4/5" this
# whole session's diagnostic work was building toward. The comment above
# calling Rototoa "already fully calibrated through stage 3" is stale -
# no s3_LID50270_rototoa.rds exists on disk (confirmed directly): the
# earlier stage-3 attempt crashed mid iteration-3 and was never
# checkpointed (see SESSION_CHECKPOINT_2026-09-25.md). This run produces
# Rototoa's first real stage-3 checkpoint, with vars_bgc_all/sr_all/w_cap
# trimmed to temp/oxy/frp/nit/amm/chl-a/NCS_ss1 (see those blocks' own
# comments) and aed_sa_calib_params.csv's 5 aed_noncohesive rows seeded
# from tonight's dedicated TSS calibration (checkpoints/
# s26_LID50270_rototoa.rds) rather than the original broken template
# defaults.
LAKE_IDX <- c(8L, 9L, 1L, 4L, 5L, 6L)  # 2026-10-01: RESTART after fixing a
                 # real resolve_targets() bug - Spectacle(2) already
                 # complete (checkpointed, not rerun). Pehiakura Small(7)
                 # failed with an UNRELATED error ("No observations fall
                 # within the simulation period") - a genuine, separate
                 # data-availability problem for that lake, left out of
                 # this restart rather than retried blindly; needs its own
                 # investigation. Te Kanae(8) is why this restart exists:
                 # its surf_amm sub-region had all 8 aligned NIT_amm
                 # observations exactly 0.005 (a detection-floor value) -
                 # zero within-sub-region variance that the existing
                 # LAKE-WIDE constant-value guard never caught (the lake's
                 # full NIT_amm record has real variance) - and PEST++'s
                 # own weight-from-variance calculation exploded to ~5e17,
                 # taking phi to ~1e30 and making the whole stage-4
                 # objective meaningless. Fixed at the root in
                 # resolve_targets() (pest_funs.R): sub-regions are now
                 # ALSO checked for degenerate variance using the exact
                 # same get_var(remove_spin_up=TRUE) alignment aemetools
                 # itself uses, not just a raw date/depth filter (a raw
                 # date-filtered check alone still showed real variance,
                 # sd=0.24, for 9 candidate rows - it's only after
                 # aligning to the model's own simulated period, which
                 # drops to 8 rows, that they're all identical). Applies
                 # generally, not just to Te Kanae - protects every lake
                 # going forward. Pupuke(9)/Tomarata(1) (recovering
                 # interrupted work) come next, then Wainamu(4)/Okaihau(5)/
                 # Whatihua(6) (no downstream
                 # dependency) last.
                 # user needs (ideally) all lakes calibrated by tomorrow
                 # evening. Rototoa (10) and Kawaupaka (3) already complete
                 # under the final recipe (narrow()+NCS_ss1-excluded+K_N/
                 # K_P-removed) - not rerun. This covers the remaining 8:
                 # Pupuke (9, never finished under recipe_v2 - was mid-IES
                 # when stopped for the sed_temp_mean/NCS_ss1 investigation),
                 # Tomarata (1, redone here under the FINAL narrow()
                 # config - its most recent checkpoint was the frozen-
                 # variant test, backed up to
                 # s4_LID21871_tomarata_frozen_ncsexcl_backup.rds and
                 # cleared), Spectacle (2, first-ever v6 run - has stage-1/2
                 # checkpoints from historical work only), and Wainamu(4)/
                 # Okaihau(5)/Whatihua(6)/Pehiakura Small(7)/Te Kanae(8) -
                 # all first-ever v6 runs, all confirmed to have stage-1/2
                 # checkpoints ready (verified directly before launch).
                 # Tomarata and Spectacle also pick up
                 # freeze_sediment_flux_data()'s real measured sediment
                 # flux values automatically (both have entries in
                 # data/processed/sediment_flux_params.csv) - a free
                 # improvement, not an extra time cost. Running strictly
                 # sequentially (the main loop's own per-lake tryCatch
                 # isolation + run_or_load() checkpointing already handles
                 # this, no separate driver needed) so a crash on one lake
                 # doesn't cost the others. NOPTMAX dropped to 3 (below)
                 # specifically for this batch's time budget - not a
                 # judgement that 3 is definitively correct, a deadline
                 # trade-off given ~8 lakes need to fit in ~24-27 hours.
                 # Queued behind the still-running Kawaupaka frozen+
                 # NCS_ss1-excluded test (master PID 34356) via a
                 # PowerShell watcher, same box/shared results.db.
                 #
                 # (Was LAKE_IDX <- 3L for that Kawaupaka-only test - see
                 # above. Before that, was LAKE_IDX <- 1L for the Tomarata-
                 # only test of frozen+NCS_ss1-excluded - see above for why
                 # that result didn't settle the narrow-vs-frozen question.
                 # Before that, was LAKE_IDX <-
                 # c(10L, 3L, 9L, 1L) for the overnight 4-lake batch - user
                 # needs a DECIDED calibration method by 2026-09-30, so
                 # rather than keep validating fixes one lake/one variable
                 # at a time, that batch locked in every
                 #
                 # (Was LAKE_IDX <- c(10L, 3L, 9L, 1L) for the overnight
                 # 4-lake batch - user needs a DECIDED calibration method
                 # by 2026-09-30, so rather than keep validating fixes one
                 # lake/one variable at a time, that batch locked in every
                 # fix landed 2026-09-29 (TSS tau_0[2]/
                 # removal) are live this run. s4 checkpoints backed up and
                 # removed for all 4 so stage 4 reruns fresh.
                 #
                 # (Was LAKE_IDX <- 10L for a Rototoa-only rerun, itself
                 # replacing LAKE_IDX <- 3L for a still-running, separately-
                 # launched Kawaupaka-only NOPTMAX=8 diagnostic - the user
                 # asked to stop that process given the deadline; see the
                 # NOPTMAX comment below for its own history.)
RUN_HEAT_FLUX_SCREEN <- TRUE  # v6: SA-only ce/ch/cd screen after stage 2, never calibrated
RUN_STAGE3 <- TRUE  # v6: TSS-only stage (new)
RUN_STAGE4 <- TRUE  # v6: temp+oxy+nutrients+chl-a+cyano, TSS narrow()'d in from stage 3 (was "stage 3" in v4)
# parallel::detectCores() (no args) is LOGICAL cores (32 on this box, via
# hyperthreading) - but each PANTHER worker runs one single-threaded
# GLM-AED forward run + R scoring, CPU-bound work that hyperthreading does
# not meaningfully double. 0.85*32=27 concurrent workers on this box's 24
# PHYSICAL cores (i9-14900K, mixed P/E) was real oversubscription, not an
# artifact: diagnosed 2026-09-21 after repeated stage-3 "stopped making
# progress" (10-min no-progress) failures on Spectacle/Wainamu/Pupuke - a
# reproduction showed every one of 27 concurrent workers taking 6-7 min for
# a run that takes ~9s alone. A Defender exclusion for the project dir (the
# AV was also scanning the heavy per-run file churn) helped only partially
# (down to ~1-2 min/run, still not enough - Wainamu re-failed even after
# it). Switched to physical core count with headroom (0.7*24=16), which
# held cleanly - but that fix was layered with a second, separate
# contention source found the same day (the whole build/PEST tree living
# under Dropbox sync - see the ROOT comment above) that was likely doing
# most of the damage. With ROOT now off Dropbox and the AV exclusion in
# place, bumped back up to 20 (2026-09-21) to test whether 16 was more
# conservative than necessary now that both root causes are addressed -
# watch for a repeat of the "stopped making progress" pattern; drop back
# toward 16 if it recurs.
NCORE    <- 24L  # raised from 20 (2026-09-28, user request) for the
                 # Tomarata/Kawaupaka/Pupuke sequential run - one lake at a
                 # time, so 24 of 32 logical cores is safe headroom, not
                 # oversubscription (would only oversubscribe if lakes ran
                 # concurrently, which they don't here - see run_3lakes_sequential.R)
MORRIS_R <- 4      # Morris trajectories; raise for a screen you will act on
# pestpp-ies iterations. NOT raised to widen the posterior: each iteration
# culls high-phi realisations (see BAD_PHI_SIGMA) and shrinks parameter
# variance, so more iterations make the ensemble thinner and narrower, not
# wider. The lever for a better-resolved posterior is N_REALS.
#
# Raised 3 -> 8 (2026-09-25 night, user request - same increase already
# applied to tonight's standalone TSS calibration). The caveat two lines
# up is real and applies MORE here than in that simpler TSS-only stage:
# stage 3 already adaptively grows its ensemble (IES_TARGET_SURVIVORS/
# IES_REALS_CAP below) specifically to compensate for a high BGC forward-
# run crash rate, and more iterations culling on top of that will narrow
# the surviving posterior further than production's own tuned default -
# a real trade-off (more convergence pressure vs. a thinner posterior),
# not a free improvement. Accepted per explicit repeated instruction, not
# because the trade-off stopped applying.
#
# Dropped 8 -> 5, 2026-09-27 (user request), based on the actual stage-4
# BGC phi trajectory from the NOPTMAX=8 Rototoa run this setting
# previously produced: 3826 -> 2011 -> 1377 -> 1250 -> 1272 -> 1266 ->
# 1257 (iterations 0-6) - effectively flat/plateaued from iteration 3
# onward, with iterations 4-6 spending three more expensive iterations
# (each culling the ensemble further per the caveat above) to move phi by
# <1%. 5 iterations keeps the same convergence with less unnecessary
# posterior-narrowing; watch the new run's own trajectory in case a
# different parameter set (sed_temp_mean now narrowed in, see
# S2_NARROW_NAMES above) changes this plateau point.
NOPTMAX  <- 3  # 2026-09-30: back to 3 for the 8-lake deadline batch - user
                # needs (ideally) all lakes calibrated by tomorrow evening,
                # ~8 lakes need to fit in ~24-27 hours, and 3 is the
                # well-justified production default (Rototoa's own
                # historical plateau point). Not re-opening the NOPTMAX=8
                # question tonight - a deadline trade-off, not new evidence
                # that 3 is definitively sufficient everywhere.
                #
                # (Was 4, 2026-09-29: raised 3 -> 4, user request, for the
                # consolidated 4-lake batch (Rototoa/Kawaupaka/Pupuke/
                # Tomarata) - a compromise between the well-justified
                # production default (3) and the still-open NOPTMAX=8
                # question (that Kawaupaka diagnostic was stopped early,
                # deadline permitting no time to finish it properly - see
                # LAKE_IDX above). Not chosen from completed evidence that
                # 4 is the right number; a pragmatic middle ground given
                # time constraints.
                #
                # dropped 5 -> 3 (2026-09-28, user request: "less iterations")
                # for the Tomarata/Kawaupaka/Pupuke validation run - Rototoa's
                # own completed phi trajectory (1882 -> 742.4 -> 394.3 -> 338 ->
                # 308.2 -> 290, iterations 0-5) had already plateaued by
                # iteration 3-4 (>90% of the total phi reduction captured by
                # iteration 3), so 3 iterations should capture most of the
                # convergence at roughly half the wall-clock, appropriate for
                # a validation run on lakes not yet known to behave like
                # Rototoa. Revisit per-lake if a phi trajectory clearly hasn't
                # plateaued by iteration 3.
# Ensemble size. At 50, 60-78% of the stage-3 prior draws failed the GLM-AED
# forward run and pestpp-ies then culled more each iteration, leaving 3-12
# surviving realisations per lake - too thin for the downstream load bands
# (pest_calib_downstream.R). Raised to 200 for this re-run of the 4
# downstream-feeding lakes; expect ~35-50 survivors.
#
# 2026-09-25: dropped to 40 (a clean multiple of NCORE=20, so every batch
# fully occupies all workers with no partial/idle batch) for a fast
# physics-only (stage 1/2) run across the other 9 lakes - user's explicit
# request, trading ensemble width for turnaround. Not appropriate for a
# BGC stage-3 run (see the 50-vs-200 feasible-fraction note above - a
# narrow prior draw there leaves too few survivors), but stage 1/2's water-
# balance/thermal targets are far better-constrained and don't need 200.
N_REALS  <- 40    # ensemble size (floor; stage 3 grows it - see below)

# Stage-3 adaptive draw size. The wide BGC prior is mostly infeasible - the
# crash is combinatorial (a phytoplankton group blooms and drives a state
# negative), not one bad bound - so run_stage() measures the feasible fraction
# (from a prior run's .rec, else the Morris runs) and inflates ies_num_reals so
# roughly IES_TARGET_SURVIVORS realisations get through the initial forward-run
# cull, up to IES_REALS_CAP. Set IES_TARGET_SURVIVORS to NULL / remove to keep
# the fixed N_REALS. Each extra realisation is one more parallel forward run
# per iteration.
IES_TARGET_SURVIVORS <- 120L
IES_REALS_CAP        <- 500L
# Loosen the per-iteration phi cull for stage 3 (pest_funs.R reads this for
# stage 3 only): keep realisations out to mean + 3*sd instead of 2*sd, so a
# marginally worse fit is retained as spread rather than dropped. Stages 1-2
# keep the default 2.
BAD_PHI_SIGMA <- 3
THR      <- 0.10   # keep a parameter reaching 10% of the leading mu_star

# Per-lake outlet override, read by fix_outlet_depth() in pest_funs.R. The
# floating skim (OUTLET_DEPTH) crashed Te Kanae even at 0.2 m: its level
# genuinely dries to 0 in the last month of the 9-year record, and ANY
# positive floating depth is invalid once the surface it's relative to
# vanishes. A fixed (non-floating) outlet doesn't have that failure mode -
# confirmed clean (0 non-finite over the full run, vs 43/1986 floating) at a
# sill set to the lake's own typical stand: base_elev + the median simulated
# LKE_vol from its (already good) stage-2 fit.
#
# Pehiakura Small (LID49307) has the identical failure mode, diagnosed
# 2026-09-20 after two identical silent stage-2 IES failures ("forward run
# failed... wrote no output", no other diagnostic): aemetools's internal
# run_and_fit()/.raf_wlev() silently returns na_value (999) for EVERY target
# variable, not just LKE_vol, whenever the simulated level is <= 0 on ANY
# day in the window - and it is, for the last 5 days of the 2018-2022 run,
# under the floating outlet. Confirmed fixed the same way: a fixed-elevation
# outlet at base_elev (66.11) + median non-zero simulated level (6.50) =
# 72.61 keeps level entirely positive (6.4-12.4 m) across the full run (0
# days <= 0, vs 5 floating). Spectacle/Rototoa completed cleanly on the
# floating skim and are left as-is.
OUTLET_OVERRIDE <- list(
  LID49895 = list(elev = 49.3 + 13.41),  # Te Kanae: base_elev + stage-2 median level
  LID49307 = list(elev = 72.61)          # Pehiakura Small: base_elev + median non-zero level
)

# Per-lake GLM &mixing namelist override, read by fix_mixing() in
# pest_funs.R (applied in fresh_build(), so every stage/rebuild for this
# lake gets it automatically). Pupuke's shipped deep_mixing=2 default is too
# weak (at the default coef_mix_hyp=0.5) to fully re-entrain its ~53 m water
# column once stratified, causing the seasonally-oxygenated depth to shrink
# monotonically every year (48.9 m -> 15.1 m over 2013-2022 in the
# unmodified baseline). Confirmed via a 5-variant mixing-scheme experiment
# on Pupuke's default model (2026-09-20): switching to deep_mixing=1 (or
# quadrupling coef_mix_hyp under deep_mixing=2) both hold the oxygenated
# depth rock-steady at ~52 m every year. deep_mixing=1 is the simpler fix.
MIXING_OVERRIDE <- list(
  LID50151 = list(name = "deep_mixing", value = 1)   # Pupuke
  # Rototoa REMOVED from this override 2026-09-24 (was deep_mixing=1,
  # 2026-09-22 - 2026-09-24): deep_mixing=1 fixed the permanent-
  # stratification/anoxia problem deep_mixing=2's shipped default caused,
  # but at the cost of a severe new one - direct plot_ts(depth_range=
  # c(20,22)) inspection of the stage-2 checkpoint showed the simulated
  # bottom temperature doing a sharp, complete annual sawtooth (~11 ->
  # 18degC every year, closely tracking the SURFACE seasonal range) that
  # the real lake's bottom does not do (observations stay damped, mostly
  # 13-16degC). Quantified: bottom KGE=-0.48, driven almost entirely by
  # variability (alpha=2.39, i.e. the model swings 2.4x more than
  # observed) rather than bias (beta=0.97, nearly unbiased) - confirmed
  # this is deep_mixing=1's own scheme behaviour, not a tunable side
  # effect: sweeping every OTHER frozen mixing coefficient
  # (coef_mix_conv/shear/turb/KH) under deep_mixing=1 moved alpha by only
  # ~2%, nothing close to fixing it. Rototoa now uses the shipped
  # deep_mixing=2 scheme again, with coef_mix_hyp re-opened (below) as the
  # actual calibrated fix for the original stratification-collapse
  # problem, instead of the deep_mixing=1 structural override.
)

# Keep every PEST run directory, so each lake's posterior parameter ensemble
# stays readable after the run. The agent_* subdirectories inside them are
# deleted once a solver finishes: they are 27 copies of the lake directory and
# account for 10.6 GB of an 11 GB stage-3 tree, while everything
# read_pest_ensemble() needs lives in the 440 MB master.
KEEP_PEST_AGENTS <- FALSE

# NOTE: this vector is used ONLY for suggest_sim_period()'s window
# selection below, not as the actual stage targets (each stage declares
# its own). Deliberately NOT widened to include NIT_tn/PHS_tp/LKE_tli3
# (2026-09-18): suggest_sim_period() picks a window balancing coverage
# across everything in this list, and those three are much sparser than
# the core physical/BGC variables - adding them shifted the chosen window
# for at least one lake to a period with NO HYD_temp/thmcln/strat data at
# all, crashing calib_aeme_pest()'s pest_obs_table() ("No observations for
# HYD_temp, HYD_thmcln, HYD_strat within the simulation period"). The new
# stage-3 targets still get evaluated over whatever period this list
# chooses; they just don't get a vote in choosing it.
targets <- c("LKE_vol", "HYD_temp", "HYD_schstb", "LKE_nrgtot",
             "CHM_oxy", "NIT_amm", "NIT_nit", "PHS_frp", "PHY_tchla")

PCOLS <- c("model", "file", "name", "value", "min", "max", "group", "index")

# Stage-1 water-balance parameter names (as they appear in s1_adj$name),
# narrowed rather than frozen going into stage 2 - see the v2 header note.
WBAL_NAMES <- c("outflow", "outflow/seepage_rate")

# v4 (2026-09-21): evaporation-relevant parameters, moved INTO stage 1 -
# see the Rototoa root-cause investigation this constant exists to fix.
#
# Diagnosis: Rototoa's simulated LKE_vol drains ~13 m over 2013-2023
# under stage-1-best parameters alone, while the OBSERVED level is flat
# (68.2-69.0 m, no trend) and the raw inflow+precip-outflow boundary-
# condition balance is net POSITIVE every year (would predict a RISING
# lake, not a draining one). The only sink big enough and not visible in
# those CSVs is evaporation, computed internally by GLM from the met
# forcing. Stage 1's only parameters (outflow, outflow/seepage_rate) are
# boundary-condition multipliers - neither can touch evaporation at all,
# so an evaporation-forcing bias is structurally invisible to stage 1,
# however well its own (narrow) objective scores.
#
# It gets worse, not better, in stage 2: MET_wndspd and rh_factor (the
# two parameters a bulk aerodynamic evaporation formula is most directly
# sensitive to) DO become adjustable there - but stage 2's LKE_vol
# weight comes from balance_weights(), which sets each target's weight
# from its BASELINE (1-KGE) residual relative to whichever target fits
# best at baseline (stage2_baseline_kge(), using stage 1's OWN best
# params). Because stage 1 was blind to the evaporation bias, that
# baseline already looks bad for LKE_vol before stage 2 touches
# anything - so balance_weights() discounts it further, right when it's
# needed most. Confirmed directly: stage-1-params + stage-2-best met
# factors drains the lake an additional ~2 m beyond stage-1-best alone by
# 2023, growing from near-zero difference in 2013.
#
# Fix: give stage 1 the lever it's missing, tied to the only observation
# it has (LKE_vol). MET_wndspd/rh_factor move here; MET_radswd/
# MET_tmpair deliberately do NOT - they are the dominant drivers of the
# thermal profile specifically, and stage 1 has no temperature
# observation to keep them honest, so letting them move here risks
# silently corrupting temperature before stage 2 even starts. This also
# repairs the stage-2 weighting side effect for free: stage2_baseline_kge()
# is computed from stage 1's best params, so once stage 1 can actually
# fix the water balance, LKE_vol's baseline stops looking artificially
# bad and balance_weights() stops artificially discounting it - no direct
# change to balance_weights()/the LKE_vol cap needed for this first
# test. If the drift persists even with this fix, revisit the cap next.
S1_NARROW_NAMES <- c("MET_wndspd", "meteorology/rh_factor")

# Stage-2 parameter names narrowed (not frozen) into stage 3, as of v3
# (point d in the header note): light/Kw (v2, unchanged) plus mixing
# coefficients and MET_wndspd/MET_radswd, each with a real BGC channel
# (stratification/turnover -> oxygen & nutrient recycling; wind -> mixing
# energy and gas exchange; shortwave -> GPP, not just heat) and HYD_temp
# staying a stage-3 anchor to push back on them. Pure bias-correction
# factors (MET_tmpair, MET_radlwd, rh_factor) are deliberately NOT
# included - no principled BGC channel by which a chlorophyll/oxygen
# residual should re-tune an air-temperature bias correction. This split
# is a design choice, not re-validated per lake - see the header note for
# the recommended single-lake sanity check before trusting it at the full
# 10-lake scale.
#
# mixing/coef_mix_* REMOVED, 2026-09-19: no longer stage-2 adjustable at
# all (frozen at AEME's shipped defaults - see the s2_adj comment in stage
# 2 below), so there is nothing for stage 3 to re-open here anymore.
#
# mixing/coef_mix_hyp is the one exception, added back 2026-09-21 for
# Pupuke only (see MIXING_OVERRIDE and the s2_adj comment below): once
# deep_mixing is overridden to 1 for that lake, coef_mix_hyp moves from "a
# generic turbulence-closure coefficient" to the primary control on how
# much energy reaches the hypolimnion each timestep under GLM's simpler
# deep-mixing scheme - exactly the kind of lake-specific physical
# sensitivity S2_NARROW_NAMES already exists to admit. Listing it here is
# harmless for every other lake: narrow()/carry() only act on a name that
# is actually present in that lake's stage-2 adjustable/kept set, and
# coef_mix_hyp is added to s2_adj for Pupuke alone.
#
# sediment/sed_temp_mean ADDED, 2026-09-27: originally excluded from this
# list on the same "no principled BGC channel" reasoning as MET_tmpair/
# MET_radlwd/rh_factor above - but a Morris screen re-opening every
# currently-carried physics parameter against stage 4's own BGC targets
# (diagnose_carry_sensitivity_check.R, run against the Rototoa stage-3
# checkpoint) found sed_temp_mean's relative sensitivity on bot_amm/
# bot_frp/bot_nit exceeded every parameter in the still-adjustable BGC
# sample (R_resp x3 groups, rnitrif, kpo4p, rdom_minerl) - consistent with
# AED's sediment nutrient flux terms (Fsed_amm/Fsed_frp/Fsed_nit) being
# temperature-scaled, so a mis-set sediment temperature directly biases
# bottom-water nutrient release in a way no BGC parameter can compensate
# for. sed_temp_amplitude was screened at the same time and showed exactly
# zero sensitivity on every stage-4 target - left carried/frozen. Caveat
# noted at the time: both findings came from only 4 Morris trajectories
# with sensitivities 10+ orders of magnitude below outflow's - a real
# signal, not a definitive one; flagged as worth re-checking once stage 4
# completes.
#
# sediment/sed_temp_mean REMOVED again, 2026-09-29: that re-check now
# done (compare_thermal_freeze_vs_narrow.R, Rototoa's canonical s4 vs the
# earlier s4_v1_no_sedtemp_narrow checkpoint where it was frozen) -
# letting it move under stage 4's full joint objective actively HURTS
# fit almost everywhere it was supposed to help: CHM_oxy NSE 0.664 -> 0.281,
# NIT_amm 0.340 -> -0.002, HYD_temp 0.559 -> 0.509, PHS_frp -3.07 -> -11.7
# (frozen -> narrowed, i.e. narrowing makes all four WORSE). The only
# variable that improves when it's adjustable is NCS_ss1 (-30.1 -> -1.4) -
# and NCS_ss1 is a MONITORING target here, not something stage 4 has any
# adjustable aed_noncohesive parameters left to fit directly (see the
# stage-4 header) - so the screen's original finding (sed_temp_mean is the
# most SENSITIVE parameter for bot_amm/bot_frp/bot_nit) was true but
# pointed the wrong direction for what the optimizer actually does with
# that sensitivity: it uses the lever to chase NCS_ss1's residual (a
# variable independently found to be structurally hard to fit - flat phi
# at every lake tested, likely an event-timing mismatch no parameter can
# fix) at the cost of three well-behaved, physically real variables.
# Sensitivity to a target is not the same as narrowing being a net
# improvement once the full multi-objective trade-off plays out - back to
# frozen/carried, matching sed_temp_amplitude's treatment.
# sediment/sed_temp_mean REMOVED AGAIN, 2026-09-30 (3rd reversal): the
# narrow()+NCS_ss1-excluded combination WAS run overnight (Rototoa,
# Kawaupaka - both complete) and did NOT deliver on its own hypothesis:
# Rototoa's NIT_amm NSE went to -0.385, WORSE than both the original
# narrow()+NCS_ss1-included run (-0.002) AND the frozen+NCS_ss1-included
# run (0.340, the best of all three). PHS_frp improved vs. the original
# narrow() run (-11.7 -> -4.2) but still well short of frozen's -3.07.
# Overall phi did genuinely improve and kept converging (unlike every
# prior attempt) - HYD_temp/CHM_oxy/PHY_tchla all better than the
# original narrow() run - so excluding NCS_ss1 is doing real, separate
# good; but narrow() itself, even with that confound removed, still isn't
# winning for the nutrients it was specifically added to help. Testing
# the one remaining untested combination: frozen + NCS_ss1 EXCLUDED - the
# hypothesis being that freezing protects sed_temp_mean itself (as it did
# in the frozen+NCS_ss1-included test) while NCS_ss1's exclusion still
# cleans up the other ~50 adjustable parameters' objective independently
# (freezing sed_temp_mean takes it out of the search space entirely, so
# NCS_ss1's presence/absence can no longer act through IT specifically -
# but every other shared parameter is still affected by NCS_ss1's
# exclusion). Testing on Tomarata only, per user's explicit request, given
# the deadline - not re-running Rototoa/Kawaupaka (already checkpointed,
# both showed real net improvement under narrow()+excluded, left as-is)
# or Pupuke (was mid-run, stopped, to be resumed/decided separately).
# 2026-09-30: FINAL for tonight's deadline-driven 8-lake batch - back to
# NARROW (not frozen). This is the one combination with a complete,
# real-calibration validation (Rototoa: phi 480->252, genuine sustained
# convergence, HYD_temp/CHM_oxy/PHY_tchla all improved; Kawaupaka:
# clean-data confirmation run, completed and restored). The frozen
# variant's Kawaupaka test (the one that would have given a clean,
# uncontaminated answer) never finished before this deadline forced a
# stop - not enough evidence to prefer it over narrow's own validated
# result. Not re-litigating further tonight; ship what's proven.
S2_NARROW_NAMES <- c("light/Kw", "MET_radswd", "MET_wndspd", "mixing/coef_mix_hyp",
                     "sediment/sed_temp_mean")

#' carry(), guarded against the degenerate "freeze nothing" case.
#'
#' carry(s1, except = WBAL_NAMES) is a real, reproducible bug in v3:
#' WBAL_NAMES covers BOTH of stage 1's only two kept parameters (outflow,
#' outflow/seepage_rate - see s1_adj), so narrowing all of them into stage 2
#' (the whole point of WBAL_NAMES) leaves carry()'s `keep` set empty.
#' aemetools::carry_param() does not handle an empty `names` gracefully
#' ("replacement has 1 row, data has 0", inside its own name_full
#' assignment) - it was never designed for "freeze nothing", only "freeze
#' this named subset". Route around it: when nothing would be frozen, hand
#' back an empty parameter table directly (the same POOL[0, PCOLS] pattern
#' stage 1 already uses for its own empty frozen argument) instead of
#' calling carry_param() on an empty selection at all.
carry_except <- function(res, except) {
  # carry()'s real output is res$best filtered to `keep`, columns
  # PCOLS + name_full (add_name_full() is applied inside carry_param()) -
  # `res$best[0, ]` matches that shape exactly (same columns, same names),
  # unlike POOL[0, PCOLS] (no name_full column), which would silently
  # misalign or error when rbind()'d against a real carry() result at the
  # call site (stage 3's frozen argument combines this with carry(s2, ...)).
  if (length(setdiff(res$kept$name, except)) == 0) return(res$best[0, ])
  carry(res, except = except)
}

# ============================================================================
# Stage-2 target weight balancing (v2)
# ============================================================================
#
# calib_aeme() turns each stage target into one pseudo-observation
# (value = 1, sim = KGE(model, obs)) scaled by `weights[[var]]`, so a
# target's phi share is weight^2 * (1 - KGE)^2. A target whose baseline
# (1-KGE) is far larger than the others' - for any reason, not just a real
# calibration difficulty - will dominate phi from iteration 0 regardless of
# the weight cap you hand it, unless that cap accounts for the baseline
# gap. stage2_baseline_kge() measures each target's (1-KGE) from a single
# forward run at stage-1-best / stage-2-default parameters (no PEST
# involved); balance_weights() turns that into weights that equalise each
# target's *baseline* phi share, before any hand-set importance cap is
# applied on top.

#' One forward GLM-AED run: stage-1-best parameters, stage-2 parameters at
#' their build/prior defaults. Used only to measure each stage-2 target's
#' baseline fit before pestpp-ies runs - not part of the calibration itself.
#'
#' @param build_path the already-built model directory (b2$path) -
#'   run_aeme_param() requires `path` to already contain a built model
#'   ("Make sure to run build_aeme() before calling this function"), so the
#'   built lake directory is copied into `workdir` first, exactly as
#'   .run_perf_member() does in analyse_model_performance.R.
stage2_baseline_run <- function(aeme, model, s1, build_path, workdir) {
  unlink(workdir, recursive = TRUE)
  dir.create(workdir, recursive = TRUE, showWarnings = FALSE)
  ld <- AEME::get_lake_dir(aeme, path = build_path)
  file.copy(ld, workdir, recursive = TRUE)
  a0 <- aemetools::run_aeme_param(aeme = aeme, param = s1$best[, PCOLS],
                                  model = model, path = workdir,
                                  na_value = 999, return_aeme = TRUE)
  if (identical(a0, 999))
    stop("stage2_baseline_run: forward run failed (na_value returned)")
  a0
}

#' Baseline KGE for each candidate stage-2 target, from a
#' stage2_baseline_run() object.
stage2_baseline_kge <- function(a0, model, vars) {
  vapply(vars, function(v) {
    df <- tryCatch(AEME::get_var(a0, model = model, var_sim = v,
                                 use_obs = TRUE, remove_spin_up = TRUE),
                   error = function(e) NULL)
    if (is.null(df) || !nrow(df) || !"obs" %in% names(df)) return(NA_real_)
    names(df)[names(df) == "sim"] <- "model"
    aemetools::kge(df)
  }, numeric(1))
}

#' Weights that equalise each target's baseline phi share (weight^2 *
#' (1-KGE)^2), floored so a well-fit target never drops to zero influence
#' and capped by any hand-set importance ceiling (`cap`, same role as
#' resolve_targets()'s base_weights for stage 3).
balance_weights <- function(kge_base, cap = NULL, floor = 0.1) {
  resid <- pmax(abs(1 - kge_base), 1e-6)
  best  <- min(resid, na.rm = TRUE)
  w <- best / resid
  w[!is.finite(w)] <- floor
  w <- pmax(w, floor)
  if (!is.null(cap)) w <- pmin(w, cap[names(w)])
  w
}

# ---- AED parameter and switch tables ---------------------------------------

# aed_sa_calib_params.csv's phyto/R_resp bounds fixed (2026-09-26 night,
# user request after a comprehensive cross-group bound review): green's
# ceiling was 0.05, BELOW diatom's floor (0.06) and barely above cyano's
# floor (0.04) - a hard structural lockout, not a data-driven finding.
# Confirmed via a dedicated TSS-off/zoo-off succession diagnostic
# (diagnose_succession_tss_off.R) that even with unlimited light, green
# dominates all 15 simulated years by 1000-3000x mean biomass over
# cyano/diatom - both of which never show more than a single early
# transient. Root cause: green's default R_resp (0.012/day) is nearly an
# order of magnitude below cyano (0.08) and diatom (0.12) - both in the
# normal 0.05-0.15/day literature range for phytoplankton dark
# respiration, unlike green's value - and the calibration bounds ([0.006,
# 0.05]) were set as a fixed proportion of that already-anomalous default,
# faithfully preserving the lockout rather than catching it. No amount of
# calibration against total chl-a alone could ever have closed this gap.
# Widened to [0.006, 0.2] (matching diatom's own ceiling) so a real,
# data-informed rebalancing is at least POSSIBLE - not a guarantee PEST
# will find one, since the only observation (PHY_tchla) is a group-blind
# total with no direct pressure toward any particular group's dominance.
# phyto/R_growth's green ceiling (was 1.05, below diatom's floor of 1.4)
# widened more conservatively, to 1.5 - this asymmetry is comparably less
# alarming (diatoms genuinely having a higher realistic max growth rate is
# a well-documented, defensible trait difference, unlike the respiration
# gap), so only enough overlap was added to stop ruling out a rebalance
# entirely, not to erase the difference. Cross-checked every other phyto
# parameter (I_K, K_N, K_P, KePHY, R_nuptake, R_puptake, k_fres, w_p) for
# the same kind of non-overlapping-bounds problem - none found, all already
# overlap sensibly across the three groups.
PARAM_CSV  <- file.path("data", "processed", "aed_sa_calib_params.csv")
SWITCH_CSV <- file.path("data", "processed", "aed_sa_calib_switches.csv")

# Phytoplankton groups, in the order they are handed to AED. These must be
# column names in the built aed_phyto_pars.csv; the_phytos is derived from
# where they actually sit in that file (2, 3, 7 for this database - NOT the
# 1, 2, 3 the switch table gives as an illustration).
PHYTO_GROUPS <- c("cyano", "green", "diatom")

# Switch values the table deliberately leaves open ("1 or 2", "0 or 1 per
# group"). Keyed by the switch name exactly as the table spells it.
#   PO4AdsorptionModel = 1 -> kpo4p is live; kadsratio and qmax are inert and
#                             are dropped from the adjustable set below.
#   settling           = 1 -> _MOB_CONST_, so phyto/w_p is used directly
#                             (3 = Stokes and 4 = motile both ignore w_p).
SWITCH_CHOICE <- list(
  "aed_phosphorus/PO4AdsorptionModel" = 1,
  "aed_phytoplankton/settling"        = 1
)

# Switches this AED build cannot honour. Anything else that fails to resolve
# aborts (STRICT_TABLES), so a table edit can never silently do nothing.
#   aed_nitrogen/oxy_lim - no such key in this build's &aed_nitrogen. O2
#     limitation of nitrification is controlled here by
#     nitrif_reactant_variable together with knitrif.
SWITCH_SKIP <- c("aed_nitrogen/oxy_lim")

# Modules the switch table asks for that this build has no &block for are left
# inactive with a warning rather than activated - AED aborts on a module with
# no namelist block. aed_carbon is the one that applies here, and it is only
# needed if P adsorption uses pH, which it does not under
# PO4AdsorptionModel = 1 with ads_use_external_tss.

# Parameter keys spelled differently in this AED build.
PARAM_ALIAS <- c("aed_organic_matter/rdoc_minerl" = "aed_organic_matter/rdom_minerl")

# Phytoplankton switches that only apply to some groups; every other group
# gets 0. IMPORTANT: a switch key missing from this list is NOT "off for
# everyone" - resolve_switches()'s `only <- PHYTO_SWITCH_GROUPS[[p]]` falls
# through to is.null(only) -> TRUE, applying the switch table's value to
# EVERY group. Any new per-group phyto switch added to
# aed_sa_calib_switches.csv MUST get an entry here or it silently applies
# to all three groups instead of the intended one.
#
# simNFixation added (2026-09-25, phyto-vs-zoo diagnostic session): the
# example CSV ships this =0 for every group, disabling cyanobacteria's
# real-world competitive edge under N-limitation (fixing atmospheric N2
# when DIN is scarce relative to P). Confirmed directly on Rototoa: with it
# off, cyano (T_opt=28, well matched to Rototoa's summer peak) never gets a
# foothold at all - green dominates unconditionally instead, because its
# R_resp (0.012) is 7-10x lower than cyano's (0.08) or diatom's (0.12),
# giving it a structural net-growth advantage regardless of season. This
# doesn't fix the respiration asymmetry (that may be genuine trait
# differentiation, not a bug - left alone), but restores cyano's own real
# mechanism for competing under nutrient limitation. k_nfix/R_nfix already
# carry real values in the template for every group, just previously inert
# with the switch off.
PHYTO_SWITCH_GROUPS <- list(simSiUptake = "diatom", simNFixation = "cyano")

# AEME::write_aed_param_csv() rounds every numeric column to 3 dp, so any
# aed_phyto_pars.csv parameter finer than 0.001 is destroyed on its way to the
# model and PEST ends up searching a coordinate the model never sees. Rows
# whose range cannot carry at least PHYTO_MIN_STEPS distinct values at that
# resolution are dropped from the adjustable set. phyto_precision_report()
# below also lists the template values the same round-trip damages.
PHYTO_CSV_DIGITS <- 3
PHYTO_MIN_STEPS  <- 20
DROP_UNRESOLVED  <- TRUE

# Log-transform strictly positive parameters whose bounds span at least this
# ratio - most AED rate constants do. Inf disables it.
LOG_RATIO <- 10

# Variables kept in the stage-3 target set purely to anchor the physics
# parameters stage 3 re-opens (see narrow(), above). Set to character() if you
# would rather freeze light/Kw outright.
#
# HYD_schstb added (2026-09-22): raw temperature state (HYD_temp) can look
# well-fit (good KGE, matched seasonal amplitude) while the THERMOCLINE
# DEPTH is systematically off, if the error compensates across the
# profile - exactly the structural error that matters most for stage 3's
# BGC processes (bottom-water oxygen depletion, nutrient recycling are
# governed by whether/when the lake stratifies, not by absolute
# temperature). Schmidt stability (AEME::calc_HYD_schstb(), via
# rLakeAnalyzer) is a continuous, single-number stratification-STRENGTH
# metric - preferred here over HYD_thmcln (thermocline depth) because a
# depth-threshold metric is noisier against small profile errors, while
# Schmidt stability integrates the whole profile. Rototoa has a decent
# record (n=574, comparable to HYD_temp's own density). HYD_temp stays
# the primary anchor (dense, stable, directly interpretable in degC);
# this is an addition, not a replacement.
ANCHOR_VARS   <- c("HYD_temp", "HYD_schstb")
ANCHOR_WEIGHT <- 0.5

# TRUE aborts on a table entry that does not match the built model, rather
# than warning and carrying on with a quietly smaller problem.
STRICT_TABLES <- TRUE

# The posterior-predictive-ensemble diagnostics (run_aeme_ensemble() and
# everything downstream of it) have crashed the whole run three times with no
# catchable R error - see the note where this is used, in stage 3 below.
# Leave FALSE until that is isolated into its own subprocess. The code is
# preserved in archive_ensemble_diagnostics.R, sourced only when this is TRUE.
RUN_POSTERIOR_ENSEMBLE <- FALSE
if (isTRUE(RUN_POSTERIOR_ENSEMBLE)) source("archive_ensemble_diagnostics.R")

dir.create(ROOT, recursive = TRUE, showWarnings = FALSE)

# Stage 1 is unchanged from pest_calib.R (v1) - bootstrap its checkpoints
# across so v3 does not re-solve a stage that would give an identical
# result, rather than re-running the whole 10-lake stage-1 sweep. The
# checkpoint's $cal_ctrl keeps its PEST run under v1's ROOT
# ("staged-calibration/pest/..."), which v3 only ever reads (narrow(),
# carry()) - never rewrites - so this is safe even if v1's own tree is
# still in use elsewhere.
V1_ROOT <- "staged-calibration"
for (f in list.files(V1_ROOT, pattern = "^s1_.*\\.rds$", full.names = FALSE)) {
  dest <- file.path(ROOT, f)
  if (!file.exists(dest)) {
    file.copy(file.path(V1_ROOT, f), dest)
    message("  bootstrapped stage-1 checkpoint from v1: ", f)
  }
}

message("Using ", NCORE, " cores")



# ============================================================================

aeme_baseline_upstream <- tar_read(aeme_baseline_upstream)
# lapply(aeme_baseline_upstream, is.null)

# ---- PEST workflow hooks (R/pest_workflow/) --------------------------------
# Both are no-ops unless the env vars are set, so this script behaves exactly
# as before when launched by hand.
#   PEST_DOWNSTREAM_LIDS="LID50269"  append downstream lakes (parent's CURRENT
#       baseline outflow swapped in as inflow) as entries 11, 12, ... of
#       aeme_baseline_upstream, so they run through the same stages/checkpoints
#       (checkpoint names come from the lake dir, e.g. s4_LID50269_kuwakatai).
#   PEST_LAKE_IDX="11"               overrides LAKE_IDX (comma separated).
if (nzchar(Sys.getenv("PEST_DOWNSTREAM_LIDS"))) {
  source("R/pest_workflow/downstream.R")
  for (.lid in strsplit(Sys.getenv("PEST_DOWNSTREAM_LIDS"), ",")[[1]]) {
    .ptag <- parent_tag_for(.lid)
    aeme_baseline_upstream[[length(aeme_baseline_upstream) + 1L]] <-
      build_downstream_aeme(.lid, file.path(Sys.getenv("PEST_WF_ROOT", "pest_workflow"), .ptag, "outflow", "baseline_outflow.csv"))
    message("PEST hook: appended downstream ", .lid, " as index ", length(aeme_baseline_upstream),
            " (inflow from ", .ptag, ")")
  }
}
if (nzchar(Sys.getenv("PEST_LAKE_IDX"))) {
  LAKE_IDX <- as.integer(strsplit(Sys.getenv("PEST_LAKE_IDX"), ",")[[1]])
  message("PEST hook: LAKE_IDX overridden to ", paste(LAKE_IDX, collapse = ","))
}
path <- "pest_calib"

SWITCHES <- read_switches(SWITCH_CSV)

# Reduced switch table for stage 1 (2026-09-22): use_bgc = FALSE alone was
# found to silently downgrade GLM's sed_heat_model from 2 to 1 (AEME
# forces this when no WQ module is active), a real thermal difference -
# confirmed directly on Rototoa: mean 1.98degC / max 6.9degC vs the full
# use_bgc = TRUE run, not a formality. So stage 1's calibrated sediment
# thermal parameters (sed_temp_mean, sed_heat_Ksoil, ...) were being fit
# against the WRONG sediment heat scheme - one stage 3 never actually
# uses. Fix: keep use_bgc = TRUE for stage 1 too (so sed_heat_model = 2
# activates), but reduce the active AED model list to "aed_sedflux" only,
# skipping the full BGC suite's cost. Confirmed bit-for-bit identical
# temperature output to a full-BGC run (2026-09-22) - this is not an
# approximation, it reproduces the exact physics stage 3 will actually see.
PHYS_SWITCHES <- SWITCHES
PHYS_SWITCHES$value_txt[PHYS_SWITCHES$key == "models"] <- "aed_sedflux"

# ============================================================================
# Pre-flight: observation dedup verification + inflow validation
# ============================================================================
#
# Both checks run ONCE, before the calibration loop below (v3: for all 10
# lakes; v4: for LAKE_IDX only) - not per-lake-iteration - since both are
# properties of the shared `targets` pipeline output, not of anything this
# script does per lake.

#' Duplicate-observation guard. R/update_aeme.R collapses duplicate lake_wq
#' rows (three overlapping raw WQ exports bind_rows()'d with no dedup used
#' to leave ~12% of observations duplicated) for anything reaching that
#' function - that fix cut this lake's duplicate rate from ~12% to ~1.2%
#' (209/16855 rows for Tomarata, 2026-09-18), all in vars with an NA depth
#' and the exact same value under both copies (one with `lake` populated,
#' one without) - a smaller, separate merge artifact upstream of
#' R/update_aeme.R's fix, not evidence the fix itself failed. Rather than
#' hard-stop the whole run over a <2% residual with no value impact,
#' collapse it here (mean of duplicates, same treatment as the main fix)
#' and return the cleaned aeme - if the residual is ever large (>5%), that
#' IS worth stopping for, since it would suggest the main fix regressed.
fix_obs_dedup <- function(aeme, lake_label, warn_threshold = 0.05) {
  obs <- AEME::observations(aeme)
  changed <- FALSE
  if (!is.null(obs$lake) && nrow(obs$lake) > 0) {
    dep_col <- intersect(c("depth", "depth_from"), names(obs$lake))[1]
    key_cols <- c("Date", "var_aeme", if (!is.na(dep_col)) dep_col)
    n_dup <- sum(duplicated(obs$lake[, key_cols, drop = FALSE]))
    if (n_dup > 0) {
      frac <- n_dup / nrow(obs$lake)
      if (frac > warn_threshold)
        stop(sprintf(
          "fix_obs_dedup(%s): observations()$lake is %.1f%% duplicated (%d/%d rows) - ",
          lake_label, 100 * frac, n_dup, nrow(obs$lake)),
          "too large a fraction to be the small residual this function expects; ",
          "the R/update_aeme.R dedup fix likely has not reached this cached object. ",
          "Run: targets::tar_invalidate('lake_obs'); targets::tar_make('aeme_baseline_upstream'), ",
          "then re-run this script.", call. = FALSE)
      message(sprintf("  fix_obs_dedup(%s): collapsing %d/%d duplicate observations()$lake rows (%.1f%%)",
                      lake_label, n_dup, nrow(obs$lake), 100 * frac))
      other_cols <- setdiff(names(obs$lake), c(key_cols, "value"))
      obs$lake <- obs$lake |>
        dplyr::group_by(dplyr::across(dplyr::all_of(key_cols))) |>
        dplyr::summarise(value = mean(value, na.rm = TRUE),
                         dplyr::across(dplyr::all_of(other_cols), dplyr::first),
                         .groups = "drop")
      changed <- TRUE
    }
  }
  if (!is.null(obs$level) && nrow(obs$level) > 0) {
    key_cols <- intersect(c("Date", "var_aeme"), names(obs$level))
    n_dup <- sum(duplicated(obs$level[, key_cols, drop = FALSE]))
    if (n_dup > 0) {
      frac <- n_dup / nrow(obs$level)
      if (frac > warn_threshold)
        stop(sprintf(
          "fix_obs_dedup(%s): observations()$level is %.1f%% duplicated (%d/%d rows) - ",
          lake_label, 100 * frac, n_dup, nrow(obs$level)),
          "unexpectedly large - stopping rather than silently collapsing.", call. = FALSE)
      message(sprintf("  fix_obs_dedup(%s): collapsing %d/%d duplicate observations()$level rows (%.1f%%)",
                      lake_label, n_dup, nrow(obs$level), 100 * frac))
      other_cols <- setdiff(names(obs$level), c(key_cols, "value"))
      obs$level <- obs$level |>
        dplyr::group_by(dplyr::across(dplyr::all_of(key_cols))) |>
        dplyr::summarise(value = mean(value, na.rm = TRUE),
                         dplyr::across(dplyr::all_of(other_cols), dplyr::first),
                         .groups = "drop")
      changed <- TRUE
    }
  }
  if (changed) AEME::observations(aeme) <- obs
  aeme
}

message("\n=== pre-flight: verifying/cleaning observation duplicates (lake ", LAKE_IDX, " only) ===")
for (i in LAKE_IDX) {
  aeme_baseline_upstream[[i]] <- fix_obs_dedup(aeme = aeme_baseline_upstream[[i]],
                                               lake_label = paste0("lake ", i))
}
message("  lake ", LAKE_IDX, "'s observations are duplicate-free.")

#' Inflow sanity guard. Nothing in the inflow chain (lake_flows_hourly_
#' baseline.csv -> lspc_to_aeme() -> aeme_baseline_upstream) validates units
#' or magnitude anywhere - this catches a gross unit-scale error (e.g. an
#' accidental x1000/1000), not tight calibration bounds. Ranges are
#' deliberately generous mg/L bounds, not physically tight ones.
validate_inflows <- function(aeme, lake_label) {
  inflows <- AEME::get_inflows(aeme)
  problems <- character(0)
  ranges <- list(NIT_amm = c(0, 50), NIT_nit = c(0, 50), PHS_frp = c(0, 20),
                 PHS_tp = c(0, 20), CHM_oxy = c(0, 20))
  for (nm in names(inflows)) {
    df <- inflows[[nm]]
    if (!"HYD_flow" %in% names(df)) {
      problems <- c(problems, paste0(nm, ": missing HYD_flow column"))
    } else if (any(df$HYD_flow < 0, na.rm = TRUE)) {
      problems <- c(problems, paste0(nm, ": negative HYD_flow"))
    }
    for (v in names(ranges)) {
      if (!v %in% names(df)) next
      vals <- df[[v]]
      if (any(vals < 0, na.rm = TRUE))
        problems <- c(problems, paste0(nm, "/", v, ": negative concentration"))
      rng <- suppressWarnings(range(vals, na.rm = TRUE))
      if (is.finite(rng[2]) && rng[2] > ranges[[v]][2])
        problems <- c(problems, sprintf("%s/%s: max %.3g exceeds plausible %.3g mg/L",
                                        nm, v, rng[2], ranges[[v]][2]))
    }
  }
  list(ok = length(problems) == 0, problems = problems)
}

message("\n=== pre-flight: validating inflows (lake ", LAKE_IDX, " only) ===")
bad_lakes <- character(0)
for (i in LAKE_IDX) {
  chk <- validate_inflows(aeme_baseline_upstream[[i]], paste0("lake ", i))
  if (!chk$ok) {
    bad_lakes <- c(bad_lakes, paste0("lake ", i))
    message("  lake ", i, ": ", paste(chk$problems, collapse = "; "))
  }
}
if (length(bad_lakes) > 0) {
  message("  inflow problems found for: ", paste(bad_lakes, collapse = ", "),
          " - invalidating aeme_baseline_upstream and rebuilding aeme_baseline_upstream.")
  message("  NOTE: this cascades through the full targets DAG (aeme_baseline_downstream, ",
          "baseline_bgc_calib, and every other branch depending on aeme_baseline_upstream) - ",
          "a deliberate, potentially hours-long, ONE-TIME cost paid up front, not per lake.")
  targets::tar_invalidate("aeme_baseline_upstream")
  targets::tar_make("aeme_baseline_upstream")
  aeme_baseline_upstream <- targets::tar_read(aeme_baseline_upstream)
  still_bad <- character(0)
  for (i in LAKE_IDX) {
    chk <- validate_inflows(aeme_baseline_upstream[[i]], paste0("lake ", i))
    if (!chk$ok) still_bad <- c(still_bad, paste0("lake ", i))
  }
  if (length(still_bad) > 0)
    stop("Inflows still invalid after one regeneration attempt for: ",
        paste(still_bad, collapse = ", "),
        " - the problem is upstream of the targets cache (likely ",
        "lake_flows_hourly_baseline.csv itself or lspc_to_aeme.R), not staleness.",
        call. = FALSE)
  message("  inflows pass validation after regeneration.")
} else {
  message("  lake ", LAKE_IDX, "'s inflows passed validation.")
}

# Full stages 1-3 re-run for the four upstream lakes that feed a downstream
# site: 2 Spectacle -> Slipper, 7 Pehiakura Small -> Pehiakura Big, 8 Te Kanae
# -> Kereta, 10 Rototoa -> Kuwakatai. All three stage checkpoints (s1/s2/s3
# _*.rds) and their pest/ dirs were archived, so every stage re-solves from
# scratch with (a) the outlet-depth fix - fresh_build() now forces
# outl_elvs = 1 m below surface instead of build_aeme()'s ~6 m-above-bed
# default (see fix_outlet_depth() in pest_funs.R); the old deep withdrawal
# biased the exported outflow temperature/chemistry and each lake's own
# hypolimnetic budget - and (b) N_REALS = 200 / BAD_PHI_SIGMA = 3.
# 2026-09-14: bringing the other six lakes (1,3,4,5 no downstream site; 6
# whatihua and 9 pupuke previously reported STATUS_ACCESS_VIOLATION / a
# malformed glm4.nml on some earlier attempt - but both completed cleanly on
# the N_REALS=50 run whose s2/s3 checkpoints are archived in
# archive_pre_targeted_vars/*_old.rds, so that issue is not currently
# reproducing) up to the same standard as the 4 downstream-feeders: nitrate
# target, trimmed phyto set, met factors, adaptive per-lake targets and
# ensemble sizing. s1 (water balance) kept for all 10; s2/s3 archived and
# re-solved from scratch for these six.
# SKIP_LAKES (v3's Tomarata/Kawaupaka before/after-comparison mechanism) is
# gone in v4 - LAKE_IDX already restricts the loop to one lake, nothing
# left to skip.

failed_lakes <- integer()
i <- 10
for (i in LAKE_IDX) {

  # One lake must not be able to cost the other nine. Every stage is
  # checkpointed by run_or_load(), so a lake that dies here is retried from
  # wherever it got to on the next run rather than restarted from scratch.
  #
  # withCallingHandlers() INSIDE tryCatch(), not the reverse: a calling
  # handler runs while the stack is still intact, before anything unwinds, so
  # sys.calls() inside it is the real call chain at the error site. tryCatch's
  # own handler only ever sees conditionMessage(e) - the stack is long gone by
  # the time it runs. Needed after lake 7 failed identically twice in the live
  # sequential loop ("Cannot pluralize without a quantity", stage 2) while
  # succeeding twice in an isolated single-lake reproduction - i.e. something
  # about running after several other lakes in the same session, not lake 7's
  # data itself, and the bare message alone was not enough to find it.
  .err_trace <- NULL
  lake_ok <- tryCatch({
    withCallingHandlers({
      
      aeme0 <- aeme_baseline_upstream[[i]]
      
      inflows <- get_inflows(aeme0)
      # remove NCS_tss column from each inflow
      inflows <- lapply(inflows, function(df) df[, !colnames(df) %in% "NCS_tss"])
      aeme0 <- aeme0 |>
        remove_inflow(all = TRUE) |>
        add_inflows(inflows)
      # 2026-10-03: the cached inflows carry suspended solids 1e6 too small
      # (R/lspc_to_aeme.R copies LSPC's t/m3 `ssedout` as "mg/L"); guarded
      # correction, see R/pest_workflow/inflow_fix.R
      source("R/pest_workflow/inflow_fix.R")
      aeme0 <- fix_inflow_ss_units(aeme0)
      
      # get_calib_periods(aeme0, vars_sim = targets)
      # aeme0_full is the only window in use (a separate, shorter one for
      # stages 1/2 was attempted and reverted - see START_PHYS above).
      # aeme0 is reassigned from it right before each stage's own
      # fresh_build() - it (not a stage-specific parameter)
      # is what fresh_build() reads from this scope (pest_funs.R).
      aeme0_base <- aeme0

      # USE_SHORT_PERIOD (2026-09-25): a shortened, per-lake ADAPTIVE
      # window - pick_dense_window() (pest_funs.R) scores sliding
      # window_years-long candidate windows against THIS lake's own
      # observation record and picks the densest one, adaptively (a lake
      # missing a target variable entirely, e.g. no Schmidt stability
      # samples, just drops it from scoring rather than zeroing every
      # window because of a variable it structurally lacks - see that
      # function's own header). Supersedes the earlier Rototoa-only
      # hardcoded ANALYSIS_START/FORCING_START dates - those were the
      # right MANUAL answer for Rototoa specifically, this is the general
      # per-lake version (2026-09-25, user request for the 9-lake run).
      # Falls back to the long/default period if this lake has no usable
      # window at all (pick_dense_window() returns NULL - e.g. a lake with
      # essentially no observations for any of `targets`).
      if (exists("USE_SHORT_PERIOD") && isTRUE(USE_SHORT_PERIOD)) {
        win <- pick_dense_window(aeme0_base, targets,
                                 window_years = SHORT_WINDOW_YEARS,
                                 spin_up_years = SHORT_SPINUP_YEARS)
        if (is.null(win)) {
          message("  pick_dense_window(): no usable window found for lake ", i,
                  " - falling back to the long/default period.")
          aeme0_full <- AEME::set_time(aeme0_base, start = START, stop = STOP, spin_up = SPIN_UP)
          obs_summary <- summarise_obs(aeme0_full, vars_sim = targets)
          period_full <- suggest_sim_period(aeme0_full, vars_sim = targets,
                                            spin_up = ceiling(365 * 1.5))
        } else {
          message("  pick_dense_window(): lake ", i, " -> ",
                  win$window_start, " to ", win$window_end,
                  " (vars scored: ", paste(win$vars_used, collapse = ", "), ")")
          spin_up_days <- as.numeric(as.Date(win$window_start) - as.Date(win$forcing_start))
          aeme0_base <- trim_forcing_period(aeme0_base, win$forcing_start, win$forcing_stop)
          aeme0_full <- AEME::set_time(aeme0_base, start = win$forcing_start,
                                       stop = win$forcing_stop, spin_up = spin_up_days)
          obs_summary <- summarise_obs(aeme0_full, vars_sim = targets)
          period_full <- suggest_sim_period(aeme0_full, vars_sim = targets,
                                            spin_up = spin_up_days)
        }
      } else {
        aeme0_full <- AEME::set_time(aeme0_base, start = START, stop = STOP, spin_up = SPIN_UP)
        obs_summary <- summarise_obs(aeme0_full, vars_sim = targets)
        period_full <- suggest_sim_period(aeme0_full, vars_sim = targets,
                                          spin_up = ceiling(365 * 1.5))
      }
      aeme0_full  <- set_sim_period(aeme0_full, period_full)

      # v4 shortened-window attempt for stages 1/2 REVERTED, 2026-09-21:
      # suggest_sim_period() does not respect an explicit later start date
      # at all - it always resolves its own window from the model's
      # underlying forcing-data availability (`earliest <- window$start +
      # spin_up`, where `window$start` is close to the forcing data's own
      # native beginning, not START_PHYS), using `spin_up` only to decide
      # how much of THAT to exclude from scoring. Passing START_PHYS to
      # set_time() had no effect (confirmed directly: suggest_sim_period()
      # on the resulting object still resolved 2013-2023, not the intended
      # ~2018-2023). A smaller spin_up doesn't shorten the run either - it
      # just starts SCORING earlier within the same, still-full-length,
      # simulated range. Genuinely shortening total runtime would need the
      # underlying inflow/met boundary-condition data trimmed to a shorter
      # absolute range before suggest_sim_period() ever sees it - a bigger
      # change, deferred as separate follow-up work rather than block this
      # evaluation. Stages 1/2 use the same aeme0_full/period_full as
      # stage 3 for now, matching v3.
      aeme0  <- aeme0_full
      period <- period_full

      # ---- parameter helpers -----------------------------------------------------
      
      utils::data("aeme_parameters", package = "AEME")
      utils::data("aeme_parameters_bgc", package = "AEME")
      
      POOL <- unique(rbind(
        aeme_parameters[aeme_parameters$model == MODEL, PCOLS],
        aeme_parameters_bgc[aeme_parameters_bgc$model == MODEL, PCOLS]))
      
      # ============================================================================
      # Stage 1 - water balance against lake volume
      # ============================================================================
      # Reverted to a separate water-only stage (2026-09-22, user call).
      # The merged stage 1/2 (2026-09-21) fixed the original narrow()-
      # median drift bug (see the Change-2 header note above), but a
      # single wind-speed parameter serving BOTH water-balance and
      # thermal-structure objectives at once created a NEW problem: the
      # two signals pulled MET_wndspd in different directions and the
      # optimizer settled on a compromise that served neither well
      # (confirmed: surface temperature bias stuck at -4.06degC across
      # three different mixing-parameter fixes, unmoving because the real
      # constraint was two objectives fighting over one parameter, not
      # the parameter's value). Splitting gives each objective a clean,
      # uncontested signal again - but the stage-1 -> stage-2 hand-off
      # below explicitly uses narrow()'s BEST-FIT value override (not the
      # posterior median that caused the original drift), so the actual
      # fix that motivated the merge travels with the split, not just the
      # merge itself.
      #
      # sediment/sed_heat_model frozen at 1 (2026-09-22, min = max =
      # value): AEME silently sets this to 1 under use_bgc = FALSE and 2
      # under use_bgc = TRUE (2 "needs an active WQ module"), and that
      # difference is NOT cosmetic - confirmed directly on Rototoa, mean
      # 1.98degC / max 6.9degC water temperature difference between the
      # two schemes at otherwise-identical calibrated parameters. Tried
      # forcing scheme 2 everywhere via a reduced aed_sedflux-only BGC
      # build (use_bgc = TRUE); it reproduced scheme 2's physics exactly,
      # but activating the WQ plugin at all cost a ~60-100x per-run
      # slowdown (1.5-4.5 min vs ~1-2s) and a ~25-30% run failure rate -
      # not worth it for a difference this project doesn't need resolved.
      # Standardising on scheme 1 everywhere instead, as an explicit
      # PARAMETER (not a one-off nml patch like MIXING_OVERRIDE/
      # fix_mixing() needed - see that function's own comment on why a
      # file-only edit doesn't survive run_aeme_param()'s regeneration): a
      # real parameter row goes through input_model_parameters() the
      # normal way, so it persists correctly on every forward run without
      # needing a special-cased fix, and carries forward through both
      # stage 2 and stage 3 via carry(), like every other frozen value.
      b1 <- fresh_build(tag = "s1", vars_sim = "LKE_vol", use_bgc = FALSE)
      plot_wlev(b1$aeme)
      plot_output(b1$aeme)
      plot_output(b1$aeme, "LKE_vol", remove_spin_up = F)
      cfg <- AEME::configuration(b1$aeme)$glm_aed
      n_glm_zones <- cfg$hydrodynamic$sediment$n_zones
      z <- AEME::input(b1$aeme)$init_depth
      sed <- cfg$hydrodynamic$sediment

      # Some lakes have no observed lake volume at all, or (2026-09-25,
      # Pehiakura Small) real level data that just doesn't happen to fall
      # inside a short adaptively-chosen period - either way calib_aeme()
      # cannot target LKE_vol without at least one IN-PERIOD observation
      # row ("no rows"). has_level_obs_in_period() (pest_funs.R) checks
      # the period, not just whether the lake's raw record is globally
      # empty - see that function's own header. This is applied to
      # b1$aeme, the BUILT object, never to aeme0: aeme0 feeds every
      # stage's build (including stage 2 and 3's, further down), and injecting
      # fabricated data upstream of a build would let it distort the water-balance
      # calculation those builds do - see synthesize_level_target() for why.
      if (!has_level_obs_in_period(b1$aeme, period))
        b1$aeme <- synthesize_level_target(b1$aeme, MODEL, period)

      # NOTE on `outflow` bounds: v3's shipped range is 0.5 - 2.5, and this
      # header previously warned against widening it further (it was
      # widened to 0.1 - 2.5 once, on the mistaken reading that the
      # posterior was railed at 0.5 - but get_best_params() reports the
      # ENSEMBLE range in its min/max columns, not the prior bounds; the
      # true posterior sat at 0.80 - 0.92 (median 0.87), nowhere near
      # either bound). v4 goes further and NARROWS it to 0.8 - 1.2
      # (2026-09-21, user call): AEME::build_aeme() already runs with
      # wb_method = 2, calc_wbal = TRUE ("Correcting water balance using
      # estimated outflows" in the run log) - it estimates an outflow
      # series that balances the budget BEFORE this multiplier is ever
      # applied, so a wide range lets calibration re-fight that estimate
      # wholesale rather than making the small residual correction it
      # should be. seepage_rate stays available alongside it to carry the
      # rest of any residual - a real physical loss term is a more
      # defensible place for a small bias than stretching outflow.
      # MET_wndspd bounds widened 0.7-1.3 -> 0.5-2.0 (2026-09-23): ERA5
      # daily wind speed over small enclosed lakes in complex terrain
      # (Auckland volcanic field) is known to be systematically biased -
      # the prior v4 run had the posterior at 1.20 (stage 1) and 1.217
      # (stage 2), both near the old 1.3 upper bound, suggesting the
      # shipped range was actively constraining the fit. A factor of 2.0
      # upper bound is physically defensible (sheltered crater lakes can
      # easily see ERA5 wind underestimated by 50-100%), and 0.5 allows
      # for exposed lakes where ERA5 overestimates. Using prow() with
      # explicit bounds rather than shipped() so the range is declared
      # here, not hidden inside the AEME parameter table.
      #
      # rh_factor bounds widened 0.9-1.1 -> 0.7-1.3 (2026-09-23):
      # ERA5 relative humidity over lakes also carries a systematic bias
      # (surface moisture flux parameterisation does not resolve small
      # open-water bodies). rh_factor directly scales GLM's bulk
      # aerodynamic latent heat flux, so it is the natural lever for any
      # residual water-balance error the wind correction alone does not
      # fix. The old ±10% bounds were tighter than the ERA5 humidity
      # literature justifies; ±30% (0.7-1.3) gives the same physical
      # headroom as the now-widened wind factor without opening up more
      # than one aerodynamic degree of freedom per adjustment.
      # 2026-09-24: sourced from data/processed/glm_phys_calib_params.csv
      # (read_phys_param_table(), pest_funs.R) instead of scattered
      # prow()/shipped() calls - one editable table for every stage 1-2
      # physical parameter, with its own value/bounds AND the reasoning
      # for each documented in the CSV's own notes column. Filtered to
      # this stage's own region ("whole_lvl" - stage 1 targets LKE_vol
      # alone) via var_sim, the same convention stage 3's BGC table
      # already uses (var_sim -> vars -> keep <- any(v %in% targets)).
      PARAM_CSV_PHYS <- file.path("data", "processed", "glm_phys_calib_params.csv")
      ptab_phys_s1 <- read_phys_param_table(PARAM_CSV_PHYS, b1$aeme, n_zones = n_glm_zones)
      s1_keep <- vapply(ptab_phys_s1$vars, function(v) "whole_lvl" %in% v, logical(1))
      s1_adj <- ptab_phys_s1[s1_keep, PCOLS]

      get_obs(b1$aeme)
      assess_aeme(b1$aeme)

      # screen = FALSE: only 5 parameters, nothing to screen out, and the
      # SA path (create_sen_control()) cannot handle a water-level-only
      # sub-region set - documented in run_stage()'s own screen param doc.
      s1 <- run_or_load(1, b1$aeme, run_stage(1, b1$aeme, b1$path, s1_adj, POOL[0, PCOLS],
                                              targets = "LKE_vol", sub_regions = NULL,
                                              screen = FALSE))

      # ============================================================================
      # Stage 2 - thermal structure against temperature and lake volume
      # ============================================================================
      vars_phys <- c("HYD_temp", "HYD_schstb", "LKE_vol", "LKE_nrgtot")
      b2 <- fresh_build("s2", vars_phys, use_bgc = FALSE)
      plot_output(b2$aeme, "HYD_schstb")
      plot_output(b2$aeme, "LKE_nrgtot")

      # Mirrors stage 1's handling: calib_aeme() cannot target LKE_vol
      # without at least one IN-PERIOD observation row (see
      # has_level_obs_in_period()'s own header for why "in-period", not
      # just "the lake has some level data somewhere"). Applied to
      # b2$aeme, the BUILT object, never to aeme0 - see the stage-1 comment
      # above for why.
      if (!has_level_obs_in_period(b2$aeme, period))
        b2$aeme <- synthesize_level_target(b2$aeme, MODEL, period)

      # 2026-09-24: REGION-NATIVE redesign, replacing the trim_obs_profile()
      # masking hack from earlier today. aemetools' calib_aeme() now
      # accepts the same named sub_regions list sa_aeme()/
      # create_sen_control() already use (user-added feature, confirmed in
      # the installed package - see .as_calib_regions() in aemetools'
      # source) - AND its pvm/localizer construction is now region-aware
      # too (see run_stage()'s own updated comment, pest_funs.R). So
      # HYD_temp's objective is split into surf_temp/meta_temp/bot_temp as
      # three SEPARATELY-WEIGHTED PEST observation groups, natively, not
      # by pre-filtering which points feed one pooled metric - a properly
      # structural fix rather than the point-count rebalancing trick from
      # earlier. trim_obs_profile() itself is left in pest_funs.R (still a
      # reasonable general tool for other cases) but no longer called here.
      #
      # ONE definition, used for BOTH the Morris screen (via sub_regions
      # below) AND the real pestpp-ies objective (calib_aeme(), also via
      # sub_regions now that run_stage() threads it through) - previously
      # these were built separately and could silently drift apart.
      #
      # Depth bands: surf/meta/bot deliberately leave a small (0.5 m) dead
      # zone at each boundary (2-2.5 m, z-4.5 to z-4 m) so
      # .check_region_overlap() never sees two regions touch at an exact
      # shared edge - a handful of observations right at the boundary go
      # unused, an acceptable cost for genuinely non-overlapping groups.
      # month = 1:12 (not the old DJF/JJA split) for all three - now that
      # depth itself provides the structural split, a separate whole-
      # column winter check is redundant; each band's own winter
      # observations already constrain it.
      #
      # Region weights (bot_temp=2, surf_temp=1, meta_temp=0.5) are a
      # deliberate, INSPECTABLE choice - bottom is currently the broken
      # one (KGE around -1.3 to -1.8 across today's attempts), surface is
      # already solid (~0.79-0.8), metalimnion present but subordinate so
      # the thermocline's shape still has SOME influence without
      # dominating by point density the way the old pooled metric let it.
      # These MULTIPLY vars_phys's own HYD_temp entry in w2 below (see
      # calib_aeme()'s own doc: "weights sets HYD_temp's importance
      # relative to other variables, the region weight splits that
      # between sub-regions") - revisit both together, not in isolation.
      # BUG FIXED 2026-09-24: schmidt_stab/whole_lvl/whole_nrg's
      # depth_range originally read c(0, z), the same "whole column"
      # convention the old sub_regions list used purely for SCREENING.
      # aemetools' region-aware pest_obs_table() (now also driving the
      # real calibration objective, not just screening) requires
      # !is.na(df$depth) whenever a region declares a non-NULL
      # depth_range - but HYD_schstb/LKE_vol/LKE_nrgtot are whole-lake
      # scalars with NO depth dimension at all (depth = NA in every one
      # of their observation rows), so EVERY observation failed the
      # depth check and all three regions matched zero rows ("No
      # observations for schmidt_stab, whole_lvl, and whole_nrg within
      # the simulation period"). depth_range = NULL skips the depth
      # check entirely (matching how these variables always worked in
      # flat vars_sim mode, before this region-native redesign).
      # tb: depth-adaptive surf/meta/bot boundaries (temp_bands(), pest_funs.R)
      # - reproduces the original fixed c(0,2)/c(2.5,z-4.5)/c(z-4,z) bands
      # exactly for a deep lake like Rototoa (z=26.06), but scales/collapses
      # them for a shallower lake instead of emitting an inverted,
      # overlapping meta_temp range. Confirmed necessary 2026-09-25: the
      # fixed offsets aborted on the first two lakes this pipeline ever ran
      # besides Rototoa ("Regions surf_temp and meta_temp both target
      # HYD_temp and overlap").
      tb <- temp_bands(z)
      s2_regions <- list(
        surf_temp    = list(var = "HYD_temp",   month = 1:12, depth_range = c(0, tb$surf_hi),
                            weight = 1.0),
        bot_temp     = list(var = "HYD_temp",   month = 1:12, depth_range = c(tb$bot_lo, z),
                            weight = 2.0),
        # month/depth_range pairing matters for AEME::get_var_indices():
        #   - depth_range = c(0, z) (whole-lake DERIVED vars: Schmidt
        #     stability, total energy) needs month = 1:12, NOT NULL - its
        #     `else` branch does which(month %in% NULL), which silently
        #     returns zero dates and gives the region no screening indices.
        #   - depth_range = NULL (LKE_vol) needs month = NULL too - only the
        #     is.null(month) & is.null(depth_range) branch skips the
        #     seq(min(depth_range), ...) call that crashes on NULL.
        schmidt_stab = list(var = "HYD_schstb", month = 1:12, depth_range = c(0, z),
                            weight = 1.0),
        whole_lvl    = list(var = "LKE_vol",    month = NULL, depth_range = NULL,
                            weight = 1.0),
        whole_nrg    = list(var = "LKE_nrgtot", month = 1:12, depth_range = c(0, z),
                            weight = 1.0)
      )
      # meta_temp only added when there's genuinely room for it (temp_bands()
      # requires >=0.5m of column between surf_hi and bot_lo) - a lake too
      # shallow for three distinct bands just gets surf_temp/bot_temp.
      if (tb$has_meta) {
        s2_regions$meta_temp <- list(var = "HYD_temp", month = 1:12,
                                     depth_range = c(tb$meta_lo, tb$meta_hi),
                                     weight = 0.5)
      } else {
        message("  temp_bands(): z=", round(z, 2),
                " too shallow for a distinct meta_temp band - using surf_temp/bot_temp only.")
      }

      base_a0   <- stage2_baseline_run(b2$aeme, MODEL, s1, b2$path,
                                       file.path(ROOT, "s2_baseline"))
      kge_base  <- stage2_baseline_kge(base_a0, MODEL, vars_phys)
      message("  stage-2 baseline KGE (stage-1-best, stage-2-default): ",
              paste(sprintf("%s=%.3f", names(kge_base), kge_base), collapse = ", "))
      # Data-density-scaled importance ceiling (stage2_n_caps(),
      # pest_funs.R) - the same n/25 formula resolve_targets() uses for
      # stage 3, applied here so a lake with a thin thermal record doesn't
      # get the same ceiling as one with a dense one. LKE_vol's cap is
      # deliberately lower than the temperature targets - the recurring
      # datum/bias issues in the raw level series (see
      # correct_stage_height.R) make it a less trustworthy signal than
      # thermal structure even once corrected, so it's capped rather than
      # allowed to dominate phi at parity with the others. Unlike the
      # merged stage's LKE_vol-weight investigation (2026-09-21/22),
      # this circularity concern shouldn't recur here: stage 1 has already
      # fixed water balance by the time this baseline is computed
      # (base_a0 uses s1$best, not shipped defaults), so
      # balance_weights()'s residual-based discount is now acting on a
      # target that's actually already well-fit, not one still needing
      # the fix balance_weights() is discounting it out of getting.
      n_caps2 <- stage2_n_caps(
        b2$aeme, vars_phys, period,
        max_cap = c(HYD_temp = 1, HYD_schstb = 1, LKE_vol = 0.5, LKE_nrgtot = 0.7))
      message("  stage-2 data-density caps: ",
              paste(sprintf("%s=%.3f", names(n_caps2), n_caps2), collapse = ", "))
      w2 <- balance_weights(kge_base, cap = n_caps2)
      message("  stage-2 balanced weights: ",
              paste(sprintf("%s=%.3f", names(w2), w2), collapse = ", "))

      # 2026-09-24: sourced from data/processed/glm_phys_calib_params.csv
      # (read_phys_param_table()) instead of scattered prow()/shipped()
      # calls - see that CSV's own notes column for the parameter-by-
      # parameter reasoning (why each is adjustable/frozen, why its
      # bounds are what they are). Kept if it's linked to ANY of this
      # stage's HYD_temp regions, excluding: MET_wndspd/rh_factor
      # (S1_NARROW_NAMES - these get the special best-fit hand-off from
      # stage 1 below, not a fresh CSV row) and sed_heat_model (carried
      # forward frozen from stage 1 via carry(), not re-added here).
      ptab_phys_s2 <- read_phys_param_table(PARAM_CSV_PHYS, b2$aeme, n_zones = n_glm_zones)
      s2_region_names <- c("surf_temp", "meta_temp", "bot_temp")
      csv_exclude_s2 <- c(S1_NARROW_NAMES, "sediment/sed_heat_model")
      s2_keep <- vapply(ptab_phys_s2$vars, function(v) any(v %in% s2_region_names), logical(1)) &
                 !(ptab_phys_s2$name %in% csv_exclude_s2)
      s2_csv_adj <- ptab_phys_s2[s2_keep, PCOLS]

      # MET_wndspd/rh_factor: hand-off from stage 1 using its BEST-FIT
      # value (not narrow()'s posterior-median default) with stage 1's OWN
      # bounds (0.5-2.0 / 0.7-1.3, from glm_phys_calib_params.csv) - not a
      # re-narrowed range. "Reopen all the parameters": nothing here
      # stays anchored to a range a previous fit narrowed. See
      # S1_NARROW_NAMES' own header for the original median-vs-best-fit
      # drift bug this hand-off exists to avoid.
      s1_wndspd_rh <- ptab_phys_s1[ptab_phys_s1$name %in% S1_NARROW_NAMES, PCOLS]
      s1_best_val <- stats::setNames(s1$best$value, s1$best$name)
      s1_wndspd_rh$value <- s1_best_val[s1_wndspd_rh$name]

      s2_adj <- rbind(s2_csv_adj, s1_wndspd_rh)

      get_obs(b2$aeme)
      assess_aeme(b2$aeme)

      # except = S1_NARROW_NAMES: MET_wndspd/rh_factor are re-added into
      # s2_adj above instead, not frozen - carrying them here too would
      # duplicate the parameter (once frozen at stage-1's value via this
      # carry(), once adjustable via s2_adj). sed_heat_model carries
      # forward frozen at 1, same as every other frozen value.
      s2 <- run_or_load(2, b2$aeme, run_stage(
        2, b2$aeme, b2$path, s2_adj,
        carry(s1, except = S1_NARROW_NAMES), targets = vars_phys,
        sub_regions = s2_regions,
        weights = set_weights(vars_phys, w2[vars_phys]),
        # 2026-09-24: mixing/coef_mix_hyp force-kept AND now linked to
        # bot_temp SPECIFICALLY (not HYD_temp broadly) - the region-aware
        # pvm/localizer (see run_stage()'s updated comment) makes this the
        # actually-correct fix, not the earlier HYD_temp-only workaround
        # that diluted its effect against surf_temp/meta_temp sharing the
        # same pooled bucket. Still needs force_keep regardless of region
        # linkage - the Morris screen (a short window) does not span a
        # full annual turnover cycle and drops this parameter as
        # "insensitive" otherwise (confirmed directly: bottom KGE
        # regressed -0.48 -> -1.75 the one time this was missed).
        #
        # sed_temp_mean/sed_temp_amplitude are zoned (one row per GLM
        # sediment zone) and force-linked PER ZONE via sed_zone_regions()
        # (pest_funs.R): zone 1 sits at the lake bed and only bot_temp
        # should see it, the top zone reaches the surface and only
        # surf_temp should, and any zones between fall to meta_temp - a
        # zone's physical position, not a Morris correlation over a short
        # window, decides which region it can plausibly influence.
        # sed_heat_Ksoil/sed_temp_depth are frozen (adjustable=FALSE in the
        # CSV) and never enter keep_adj at all, so they need no linkage.
        force_keep = c(
          list(`mixing/coef_mix_hyp` = "bot_temp"),
          stats::setNames(
            list(sed_zone_regions(n_glm_zones, has_meta = tb$has_meta),
                sed_zone_regions(n_glm_zones, has_meta = tb$has_meta)),
            c("sediment/sed_temp_mean", "sediment/sed_temp_amplitude"))),
        # rebuild() calls fresh_build() again - a SEPARATE Aeme object from
        # b2$aeme above, so the synthesize_level_target() applied to b2$aeme
        # earlier does NOT carry over here. Confirmed missing in practice
        # (2026-09-19): Pehiakura Small has zero real LKE_vol
        # observations, and calib_aeme() aborted with "observations(aeme)
        # $level has no rows" because this rebuilt object still had none.
        # Re-applied on the same is-it-empty check, not unconditionally, to
        # match every other lake's real data untouched.
        rebuild = function() {
          rb <- fresh_build("s2cal", vars_phys, use_bgc = FALSE)
          if (!has_level_obs_in_period(rb$aeme, period))
            rb$aeme <- synthesize_level_target(rb$aeme, MODEL, period)
          rb
        }))

      # ============================================================================
      # v6 NEW: heat-flux (ce/ch/cd) sensitivity screen - SA ONLY, never calibrated
      # ============================================================================
      # GLM's own bulk aerodynamic transfer coefficients (glm3.nml
      # &meteorology: ce = latent heat/evaporation, ch = sensible heat,
      # cd = momentum/drag - all default 0.0013, never touched by any
      # calibration in this project). Distinct from the already-calibrated
      # MET_radswd/radlwd/tmpair/wndspd SCALING FACTORS on the raw met
      # inputs - these instead control how strongly the model converts a
      # given wind/humidity/temperature difference into an actual heat
      # flux. Screened here purely for information (does this lake's own
      # thermal fit look sensitive to them at all?) against stage 2's own
      # targets (s2_regions) - deliberately NOT added to s2_adj/keep_adj,
      # regardless of the result: they are easily confounded with the
      # wind/humidity factors already calibrated in stage 2, and adding
      # three more simultaneously-adjustable, weakly-identifiable
      # parameters to that search space is a cost this screen is meant to
      # let us avoid paying without evidence it is worth it.
      if (RUN_HEAT_FLUX_SCREEN) {
        heat_flux_params <- data.frame(
          model = MODEL, file = "glm3.nml",
          name  = c("meteorology/ce", "meteorology/ch", "meteorology/cd"),
          value = 0.0013, min = 0.0008, max = 0.0020,
          group = NA_character_, index = NA_real_, stringsAsFactors = FALSE
        )
        message("\n=== v6: heat-flux (ce/ch/cd) sensitivity screen, SA-only (stage 2 targets) ===")
        hf_fdir <- file.path(ROOT, "calib_sa")
        hf_pest_root <- normalizePath(file.path(ROOT, "pest"), winslash = "/", mustWork = FALSE)
        dir.create(hf_pest_root, recursive = TRUE, showWarnings = FALSE)
        hf_sen_dir <- file.path(hf_pest_root, basename(AEME::get_lake_dir(b2$aeme)), "sen_heatflux")
        hf_ctrl <- create_sen_control(file_dir = hf_fdir, morris_r = MORRIS_R, ncore = NCORE,
                                      vars_sim = s2_regions, pest_dir = hf_sen_dir,
                                      stall_minutes = 10)
        mean_fit_hf <- function(df) mean(df$model, na.rm = TRUE)
        hf_targets <- vapply(s2_regions, function(r) r$var, character(1))
        hf_sa_id <- unname(sa_aeme(
          aeme = b2$aeme, model = MODEL,
          param = add_name_full(rbind(heat_flux_params[, PCOLS],
                                      carry(s2, except = character())[, PCOLS])),
          FUN_list = stats::setNames(lapply(unique(hf_targets), function(v) mean_fit_hf), unique(hf_targets)),
          path = b2$path, ctrl = hf_ctrl)[[1]])
        prune_pest_agents(hf_sen_dir)
        hf_sen <- read_sen(ctrl = hf_ctrl, sim_id = hf_sa_id)
        hf_mu <- hf_sen[[hf_sa_id]]$indices
        hf_mu <- hf_mu[hf_mu$index_type == "mu_star" &
                      hf_mu$parameter %in% c("meteorology/ce", "meteorology/ch", "meteorology/cd"), ]
        hf_mu$rel <- stats::ave(hf_mu$value, hf_mu$variable, FUN = function(v)
          if (max(v, na.rm = TRUE) > 0) v / max(v, na.rm = TRUE) else v * 0)
        message("Heat-flux screen result (informational only - NOT added to any adjustable set):")
        print(hf_mu[order(-hf_mu$rel), c("variable", "parameter", "value", "rel")], row.names = FALSE)
        saveRDS(hf_mu, file.path(ROOT, "analysis", sprintf("heatflux_screen_%s.rds", basename(AEME::get_lake_dir(b2$aeme)))))
      }

      # ============================================================================
      # v6 NEW: Stage 3 - TSS only (light climate), formalised from the ad hoc
      # "stage 2b" diagnostic (tss_phyto_update report, section 2.2)
      # ============================================================================
      # Physics frozen from stage 2 (carry()). Modules restricted to
      # aed_sedflux + aed_noncohesive only (matching stage 2b's approach) so
      # NCS_ss1 is fit without any BGC/phyto coupling confound. Target:
      # NCS_ss1 only.
      if (RUN_STAGE3) {
      aeme0  <- aeme0_full
      period <- period_full

      tss_switches <- SWITCHES  # BGC modules other than sedflux/noncohesive
                                 # switched off via a dedicated model_controls
                                 # build below, matching stage 2b's isolation.
      b3_tss <- fresh_build("s3_tss", c("NCS_ss1", "HYD_temp"), use_bgc = TRUE,
                            n_zones = n_glm_zones, switches = tss_switches)

      tss_ptab <- expand_zones(read_param_table(PARAM_CSV), n_glm_zones)
      tss_adj <- tss_ptab[tss_ptab$file == "aed.nml" &
                          grepl("^aed_noncohesive/", tss_ptab$name), ]
      # 2026-09-29 fix ABANDONED 2026-09-30 (deadline): attempted freezing
      # the second (unobserved) particle class - tau_0[2]/w_ss[2] - since
      # this stage's own target (NCS_ss1) can't identify them (see the TSS
      # diagnosis in tss_phyto_update_2026-09.qmd and the Morris screen
      # results across 4 lakes this session: Tomarata kept 5/5, Kawaupaka
      # 1/5, Pupuke 3/5 - inconsistent "identification" of params never
      # really anchored by this stage's objective). Reverted: freezing them
      # via the frozen-table mechanism crashes calib_aeme_pest()'s
      # .pest_setup_localizer() - it requires EVERY parameter (adjustable
      # OR frozen) passed to calib_aeme() to have a row in the Morris
      # screen's own param_var_matrix, and a parameter frozen from its
      # first introduction (never screened at any earlier stage either,
      # unlike carry()'d physics params which WERE screened when they were
      # still adjustable) has no such row - confirmed directly, 2026-09-30,
      # first time this code path actually ran (every earlier "successful"
      # run this session reused a pre-fix stage-3 checkpoint and never
      # exercised it). Both particle classes adjustable again, matching
      # every lake actually completed so far. Worth revisiting properly
      # (screen tau_0[2]/w_ss[2] too, just expect/accept it gets dropped)
      # once there's time to test it, not under deadline pressure.
      # 2026-10-03: depth-structured solids target. Resuspension acts at the bed and
      # settling through the whole column, so a surface-only target (the previous
      # c(0, 2) region) cannot separate tau_0/epsilon from w_ss. Surface, mid and
      # bottom regions (temp_bands(): non-overlapping for any depth, like stage 4's)
      # are offered to resolve_targets(), which keeps only those with enough
      # non-constant observations in the calibration window. If it drops NCS_ss1
      # entirely, or keeps no region, fall back to the previous surface-only region.
      tb3 <- temp_bands(z)
      tss_sr_all <- list(
        surf_ncs_ss1 = list(var = "NCS_ss1", month = 1:12, depth_range = c(0, tb3$surf_hi), weight = 1),
        mid_ncs_ss1  = list(var = "NCS_ss1", month = 1:12,
                            depth_range = c(tb3$meta_lo, tb3$meta_hi), weight = 1),
        bot_ncs_ss1  = list(var = "NCS_ss1", month = 1:12, depth_range = c(tb3$bot_lo, z), weight = 1)
      )
      if (!isTRUE(tb3$has_meta)) tss_sr_all$mid_ncs_ss1 <- NULL
      tss_sr_all <- add_region_transform(tss_sr_all)   # PEST_REGION_TRANSFORM e.g. NCS_ss1=log1p
      rt_tss <- resolve_targets(b3_tss$aeme, "NCS_ss1", period, tss_sr_all,
                                base_weights = c(NCS_ss1 = 1), min_obs = 8)
      tss_regions <- if (length(rt_tss$sub_regions)) rt_tss$sub_regions else
        list(surf_ncs_ss1 = list(var = "NCS_ss1", month = 1:12, depth_range = c(0, 2), weight = 1))
      message("  stage 3 solids regions: ", paste(names(tss_regions), collapse = ", "))
      s3_tss <- run_or_load(3, b3_tss$aeme, run_stage(
        3, b3_tss$aeme, b3_tss$path, tss_adj,
        carry(s2, except = character()), targets = "NCS_ss1",
        sub_regions = tss_regions, weights = c(NCS_ss1 = 1),
        # v6: run_stage()'s adaptive-ensemble-size default is
        # `stage == 3`, matching v4/v5 where stage 3 is the many-parameter
        # BGC stage - here stage 3 is the new TSS-only stage (plus sedflux
        # rows added into tss_adj at IES time; epsilon/tau_0[1]/w_ss[1]
        # adjustable directly, tau_0[2]/w_ss[2] frozen - see above), which
        # does not need the inflated ensemble that default would otherwise
        # still trigger (confirmed directly: it grew to 343 realisations
        # for this stage on the first run, each iteration taking 35-45
        # min against an already-flat phi trajectory - wasted compute for
        # a parameter count the plain N_REALS floor already covers many
        # times over). Explicitly disabled here; stage 4's own call
        # explicitly enables it instead.
        adaptive_reals = FALSE,
        # 2026-10-04: screened-out solids parameters stay FIXED at their starting values (and appear
        # in `best`) instead of silently reverting to config defaults (Kuwakatai kept only epsilon).
        freeze_dropped = !nzchar(Sys.getenv("PEST_NO_FREEZE_DROPPED")),
        # 2026-10-04: the solids parameters are ALWAYS calibrated in this stage (this is the
        # stage that owns them), linked to the solids regions, regardless of the Morris screen.
        # Kuwakatai's screen kept only epsilon, so tau_0/w_ss ran at their defaults, yet a
        # forward test (w_ss[1] 0.46 -> 2.0) removed most of its mid-depth solids bias
        # (+3.2 -> +0.5 mg/L). PEST_NO_FORCE_SOLIDS disables.
        force_keep = if (nzchar(Sys.getenv("PEST_NO_FORCE_SOLIDS"))) NULL else
          setNames(rep(list(names(tss_regions)), 3),
                   c("aed_noncohesive/epsilon", "aed_noncohesive/tau_0", "aed_noncohesive/w_ss")),
        rebuild = function() fresh_build("s3_tss_cal", c("NCS_ss1", "HYD_temp"),
                                         use_bgc = TRUE, n_zones = n_glm_zones,
                                         switches = tss_switches)))
      message("stage 3 (TSS) complete - checkpoint: s3_", basename(AEME::get_lake_dir(b3_tss$aeme)), ".rds")
      } # RUN_STAGE3

      if (RUN_STAGE4) {
      # aeme0/period are already aeme0_full/period_full (the shortened-
      # window attempt for stages 1/2 was reverted - see the comment
      # where aeme0_phys used to be built, above). Kept as an explicit
      # reassignment (a no-op today) so this still reads correctly if a
      # future, correctly-implemented shortened window is reintroduced.
      aeme0  <- aeme0_full
      period <- period_full

      # ============================================================================
      # Stage 4 (was "stage 3" in v4) - biogeochemistry, minus TSS calibration
      # ============================================================================
      # v6: TSS parameters (aed_noncohesive) are no longer adjustable here -
      # stage 3 (above) now owns them. This stage inherits stage 3's result
      # narrow()'d (observed particle class) / carry()'d (unobserved class)
      # rather than fully re-opening them - see this file's header for why
      # a full freeze was rejected (the tss_phyto_update report's phosphorus-
      # TSS interaction finding, section 6). NCS_ss1 is kept as a
      # MONITORING target here (to confirm the TSS fit still holds once
      # full BGC coupling is switched back on), not a driver of new
      # aed_noncohesive adjustment.
      #
      # Lake volume is deliberately not a target here: use_bgc = TRUE together with
      # LKE_vol returns empty variable indices. The water balance is fixed by
      # stage 2's refit (v2) regardless. HYD_temp IS a target, at a reduced weight,
      # to anchor the stage-2 parameters this stage re-opens (S2_NARROW_NAMES,
      # v3: light/Kw, mixing coefficients, MET_wndspd/MET_radswd) - CHM_oxy
      # additionally anchors the mixing/wind ones, which affect oxygen via
      # stratification/turnover/gas-exchange as much as temperature.
      
      # Targets chosen to constrain the outputs the scenario work actually uses
      # (the TLI components), not just the pools AED happens to expose cheaply:
      #   NIT_nit  - nitrate is the dominant DIN pool in most of these lakes and
      #              the substrate for denitrification (the main water-column N
      #              sink) and for phytoplankton N uptake; calibrating only
      #              NIT_amm leaves the larger, more decision-relevant part of
      #              DIN - and hence TN - unconstrained.
      # Secchi (RAD_secchi) would be the natural light-climate target but
      # GLM-AED does not emit it in a calibration forward run - see the file
      # header. NIT_tn / PHS_tp ARE direct targets as of v3 - the ~25x
      # TOT_tn/TOT_tp issue that excluded them in v1/v2 was the
      # aed_nitrogen/simn2o=2 bug (denitrification structurally disabled),
      # now fixed (aed_sa_calib_params.csv).
      #
      # LKE_tli3 (chla+TN+TP composite, no Secchi) EXCLUDED again (2026-09-19):
      # AEME::calc_LKE_tli3()/calc_tli_chla()/calc_tli_n()/calc_tli_p() take
      # log10() of simulated concentrations with no floor, so a simulated
      # value of exactly/near zero on any single date produces -Inf/NaN,
      # which aemetools' forward-run harness treats as a hard failure for
      # the WHOLE calibration (not just that one observation). Patched
      # AEME's epi_idx() to stop the empty-epilimnion-selection case from
      # producing NaN (add_deriv_output.R), but the same "not finite:
      # LKE_tli3" abort still recurred for both Tomarata and Spectacle after
      # that fix and a full reinstall, so a second, un-diagnosed source of
      # non-finite values remains. Given TN, TP and PHY_tchla - LKE_tli3's
      # three components - are already direct targets in their own right,
      # dropping the composite costs little real signal while removing a
      # recurring blocker. Revisit once the root cause is actually found.
      # CHM_oxynal/CHM_oxycln added (2026-09-22), after checking Rototoa's
      # full observation set for anything usable that wasn't already a
      # target. Both are AEME derived-output calcs (calc_CHM_oxynal(),
      # calc_CHM_oxycln(), AEME namespace), not raw AED state, and both
      # use safe_apply()/NA-on-failure internally - unlike LKE_tli3's
      # log10(0) crash, there's no known non-finite-value failure mode
      # here. CHM_oxynal is NOT a redundant depth-slice of CHM_oxy the
      # way CHM_oxyepi/oxymet/oxymom are (the reason those three stayed
      # excluded, see the file header) - it interpolates the oxygen
      # profile to the current lake level at 0.2 m resolution and counts
      # how many increments read below 1 mg/L, i.e. it is a hypoxic-layer
      # EXTENT metric, integrating the whole profile rather than reading
      # one depth. CHM_oxycln (the oxycline depth, cline_depth() on the
      # oxygen profile) is likewise a whole-profile structural metric,
      # not a slice. Both have decent Rototoa records (oxynal n=352,
      # oxycln n=274 - comparable to PHY_tchla's 682 and PHS_tp's 849).
      #
      # NCS_ss1 added (2026-09-25, phyto-vs-zoo diagnostic session): before
      # this, aed_noncohesive (suspended-sediment resuspension/settling)
      # ran on AEME's own unmodified example template for every lake -
      # confirmed byte-for-byte identical across Rototoa/Tomarata/Kawaupaka
      # and absent from this table entirely until now. Tracing an
      # unexplained multi-year decline in simulated chl-a bloom peaks found
      # the true cause: resuspension = 1 (shear-stress driven) with
      # simSedimentMass = .false. (no benthic mass ever depletes - an
      # infinite erodible bed) let simulated NCS_ss1 climb to 100-500x
      # Rototoa's own observed range (obs 0.35-17.6 mg/L; simulated up to
      # 9630 at default params), which - via aed_phyto_pars.csv's fixed
      # Ke_ss = 0.06 /m per g/m3 - produced a physically impossible light
      # extinction coefficient (median ~440 m-1, vs ~0.1-5 m-1 for a real
      # lake) that choked phytoplankton growth. Confirmed directly: setting
      # resuspension = 0 dropped NCS_ss1 to [0, 12.51] (matching obs) and
      # extc to a realistic median 1.08 m-1, but OVERCORRECTED chl-a into
      # an unrealistic bloom (up to 57 ug/L, worse KGE) - neither extreme
      # is right, and the true fix is calibrating aed_noncohesive's
      # tau_0/epsilon/w_ss (now in aed_sa_calib_params.csv) against
      # Rototoa's 871 NCS_ss1 observations to find a realistic middle
      # ground, not a fixed on/off switch. This affects every lake equally
      # (shared template), not just Rototoa, so it belongs in the shared
      # target list here rather than a Rototoa-only override.
      # Trimmed (2026-09-25 night, user request - the "stage 4/5" combined
      # TSS+BGC run): drop HYD_thmcln/CHM_oxynal/CHM_oxycln/NIT_tn/PHS_tp,
      # keeping only temp/oxy/frp/nit/amm + the two this whole session has
      # been about (chl-a, NCS_ss1). HYD_temp needs no special anchor
      # treatment to count as a "real" target here - see the w3 comment
      # below (its resolve_targets() weight already wins over the
      # ANCHOR_WEIGHT duplicate via the c()-then-index-by-name pattern's
      # first-match semantics).
      # PHY_cyano attempted as a stage-3 target (2026-09-26): the lake-ID
      # linkage fix revealed 64 previously-invisible PHY_cyano observations
      # (2013-2022), comfortably above min_obs=8, and an extensive
      # mechanistic investigation (tss_phyto_update report, section 5)
      # found total chl-a alone cannot identify which phytoplankton group
      # should dominate - PHY_cyano looked like the first real, direct
      # group-level signal available to break that tie via calibration.
      #
      # REVERTED for now: resolve_targets()/the screen (pestpp-sen, "sa"
      # mode) accept PHY_cyano fine, but the actual pestpp-ies calibration
      # step ("fit"/"calib" mode) fails its pre-flight forward run with
      # "47 of 1412 simulated values were not finite: PHY_cyano 47/47.
      # Never produced at all by this configuration" - confirmed via
      # aemetools::pest_forward_run()'s own diagnostic message, and traced
      # to .pest_run_fit() vs .pest_run_sa() (both aemetools-internal,
      # compiled): both call run_and_fit(), but fit mode passes
      # method="calib" + regions=p$regions while sa mode passes
      # method="sa" + sa_ctrl=p$sa_ctrl - the actual divergence traced to
      # AEME::get_var_indices() (AEME/R/get_var_indices.R): its own
      # deliberate "prefer real observation depths over a synthetic grid"
      # optimisation narrows a region's requested depths to real
      # observation depths within depth_range - and since EVERY PHY_cyano
      # observation at Rototoa sits at exactly depth 0, that narrowing
      # legitimately collapses to a single, known depth. R's default
      # array-dim-dropping then returns that as a plain vector rather than
      # a matrix, and aemetools's .raf_extract_var() (run_and_fit.R)
      # unconditionally discarded a plain-vector result's depth to NA,
      # assuming "plain vector = depth-less surface variable" - true for
      # LKE_vol/LKE_lvlwtr, wrong here: PHY_cyano genuinely IS
      # depth-resolved (confirmed directly against the raw netCDF: written
      # [lon, lat, z=300, time], identical in shape to PHY_tchla, which
      # extracts correctly only because ITS own observations are not all
      # at one depth). Discarding the known depth to NA then broke
      # run_and_fit()'s "calib" mode exact (Date, depth, var_aeme) join.
      #
      # FIXED (2026-09-26): patched aemetools's .raf_extract_var() to check
      # the netCDF variable's own dimensions (does it have a "z" axis at
      # all?) before deciding whether a plain-vector result means "no depth
      # axis" (keep NA) or "one depth was requested/narrowed to" (keep the
      # real value) - see C:/Users/data/git/aemetools/R/run_and_fit.R.
      # Rebuilt and reinstalled into this project's renv library; verified
      # directly (aemetools' own 1-year Rototoa test fixture): calib mode
      # now returns a real, finite PHY_cyano value in both flat and region
      # form, "sa" mode and HYD_temp's own calib value are bit-identical
      # before/after (no regression). Full trace in the tss_phyto_update
      # report, section 10 item 0.
      vars_bgc_all <- c(
        "HYD_temp",
        "CHM_oxy",
        "NIT_amm", "NIT_nit",
        "PHS_frp",
        "PHY_tchla", "PHY_cyano", "NCS_ss1"
        )

      vars_bgc_all <- map_log_phy(vars_bgc_all)
      b3 <- fresh_build("s3", unique(c(vars_bgc_all, if (LOG_PHY) LOG_PHY_RAW, ANCHOR_VARS)),
                        use_bgc = TRUE, n_zones = n_glm_zones)
      assess_aeme(b3$aeme)

      # Adaptive per-lake targets. resolve_targets() (pest_funs.R) keeps only
      # the BGC variables this lake actually has usable data for inside its
      # simulation window, gives each a weight scaled by how much data it
      # carries (nitrate capped low - low-signal, near-detection-limit), and
      # filters the sub-region list to boxes that contain observations. Guards
      # against calib_aeme()'s hard error on an empty target (Pehiakura Small
      # has nitrate data but none in its window) and against a degenerate KGE
      # term running away with phi (a near-zero nitrate series took Rototoa's
      # stage-3 phi to 283 vs ~12 elsewhere).
      # oxynal/oxycln are whole-profile scalars (one value per timestep,
      # already integrating over depth - see the vars_bgc_all comment
      # above), so depth_range spans the full water column rather than a
      # surf/bot slice, matching how stage 1 treats HYD_thmcln/HYD_strat.
      # month restricted to summer stratification (Dec-Feb), same as
      # bot_oxy: hypoxia/oxycline development is a stratified-season
      # signal, matching this lake's actual anoxia pattern.
      # oxy_nal/oxy_cline/surf_tn/bot_tn/surf_tp/bot_tp/thermo_cline removed
      # (2026-09-25 night, "stage 4/5" trim) alongside CHM_oxynal/CHM_oxycln/
      # NIT_tn/PHS_tp/HYD_thmcln dropping out of vars_bgc_all above - these
      # sub-regions would just be silently unused since resolve_targets()
      # only keeps sub-regions for vars still in the candidate list.
      # tb4: depth-adaptive surf/bot boundaries for ALL stage-4 surf_*/bot_*
      # regions, not just temperature - reusing temp_bands() (originally
      # built for stage 2's surf_temp/meta_temp/bot_temp) rather than a
      # second hand-rolled scaling. The fixed c(0,2)/c(z-4,z) bands below
      # were tuned against, and only ever run against, Rototoa (z=26.06,
      # where they reproduce byte-identically via temp_bands()'s own
      # floor/ceiling choice). Confirmed broken 2026-09-28: Tomarata
      # resolves to z~5.2m, so bot_oxy (z-4:z = 1.2-5.2m) overlapped
      # surf_oxy (0-2m) and calib_aeme()'s .check_region_overlap() aborted
      # stage 4 outright ("Regions bot_oxy and surf_oxy both target CHM_oxy
      # and overlap"). temp_bands()'s surf_hi/bot_lo are constructed so
      # bot_lo - surf_hi = 0.7z - 0.3z = 0.4z > 0 for any z > 0, i.e. they
      # can never overlap regardless of lake depth - same guarantee that
      # already protected stage 2's surf_temp/bot_temp bands, now extended
      # to every other surf_*/bot_* pair here (oxy/amm/nit/frp/temp).
      tb4 <- temp_bands(z)
      sr_all <- list(
        bot_oxy  = list(var = "CHM_oxy",   month = c(12, 1, 2), depth_range = c(tb4$bot_lo, z)),
        surf_oxy = list(var = "CHM_oxy",   month = c(12, 1, 2), depth_range = c(0, tb4$surf_hi)),
        # surf_amm added (2026-09-24, user request: full top/bottom
        # coverage for nit/amm/frp/tn/tp) - bot_amm alone anchors only the
        # near-bed release/regeneration signal; without a surface
        # counterpart the calibration has no direct check on how much of
        # that ammonium actually reaches/persists in the epilimnion
        # (uptake/nitrification along the way), the same asymmetry bot_frp
        # was added to fix for phosphorus below. Same summer-only window
        # as bot_amm - ammonium accumulation is a stratified-season signal
        # for this lake, matching the existing bot_amm/bot_oxy convention.
        surf_amm = list(var = "NIT_amm",   month = c(12, 1, 2), depth_range = c(0, tb4$surf_hi)),
        bot_amm  = list(var = "NIT_amm",   month = c(12, 1, 2), depth_range = c(tb4$bot_lo, z)),
        surf_nit = list(var = "NIT_nit",   month = 1:12,        depth_range = c(0, tb4$surf_hi)),
        bot_nit  = list(var = "NIT_nit",   month = c(12, 1, 2), depth_range = c(tb4$bot_lo, z)),
        surf_frp = list(var = "PHS_frp",   month = c(12, 1, 2), depth_range = c(0, tb4$surf_hi)),
        # bot_frp added (2026-09-22, user request): sediment P release
        # under anoxia is a near-bed process - surf_frp alone dilutes/
        # transports that signal away from where release actually
        # happens, so it can't identify sediment-release parameters on
        # its own. Rototoa has ~30-45 near-bottom (depth >= 20 m, out of
        # ~26 m) PHS_frp observations - comparable in size to bot_nit.
        bot_frp  = list(var = "PHS_frp",   month = c(12, 1, 2), depth_range = c(tb4$bot_lo, z)),
        surf_chla = list(var = map_log_phy("PHY_tchla"), month = 1:12,       depth_range = c(0, tb4$surf_hi)),
        # surf_cyano added (2026-09-26) alongside PHY_cyano in vars_bgc_all
        # above - same surface convention as surf_chla (Rototoa's PHY_cyano
        # obs, like PHY_tchla's, are near-surface grabs, not depth-profiled).
        surf_cyano = list(var = map_log_phy("PHY_cyano"), month = 1:12,      depth_range = c(0, tb4$surf_hi)),
        # surf_ncs_ss1 added (2026-09-25) alongside NCS_ss1 in
        # vars_bgc_all above - see that block's comment for the full
        # rationale (aed_noncohesive resuspension tracing). Same surface
        # convention as surf_chla: Rototoa's NCS_ss1 obs are near-surface
        # grabs, not depth-profiled.
        surf_ncs_ss1 = list(var = "NCS_ss1", month = 1:12,      depth_range = c(0, tb4$surf_hi)),
        surf_temp = list(var = "HYD_temp", month = c(12, 1, 2), depth_range = c(0, tb4$surf_hi)),
        bot_temp  = list(var = "HYD_temp", month = c(12, 1, 2), depth_range = c(tb4$bot_lo, z)),
        win_temp  = list(var = "HYD_temp", month = c(6, 7, 8), depth_range = c(0, z)),
        schmidt_stab = list(var = "HYD_schstb", month = 1:12, depth_range = c(0, z)),
        whole_lvl    = list(var = "LKE_vol", month = 1:12, depth_range = c(0, z)),
        whole_nrg    = list(var = "LKE_nrgtot", month = 1:12, depth_range = c(0, z))
        )
      sr_all <- add_region_transform(sr_all)    # PEST_REGION_TRANSFORM (no-op when unset)
      # Trimmed to match vars_bgc_all's reduced target set (2026-09-25
      # night) - CHM_oxynal/CHM_oxycln/NIT_tn/PHS_tp caps removed along with
      # those targets. HYD_temp deliberately left OUT of this cap
      # (uncapped, i.e. resolve_targets() gives it its own data-driven
      # weight) - it's a real kept target this run, not just an anchor;
      # see the w3 comment below for why no ANCHOR_WEIGHT override reaches it.
      # PHY_cyano capped at 1, same as PHY_tchla - it has far fewer
      # observations (64 vs PHY_tchla's 222) so resolve_targets()'s own
      # data-driven weighting will already keep it appropriately modest;
      # the cap just guards against a low-observation-count series
      # producing a runaway weight the way NIT_nit's near-detection-limit
      # signal did before NIT_nit was capped.
      w_cap <- c(CHM_oxy = 1,
                NIT_amm = 1, NIT_nit = 0.4,
                PHS_frp = 1, PHY_tchla = 1, PHY_cyano = 1)
      names(w_cap) <- map_log_phy(names(w_cap))

      # 2026-09-29: NCS_ss1 EXCLUDED from resolve_targets()'s candidate list
      # here (was included with cap=1 - one of the HIGHEST caps of any
      # stage-4 target, alongside CHM_oxy/PHY_tchla). The comment this
      # replaced called it "the only direct constraint on aed_noncohesive's
      # tau_0/epsilon/w_ss" - stale for v6: those parameters aren't
      # adjustable at stage 4 at all any more (stage 3 owns them
      # exclusively - see this file's v6 header), so that justification no
      # longer applies. A second, separate comment (this stage's own
      # header, "NCS_ss1 is kept as a MONITORING target here... not a
      # driver of new aed_noncohesive adjustment") stated the actual
      # intent - but with real data-driven weight up to cap=1
      # (n_scale_weight(), 871-2056 obs at the 4 lakes tested = comfortably
      # at or near that cap), NCS_ss1 was functioning as one of the
      # STRONGEST drivers in stage 4's actual objective, contradicting its
      # own "monitoring, not driver" design intent. This is what let it
      # pull sed_temp_mean toward improving NCS_ss1's fit at the cost of
      # temperature/oxygen/nutrients (compare_thermal_freeze_vs_narrow.R) -
      # a general problem (ANY shared parameter could be pulled the same
      # way), not specific to sed_temp_mean, so excluding NCS_ss1 from the
      # objective here is a more complete fix than freezing sed_temp_mean
      # alone (kept as a second layer of protection, not redundant - other
      # shared parameters could still have weaker versions of the same
      # pathway through other targets).
      #
      # NCS_ss1 stays in vars_bgc_all/VARS_SIM above, so it is still fully
      # simulated and written out - only excluded from the weighted
      # phi objective. A real post-hoc check of whether the TSS fit still
      # holds once full BGC coupling is on (this stage's original stated
      # purpose for including it) now goes through assess_censored()
      # (pest_funs.R) after the run, same as every other cross-lake
      # comparison this session, not through the PEST objective itself -
      # matching what "monitoring, not driver" was supposed to mean all
      # along.
      vars_bgc_targets <- setdiff(vars_bgc_all, "NCS_ss1")

      rt       <- resolve_targets(b3$aeme, vars_bgc_targets, period, sr_all,
                                  base_weights = w_cap, min_obs = 8)
      vars_bgc <- rt$vars
      vars_s3  <- unique(c(vars_bgc, ANCHOR_VARS))
      # sub-regions: the ones resolve_targets() kept, plus the always-on
      # near-bed temperature anchor and (2026-09-22) the whole-lake Schmidt
      # stability anchor - a profile-integrated scalar like oxynal/oxycln
      # above, so depth_range spans the full column, scored year-round
      # (stratification strength is a 12-month signal, not summer-only).
      # Drop bot_temp/schmidt_stab from resolve_targets()'s own kept set
      # before re-adding them as always-on anchors below: sr_all already
      # defines both (identically), so when resolve_targets() also keeps
      # one (bot_temp does, whenever HYD_temp has near-bed data - true for
      # every lake tested so far), the plain c() used to produce TWO list
      # entries named "bot_temp" with the same content. A duplicate NAME
      # is legal in an R list but not for PEST: sa_aeme_pest()'s
      # pest_sa_obs_table() sanitises every sub-region name into a PEST
      # observation-group name and aborts on any collision - confirmed
      # 2026-09-24, the first time RUN_STAGE3 actually ran this far
      # end-to-end (previously gated off), as "Sub-region names collide
      # once sanitised for PEST: ... bot_temp ... bot_temp ...".
      sr3 <- c(rt$sub_regions[!names(rt$sub_regions) %in%
                              c("bot_temp", "schmidt_stab")],
               list(bot_temp = list(var = "HYD_temp", month = c(12, 1, 2),
                                    depth_range = c(tb4$bot_lo, z)),
                    schmidt_stab = list(var = "HYD_schstb", month = 1:12,
                                        depth_range = c(0, z))))
      # weights over vars_s3: data-scaled for BGC (from resolve_targets), the
      # reduced anchor weight for the physics anchor(s).
      #
      # HYD_temp note (2026-09-25 night): it sits in BOTH rt$weights (a real
      # vars_bgc_all candidate now, uncapped in w_cap above) and the
      # ANCHOR_VARS-weight vector appended after it. c()'s result keeps
      # both same-named entries, but `w3[vars_s3]` indexing a vector by
      # name resolves each name to its FIRST match - so w3["HYD_temp"]
      # is rt$weights's data-driven value, never the ANCHOR_WEIGHT=0.5
      # duplicate appended later. HYD_temp is therefore already a fully-
      # weighted real target this run, not just an anchor - no code
      # change needed beyond adding it to vars_bgc_all/leaving it out of
      # w_cap's caps. HYD_schstb (ANCHOR_VARS-only, never a vars_bgc_all
      # candidate) has no earlier entry to be shadowed by, so it still
      # gets the reduced ANCHOR_WEIGHT as intended.
      w3 <- c(rt$weights,
              stats::setNames(rep(ANCHOR_WEIGHT, length(ANCHOR_VARS)), ANCHOR_VARS))
      w3 <- w3[vars_s3]
      w3[!is.finite(w3)] <- 1

      cfg3 <- AEME::configuration(b3$aeme)$glm_aed
      aed <- cfg3$bgc$aed
      phyto <- cfg3$bgc$aed_phyto_pars
      n_aed_zones <- aed$aed_sed_const2d$n_zones
      
      # Everything AEME writes back to aed_phyto_pars.csv passes through
      # write_aed_param_csv(), which rounds to 3 dp. This lists the template values
      # that round-trip already changes - X_pmin, R_puptake and friends are zeroed
      # whether or not they are calibrated.
      prec <- phyto_precision_report(phyto)
      if (!is.null(prec)) prec <- prec[prec$group %in% PHYTO_GROUPS, ]
      if (!is.null(prec) && nrow(prec) > 0) {
        message("  aed_phyto_pars.csv values altered by the 3 dp write-out:")
        print(prec, row.names = FALSE)
      }
      
      # ---- stage-4 parameters, from the table ----------------------------------

      ptab <- expand_zones(read_param_table(PARAM_CSV), n_aed_zones)
      if (LOG_PHY) ptab$vars <- lapply(ptab$vars, map_log_phy)   # parameter -> target links follow the targets

      # v6: aed_noncohesive (TSS) rows excluded here - stage 3 now owns them.
      # The narrow()'d/carry()'d stage-3 result is added to this stage's
      # FROZEN/adjustable set separately below (see s3_tss_narrow/
      # s3_tss_frozen), not re-opened as a fresh adjustable row from the CSV.
      ptab <- ptab[!grepl("^aed_noncohesive/", ptab$name), ]

      # aemetools::check_param_targets() takes the built aeme (not the aed/phyto
      # pair the project-local version used) and returns the OFFENDING ROWS
      # invisibly, warning or aborting on `error`. Anything it flags either kills a
      # forward run or is silently ignored, so under STRICT_TABLES it aborts.
      bad <- aemetools::check_param_targets(ptab, aeme = b3$aeme, model = MODEL,
                                            error = STRICT_TABLES)
      if (nrow(bad) > 0) ptab <- ptab[!ptab$name %in% bad$name, ]
      
      # PO4AdsorptionModel picks which sorption parameters are live; the rest are
      # inert, and an inert adjustable parameter is pure noise in the ensemble.
      inert <- if (identical(as.numeric(SWITCH_CHOICE[["aed_phosphorus/PO4AdsorptionModel"]]), 1))
        c("aed_phosphorus/kadsratio", "aed_phosphorus/qmax") else "aed_phosphorus/kpo4p"
      ptab <- ptab[!ptab$name %in% inert, ]
      
      # A parameter the table does not link to any stage-3 target has nothing here
      # to constrain it.
      keep <- vapply(ptab$vars, function(v) any(v %in% vars_bgc), logical(1))
      if (any(!keep))
        message("  no stage-3 target for: ",
                paste(unique(ptab$name[!keep]), collapse = ", "))
      ptab <- ptab[keep, ]
      
      # Phytoplankton rows the 3 dp write-out cannot resolve: PEST would search a
      # coordinate the model never sees.
      unres <- phyto_unresolved(ptab)
      if (any(unres)) {
        message("  phyto rows below the 3 dp write-out resolution",
                if (DROP_UNRESOLVED) " (dropped): " else " (kept, will not act): ",
                paste(unique(ptab$name[unres]), collapse = ", "))
        if (DROP_UNRESOLVED) ptab <- ptab[!unres, ]
      }
      
      # The phyto index is cosmetic to AEME (the group name selects the column),
      # but keeping it aligned with the_phytos keeps name_full readable and stable.
      ph <- ptab$file == "aed_phyto_pars.csv"
      ptab <- ptab[!ph | ptab$group %in% PHYTO_GROUPS, ]
      ph <- ptab$file == "aed_phyto_pars.csv"
      ptab$index[ph] <- match(ptab$group[ph], PHYTO_GROUPS)
      
      message("  stage 3: ", nrow(ptab), " parameters from ", PARAM_CSV)

      # PEST workflow hook (R/pest_workflow/sedflux_pin.R): for lakes named in env
      # PEST_PIN_SEDFLUX_LAKES, pin the measured chamber sediment fluxes
      # (data/processed/sediment_flux_params.csv). No-op otherwise.
      sedflux_fixed <- NULL
      if (nzchar(Sys.getenv("PEST_PIN_SEDFLUX_LAKES"))) {
        source("R/pest_workflow/sedflux_pin.R")
        ptab <- pin_sediment_flux(ptab, AEME::lake(aeme0)$id)
        sedflux_fixed <- attr(ptab, "pinned")      # fixed (min = max) rows; NULL if none
      }
      print(table(ptab$module))

      # 2026-09-30: freeze_sediment_flux_data() ABANDONED same day it was
      # added (deadline) - same root cause as the TSS tau_0[2]/w_ss[2]
      # freeze attempt above: calib_aeme_pest()'s .pest_setup_localizer()
      # requires every parameter (adjustable OR frozen) to have a row in
      # THIS stage's own Morris screen param_var_matrix, and a parameter
      # frozen from its first introduction (fsed_*/Ksed_* are stage-4-only,
      # never adjustable/screened at any earlier stage) has none - would
      # crash Tomarata/Spectacle's stage 4 the same way it crashed stage 3
      # for every lake needing a fresh screen. sedflux_frozen left
      # unassigned deliberately (rather than always-empty) so a stray
      # reference downstream fails loudly instead of silently doing
      # nothing - revisit this properly (needs either screening the frozen
      # params too and accepting them being dropped, or a genuinely
      # different mechanism for a hard-fixed value) once there's time,
      # not under deadline pressure. Real measured data (data/processed/
      # sediment_flux_params.csv, Tomarata/Spectacle/Slipper) still exists
      # and is unused - the finding stands, just not wired in tonight.

      # Light attenuation and wind/shortwave re-opened, bounded by
      # stage 2's posterior (S2_NARROW_NAMES): each is a place the thermal
      # and biogeochemical calibrations genuinely meet, adjustable rather
      # than frozen only because HYD_temp (and, for wind/shortwave, CHM_oxy)
      # stay stage-3 targets to push back on them. mixing/coef_mix_* stays
      # frozen for every lake except Pupuke (2026-09-19 freeze; 2026-09-21
      # coef_mix_hyp exception - see the stage-2 s2_adj comment) - for every
      # other lake narrow() below simply finds nothing under that name and
      # contributes no row.
      kw_and_mix <- narrow(s2, S2_NARROW_NAMES)

      s3_adj <- rbind(ptab[, PCOLS], kw_and_mix)
      s3_meta <- s3_adj
      # Kw drives temperature (anchor) and chlorophyll (light limitation);
      # the rest (wind/shortwave) drive temperature and oxygen
      # (mixing energy/gas-exchange). Matched by row name, not
      # position/count, since narrow() does not guarantee S2_NARROW_NAMES'
      # own order.
      is_kw <- kw_and_mix$name == "light/Kw"
      s3_meta$vars <- c(ptab$vars,
                        lapply(is_kw, function(k)
                          if (k) unique(c(ANCHOR_VARS, map_log_phy("PHY_tchla")))
                          else unique(c(ANCHOR_VARS, "CHM_oxy"))))
      s3_pvm <- prior_localizer(s3_meta, vars_s3)
      
      # carry() pulls from BOTH prior stages: stage 1's frozen values
      # (outflow/seepage_rate/rh_factor/sed_heat_model - everything not
      # re-narrowed into stage 2, i.e. except = S1_NARROW_NAMES) plus
      # stage 2's frozen values (sediment thermal params, met bias
      # factors, coef_mix_hyp where adjustable - everything not
      # re-narrowed into stage 3, i.e. except = S2_NARROW_NAMES). The
      # Kw/wind/shortwave/coef_mix_hyp set that narrow(s2, S2_NARROW_NAMES)
      # above re-opens as adjustable is excluded from stage 2's carry so
      # it isn't duplicated at two different values.
      # v6: hand off stage 3's (TSS) result into stage 4 - narrow()'d, not
      # fully carry()'d, so the phosphorus-TSS interaction (tss_phyto_update
      # report, section 6) still has room to move slightly if stage 4's own
      # phosphorus-cycle fit calls for it. SIMPLIFICATION flagged rather
      # than hidden: narrow() matches on the bare parameter `name`, not
      # `name_full`/index, so this narrows tau_0/w_ss/epsilon for BOTH
      # particle classes together (observed and unobserved) rather than
      # narrowing only the data-constrained observed class and carry()ing
      # the unconstrained one separately, as this file's header describes
      # as the intended eventual design - a per-index-aware split needs a
      # small change to narrow() itself (pest_funs.R), out of scope for
      # this first sketch.
      s3_tss_narrow <- narrow(s3_tss, c("aed_noncohesive/tau_0",
                                        "aed_noncohesive/w_ss",
                                        "aed_noncohesive/epsilon"))
      # No s3_tss_frozen term: stage 3's ENTIRE adjustable set was exactly
      # these 3 parameter names (tau_0/w_ss/epsilon), all of which are
      # narrow()'d above - there is nothing else from stage 3 left to
      # carry(). (carry(s3_tss, except = <all 3 names>) would correctly
      # compute zero rows here, but aemetools::carry_param() does not
      # handle an all-excepted/empty result gracefully - confirmed
      # directly, "replacement has 1 row, data has 0" - so this term is
      # omitted rather than worked around.)
      # 2026-10-03: stage 3's solids parameters (epsilon, tau_0, w_ss) are carried into
      # stage 4 as FIXED parameters (min = max = the stage-3 best value), exactly like
      # the stage 1/2 values carried above. Solids affect phosphorus sorption and light
      # but nothing in stage 4 affects solids, so stage 3 fully determines them.
      #
      # NOT as +/-0.1% "adjustable in name only" bounds (the first version): a Morris
      # elementary effect is the response change per unit of parameter change, so a
      # parameter with a ~0.00005-wide range turns ANY response noise (one aberrant run
      # in 268) into mu* ~ 4e5. That single value set every region's maximum, so every
      # real parameter fell below the keep-threshold ("kept 3 of 67"). Fixed parameters
      # are not varied by the screen and are ignored by the localizer.
      s3_best_nf <- add_name_full(s3_tss$best)
      s3_tss_fixed <- add_name_full(s3_tss_narrow[, PCOLS])
      for (k in seq_len(nrow(s3_tss_fixed))) {
        v <- s3_best_nf$value[match(s3_tss_fixed$name_full[k], s3_best_nf$name_full)]
        if (is.na(v)) next
        s3_tss_fixed$value[k] <- v; s3_tss_fixed$min[k] <- v; s3_tss_fixed$max[k] <- v
      }
      message("  stage 4: stage-3 solids parameters carried as FIXED: ",
              paste(sprintf("%s=%.4g", sub("^NA/", "", s3_tss_fixed$name_full), s3_tss_fixed$value), collapse = ", "))


      s4 <- run_or_load(4, b3$aeme, run_stage(
        4, b3$aeme, b3$path, s3_adj,
        rbind(carry(s1, except = S1_NARROW_NAMES)[, PCOLS],
              carry(s2, except = S2_NARROW_NAMES)[, PCOLS],
              s3_tss_fixed[, PCOLS],
              if (!is.null(sedflux_fixed)) sedflux_fixed[, PCOLS]),
        targets = vars_s3,
        prior_pvm = s3_pvm,
        weights = set_weights(vars_s3, w3),
        sub_regions = sr3,
        # 2026-10-03: screened-out stage-4 parameters stay in the control file as FIXED
        # at their starting values (not reverting to model defaults) - see run_stage()
        freeze_dropped = !nzchar(Sys.getenv("PEST_NO_FREEZE_DROPPED")),
        # 2026-10-04: PEST_RESTART_S4=<finished/interrupted cal_s4 run directory> warm-starts the
        # iES from that run's final ensembles (one extra iteration) instead of repeating ~1300
        # model runs, e.g. after the post-iES results read crashed (Spectacle).
        restart_from = if (nzchar(Sys.getenv("PEST_RESTART_S4"))) Sys.getenv("PEST_RESTART_S4") else NULL,
        restart_noptmax = 1L,
        # 2026-10-04: bloom-seeded prior. PEST_BLOOM_PRIOR=<csv> (rows = accepted forward-model
        # screen runs, columns = aemetools name_full; written by R/pest_workflow/build_bloom_prior.R)
        # seeds the stage-4 iES prior with parameter sets whose modelled chl-a is in the observed range,
        # so the smoother does not start (and stay) in the collapsed no-phytoplankton regime. Needs the
        # aemetools build with data-frame priors (library renv_lib_regtr2_root).
        prior_par_ensemble = if (nzchar(Sys.getenv("PEST_BLOOM_PRIOR")))
          utils::read.csv(Sys.getenv("PEST_BLOOM_PRIOR"), check.names = FALSE) else NULL,
        # stall_minutes raised from the default 10 (2026-09-24): a stage-3
        # (use_bgc = TRUE) forward run writes a MUCH larger netCDF than
        # physics-only stages 1/2 - confirmed directly, ~1.9 GB per
        # evaluation for Rototoa, against a few hundred MB for stage 2 -
        # because every AED state variable (nutrients, per-group
        # phytoplankton, sediment zones) gets written at every layer and
        # timestep, not just temperature/salinity. With NCORE=20 running
        # fully in parallel, that is up to ~40 GB of concurrent netCDF I/O
        # per screening/solver batch. A single uncontended replay of one
        # stage-3 forward run completed in well under a minute end to end,
        # but stage 3's actual first pestpp-sen batch stalled with "0
        # model runs logged" after the default 10-minute timeout across
        # all 20 workers simultaneously - consistent with disk I/O
        # contention under full parallelism, not a genuine hang or code
        # bug (no error in any individual agent's log; the model run
        # itself completed, then PANTHER simply never heard back in time).
        stall_minutes = 30,
        # v6: run_stage()'s adaptive-ensemble-size logic defaults to
        # `stage == 3`, matching v4/v5 where stage 3 is the BGC stage -
        # here the BGC stage is 4 (stage 3 is the new TSS-only stage,
        # which has too few parameters to need this), so it must be
        # requested explicitly or this stage would silently run at the
        # fixed N_REALS=40 floor instead of growing toward
        # IES_TARGET_SURVIVORS as its ~26-38 parameters need.
        adaptive_reals = TRUE,
        rebuild = function() fresh_build("s4cal", unique(c(vars_s3, if (LOG_PHY) LOG_PHY_RAW)), use_bgc = TRUE,
                                         n_zones = n_glm_zones)))
      } # RUN_STAGE4

      # ---- results ---------------------------------------------------------------

      for (s in c(list(s1, s2), if (RUN_STAGE3) list(s3_tss), if (RUN_STAGE4) list(s4))) {
        cat("\nstage", s$stage, "- kept", nrow(s$kept), "parameters, phi",
            paste(signif(s$phi$min, 4), collapse = " -> "), "\n")
        print(s$best[, c("file", "name", "group", "index", "value")],
              row.names = FALSE)
      }
      
      # ---- posterior predictive ensemble --------------------------------------
      #
      # DISABLED (RUN_POSTERIOR_ENSEMBLE, top of file). run_aeme_ensemble() has
      # silently killed the whole run_pest_calib.R session three times now, twice
      # in the parallel PSOCK path and once even after forcing parallel = FALSE
      # (a single posterior draw crashed the GLM binary or the netCDF read of its
      # missing output, with no R traceback either time - a process crash, which
      # no tryCatch here can see or recover from). All three losses were pure
      # diagnostics; every calibration result was already safe in its checkpoint.
      # Until that is isolated (e.g. run per-lake in its own subprocess via
      # callr::r_safe(), so a crash there cannot take the master down), stick to
      # the calibration itself: stages 1-3, checkpointed and reported below.
      #
      # pestpp-ies is an iterative ensemble smoother: its real product is the
      # posterior *ensemble*, one parameter vector per realisation. Pushing that
      # back through the model - what this block did - shows the spread of the
      # modelled series against observations, a far more honest picture than a
      # single best fit. To resume that once the isolation above is in place, see
      # `archive_ensemble_diagnostics.R` for the code that was here.
      if (isTRUE(RUN_POSTERIOR_ENSEMBLE)) {
        if (!exists("s4")) s4 <- readRDS(stage_rds(4, b3$aeme))
        diag_ok <- tryCatch({
          run_posterior_ensemble_diagnostics(
            s1 = s1, s2 = s2, s3 = s4, b3 = b3, vars_s3 = vars_s3, z = z,
            n_glm_zones = n_glm_zones)
          TRUE
        }, error = function(e) {
          message("  post-processing failed for ",
                  basename(AEME::get_lake_dir(b3$aeme)), ": ", conditionMessage(e))
          FALSE
        })
        if (!isTRUE(diag_ok))
          message("  stages 1-3 are checkpointed; diagnostics skipped for this lake")
      }
      
      TRUE
    }, error = function(e) {
      # Calling handler: snapshot the stack and let the condition keep
      # propagating (no return value used - this must NOT unwind anything).
      .err_trace <<- vapply(sys.calls(), function(cl)
        paste(utils::head(deparse(cl), 1), collapse = " "), character(1))
    })
  }, error = function(e) {
    message("\n!!! lake ", i, " FAILED: ", conditionMessage(e),
            "\n!!! continuing with the next lake")
    if (!is.null(.err_trace)) {
      show <- utils::tail(.err_trace, 25)
      message("!!! call stack (innermost first, last ", length(show), " of ",
              length(.err_trace), " frames):\n",
              paste0("   ", rev(show), collapse = "\n"))
    }
    FALSE
  })
  if (!isTRUE(lake_ok)) failed_lakes <- c(failed_lakes, i)
  
}

if (length(failed_lakes) > 0) {
  message("\nLakes that failed: ", paste(failed_lakes, collapse = ", "),
          " - re-run to resume them from their last checkpoint.")
} else {
  message("\nAll lakes completed.")
}
