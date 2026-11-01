# PEST calibration → ensemble workflow for AEME lakes (GLM-AED)

Package assembled 5 October 2026 from the `ac_lake_modelling` project. It holds the scripts that calibrate a lake
with PEST++ (`pest_calib_v6.R`, four stages), extract the posterior, run a baseline ensemble and check it against
observations. Most of it is lake-agnostic and is driven by a lake tag such as `LID45818_kawaupaka`.

Read `reports/pest_workflow_summary_2026-10.qmd` for the history, the findings behind each design choice and the
known problems. The short version is under "Known problems" below.

## 1. What is in this zip

```
README.md
scripts/
  run_pest_calib_v6.R       runner: logging + traceback; sources pest_calib_v6.R. Run from the project root.
  pest_calib_v6.R           the four-stage calibration (1 water balance, 2 physics/heat flux, 3 solids, 4 BGC)
  pest_funs.R               helpers: run_stage(), carry(), build_lake_aeme0(), fresh_build(), run_or_load() ...
  run_kawaupaka_calib.sh    TEMPLATE driver: calibrate -> posterior -> 50-member baseline ensemble -> sanity checks
  run_kawaupaka_seeded.sh   TEMPLATE: same, with a bloom-seeded stage-4 prior (optional fallback, see section 6)
  run_kuwakatai_overnight.sh TEMPLATE for a downstream lake (inflow = upstream lake's ensemble outflow)
R/
  set_lake_inflows.R, lspc_to_aeme.R, add_block_spinup.R    input builders the workflow sources
  pest_workflow/
    setup.R                 model configuration shared by calibration and forward runs (switches, overrides, paths)
    manifest.R              sha256/input manifests; later steps refuse to run on stale or missing upstream steps
    pest_extract.R, extract_posterior.R   posterior members, best parameters, Morris sensitivity
    run_ensemble.R          parallel baseline/scenario ensemble (nested seeded member selection, resumable)
    inflow_fix.R            corrects the 1e6 unit error in LSPC inflow solids (see section 7)
    downstream.R            downstream lakes: parent lake's outflow becomes the inflow
    sedflux_pin.R           fixes measured sediment fluxes (only used for lakes with chamber data)
    sanity_checks.R         bands vs observations, obs-vs-model, integrity checks, metrics.csv
    member_daily.R, scenario_comparison.R   daily member series; paired scenario-vs-baseline changes
    sa_bloom.R, build_bloom_prior.R         bloom Morris screen and the bloom-seeded prior (optional)
    choose_recipe.R, compare_ab_calibrations.R, diagnostics_fit.R   recipe comparison/diagnostics
    test_forward_best.R, test_spectacle_solids.R   single forward-run diagnostics (examples to adapt)
data/processed/
  aed_sa_calib_params.csv   parameter table: values, bounds, the variables each parameter is linked to
  aed_sa_calib_switches.csv model switches
  sediment_flux_params.csv  measured sediment fluxes (Spectacle, Slipper, Tomarata only)
  lspc_output_key.csv       LSPC output column key (units)
aemetools_patched/
  aemetools_0.3.0.9000_patched.tar.gz   source of the patched aemetools (see section 3)
reports/
  pest_workflow_summary_2026-10.qmd     full write-up of the work and findings
```

## 2. What you must supply (not in this zip)

* **A project with a `targets` store** (`_targets.R`, `_targets/`) containing these objects, which the scripts read
  with `tar_read()`:
  `aeme_baseline_upstream` (list of Aeme objects, one per lake, with observations, met and inflows),
  `aeme_baseline_downstream` (only if you run downstream lakes), `lake_metadata` (needs `lernzmp_id`, `lake_upstream`),
  `lake_routing`, `simulation_time` (list with `start`, `stop`), `spinup_mapping`, `flux_params`.
  The new lake must be one of the elements of `aeme_baseline_upstream`.
* **The lake's AEME folder** under `aeme/` (for example `aeme/LID45818_kawaupaka/`), as referenced by the Aeme object.
* **Software:** R 4.6, the `AEME` package (0.4.0), `aemetools` (patched, section 3), PEST++ (`pestpp-ies`,
  `pestpp-sen`) as installed by aemetools, and the GLM-AED binary AEME uses. CRAN packages: targets, tarchetypes,
  dplyr, tidyr, arrow, parallel, callr, jsonlite, openssl, cli, duckdb.
* **Disk and cores:** the PEST working dirs and per-member ensemble caches are large (tens of GB per lake while
  running). The ensemble runner uses 24 workers by default and about 1.5 GB per worker.

## 3. The patched aemetools

The workflow needs two additions to aemetools that are not in the released package:

1. a per-sub-region `transform` field (`"log"`, `"log1p"`, `"log10"`, `"sqrt"` or `list(fun, offset)`) applied to
   observations and model values in the PEST observation table, in the calibration data frame and in the SA
   (new file `R/region_transform.R`, plus changes to `calib_regions.R`, `create_sen_control.R`, `write_pst.R`,
   `run_and_fit.R`);
2. `prior_par_ensemble` accepts a data frame of parameter sets (rows = realisations, columns = `name_full`)
   (`calib_aeme_pest.R`, used by the bloom-seeded prior).

Install it into a separate library so the project's own aemetools is untouched:

```bash
mkdir -p /path/to/lib
R CMD INSTALL --no-docs --no-multiarch --library=/path/to/lib aemetools_patched/aemetools_0.3.0.9000_patched.tar.gz
```

Then run with `RENV_PATHS_LIBRARY=/path/to/lib` (if you use renv) or put `/path/to/lib` first on `.libPaths()`.
If you do not need the log1p chl-a targets, the unpatched aemetools works but `PEST_REGION_TRANSFORM` must stay unset.
These changes are uncommitted drafts; merge them into the aemetools repository before relying on them.

## 4. Set up in a new project

1. Copy `scripts/`, `R/` and `data/processed/` into the project root (the scripts use paths relative to the root;
   run everything from the root).
2. Edit the three hard-coded defaults (or set the environment variables):
   * `R/pest_workflow/setup.R`: `LOCAL_ROOT` / env `PEST_LOCAL_ROOT` – bulky ensemble files and scratch builds.
   * `scripts/pest_calib_v6.R` line 383: `ROOT` / env `PEST_CALIB_ROOT` – PEST working directories and results DB.
   * `R/pest_workflow/member_daily.R` and `scenario_comparison.R` read `PEST_LOCAL_ROOT` too.
   * The shell templates contain `cd "C:/Users/..."`, `R=".../Rscript.exe"` and `LIBROOT=...`; change them.
3. Output folders (created on demand): `pest_workflow/` (small files: posterior, manifests, checks; override with
   `PEST_WF_ROOT`) and `staged-calibration-v4/checkpoints/` (stage checkpoints; override with `PEST_CHECKPOINT_ROOT`).

## 5. Running a lake

### 5.1 Choose the lake

`PEST_LAKE_IDX` is the lake's position in `aeme_baseline_upstream`:

```r
a <- targets::tar_read(aeme_baseline_upstream)
data.frame(i = seq_along(a), id = vapply(a, function(x) AEME::lake(x)$id, ""), name = vapply(a, function(x) AEME::lake(x)$name, ""))
```

For a downstream lake use `PEST_DOWNSTREAM_LIDS=<LID>` (the lake is appended to the list, so its index is
`length(aeme_baseline_upstream) + 1` for the first one). The parent lake's baseline ensemble must already exist in the
same `PEST_WF_ROOT` (`<parent>/outflow/baseline_outflow.csv`).

### 5.2 Recipe (environment variables)

| Variable | Meaning |
|---|---|
| `PEST_LAKE_IDX` | which lake (section 5.1) |
| `PEST_DOWNSTREAM_LIDS` | downstream lake id(s) to append |
| `PEST_REGION_TRANSFORM="PHY_tchla=log1p,PHY_cyano=log1p"` | log1p chl-a/cyano targets (the recipe used from 3 Oct) |
| `PEST_PIN_SEDFLUX_LAKES=<LID>` | fix measured sediment fluxes (needs a row in `flux_params`) |
| `PEST_CHECKPOINT_ROOT`, `PEST_WF_ROOT`, `PEST_LOCAL_ROOT`, `PEST_CALIB_ROOT` | output locations |
| `PEST_NO_FREEZE_DROPPED` | disable holding screened-out parameters fixed (stages 3 and 4) |
| `PEST_NO_FORCE_SOLIDS` | disable forcing `epsilon`, `tau_0`, `w_ss` into stage-3 calibration |
| `PEST_RESTART_S4=<cal_s4 run dir>` | warm-start stage 4 from a previous (e.g. crashed) run's final ensembles |
| `PEST_BLOOM_PRIOR=<csv>` | bloom-seeded stage-4 prior (section 6) |
| `PEST_NO_SS_FIX` | disable the inflow solids unit correction (control experiments only) |
| `RENV_CONFIG_SANDBOX_ENABLED=FALSE` | needed because each PEST agent starts its own R session |

### 5.3 The steps

The simplest route is to copy `scripts/run_kawaupaka_calib.sh` and change the lake tag and `PEST_LAKE_IDX`. It does:

```bash
# 1. calibrate (resumable: each stage checkpoints; re-run to resume; retries up to 4 times)
Rscript scripts/run_pest_calib_v6.R                       # with the env vars above
# 2. posterior, best parameters, sensitivity
Rscript R/pest_workflow/extract_posterior.R <LAKE_TAG> <CHECKPOINT_ROOT>
# 3. 50-member baseline ensemble, 24 workers (resumable from the per-member cache)
Rscript R/pest_workflow/run_ensemble.R <LAKE_TAG> 50 24 baseline
# 4. checks against observations (writes metrics.csv, integrity.csv and plots)
CHECK_WORKERS=8 Rscript R/pest_workflow/sanity_checks.R <LAKE_TAG> baseline
```

Reading the results: `<PEST_WF_ROOT>/<LAKE_TAG>/checks/baseline/metrics.csv` (bias, RMSE, KGE, % observations inside
the 5–95% band by variable and depth class) and `integrity.csv`. Stages 1 and 2 are physics only; if the lake already
has stage-1/2 checkpoints from an earlier calibration they can be copied into the new checkpoint folder (the template
does this) – stage 3 onward should be fresh.

Silent native crashes (GLM, PEST agents) are routine during long runs. Re-running resumes from the last checkpoint.
An identical failure repeated is a real bug.

## 6. Optional: bloom screen and bloom-seeded prior

For a lake whose stage 4 lands in a "no phytoplankton" state:

```bash
Rscript R/pest_workflow/sa_bloom.R <LAKE_TAG>           # ~600 full-model runs (Morris), needs an existing posterior as reference
Rscript R/pest_workflow/build_bloom_prior.R <LAKE_TAG>  # writes bloom_prior.csv: screen runs whose chl-a matches observations
# then calibrate stage 4 with PEST_BLOOM_PRIOR=<that csv>  (see scripts/run_kawaupaka_seeded.sh)
```

The screen output (`morris.csv`, `reach.csv`, `runs.csv`, `params.csv`) also shows which parameters control surface
chl-a, cyano, green and diatom. In the Kawaupaka test the seeded stage 4 was only marginally better than the unseeded
one, so this is a fallback and not a standard step. It needs the patched aemetools (section 3).

## 7. Things learnt that the scripts now handle (do not undo)

* **Inflow solids units:** `lspc_to_aeme.R` writes LSPC `ssedout` as mg/L but it is already t/m³, so inflow solids
  were 1e6 too small. `fix_inflow_ss_units()` (applied in calibration and in `build_ensemble_aeme0`) corrects it
  when the flow-weighted value is below 1e-3 mg/L; applying it twice is harmless.
* **Screened-out parameters** used to revert silently to model defaults. `freeze_dropped` holds them fixed at their
  starting values and records them in `best_params.csv`. Check `best_params.csv` contains every parameter you expect.
* **Pins must be fixed parameters (min = max), never tight bounds:** a ±0.1% bound wrecks the Morris screen.
* **Stage-3 solids:** `epsilon`, `tau_0` and `w_ss` are always calibrated (forced past the screen).
* **Model window:** `build_lake_aeme0()` sizes the window to a lake's observations, so a lake with a short record gets
  a short window. `build_ensemble_aeme0()` now widens it to `simulation_time` (met and inflows cover it).
* **Parallel R:** keep worker functions at top level; do not name an argument `fun`; do not edit a running Rscript.
* **Process management on Windows:** kill by PID. A pattern match on the command line can match your own shell.

## 8. Known problems (as of 5 October 2026)

* **Chl-a is not yet right everywhere.** Rototoa (a clear lake) and Kawaupaka are in range or modestly off;
  Kuwakatai is about +50 µg/L at the surface, Kawaupaka roughly double the observed mean.
* **Phosphorus is not calibrated** where most FRP observations sit at the detection floor: the FRP target is dropped
  and its parameters have no target (one-sided handling of censored data would fix it; not implemented).
* **Kawaupaka:** solids profile (surface +5 to +7, mid-depth −26, bottom −18 mg/L), water level (−3 m) and thermocline
  depth (−3 m) are poor.
* **Spectacle:** its apparent solids runaway was the settling-sign problem (section 9); it is being recalibrated, so treat earlier Spectacle results as invalid.
  parameters or inflow solids are set to, and temperature is 3–4 °C too cold. The cause is not found; the older
  calibration was equally bad. Do not use it before diagnosing (see `test_spectacle_solids.R`).
* **Rototoa's ensemble** predates the model-window fix and needs rerunning.
* Scenario ensembles (`run_ensemble.R <TAG> 50 24 all`) and the dashboard staging
  (`R/dashboard_extract/stage_pest_workflow.R`, not included) have not been rerun with the current recipe.
* The patched aemetools is an uncommitted draft.

## 9. IMPORTANT – particle settling sign (added 5 October 2026)

In AED a **negative** `w_ss` is settling and a positive `w_ss` is buoyancy. The AEME package template (not the files under
`aeme/`) carries `w_ss = 0.5, 0.03`, `fs = 1, 1` and `epsilon = 0.02`, so every solids calibration before this date ran
with floating particles. `fix_noncohesive()` in `scripts/pest_funs.R` (called from `fresh_build()`) now sets `w_ss =
-0.5, -0.03`, `fs = 0.6, 0.4`, `epsilon = 3e-4`, `tau_0 = 0.08, 0.05` in the built `aed.nml` and in the Aeme object
(`run_aeme_param()` regenerates the files from the object, so both are needed). `data/processed/aed_sa_calib_params.csv`
has matching bounds (`w_ss` -2.0 to -0.1 and -1.0 to -0.03; `epsilon` 5e-5 to 2e-3). `PEST_NO_NC_FIX=1` restores the old
defaults for control runs. Any stage-3 or later result produced without this fix has the wrong particle physics and should
be redone. `scripts/run_fixC_all.sh` is the all-lakes driver that does so. `R/pest_workflow/inspect_glm_output.R` opens the raw GLM
netCDF of a forward run (solids, resuspension and settling diagnostics), which is how the problem was found.
