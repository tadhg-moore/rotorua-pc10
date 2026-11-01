#!/bin/bash
# Overnight chain: wait for fixA/fixB, pick the recipe (R/pest_workflow/choose_recipe.R), then
#   Kuwakatai -> Spectacle (measured fluxes FIXED) -> Slipper: calibrate -> posterior -> BASELINE ensemble -> sanity.
# Scenarios are deliberately NOT run (they need Rototoa/Spectacle scenario outflows in the chosen root).
# Everything lands in pest_workflow_<A|B> / checkpoints_<A|B> (same roots as the fix runs, so Rototoa's
# outflow for Kuwakatai is already there). Progress: logs/chain2/STATUS.txt
cd "C:/Users/data/Dropbox/AC/modelling/phase2/ac_lake_modelling" || exit 1
R="C:/Program Files/R/R-4.6.1/bin/x64/Rscript.exe"
LOGD=logs/chain2; mkdir -p $LOGD; STATUS=$LOGD/STATUS.txt
LIBROOT="C:/Users/data/Desktop/ac_lake_calib/renv_lib_regtr2_root"
log() { echo "[$(date '+%a %d %H:%M:%S')] $*" | tee -a "$STATUS"; }
free_gb() { powershell -NoProfile -Command "[int][math]::Floor((Get-PSDrive C).Free/1GB)" | tr -d '\r'; }
wait_disk() { local n=0; while [ "$(free_gb)" -lt "$1" ]; do [ $n -ge 72 ] && return 1; [ $n -eq 0 ] && log "waiting for $1 GB (now $(free_gb))"; n=$((n+1)); sleep 300; done; }

log "fixC (corrected w_ss sign / fs / epsilon) all-lakes driver started"
until grep -q "freeze-fix rerun FINISHED" logs/freezefix/STATUS.txt; do sleep 120; done
REC=$("$R" R/pest_workflow/choose_recipe.R 2> "$LOGD/choose.log" | tail -1 | tr -d '\r')
[ "$REC" = "A" ] || [ "$REC" = "B" ] || REC=A
log "recipe chosen: $REC ($(cat $LOGD/choose.log | tr '\n' ' '))"
V=fixC
WF=pest_workflow_$V; CK=staged-calibration-v4/checkpoints_$V; LOC="C:/Users/data/Desktop/ac_lake_calib/pest_workflow_$V"
EXTRA=(); [ "$REC" = "B" ] && EXTRA=(PEST_REGION_TRANSFORM="PHY_tchla=log1p,PHY_cyano=log1p")
true
mkdir -p "$CK"

ENVV=(RENV_CONFIG_SANDBOX_ENABLED=FALSE RENV_PATHS_LIBRARY="$LIBROOT" PEST_CHECKPOINT_ROOT="$CK" PEST_WF_ROOT="$WF" PEST_LOCAL_ROOT="$LOC" "${EXTRA[@]}")

calibrate() {  # tag, extra env...
  local tag=$1; shift
  [ -f "$CK/s4_$tag.rds" ] && return 0
  for i in 1 2 3 4; do
    wait_disk 40 || return 1
    log "$tag: calibration attempt $i/4"
    RS=(); [ -n "$RESTART_DIR" ] && [ $i -le 2 ] && RS=(PEST_RESTART_S4="$RESTART_DIR"); env "${ENVV[@]}" "${RS[@]}" "$@" "$R" scripts/run_pest_calib_v6.R > "$LOGD/calib_${tag}_$i.log" 2>&1
    [ -f "$CK/s4_$tag.rds" ] && { log "$tag: calibration COMPLETE"; return 0; }
    log "$tag: no s4 after attempt $i - resuming"; sleep 60
  done
  return 1
}
post() {
  local tag=$1
  wait_disk 45 || return 1
  env "${ENVV[@]}" "$R" R/pest_workflow/extract_posterior.R "$tag" "$CK" > "$LOGD/posterior_$tag.log" 2>&1 || { log "$tag: posterior FAILED"; return 1; }
  for i in 1 2 3; do env "${ENVV[@]}" "$R" R/pest_workflow/run_ensemble.R "$tag" 50 24 baseline > "$LOGD/ens_${tag}_$i.log" 2>&1 && break; log "$tag: ensemble attempt $i failed"; done
  env "${ENVV[@]}" CHECK_WORKERS=8 "$R" R/pest_workflow/sanity_checks.R "$tag" baseline > "$LOGD/sanity_$tag.log" 2>&1
  [ -f "$WF/$tag/baseline/manifest.json" ] || { log "$tag: no baseline manifest"; return 1; }
  rm -rf "$LOC/$tag/baseline/members" "$LOC/_builds/${tag}"_w* 2>/dev/null
  log "$tag: baseline ensemble + sanity COMPLETE"
}

# Everything from stage 3 on is recalibrated with the corrected particle settings (fix_noncohesive() in pest_funs.R:
# w_ss negative = settling, fs = 0.6/0.4, epsilon 3e-4, tau_0 0.08/0.05; parameter-table bounds updated).
# Stages 1-2 (physics) are copied from the earlier runs. New roots: pest_workflow_fixC, checkpoints_fixC.
RESTART_DIR=""
seed() {  # tag
  for s in s1 s2 screen_s2; do for src in staged-calibration-v4/checkpoints_fixB staged-calibration-v4/checkpoints; do
    [ -f "$src/${s}_$1.rds" ] && { cp -n "$src/${s}_$1.rds" "$CK/"; break; }; done; done
}
lake() {  # tag, calibrate-env...
  local tag=$1; shift; seed "$tag"
  calibrate "$tag" "$@" && post "$tag" || log "$tag FAILED"
}
lake LID45818_kawaupaka PEST_LAKE_IDX=3
lake LID22999_spectacle PEST_LAKE_IDX=2 PEST_PIN_SEDFLUX_LAKES=LID22999
lake LID50270_rototoa PEST_LAKE_IDX=10
lake LID50269_kuwakatai PEST_DOWNSTREAM_LIDS=LID50269 PEST_LAKE_IDX=11
lake LID49294_whatihua PEST_LAKE_IDX=6
lake LID21871_tomarata PEST_LAKE_IDX=1 PEST_PIN_SEDFLUX_LAKES=LID21871
[ -f "$WF/LID22999_spectacle/baseline/manifest.json" ] && lake LID23000_slipper PEST_DOWNSTREAM_LIDS=LID23000 PEST_LAKE_IDX=11 PEST_PIN_SEDFLUX_LAKES=LID23000
log "================ fixC all lakes finished ================"
