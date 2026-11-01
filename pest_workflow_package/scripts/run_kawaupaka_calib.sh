#!/bin/bash
# Overnight chain: wait for fixA/fixB, pick the recipe (R/pest_workflow/choose_recipe.R), then
#   Kuwakatai -> Spectacle (measured fluxes FIXED) -> Slipper: calibrate -> posterior -> BASELINE ensemble -> sanity.
# Scenarios are deliberately NOT run (they need Rototoa/Spectacle scenario outflows in the chosen root).
# Everything lands in pest_workflow_<A|B> / checkpoints_<A|B> (same roots as the fix runs, so Rototoa's
# outflow for Kuwakatai is already there). Progress: logs/chain2/STATUS.txt
cd "C:/Users/data/Dropbox/AC/modelling/phase2/ac_lake_modelling" || exit 1
R="C:/Program Files/R/R-4.6.1/bin/x64/Rscript.exe"
LOGD=logs/chain2; mkdir -p $LOGD; STATUS=$LOGD/STATUS.txt
LIBROOT="C:/Users/data/Desktop/ac_lake_calib/renv_lib_regtr_root"
log() { echo "[$(date '+%a %d %H:%M:%S')] $*" | tee -a "$STATUS"; }
free_gb() { powershell -NoProfile -Command "[int][math]::Floor((Get-PSDrive C).Free/1GB)" | tr -d '\r'; }
wait_disk() { local n=0; while [ "$(free_gb)" -lt "$1" ]; do [ $n -ge 72 ] && return 1; [ $n -eq 0 ] && log "waiting for $1 GB (now $(free_gb))"; n=$((n+1)); sleep 300; done; }

log "waiting for the fix runs to finish"
until grep -q "freeze-fix rerun FINISHED" logs/freezefix/STATUS.txt; do sleep 120; done
REC=$("$R" R/pest_workflow/choose_recipe.R 2> "$LOGD/choose.log" | tail -1 | tr -d '\r')
[ "$REC" = "A" ] || [ "$REC" = "B" ] || REC=A
log "recipe chosen: $REC ($(cat $LOGD/choose.log | tr '\n' ' '))"
V=fix$REC
WF=pest_workflow_$V; CK=staged-calibration-v4/checkpoints_$V; LOC="C:/Users/data/Desktop/ac_lake_calib/pest_workflow_$V"
EXTRA=(); [ "$REC" = "B" ] && EXTRA=(PEST_REGION_TRANSFORM="PHY_tchla=log1p,PHY_cyano=log1p")
[ -f "$WF/LID50270_rototoa/outflow/baseline_outflow.csv" ] || { log "ABORT: no Rototoa baseline outflow in $WF"; exit 1; }
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

# Kawaupaka (bloom lake) under the current recipe: log1p chl/cyano targets, forced solids stage 3, freeze_dropped.
# Stages 1-2 reused from the earlier calibration (physics, independent of the changes); stages 3-4 fresh.
for s in s1 s2; do cp -n "staged-calibration-v4/checkpoints/${s}_LID45818_kawaupaka.rds" "$CK/" 2>/dev/null; done
RESTART_DIR=""
calibrate LID45818_kawaupaka PEST_LAKE_IDX=3 && post LID45818_kawaupaka || log "Kawaupaka FAILED"
log "================ Kawaupaka finished ================"
