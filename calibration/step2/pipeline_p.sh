#!/bin/bash
cd /c/Users/mooret/Git/rotorua-pc10 || exit 1
S=calibration/logs/pipeline.status; R="/c/Program Files/R/R-4.5.2/bin/Rscript.exe"; export RENV_CONFIG_SYNCHRONIZED_CHECK=FALSE
say() { echo "$(date +%H:%M) $*" >> $S; }
mkdir -p calibration/step2p/runs; say "P started"
powershell.exe -NoProfile -ExecutionPolicy Bypass -File calibration/step2/run_step2p.ps1 > calibration/logs/step2p_driver.out 2>&1
say "P finished"; W="calibration/step2p/runs/work"; rm -rf "${W:?}"
S2_BASE=calibration/step3/aeme_base_2005_alum.rds S2_ALUM_FROM=calibration/step2alum/runs/step2_best.rds FWD_STAGE=p FWD_ALUM=1 FWD_START=2005-01-01 FWD_STOP=2022-06-29 "$R" calibration/step2/run_forward.R > calibration/logs/forward_p.log 2>&1
say "P forward run done"
ANOX_RUN=p "$R" calibration/anoxia/01_bottom_anoxia.R > calibration/logs/anoxia_p.log 2>&1
ALUM_RUN=p "$R" calibration/alum/01_alum_fit.R > calibration/logs/alumfit_p.log 2>&1
say "P diagnostics done"
