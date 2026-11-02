#!/bin/bash
cd /c/Users/mooret/Git/rotorua-pc10 || exit 1
S=calibration/logs/pipeline.status; R="/c/Program Files/R/R-4.5.2/bin/Rscript.exe"; export RENV_CONFIG_SYNCHRONIZED_CHECK=FALSE
say() { echo "$(date +%H:%M) $*" >> $S; }
mkdir -p calibration/step2o/runs; say "O started"
powershell.exe -NoProfile -ExecutionPolicy Bypass -File calibration/step2/run_step2o.ps1 > calibration/logs/step2o_driver.out 2>&1
say "O finished"; W="calibration/step2o/runs/work"; rm -rf "${W:?}"
S2_BASE=calibration/step3/aeme_base_2005_alum.rds S2_ALUM_FROM=calibration/step2alum/runs/step2_best.rds FWD_STAGE=o FWD_ALUM=1 FWD_START=2005-01-01 FWD_STOP=2022-06-29 "$R" calibration/step2/run_forward.R > calibration/logs/forward_o.log 2>&1
say "O forward run done"
ANOX_RUN=o "$R" calibration/anoxia/01_bottom_anoxia.R > calibration/logs/anoxia_o.log 2>&1
ALUM_RUN=o "$R" calibration/alum/01_alum_fit.R > calibration/logs/alumfit_o.log 2>&1
say "O diagnostics done"
