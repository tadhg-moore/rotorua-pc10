#!/bin/bash
cd /c/Users/mooret/Git/rotorua-pc10 || exit 1
S=calibration/logs/pipeline.status; R="/c/Program Files/R/R-4.5.2/bin/Rscript.exe"; export RENV_CONFIG_SYNCHRONIZED_CHECK=FALSE
say() { echo "$(date +%H:%M) $*" >> $S; }
mkdir -p calibration/step2l2/runs; say "L2 started"
powershell.exe -NoProfile -ExecutionPolicy Bypass -File calibration/step2/run_step2l2.ps1 > calibration/logs/step2l2_driver.out 2>&1
say "L2 finished"; W="calibration/step2l2/runs/work"; rm -rf "${W:?}"
S2_BASE=calibration/step3/aeme_base_2005_alum.rds S2_L_FROM=calibration/step2o/runs/step2_best.rds FWD_STAGE=l FWD_TAG=2 FWD_SRC=calibration/step2l2/runs FWD_ALUM=1 FWD_START=2005-01-01 FWD_STOP=2022-06-29 "$R" calibration/step2/run_forward.R > calibration/logs/forward_l2.log 2>&1
say "L2 forward run done"
ANOX_RUN=l2 "$R" calibration/anoxia/01_bottom_anoxia.R > calibration/logs/anoxia_l2.log 2>&1
ALUM_RUN=l2 "$R" calibration/alum/01_alum_fit.R > calibration/logs/alumfit_l2.log 2>&1
FWD_STAGES="l2" "$R" calibration/step2/run_limitation.R > calibration/logs/limitation_l2.log 2>&1
LIGHT_STAGE=l2 "$R" calibration/light/02_model_light_check.R > calibration/logs/lightcheck_l2.log 2>&1
say "L2 diagnostics done"
