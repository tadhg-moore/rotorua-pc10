#!/bin/bash
cd /c/Users/mooret/Git/rotorua-pc10 || exit 1
S=calibration/logs/pipeline.status; R="/c/Program Files/R/R-4.5.2/bin/Rscript.exe"; export RENV_CONFIG_SYNCHRONIZED_CHECK=FALSE
say() { echo "$(date +%H:%M) $*" >> $S; }
mkdir -p calibration/step2alum2/runs; say "alum2 started"
powershell.exe -NoProfile -ExecutionPolicy Bypass -File calibration/step2/run_step2alum2.ps1 > calibration/logs/step2alum2_driver.out 2>&1
say "alum2 finished"; W="calibration/step2alum2/runs/work"; rm -rf "${W:?}"
S2_BASE=calibration/step3/aeme_base_2005_alum.rds FWD_STAGE=alum2 FWD_S2STAGE=alum FWD_SRC=calibration/step2alum2/runs FWD_ALUM=1 FWD_START=2005-01-01 FWD_STOP=2022-06-29 "$R" calibration/step2/run_forward.R > calibration/logs/forward_alum2.log 2>&1
say "alum2 forward run done"
