#!/bin/bash
# After a calibration stage: 20-year forward run, N / P budget vs 2b, growth limitation, and the light check.
#   bash calibration/step2/post_stage.sh <stage>      (stage = l, f, ...)
cd /c/Users/mooret/Git/rotorua-pc10 || exit 1
s=$1; R="/c/Program Files/R/R-4.5.2/bin/Rscript.exe"; export RENV_CONFIG_SYNCHRONIZED_CHECK=FALSE
rm -rf "calibration/step2/forward/${s:?}"
FWD_STAGE=$s "$R" calibration/step2/run_forward.R > calibration/logs/forward_$s.log 2>&1
FWD_STAGES="b,$s" "$R" calibration/step2/budget_process.R > calibration/logs/budget_$s.log 2>&1
FWD_STAGES="$s" "$R" calibration/step2/run_limitation.R > calibration/logs/limitation_$s.log 2>&1
LIGHT_STAGE=$s "$R" calibration/light/02_model_light_check.R > calibration/logs/lightcheck_$s.log 2>&1
echo "post_stage $s done"
