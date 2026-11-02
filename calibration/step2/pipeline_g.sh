#!/bin/bash
cd /c/Users/mooret/Git/rotorua-pc10 || exit 1
S=calibration/logs/pipeline.status; say() { echo "$(date +%H:%M) $*" >> $S; }
mkdir -p calibration/step2g/runs; say "2G started"
powershell.exe -NoProfile -ExecutionPolicy Bypass -File calibration/step2/run_step2g.ps1 > calibration/logs/step2g_driver.out 2>&1
say "2G finished"; W="calibration/step2g/runs/work"; rm -rf "${W:?}"
bash calibration/step2/post_stage.sh g >> $S 2>&1; say "post-2G diagnostics done"
