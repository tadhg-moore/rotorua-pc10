#!/bin/bash
# Unattended chain: wait for 2L -> post-2L diagnostics -> smoke-check 2F -> run 2F -> post-2F diagnostics. Progress lines go to calibration/logs/pipeline.status.
cd /c/Users/mooret/Git/rotorua-pc10 || exit 1
S=calibration/logs/pipeline.status; R="/c/Program Files/R/R-4.5.2/bin/Rscript.exe"; export RENV_CONFIG_SYNCHRONIZED_CHECK=FALSE
say() { echo "$(date +%H:%M) $*" >> $S; }
say "pipeline started; waiting for 2L"
until grep -q "step 2L done" calibration/logs/step2l_driver.out 2>/dev/null; do sleep 60; done
say "2L finished"; W="calibration/step2l/runs/work"; rm -rf "${W:?}"
bash calibration/step2/post_stage.sh l >> $S 2>&1; say "post-2L diagnostics done"
S2_STAGE=f S2_DIR=calibration/step2f/smoke "$R" -e 'source("calibration/step2/step2_common.R"); b <- step2_build("sm"); t <- step2_table(b); cat("F_FREE", sum(t$min != t$max), "\n"); print(t[t$min != t$max, c("name","group","index","value","min","max")], row.names = FALSE)' > calibration/logs/step2f_smoke.log 2>&1
if ! grep -q "F_FREE" calibration/logs/step2f_smoke.log; then say "2F smoke check FAILED - stopping"; exit 1; fi
say "2F smoke ok: $(grep F_FREE calibration/logs/step2f_smoke.log)"; rm -rf calibration/step2f/smoke
mkdir -p calibration/step2f/runs; powershell.exe -NoProfile -ExecutionPolicy Bypass -File calibration/step2/run_step2f.ps1 > calibration/logs/step2f_driver.out 2>&1
say "2F finished"; W="calibration/step2f/runs/work"; rm -rf "${W:?}"
bash calibration/step2/post_stage.sh f >> $S 2>&1; say "post-2F diagnostics done"
