# Step 2o3: oxygen refit on the Q50 base after the phytoplankton stage s (productivity rose, anoxic days 287 vs 239 observed): sediment oxygen demand and organic-matter oxygen consumption freed, everything else held at the stage s member.
Set-Location C:\Users\mooret\Git\rotorua-pc10
$r = "C:\Program Files\R\R-4.5.2\bin\Rscript.exe"
Write-Host "$(Get-Date -Format s) step 2o3 start"
$env:RENV_CONFIG_SYNCHRONIZED_CHECK = "FALSE"
$env:S2_STAGE = "o"; $env:S2_ALUM = "1"; $env:S2_ALP = "1"
$env:S2_BASE = "calibration/alum/stream/base_Q50.rds"; $env:S2_START = "2005-01-01"; $env:S2_STOP = "2022-06-29"
$env:S2_CALEND = "2017-06-30"; $env:S2_ALUM_FROM = "calibration/step2s/runs/step2_best.rds"; $env:S2_OVERLAY = "calibration/step2o/runs/step2_best.rds,calibration/step2q2/runs/step2_best.rds,calibration/step2s/runs/step2_best.rds"
$env:S2_DIR = "calibration/step2o3/runs"
$env:S2_N = "48"; $env:S2_ITER = "4"; $env:S2_NCORE = "5"
& $r calibration/step2/step2_ies.R *> calibration\logs\step2o3_ies.out
Write-Host "$(Get-Date -Format s) step 2o3 done"
$env:STR_PARS = "calibration/step2o3/runs/step2_best.rds"; $env:STR_KSCALE = "1"; $env:STR_FLOODSCALE = "0.5"
& $r calibration/alum/stream_variant.R O3 1 1 *> calibration\logs\eval_o3.out
