# Oxygen round 2: re-tune the oxygen / anoxia parameters on top of the light-round-2 member (alum ON).
$r = "C:\Program Files\R\R-4.5.2\bin\Rscript.exe"
Write-Host "$(Get-Date -Format s) step 2O2 start"
$env:RENV_CONFIG_SYNCHRONIZED_CHECK = "FALSE"
$env:S2_STAGE = "o"; $env:S2_ALUM = "1"; $env:S2_BASE = "calibration/step3/aeme_base_2005_alum.rds"; $env:S2_START = "2005-01-01"; $env:S2_STOP = "2022-06-29"
$env:S2_ALUM_FROM = "calibration/step2alum/runs/step2_best.rds"; $env:S2_O_FROM = "calibration/step2l2/runs/step2_best.rds"; $env:S2_CALEND = "2017-06-30"
$env:S2_DIR = "calibration/step2o2/runs"
$env:S2_N = "60"; $env:S2_ITER = "5"; $env:S2_NCORE = "6"
& $r calibration/step2/step2_ies.R *> calibration\logs\step2o2_ies.out
Write-Host "$(Get-Date -Format s) step 2O2 done"
