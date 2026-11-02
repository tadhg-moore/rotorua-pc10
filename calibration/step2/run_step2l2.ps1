# Light round 2: re-fit the light / solids parameters on the final biology (2O member), alum ON, 2005-2022 (cal to 2015-12, held out 2016-2022).
$r = "C:\Program Files\R\R-4.5.2\bin\Rscript.exe"
Write-Host "$(Get-Date -Format s) step 2L2 start"
$env:RENV_CONFIG_SYNCHRONIZED_CHECK = "FALSE"
$env:S2_STAGE = "l"; $env:S2_ALUM = "1"; $env:S2_BASE = "calibration/step3/aeme_base_2005_alum.rds"; $env:S2_START = "2005-01-01"; $env:S2_STOP = "2022-06-29"
$env:S2_L_FROM = "calibration/step2o/runs/step2_best.rds"; $env:S2_CALEND = "2015-12-31"
$env:S2_DIR = "calibration/step2l2/runs"
$env:S2_N = "80"; $env:S2_ITER = "6"; $env:S2_NCORE = "6"
& $r calibration/step2/step2_ies.R *> calibration\logs\step2l2_ies.out
Write-Host "$(Get-Date -Format s) step 2L2 done"
