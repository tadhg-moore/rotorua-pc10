# Step 2O: bottom oxygen / anoxia, alum ON (round-1 alum parameters), 2g biology, light fixed. Cal 2005-2017.06, held out 2017.07-2022.06.
$r = "C:\Program Files\R\R-4.5.2\bin\Rscript.exe"
Write-Host "$(Get-Date -Format s) step 2O start"
$env:RENV_CONFIG_SYNCHRONIZED_CHECK = "FALSE"
$env:S2_STAGE = "o"; $env:S2_ALUM = "1"; $env:S2_BASE = "calibration/step3/aeme_base_2005_alum.rds"; $env:S2_START = "2005-01-01"; $env:S2_STOP = "2022-06-29"
$env:S2_ALUM_FROM = "calibration/step2alum/runs/step2_best.rds"; $env:S2_CALEND = "2017-06-30"
$env:S2_DIR = "calibration/step2o/runs"
$env:S2_N = "80"; $env:S2_ITER = "6"; $env:S2_NCORE = "6"
& $r calibration/step2/step2_ies.R *> calibration\logs\step2o_ies.out
Write-Host "$(Get-Date -Format s) step 2O done"
