# Step 2r: alum refit on the V1 inflows (in-stream bound P, no-alum files) with the stage-o and stage-q2 parameters held; alum objective plus wet-year TP terms.
$r = "C:\Program Files\R\R-4.5.2\bin\Rscript.exe"
Write-Host "$(Get-Date -Format s) step 2r start"
$env:RENV_CONFIG_SYNCHRONIZED_CHECK = "FALSE"
$env:S2_STAGE = "alum"; $env:S2_ALUM = "1"; $env:S2_ALP = "1"; $env:S2_TPTERMS = "1"; $env:S2_ALUM_WIDE = "1"
$env:S2_BASE = "calibration/alum/stream/base_V1.rds"; $env:S2_START = "2005-01-01"; $env:S2_STOP = "2022-06-29"
$env:S2_ALUM_FROM = "calibration/step2alum/runs/step2_best.rds"; $env:S2_OVERLAY = "calibration/step2o/runs/step2_best.rds,calibration/step2q2/runs/step2_best.rds"
$env:S2_DIR = "calibration/step2r/runs"
$env:S2_N = "48"; $env:S2_ITER = "4"; $env:S2_NCORE = "5"
& $r calibration/step2/step2_ies.R *> calibration\logs\step2r_ies.out
Write-Host "$(Get-Date -Format s) step 2r done"
