# Step 2p4: continuation of p3 (phi still falling): same Q50 base, starting from the p3 member.
$r = "C:\Program Files\R\R-4.5.2\bin\Rscript.exe"
Write-Host "$(Get-Date -Format s) step 2p4 start"
$env:RENV_CONFIG_SYNCHRONIZED_CHECK = "FALSE"
$env:S2_STAGE = "p"; $env:S2_ALUM = "1"; $env:S2_ALP = "1"; $env:S2_TPTERMS = "1"; $env:S2_ALUM_WIDE = "1"
$env:S2_BASE = "calibration/alum/stream/base_Q50.rds"; $env:S2_START = "2005-01-01"; $env:S2_STOP = "2022-06-29"
$env:S2_ALUM_FROM = "calibration/step2p3/runs/step2_best.rds"; $env:S2_OVERLAY = "calibration/step2o/runs/step2_best.rds,calibration/step2q2/runs/step2_best.rds,calibration/step2p3/runs/step2_best.rds"
$env:S2_DIR = "calibration/step2p4/runs"
$env:S2_N = "48"; $env:S2_ITER = "4"; $env:S2_NCORE = "5"
& $r calibration/step2/step2_ies.R *> calibration\logs\step2p4_ies.out
Write-Host "$(Get-Date -Format s) step 2p4 done"
