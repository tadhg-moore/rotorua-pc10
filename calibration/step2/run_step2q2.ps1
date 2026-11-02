# Step 2q2: narrower particulate P stage (rpop_hydrol, w_po4ads, rdom_minerl; w_pom fixed at the stage-o value), V1 inflows.
$r = "C:\Program Files\R\R-4.5.2\bin\Rscript.exe"
Write-Host "$(Get-Date -Format s) step 2q2 start"
$env:RENV_CONFIG_SYNCHRONIZED_CHECK = "FALSE"
$env:S2_STAGE = "q"; $env:S2_Q2 = "1"; $env:S2_ALUM = "1"; $env:S2_ALP = "1"; $env:S2_BASE = "calibration/alum/stream/base_V1.rds"; $env:S2_START = "2005-01-01"; $env:S2_STOP = "2022-06-29"
$env:S2_ALUM_FROM = "calibration/step2alum/runs/step2_best.rds"; $env:S2_DIR = "calibration/step2q2/runs"; $env:S2_KSCALE = "0.85"
$env:S2_N = "48"; $env:S2_ITER = "4"; $env:S2_NCORE = "5"
& $r calibration/step2/step2_ies.R *> calibration\logs\step2q2_ies.out
Write-Host "$(Get-Date -Format s) step 2q2 done"
