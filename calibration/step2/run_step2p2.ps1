# Step 2p2: joint alum + sediment P release refit on the V1 inflows, starting from the stage-r alum member, holding stage-o oxygen and q2 particulate parameters; wet-year TP terms on.
$r = "C:\Program Files\R\R-4.5.2\bin\Rscript.exe"
Write-Host "$(Get-Date -Format s) step 2p2 start"
$env:RENV_CONFIG_SYNCHRONIZED_CHECK = "FALSE"
$env:S2_STAGE = "p"; $env:S2_ALUM = "1"; $env:S2_ALP = "1"; $env:S2_TPTERMS = "1"; $env:S2_ALUM_WIDE = "1"
$env:S2_BASE = "calibration/alum/stream/base_V1.rds"; $env:S2_START = "2005-01-01"; $env:S2_STOP = "2022-06-29"
$env:S2_ALUM_FROM = "calibration/step2r/runs/step2_best.rds"; $env:S2_OVERLAY = "calibration/step2o/runs/step2_best.rds,calibration/step2q2/runs/step2_best.rds"
$env:S2_DIR = "calibration/step2p2/runs"
$env:S2_N = "48"; $env:S2_ITER = "4"; $env:S2_NCORE = "5"
& $r calibration/step2/step2_ies.R *> calibration\logs\step2p2_ies.out
Write-Host "$(Get-Date -Format s) step 2p2 done"
