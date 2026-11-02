# Step 2j: joint stage. Phytoplankton P kinetics (12), oxygen (9), sediment P release (6) and nitrogen (13) parameters are freed together on the Q50 base,
# starting from the stage o3 member; objective = alum-stage objective plus wet-year TP terms plus anoxia / bottom-DO, annual TN and thermocline-depth terms (S2_JOINT).
#   powershell -File calibration\step2\run_step2j.ps1 [smoke]
param([string]$mode = "run")
Set-Location C:\Users\mooret\Git\rotorua-pc10
$r = "C:\Program Files\R\R-4.5.2\bin\Rscript.exe"
$env:RENV_CONFIG_SYNCHRONIZED_CHECK = "FALSE"
$env:S2_STAGE = "p"; $env:S2_JOINT = "1"; $env:S2_ALUM = "1"; $env:S2_ALP = "1"; $env:S2_TPTERMS = "1"
$env:S2_BASE = "calibration/alum/stream/base_Q50.rds"; $env:S2_START = "2005-01-01"; $env:S2_STOP = "2022-06-29"
$env:S2_ALUM_FROM = "calibration/step2o3/runs/step2_best.rds"; $env:S2_OVERLAY = "calibration/step2o/runs/step2_best.rds,calibration/step2q2/runs/step2_best.rds,calibration/step2o3/runs/step2_best.rds"
if ($mode -eq "smoke") {
  $env:S2_DIR = "calibration/step2j_smoke/runs"; New-Item -ItemType Directory -Force $env:S2_DIR | Out-Null
  & $r calibration/step2/step2_smoke.R *> calibration\logs\step2j_smoke.out
  Write-Host "smoke done"; exit
}
$env:S2_DIR = "calibration/step2j/runs"; New-Item -ItemType Directory -Force $env:S2_DIR | Out-Null
$env:S2_N = "100"; $env:S2_ITER = "6"; $env:S2_NCORE = "5"
& $r calibration/step2/step2_ies.R *> calibration\logs\step2j_ies.out
Write-Host "$(Get-Date -Format s) step 2j done"
