# Alum refit under a stream-step hypothesis: the joint-stage objective, with only the alum (8) and sediment P (6) parameters free, starting from the joint-stage best member (member 46).
#   powershell -File calibration\step2\run_step2h1.ps1 -tag H1b -base calibration/alum/stream/base_H1b.rds [-beta 1] [-mode smoke]
# beta: mol P bound per mol Al (capacity = beta x Al dose in the base, calibration/alum/63_stoich_base.R); it only scales the aluminium used in the aluminium targets.
param([string]$tag = "H1b", [string]$base = "calibration/alum/stream/base_H1b.rds", [string]$beta = "1", [string]$mode = "run")
Set-Location C:\Users\mooret\Git\rotorua-pc10
$r = "C:\Program Files\R\R-4.5.2\bin\Rscript.exe"
$env:RENV_CONFIG_SYNCHRONIZED_CHECK = "FALSE"
$env:S2_STAGE = "p"; $env:S2_JOINT = "1"; $env:S2_JOINT_ALUMFIT = "1"; $env:S2_ALUM = "1"; $env:S2_ALP = "1"; $env:S2_TPTERMS = "1"; $env:S2_ALUM_WIDE = "1"; $env:S2_BETA = $beta
$env:S2_BASE = $base; $env:S2_START = "2005-01-01"; $env:S2_STOP = "2022-06-29"
$env:S2_ALUM_FROM = "calibration/step2j/runs/step2_best.rds"; $env:S2_OVERLAY = "calibration/step2j/runs/step2_best.rds"
if ($mode -eq "smoke") {
  $env:S2_DIR = "calibration/step2h1_smoke_$tag/runs"; New-Item -ItemType Directory -Force $env:S2_DIR | Out-Null
  & $r calibration/step2/step2_smoke.R *> calibration\logs\step2h1_smoke_$tag.out
  Write-Host "smoke done"; exit
}
$env:S2_DIR = "calibration/step2h1_$tag/runs"; New-Item -ItemType Directory -Force $env:S2_DIR | Out-Null
$env:S2_N = "48"; $env:S2_ITER = "4"; $env:S2_NCORE = "5"
Write-Host "$(Get-Date -Format s) step 2h1 $tag start"
& $r calibration/step2/step2_ies.R *> calibration\logs\step2h1_$tag.out
Write-Host "$(Get-Date -Format s) step 2h1 $tag done"
