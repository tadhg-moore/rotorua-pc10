# Alum round 2: wider alum parameter ranges, start from round 1, calibrate on 2006-06..2020-06 (hold out 2020-07..2022-06 -- the observed TP/FRP rebound after the 2019 gap in Puarenga dosing).
$r = "C:\Program Files\R\R-4.5.2\bin\Rscript.exe"
Write-Host "$(Get-Date -Format s) step 2alum2 start"
$env:RENV_CONFIG_SYNCHRONIZED_CHECK = "FALSE"
$env:S2_STAGE = "alum"; $env:S2_ALUM = "1"; $env:S2_BASE = "calibration/step3/aeme_base_2005_alum.rds"; $env:S2_START = "2005-01-01"; $env:S2_STOP = "2022-06-29"
$env:S2_ALUM_FROM = "calibration/step2alum/runs/step2_best.rds"; $env:S2_ALUM_WIDE = "1"; $env:S2_CALEND3 = "2020-06-30"
$env:S2_DIR = "calibration/step2alum2/runs"
$env:S2_N = "80"; $env:S2_ITER = "6"; $env:S2_NCORE = "6"
& $r calibration/step2/step2_ies.R *> calibration\logs\step2alum2_ies.out
Write-Host "$(Get-Date -Format s) step 2alum2 done"
