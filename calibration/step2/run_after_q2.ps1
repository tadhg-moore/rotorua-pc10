# Waits for stage q2, then evaluates its best member through the V1 stream variant (name Q2).
$r = "C:\Program Files\R\R-4.5.2\bin\Rscript.exe"
$env:RENV_CONFIG_SYNCHRONIZED_CHECK = "FALSE"
while (-not (Test-Path "calibration\step2q2\runs\step2_best.rds")) { Start-Sleep 60 }
Start-Sleep 30
$env:STR_PARS = "calibration/step2q2/runs/step2_best.rds"; $env:STR_KSCALE = "1"
& $r calibration/alum/stream_variant.R Q2 1 1 *> calibration\logs\eval_q2.out
Write-Host "$(Get-Date -Format s) q2 eval done"
