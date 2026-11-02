# Waits for the joint stage, then evaluates its best member on the flood x0.5 base (run name J1).
Set-Location C:\Users\mooret\Git\rotorua-pc10
while (-not (Test-Path "calibration\step2j\runs\step2_best.rds")) { Start-Sleep 30 }
Start-Sleep 30
$r = "C:\Program Files\R\R-4.5.2\bin\Rscript.exe"
$env:RENV_CONFIG_SYNCHRONIZED_CHECK = "FALSE"; $env:STR_PARS = "calibration/step2j/runs/step2_best.rds"; $env:STR_KSCALE = "1"; $env:STR_FLOODSCALE = "0.5"
& $r calibration/alum/stream_variant.R J1 1 1 *> calibration\logs\eval_j1.out
