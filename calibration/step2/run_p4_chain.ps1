# Stage p4 then its evaluation (flood particulate x0.5 base rebuilt by stream_variant.R, name P4).
Set-Location C:\Users\mooret\Git\rotorua-pc10
& powershell -NoProfile -File calibration\step2\run_step2p4.ps1
$r = "C:\Program Files\R\R-4.5.2\bin\Rscript.exe"
$env:RENV_CONFIG_SYNCHRONIZED_CHECK = "FALSE"; $env:STR_PARS = "calibration/step2p4/runs/step2_best.rds"; $env:STR_KSCALE = "1"; $env:STR_FLOODSCALE = "0.5"
& $r calibration/alum/stream_variant.R P4 1 1 *> calibration\logs\eval_p4.out
