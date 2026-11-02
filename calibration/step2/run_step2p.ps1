# Step 2P: joint FRP stage -- alum parameters + sediment P release (fsed_frp by zone, Ksed_frp, theta_sed_frp), alum ON, calibrated to FRP / TP / chl / Al. Starts from the 2O best member.
$r = "C:\Program Files\R\R-4.5.2\bin\Rscript.exe"
Write-Host "$(Get-Date -Format s) step 2P start"
$env:RENV_CONFIG_SYNCHRONIZED_CHECK = "FALSE"
$env:S2_STAGE = "p"; $env:S2_ALUM = "1"; $env:S2_BASE = "calibration/step3/aeme_base_2005_alum.rds"; $env:S2_START = "2005-01-01"; $env:S2_STOP = "2022-06-29"
$env:S2_ALUM_FROM = "calibration/step2alum/runs/step2_best.rds"; $env:S2_ALUM_WIDE = "1"
$env:S2_DIR = "calibration/step2p/runs"
$env:S2_N = "100"; $env:S2_ITER = "6"; $env:S2_NCORE = "6"
& $r calibration/step2/step2_ies.R *> calibration\logs\step2p_ies.out
Write-Host "$(Get-Date -Format s) step 2P done"
