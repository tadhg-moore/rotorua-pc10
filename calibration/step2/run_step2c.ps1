# Step 2c: sediment FRP / NH4 fluxes anchored to the measured zone fluxes, kpo4p capped, FRP / NH4 weighted more. Starts from the 2b best.
$r = "C:\Program Files\R\R-4.5.2\bin\Rscript.exe"
Write-Host "$(Get-Date -Format s) step 2c start"
$env:RENV_CONFIG_SYNCHRONIZED_CHECK = "FALSE"
$env:S2_STAGE = "c"; $env:S2_DIR = "calibration/step2c/runs"
$env:S2_N = "80"; $env:S2_ITER = "6"; $env:S2_NCORE = "8"
& $r calibration/step2/step2_ies.R *> calibration\logs\step2c_ies.out
Write-Host "$(Get-Date -Format s) step 2c done"
