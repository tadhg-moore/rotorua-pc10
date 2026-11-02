# Step 2F: phytoplankton, nutrients and sediment fluxes recalibrated under the corrected light (light parameters fixed at the 2L best member).
$r = "C:\Program Files\R\R-4.5.2\bin\Rscript.exe"
Write-Host "$(Get-Date -Format s) step 2F start"
$env:RENV_CONFIG_SYNCHRONIZED_CHECK = "FALSE"
$env:S2_STAGE = "f"; $env:S2_DIR = "calibration/step2f/runs"
$env:S2_N = "120"; $env:S2_ITER = "6"; $env:S2_NCORE = "8"
& $r calibration/step2/step2_ies.R *> calibration\logs\step2f_ies.out
Write-Host "$(Get-Date -Format s) step 2F done"
