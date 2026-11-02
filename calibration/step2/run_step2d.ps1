# Step 2d: sediment NH4 / FRP releases kept within x4 of the 2b values; kpo4p and w_po4ads fixed (inert); 2b score weights. Starts from the 2b best.
$r = "C:\Program Files\R\R-4.5.2\bin\Rscript.exe"
Write-Host "$(Get-Date -Format s) step 2d start"
$env:RENV_CONFIG_SYNCHRONIZED_CHECK = "FALSE"
$env:S2_STAGE = "d"; $env:S2_DIR = "calibration/step2d/runs"
$env:S2_N = "80"; $env:S2_ITER = "6"; $env:S2_NCORE = "8"
& $r calibration/step2/step2_ies.R *> calibration\logs\step2d_ies.out
Write-Host "$(Get-Date -Format s) step 2d done"
