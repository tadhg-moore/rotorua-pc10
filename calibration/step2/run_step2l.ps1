# Step 2L (light): resuspension / settling / extinction parameters against Kd and turbidity; biology fixed at the 2e best member.
$r = "C:\Program Files\R\R-4.5.2\bin\Rscript.exe"
Write-Host "$(Get-Date -Format s) step 2L start"
$env:RENV_CONFIG_SYNCHRONIZED_CHECK = "FALSE"
$env:S2_STAGE = "l"; $env:S2_DIR = "calibration/step2l/runs"
$env:S2_N = "80"; $env:S2_ITER = "6"; $env:S2_NCORE = "8"
& $r calibration/step2/step2_ies.R *> calibration\logs\step2l_ies.out
Write-Host "$(Get-Date -Format s) step 2L done"
