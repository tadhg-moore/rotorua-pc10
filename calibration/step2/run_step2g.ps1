# Step 2G: diatom parameters freed, silica supply 200 mmol/m3, seasonal-climatology terms in the score; starts from the 2f best member.
$r = "C:\Program Files\R\R-4.5.2\bin\Rscript.exe"
Write-Host "$(Get-Date -Format s) step 2G start"
$env:RENV_CONFIG_SYNCHRONIZED_CHECK = "FALSE"
$env:S2_STAGE = "g"; $env:S2_DIR = "calibration/step2g/runs"
$env:S2_N = "120"; $env:S2_ITER = "6"; $env:S2_NCORE = "8"
& $r calibration/step2/step2_ies.R *> calibration\logs\step2g_ies.out
Write-Host "$(Get-Date -Format s) step 2G done"
