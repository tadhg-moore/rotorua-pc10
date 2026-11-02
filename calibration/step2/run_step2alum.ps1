# Step 2alum: alum module ON, 8 alum parameters calibrated to FRP / TP / chlorophyll / aluminium over 2005-2022 on the 2g biology.
$r = "C:\Program Files\R\R-4.5.2\bin\Rscript.exe"
Write-Host "$(Get-Date -Format s) step 2alum start"
$env:RENV_CONFIG_SYNCHRONIZED_CHECK = "FALSE"
$env:S2_STAGE = "alum"; $env:S2_ALUM = "1"; $env:S2_BASE = "calibration/step3/aeme_base_2005_alum.rds"; $env:S2_START = "2005-01-01"; $env:S2_STOP = "2022-06-29"
$env:S2_DIR = "calibration/step2alum/runs"
$env:S2_N = "80"; $env:S2_ITER = "6"; $env:S2_NCORE = "6"
& $r calibration/step2/step2_ies.R *> calibration\logs\step2alum_ies.out
Write-Host "$(Get-Date -Format s) step 2alum done"
