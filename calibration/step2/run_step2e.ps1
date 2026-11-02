# Step 2e: sediment nitrate flux (Fsed_nit_model 2), Ksed_amm / Ksed_nit, capped rdenit, wider POM settling and hydrolysis ranges; P sorption left off. Starts from the 2b best.
$r = "C:\Program Files\R\R-4.5.2\bin\Rscript.exe"
Write-Host "$(Get-Date -Format s) step 2e start"
$env:RENV_CONFIG_SYNCHRONIZED_CHECK = "FALSE"
$env:S2_STAGE = "e"; $env:S2_DIR = "calibration/step2e/runs"
$env:S2_N = "120"; $env:S2_ITER = "6"; $env:S2_NCORE = "8"
& $r calibration/step2/step2_ies.R *> calibration\logs\step2e_ies.out
Write-Host "$(Get-Date -Format s) step 2e done"
