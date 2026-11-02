# Wait for the SA chain (run_sa_hourly.ps1) then run step 2.
$r = "C:\Program Files\R\R-4.5.2\bin\Rscript.exe"
while (Get-CimInstance Win32_Process | Where-Object { $_.CommandLine -match 'run_sa_hourly|run_sa_both' }) { Start-Sleep 60 }
Write-Host "$(Get-Date -Format s) step 2 start"
$env:S2_N = "80"; $env:S2_ITER = "6"; $env:S2_NCORE = "10"
& $r calibration/step2/step2_ies.R *> calibration\logs\step2_ies.out
Write-Host "$(Get-Date -Format s) step 2 done"
