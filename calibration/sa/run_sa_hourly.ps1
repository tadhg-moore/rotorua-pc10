# After the alum/no-alum daily SA: the same Morris design with hourly forcing, then with daily
# forcing on the same window (so forcing and window effects can be separated).
$r = "C:\Program Files\R\R-4.5.2\bin\Rscript.exe"
while (Get-CimInstance Win32_Process | Where-Object { $_.CommandLine -match 'run_sa_both' }) { Start-Sleep 60 }
$env:SA_R = "12"; $env:SA_NCORE = "10"; $env:SA_VARIANT = "alum"
$env:SA_START = "2012-07-01"; $env:SA_STOP = "2015-06-30"; $env:SA_SPINUP = "540"

Write-Host "$(Get-Date -Format s) hourly SA: build"
$env:SA_TAG = "_hourly"; $env:SA_BASE = "aeme_base.rds"
& $r calibration/sa/sa_smoke.R *> calibration\logs\sa_smoke_hourly.out
& $r calibration/sa/sa_morris.R *> calibration\logs\sa_morris_hourly.out
Write-Host "$(Get-Date -Format s) hourly SA done"

$env:SA_TAG = "_dailyw2"; $env:SA_BASE = "aeme_base_daily.rds"
& $r calibration/sa/sa_smoke.R *> calibration\logs\sa_smoke_dailyw2.out
& $r calibration/sa/sa_morris.R *> calibration\logs\sa_morris_dailyw2.out
Write-Host "$(Get-Date -Format s) daily same-window SA done"
