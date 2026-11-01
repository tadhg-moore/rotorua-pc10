# Morris SA with alum, then without (same design). Run from the project root.
$r = "C:\Program Files\R\R-4.5.2\bin\Rscript.exe"
$env:SA_R = "12"; $env:SA_NCORE = "10"
Write-Host "$(Get-Date -Format s) SA alum start"
& $r calibration/sa/sa_morris.R *> calibration\logs\sa_morris_alum.out
Write-Host "$(Get-Date -Format s) SA alum done; building no-alum"
$env:SA_VARIANT = "noalum"
& $r calibration/sa/sa_smoke.R *> calibration\logs\sa_smoke_noalum.out
& $r calibration/sa/sa_morris.R *> calibration\logs\sa_morris_noalum.out
Write-Host "$(Get-Date -Format s) SA noalum done"
