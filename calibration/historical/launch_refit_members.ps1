# Best member of each lake alum refit (calibration/step2h1_<tag>), run over 2005 to 2022 with its own stream base, one after the other.
$rs = "C:\Program Files\R\R-4.5.2\bin\Rscript.exe"
$runs = @(@("H0","calibration/alum/stream/base_Q50.rds"), @("H1b","calibration/alum/stream/base_H1b.rds"), @("H1b_B143","calibration/alum/stream/base_H1b_B143.rds"))
foreach ($run in $runs) {
  $t = $run[0]
  $env:HIST_SRC = "calibration/step2h1_$t/runs"; $env:HIST_BASE = $run[1]; $env:HIST_N = "1"; $env:HIST_TAG = "refit_$t"; $env:HIST_KEEP = "1"; $env:HIST_PARQUET = "0"
  "$(Get-Date -Format s) refit member $t start" | Add-Content calibration\logs\refit_members.out
  & $rs calibration/historical/run_hist.R 0 1 *>> "calibration\logs\refit_member_$t.out"
  "$(Get-Date -Format s) refit member $t done" | Add-Content calibration\logs\refit_members.out
}
