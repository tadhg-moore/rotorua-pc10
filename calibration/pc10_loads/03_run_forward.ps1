# Runs the old and new forward runs in parallel (GLM-AED, 1976-01-01 start; old stops 2022-06-29, new 2025-12-31).
# Each writes fwd_<tag>/output/output.nc (about 7-8 GB), lake.csv, WQ_2.csv, mass_balance.csv; stdout/stderr go to fwd_<tag>/glm.log / glm.err.
# Build the folders first: Rscript calibration/pc10_loads/02_build_forward.R old ; Rscript calibration/pc10_loads/02_build_forward.R new
Set-Location C:\Users\mooret\Git\rotorua-pc10
$exe = (Resolve-Path "exe\alum-diag-aed-v3.0.1").Path
$env:PATH = "$exe;$env:PATH"
$procs = foreach ($tag in "old", "new") {
  $d = (Resolve-Path "calibration\pc10_loads\fwd_$tag").Path
  Start-Process "$exe\glm.exe" -ArgumentList "glm4.nml" -WorkingDirectory $d -RedirectStandardOutput "$d\glm.log" -RedirectStandardError "$d\glm.err" -WindowStyle Hidden -PassThru
}
$procs | Wait-Process
