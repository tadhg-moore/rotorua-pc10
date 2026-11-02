# Step 2s: phytoplankton P kinetics (K_P, X_pmin, X_pmax, R_resp of the three groups) on the Q50 base, starting from the p4 member; alum, sediment P, particulate and oxygen parameters fixed.
Set-Location C:\Users\mooret\Git\rotorua-pc10
$r = "C:\Program Files\R\R-4.5.2\bin\Rscript.exe"
Write-Host "$(Get-Date -Format s) step 2s start"
$env:RENV_CONFIG_SYNCHRONIZED_CHECK = "FALSE"
$env:S2_STAGE = "p"; $env:S2_PHYTO = "1"; $env:S2_ALUM = "1"; $env:S2_ALP = "1"; $env:S2_TPTERMS = "1"
$env:S2_BASE = "calibration/alum/stream/base_Q50.rds"; $env:S2_START = "2005-01-01"; $env:S2_STOP = "2022-06-29"
$env:S2_ALUM_FROM = "calibration/step2p4/runs/step2_best.rds"; $env:S2_OVERLAY = "calibration/step2o/runs/step2_best.rds,calibration/step2q2/runs/step2_best.rds,calibration/step2p4/runs/step2_best.rds"
$env:S2_DIR = "calibration/step2s/runs"
$env:S2_N = "48"; $env:S2_ITER = "4"; $env:S2_NCORE = "5"
& $r calibration/step2/step2_ies.R *> calibration\logs\step2s_ies.out
Write-Host "$(Get-Date -Format s) step 2s done"
$env:STR_PARS = "calibration/step2s/runs/step2_best.rds"; $env:STR_KSCALE = "1"; $env:STR_FLOODSCALE = "0.5"
& $r calibration/alum/stream_variant.R S1 1 1 *> calibration\logs\eval_s1.out
