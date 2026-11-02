# Three alum refits one after the other (RAM allows one ensemble at a time): H1b with beta 1, H1b with the thesis lake ratio (beta 1.4286), and the previous rule (Q50) as the reference refit.
Set-Location C:\Users\mooret\Git\rotorua-pc10
& powershell -NoProfile -ExecutionPolicy Bypass -File calibration\step2\run_step2h1.ps1 -tag H1b -base calibration/alum/stream/base_H1b.rds -beta 1
& powershell -NoProfile -ExecutionPolicy Bypass -File calibration\step2\run_step2h1.ps1 -tag H1b_B143 -base calibration/alum/stream/base_H1b_B143.rds -beta 1.4286
& powershell -NoProfile -ExecutionPolicy Bypass -File calibration\step2\run_step2h1.ps1 -tag H0 -base calibration/alum/stream/base_Q50.rds -beta 1
