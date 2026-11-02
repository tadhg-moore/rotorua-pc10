# Kills glm.exe runs that have been alive for more than 10 minutes (a normal 17-year run takes about 2 minutes; a hung run stalls the whole ensemble iteration).
while ($true) {
  Get-Process glm -ErrorAction SilentlyContinue | Where-Object { ((Get-Date) - $_.StartTime).TotalMinutes -gt 10 } | ForEach-Object {
    Add-Content C:\Users\mooret\Git\rotorua-pc10\calibration\logs\glm_watchdog.log "$(Get-Date -Format s) killed glm pid $($_.Id) cpu $([math]::Round($_.CPU))s"
    Stop-Process -Id $_.Id -Force }
  Start-Sleep 30
}
