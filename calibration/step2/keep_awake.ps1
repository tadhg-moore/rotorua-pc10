# Holds a "system required" execution state while the calibration R workers exist, so the computer does not idle into standby mid-run (ends when the workers are gone; no system setting is changed).
Add-Type -Namespace W -Name P -MemberDefinition '[System.Runtime.InteropServices.DllImport("kernel32.dll")] public static extern uint SetThreadExecutionState(uint f);'
[void][W.P]::SetThreadExecutionState(0x80000001)   # ES_CONTINUOUS | ES_SYSTEM_REQUIRED
while (@(Get-Process Rscript -ErrorAction SilentlyContinue).Count -gt 0) { [void][W.P]::SetThreadExecutionState(0x80000001); Start-Sleep 30 }
