Add-Type @"
using System;
using System.Runtime.InteropServices;
public class SleepUtil{
    [DllImport("kernel32.dll", CharSet=CharSet.Auto, SetLastError=true)]
    public static extern uint SetThreadExecutionState(uint esFlags);

    [DllImport("user32.dll")]
    public static extern void keybd_event(byte bVk, byte bScan, uint dwFlags, UIntPtr dwExtraInfo);
}
"@

$ES_CONTINUOUS=[Convert]::ToUInt32("80000000",16)
$ES_SYSTEM_REQUIRED=[uint32]1
$ES_DISPLAY_REQUIRED=[uint32]2

$VK_F15=[byte]0x7E          # F15 - no default binding in Windows or Teams
$KEYEVENTF_KEYUP=[uint32]2
$nudgeEverySeconds=120      # Teams goes Away after ~5 min idle; 2 min keeps it Available

Write-Host "System will stay awake until you close this window...." -ForegroundColor Green
Write-Host ""

$startTime=Get-Date
$lastNudge=Get-Date

try {
while ($true) {

    [SleepUtil]::SetThreadExecutionState($ES_CONTINUOUS -bor $ES_SYSTEM_REQUIRED -bor $ES_DISPLAY_REQUIRED) | Out-Null

    # Synthetic F15 tap so Windows/Teams register user activity
    if (((Get-Date)-$lastNudge).TotalSeconds -ge $nudgeEverySeconds) {
        [SleepUtil]::keybd_event($VK_F15,0,0,[UIntPtr]::Zero)
        [SleepUtil]::keybd_event($VK_F15,0,$KEYEVENTF_KEYUP,[UIntPtr]::Zero)
        $lastNudge=Get-Date
    }

    $elapsed=(Get-Date)-$startTime
    $elapsedFormatted="{0}d {1:D2}h {2:D2}m {3:D2}s" -f $elapsed.Days, $elapsed.Hours, $elapsed.Minutes, $elapsed.Seconds

    Write-Host ("`rUptime: " + $elapsedFormatted + "   ") -NoNewline -ForegroundColor Cyan
    Start-Sleep -Seconds 1
}
} finally {
    [SleepUtil]::SetThreadExecutionState($ES_CONTINUOUS) | Out-Null
    Write-Host "`nSleep setting restored." -ForegroundColor Yellow
}
