@echo off
chcp 65001 > nul
setlocal
pushd %~dp0

set "PS_HELPER=%~dp0MeasureBuild.ps1"
if exist "%PS_HELPER%" del "%PS_HELPER%"

if exist summary.log del summary.log

:: ---------------------------------------------------------
:: Create temporary PowerShell helper script for memory monitoring
:: ---------------------------------------------------------
powershell -NoProfile -Command "$c = @(); $c += 'param('; $c += '    [string]$BuildCommand'; $c += ')'; $c += ''; $c += '$processInfo = New-Object System.Diagnostics.ProcessStartInfo'; $c += '$processInfo.FileName = ''cmd.exe'''; $c += '$processInfo.Arguments = ''/c '' + $BuildCommand'; $c += '$processInfo.UseShellExecute = $false'; $c += '$processInfo.RedirectStandardOutput = $false'; $c += '$processInfo.RedirectStandardError = $false'; $c += ''; $c += '$proc = [System.Diagnostics.Process]::Start($processInfo)'; $c += ''; $c += '$peakMem = 0'; $c += ''; $c += 'while (-not $proc.HasExited) {'; $c += '    $dccs = Get-Process dcc32 -ErrorAction SilentlyContinue'; $c += '    foreach ($d in $dccs) {'; $c += '        try {'; $c += '            $m = $d.PeakWorkingSet64'; $c += '            if ($m -gt $peakMem) { $peakMem = $m }'; $c += '        } catch {}'; $c += '    }'; $c += '    Start-Sleep -Milliseconds 100'; $c += '}'; $c += ''; $c += '$dccs = Get-Process dcc32 -ErrorAction SilentlyContinue'; $c += 'foreach ($d in $dccs) {'; $c += '    try {'; $c += '        if ($d.PeakWorkingSet64 -gt $peakMem) { $peakMem = $d.PeakWorkingSet64 }'; $c += '    } catch {}'; $c += '}'; $c += ''; $c += 'if ($peakMem -gt 0) {'; $c += '    $memMB = ''{0:N2} MB'' -f ($peakMem / 1MB)'; $c += '    Write-Host ''  Max Compiler Memory: '' $memMB'; $c += '} else {'; $c += '    Write-Host ''  Max Compiler Memory: Unknown (dcc32 not detected)'''; $c += '}'; $c += ''; $c += 'exit $proc.ExitCode'; $c | Set-Content -Path '%PS_HELPER%' -Encoding UTF8"

echo. 

echo ==============================================================================
echo  BENCHMARK: StaticGenerics vs Delphi vs Spring vs mORMot
echo ==============================================================================
echo.

echo [1/4] Compiling StaticGenerics...
cd StaticGenerics
powershell -NoProfile -ExecutionPolicy Bypass -File "%PS_HELPER%" "..\..\..\_BuildBase.bat \"Benchmark_StaticGenerics.dproj\"" > ..\build_static.log
set BUILD_ERROR=%ERRORLEVEL%
timeout /t 1 /nobreak > nul
if %BUILD_ERROR% neq 0 (
    echo BUILD FAILED! See build_static.log
    type ..\build_static.log
) else (
    echo. >> ..\summary.log
    echo [StaticGenerics] >> ..\summary.log
    findstr /C:"Embarcadero Delphi for" /C:"Copyright (c)" /C:"Zeilen," /C:"lines," /C:"Max Compiler Memory" ..\build_static.log >> ..\summary.log
    findstr /C:"Embarcadero Delphi for" /C:"Copyright (c)" /C:"Zeilen," /C:"lines," /C:"Max Compiler Memory" ..\build_static.log
)
cd ..

echo. 

echo [2/4] Compiling mORMotGenerics...
cd mORMotGenerics
powershell -NoProfile -ExecutionPolicy Bypass -File "%PS_HELPER%" "..\..\..\_BuildBase.bat \"Benchmark_mORMotGenerics.dproj\"" > ..\build_mormot.log
set BUILD_ERROR=%ERRORLEVEL%
timeout /t 1 /nobreak > nul
if %BUILD_ERROR% neq 0 (
    echo BUILD FAILED! See build_mormot.log
    type ..\build_mormot.log
) else (
    echo. >> ..\summary.log
    echo [mORMotGenerics] >> ..\summary.log
    findstr /C:"Embarcadero Delphi for" /C:"Copyright (c)" /C:"Zeilen," /C:"lines," /C:"Max Compiler Memory" ..\build_mormot.log >> ..\summary.log
    findstr /C:"Embarcadero Delphi for" /C:"Copyright (c)" /C:"Zeilen," /C:"lines," /C:"Max Compiler Memory" ..\build_mormot.log
)
cd ..

echo. 

echo [3/4] Compiling DelphiGenerics...
cd DelphiGenerics
powershell -NoProfile -ExecutionPolicy Bypass -File "%PS_HELPER%" "..\..\..\_BuildBase.bat \"Benchmark_DelphiGenerics.dproj\"" > ..\build_delphi.log
set BUILD_ERROR=%ERRORLEVEL%
timeout /t 1 /nobreak > nul
if %BUILD_ERROR% neq 0 (
    echo BUILD FAILED! See build_delphi.log
    type ..\build_delphi.log
) else (
    echo. >> ..\summary.log
    echo [DelphiGenerics] >> ..\summary.log
    findstr /C:"Embarcadero Delphi for" /C:"Copyright (c)" /C:"Zeilen," /C:"lines," /C:"Max Compiler Memory" ..\build_delphi.log >> ..\summary.log
    findstr /C:"Embarcadero Delphi for" /C:"Copyright (c)" /C:"Zeilen," /C:"lines," /C:"Max Compiler Memory" ..\build_delphi.log
)
cd ..

echo. 

echo [4/4] Compiling SpringGenerics...
cd SpringGenerics
powershell -NoProfile -ExecutionPolicy Bypass -File "%PS_HELPER%" "..\..\..\_BuildBase.bat \"Benchmark_SpringGenerics.dproj\"" > ..\build_spring.log
set BUILD_ERROR=%ERRORLEVEL%
timeout /t 1 /nobreak > nul
if %BUILD_ERROR% neq 0 (
    echo BUILD FAILED! See build_spring.log
    echo. >> ..\summary.log
    echo [SpringGenerics - FAILED] >> ..\summary.log
    findstr /C:"Max Compiler Memory" ..\build_spring.log >> ..\summary.log
    findstr /C:"Max Compiler Memory" ..\build_spring.log
) else (
    echo. >> ..\summary.log
    echo [SpringGenerics] >> ..\summary.log
    findstr /C:"Embarcadero Delphi for" /C:"Copyright (c)" /C:"Zeilen," /C:"lines," /C:"Max Compiler Memory" ..\build_spring.log >> ..\summary.log
    findstr /C:"Embarcadero Delphi for" /C:"Copyright (c)" /C:"Zeilen," /C:"lines," /C:"Max Compiler Memory" ..\build_spring.log
)
cd ..

echo. 

:: Cleanup
if exist "%PS_HELPER%" del "%PS_HELPER%"

echo Benchmark Complete.
popd
