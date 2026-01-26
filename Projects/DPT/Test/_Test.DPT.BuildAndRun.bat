@echo off
chcp 65001 > nul
setlocal
pushd %~dp0

..\DPT.exe RECENT BuildAndRun "Test.DPT.dproj" Win32 Debug --OnlyIfChanged
if %ERRORLEVEL% neq 0 (
    echo Build or Run failed.
    popd
    exit /b %ERRORLEVEL%
)

popd
endlocal