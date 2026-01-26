@echo off
chcp 65001 > nul
setlocal
pushd %~dp0

..\DPT.exe RECENT Build "Test.DPT.dproj" Win32 Debug
if %ERRORLEVEL% neq 0 (
    echo Build failed.
    popd
    exit /b %ERRORLEVEL%
)

popd
endlocal