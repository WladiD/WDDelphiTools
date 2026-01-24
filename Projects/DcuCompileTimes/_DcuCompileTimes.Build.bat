@echo off
chcp 65001 > nul
setlocal
pushd %~dp0

..\DPT\DPT.exe RECENT Build "DcuCompileTimes.dproj" %1

if %ERRORLEVEL% neq 0 (
    popd
    exit /b %ERRORLEVEL%
)

popd
endlocal