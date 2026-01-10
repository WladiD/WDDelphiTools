@echo off
chcp 65001 > nul
setlocal
pushd %~dp0

call ..\..\_BuildAndRunBase.bat "DcuCompileTimes.exe" "_DcuCompileTimes.Build.bat" %*

if %ERRORLEVEL% neq 0 (
    popd
    exit /b %ERRORLEVEL%
)

popd
endlocal