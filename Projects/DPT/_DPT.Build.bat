@echo off
chcp 65001 > nul
setlocal
pushd %~dp0

call ..\..\_BuildBase.bat "DPT.dproj" Win32 %BUILD_CONFIG%

if %ERRORLEVEL% neq 0 (
    popd
    exit /b %ERRORLEVEL%
)

popd
endlocal