@echo off
chcp 65001 > nul
setlocal
pushd %~dp0

call ..\..\_BuildBase.bat "DPT.dproj" %1

if %ERRORLEVEL% neq 0 (
    popd
    exit /b %ERRORLEVEL%
)

popd
endlocal