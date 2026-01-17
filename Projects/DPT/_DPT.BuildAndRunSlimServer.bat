@echo off
chcp 65001 > nul
setlocal
set "PROJ_DIR=%~dp0"
taskkill /IM DPT.exe /F 2>nul
del "%PROJ_DIR%DPT.exe" 2>nul
set "BUILD_CONFIG=FitNesse"
pushd "%PROJ_DIR%"
call _DPT.Build.bat
if errorlevel 1 (
    echo BUILD FAILED!
    popd
    exit /b 1
)
popd
"%PROJ_DIR%DPT.exe" %*
endlocal