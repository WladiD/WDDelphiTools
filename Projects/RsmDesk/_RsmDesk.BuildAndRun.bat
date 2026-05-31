@echo off
chcp 65001 > nul
setlocal
set "PROJ_DIR=%~dp0"

REM ==================================================================
REM Build and run RsmDesk (FMX, Win64) via the DPT build host.
REM ==================================================================

if "%BUILD_CONFIG%"=="" set BUILD_CONFIG=Debug

if not exist "..\DPT\DPT.exe" (
    echo ERROR: ..\DPT\DPT.exe build host not found.
    exit /b 1
)

"%PROJ_DIR%..\DPT\DPT.exe" LATEST BuildAndRun "%PROJ_DIR%Source\RsmDesk.dproj" Win64 %BUILD_CONFIG% --OnlyIfChanged --NoWait -- %*
set EXIT_CODE=%ERRORLEVEL%

exit /b %EXIT_CODE%
endlocal
