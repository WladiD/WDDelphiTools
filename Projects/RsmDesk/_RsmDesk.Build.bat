@echo off
chcp 65001 > nul
setlocal
pushd %~dp0

REM ==================================================================
REM Build RsmDesk (FMX, Win64) via the project's own DPT build host.
REM RsmDesk does not build itself, so the host is ..\DPT\DPT.exe.
REM No shadow copy needed: DPT only builds RsmDesk.exe, never overwrites
REM itself, and a running/locked DPT.exe can still be launched again.
REM ==================================================================

if "%BUILD_CONFIG%"=="" set BUILD_CONFIG=Debug

if not exist "..\DPT\DPT.exe" (
    echo ERROR: ..\DPT\DPT.exe build host not found.
    popd
    exit /b 1
)

echo.
echo ------------------------------------------
echo  Building RsmDesk  (Win64 %BUILD_CONFIG%)
echo ------------------------------------------
echo.

"..\DPT\DPT.exe" LATEST Build "Source\RsmDesk.dproj" Win64 %BUILD_CONFIG%
set BUILD_ERROR=%ERRORLEVEL%

if %BUILD_ERROR% neq 0 (
    echo Build failed with exit code %BUILD_ERROR%.
    popd
    exit /b %BUILD_ERROR%
)

popd
endlocal
