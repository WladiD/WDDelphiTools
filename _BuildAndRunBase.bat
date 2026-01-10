@echo off
chcp 65001 > nul
setlocal

REM ==================================================================================
REM Base BuildAndRun script for Delphi Example projects.
REM Usage: call ..\_BuildAndRunBase.bat ExePath BuildScriptPath [Args for Exe...]
REM ==================================================================================

if "%~1"=="" (
    echo ERROR: Please specify the executable path as the first parameter.
    exit /b 1
)

if "%~2"=="" (
    echo ERROR: Please specify the build script path as the second parameter.
    exit /b 1
)

set "EXE_PATH=%~f1"
set "BUILD_SCRIPT=%~f2"
set "EXE_NAME=%~nx1"
set "BUILD_DIR=%~dp2"
set "BUILD_CMD=%~nx2"

set "DO_BUILD=0"

if not exist "%EXE_PATH%" (
    echo %EXE_NAME% not found.
    set "DO_BUILD=1"
) else (
    powershell -NoProfile -Command "if ((Get-Item '%EXE_PATH%').LastWriteTime -lt (Get-Date).AddMinutes(-5)) { exit 1 } else { exit 0 }"
    if errorlevel 1 (
        echo %EXE_NAME% is older than 5 minutes.
        set "DO_BUILD=1"
    )
)

if "%DO_BUILD%"=="0" goto :run

echo Starting build process for %EXE_NAME%...
if not exist "%BUILD_SCRIPT%" (
    echo ERROR: Build script "%BUILD_SCRIPT%" not found.
    exit /b 1
)

pushd "%BUILD_DIR%"
call "%BUILD_CMD%"
set "BUILD_EXIT_CODE=%ERRORLEVEL%"
popd

if %BUILD_EXIT_CODE% neq 0 (
    echo Build failed.
    exit /b %BUILD_EXIT_CODE%
)

:run
rem Shift arguments to pass remaining parameters to the exe
shift
shift

rem Execute the exe in the original directory with remaining parameters
"%EXE_PATH%" %1 %2 %3 %4 %5 %6 %7 %8 %9